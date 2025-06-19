use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use itertools::Itertools as _;

use crate::{
    bot_logic::{bot_logic::LogicStack, MenuCloser, MovementGoal},
    game_state::{
        Chest, Inventory, Item, ItemId, Key, ObjectKind, Quality, Vector,
    },
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, GameStateExt as _,
};

const MAX_GP_PER_STAMINA: f32 = 2.5;

pub struct InventoryGoal {
    bounds: HashMap<ItemId, Bounds>,

    /// If true, any item not mentioned in `bounds` will be treated as
    /// if it has a maximum of zero items, and will be stored in a
    /// chest.  If false, any item not mentioned in `bounds` will be
    /// treated as if it has `Bounds{min: None, max: None}`, and will
    /// not be transferred into or out of the player's inventory.
    stash_unspecified_items: bool,

    /// If true, try to have one slot of items to eat for stamina
    /// recovery.
    with_stamina_recovery: bool,
}

struct Bounds {
    min: Option<usize>,
    max: Option<usize>,
}

struct Transfer {
    chest: Vector<isize>,
    direction: TransferDirection,
    slot: usize,
    size: TransferSize,
}

enum TransferDirection {
    PlayerToChest,
    ChestToPlayer,
}

pub(super) enum TransferSize {
    /// Ship the entire stack by left-clicking
    All,

    /// Ship half of the stack by shift + right-click
    Half,

    /// Ship one item by right-clicking
    One,
}

impl InventoryGoal {
    pub fn new(item: Item) -> Self {
        let bounds = [(
            item.id,
            Bounds {
                min: Some(item.count),
                max: Some(item.count),
            },
        )]
        .into_iter()
        .collect();
        Self {
            bounds,
            stash_unspecified_items: false,
            with_stamina_recovery: false,
        }
    }

    pub fn current() -> Self {
        Self {
            bounds: Default::default(),
            stash_unspecified_items: false,
            with_stamina_recovery: false,
        }
    }

    pub fn empty() -> Self {
        Self {
            bounds: Default::default(),
            stash_unspecified_items: true,
            with_stamina_recovery: false,
        }
    }

    #[allow(dead_code)]
    pub fn ignoring(mut self, item: impl AsRef<ItemId>) -> Self {
        self.bounds.insert(
            item.as_ref().clone(),
            Bounds {
                min: None,
                max: None,
            },
        );
        self
    }

    pub fn with(mut self, item: Item) -> Self {
        let count = item.count;
        self.bounds.insert(
            item.id,
            Bounds {
                min: Some(count),
                max: None,
            },
        );
        self
    }

    pub fn with_stamina_recovery(self) -> Self {
        Self {
            with_stamina_recovery: true,
            ..self
        }
    }

    pub fn otherwise_empty(self) -> Self {
        Self {
            stash_unspecified_items: true,
            ..self
        }
    }
}

impl InventoryGoal {
    fn next_transfer(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Transfer>, Error> {
        // Iterator of (tile, &Chest)
        let iter_chests = || {
            game_state.get_room("Farm").map(|location| {
                location.objects.iter().filter_map(|obj| {
                    obj.kind.as_chest().map(|chest| (obj.tile, chest))
                })
            })
        };

        // Iterator of (tile, slot, &Item)
        let iter_chest_items = || {
            iter_chests().map(|iter| {
                iter.flat_map(|(tile, chest)| {
                    chest
                        .iter_filled_slots()
                        .map(move |(slot, item)| (tile, slot, item))
                })
            })
        };

        let inventory = &game_state.player.inventory;
        // Current contents of the player inventory
        let player_contents: HashMap<ItemId, usize> = inventory.to_hash_map();
        let player_has_empty_slot = inventory.empty_slot().is_some();

        let preferred_chest: HashMap<ItemId, Vector<isize>> = iter_chests()?
            .flat_map(|(tile, chest)| {
                let has_empty_slot = chest.empty_slot().is_some();
                chest
                    .iter_items()
                    .filter(move |item| has_empty_slot || !item.is_full_stack())
                    .map(move |item| (item.id.clone(), tile))
            })
            .collect();

        let opt_stamina_item = self
            .with_stamina_recovery
            .then(|| {
                game_state
                    .player
                    .inventory
                    .iter_items()
                    .filter(|item| {
                        // If an item is explicitly set to be stored
                        // away, it may not be used as the stamina
                        // recovery item.
                        self.bounds
                            .get(&item.id)
                            .map(|bounds| !matches!(bounds.max, Some(0)))
                            .unwrap_or(true)
                    })
                    .filter_map(|item| {
                        item.gp_per_stamina().map(|gp| (gp, item))
                    })
                    .filter(|(gp, _)| *gp <= MAX_GP_PER_STAMINA)
                    .min_by(|(lhs_gp, _), (rhs_gp, _)| {
                        lhs_gp.total_cmp(&rhs_gp)
                    })
                    .map(|(_, item)| item)
            })
            .flatten();

        // Items that should be transferred from the player to a
        // chest.  Values are the desired number of items in the
        // player's inventory after the tranfer.
        //
        // Includes the following:
        //
        // - All items that are above an explicit maximum.
        //
        // - All unmentioned items if `stash_unspecifited_items` is
        //   enabled, except for the selected stamina item if
        //   `with_stamina_recovery` is enabled.
        let player_to_chest: HashMap<ItemId, usize> = player_contents
            .iter()
            .filter_map(|(item, count)| {
                let opt_new_count = if let Some(bound) = self.bounds.get(item) {
                    bound.max.filter(|max| max < count)
                } else if self.stash_unspecified_items
                    && opt_stamina_item
                        .map(|stamina| !stamina.is_same_item(&item))
                        .unwrap_or(true)
                {
                    Some(0)
                } else {
                    None
                };

                opt_new_count.map(|new_count| (item.clone(), new_count))
            })
            .collect();

        // Items that should be transferred from a chest to the
        // player.  Values are the desired number of items in the
        // player's inventory after the tranfer.
        let chest_to_player: HashMap<ItemId, usize> = self
            .bounds
            .iter()
            .filter_map(|(item, bound)| {
                let current = player_contents.get(item).cloned().unwrap_or(0);
                bound
                    .min
                    .filter(|&min| current < min)
                    .map(|new_count| (item.clone(), new_count))
            })
            .collect();

        if let Some(chest) = &game_state.chest_menu {
            let Some(tile) = chest.chest_tile else {
                // This chest doesn't exist at any location, such as
                // the "chest" displaying fishing rewards.  Not part
                // of the responsibility of `InventoryGoal`.
                return Ok(None);
            };

            let chest_has_empty_slot = chest.chest_items.empty_slot().is_some();

            let opt_transfer_to_chest = inventory
                .iter_filled_slots()
                .filter(|(_, item)| {
                    if let Some(preferred) = preferred_chest.get(&item.id) {
                        *preferred == tile
                    } else {
                        chest_has_empty_slot
                    }
                })
                .find_map(|(slot, item)| {
                    let goal = player_to_chest.get(&item.id).cloned()?;
                    let transfer_size = TransferSize::select(item.count, goal);
                    Some(Transfer {
                        chest: tile,
                        direction: TransferDirection::PlayerToChest,
                        slot,
                        size: transfer_size,
                    })
                });
            if opt_transfer_to_chest.is_some() {
                // There is a chest currently open, and it is either
                // the preferred chest to receive an item from the
                // player, or there is no preferred chest but this one
                // has spare slots.  Therefore, transfer those items
                // to the chest.
                return Ok(opt_transfer_to_chest);
            }

            let opt_transfer_to_player = chest
                .chest_items
                .iter_filled_slots()
                .find_map(|(slot, item)| {
                    let goal = chest_to_player.get(&item.id)?;
                    let current = player_contents
                        .get(&item.id)
                        .cloned()
                        .or_else(|| player_has_empty_slot.then(|| 0))?;
                    let transfer_size = TransferSize::select(
                        item.count,
                        item.count + current - goal,
                    );
                    Some(Transfer {
                        chest: tile,
                        direction: TransferDirection::ChestToPlayer,
                        slot,
                        size: transfer_size,
                    })
                });
            if opt_transfer_to_player.is_some() {
                // There is a chest open, and it contains items that
                // the player wants.  The player either has a partial
                // stack of that item type, or has an empty slot
                // available.  Therefore, transfer those items into
                // the player's inventory.
                return Ok(opt_transfer_to_player);
            }

            let opt_take_stamina_item = (opt_stamina_item.is_none()
                && self.with_stamina_recovery
                && player_has_empty_slot)
                .then(|| {
                    chest
                        .chest_items
                        .iter_filled_slots()
                        .filter(|(_, item)| {
                            item.gp_per_stamina()
                                .map(|gp| gp < MAX_GP_PER_STAMINA)
                                .unwrap_or(false)
                        })
                        .min_by(|(_, lhs), (_, rhs)| {
                            let lhs = lhs.gp_per_stamina().unwrap();
                            let rhs = rhs.gp_per_stamina().unwrap();
                            lhs.total_cmp(&rhs)
                        })
                        .map(|(slot, _)| Transfer {
                            chest: tile,
                            direction: TransferDirection::ChestToPlayer,
                            slot,
                            size: TransferSize::All,
                        })
                })
                .flatten();

            if opt_take_stamina_item.is_some() {
                // There is a chest open, and it contains an item that
                // may be consumed for stamina.  The player doesn't
                // currently have an item that may be consumed for
                // stamina, so it should be taken from the chest.
                return Ok(opt_take_stamina_item.into());
            }
        }

        let opt_open_preferred_chest = preferred_chest
            .iter()
            .filter(|(item, _)| player_to_chest.contains_key(item))
            .min_by_key(|(_, tile)| (tile.down, -tile.right))
            .map(|(item, tile)| {
                let current = player_contents.get(item).cloned()?;
                let goal = player_to_chest.get(item).cloned()?;
                let transfer_size = TransferSize::select(current, goal);
                let slot = inventory.current_slot(&item)?;
                Some(Transfer {
                    chest: *tile,
                    direction: TransferDirection::PlayerToChest,
                    slot,
                    size: transfer_size,
                })
            })
            .flatten();
        if opt_open_preferred_chest.is_some() {
            // The player has items to store, and the item already
            // exists in one of the storage chests.  Open that chest
            // to store items.
            return Ok(opt_open_preferred_chest);
        }

        let opt_retrieve_from_chest =
            iter_chest_items()?.find_map(|(tile, slot, item)| {
                let current =
                    player_contents.get(&item.id).cloned().unwrap_or(0);
                let goal = chest_to_player.get(&item.id).cloned()?;
                let transfer_size = TransferSize::select(
                    item.count,
                    item.count + current - goal,
                );
                Some(Transfer {
                    chest: tile,
                    direction: TransferDirection::ChestToPlayer,
                    slot,
                    size: transfer_size,
                })
            });
        if opt_retrieve_from_chest.is_some() {
            // There's an items the player wants in the inventory, and
            // a chest that already contains some of that item.  Open
            // the chest to retrieve the item.
            return Ok(opt_retrieve_from_chest);
        }

        let opt_retrieve_stamina_item = (opt_stamina_item.is_none()
            && self.with_stamina_recovery
            && player_has_empty_slot)
            .then(|| {
                iter_chest_items().map(|iter| {
                    iter.filter(|(_, _, item)| {
                        item.gp_per_stamina()
                            .map(|gp| gp < MAX_GP_PER_STAMINA)
                            .unwrap_or(false)
                    })
                    .min_by(|(_, _, lhs), (_, _, rhs)| {
                        let lhs = lhs.gp_per_stamina().unwrap();
                        let rhs = rhs.gp_per_stamina().unwrap();
                        lhs.total_cmp(&rhs)
                    })
                    .map(|(tile, slot, _)| Transfer {
                        chest: tile,
                        direction: TransferDirection::ChestToPlayer,
                        slot,
                        size: TransferSize::All,
                    })
                })
            })
            .transpose()?
            .flatten();
        if opt_retrieve_stamina_item.is_some() {
            // The player would like to pick up a consumable to
            // restore stamina, doesn't currently have any such item,
            // and there is a consumable available in a chest.  Open
            // the chest to retrieve it.
            return Ok(opt_retrieve_stamina_item);
        }

        let opt_store_in_empty_slot = iter_chests()?
            .filter(|(_, chest)| chest.inventory.empty_slot().is_some())
            .min_by_key(|(tile, _)| (tile.down, -tile.right))
            .and_then(|(tile, _)| {
                player_to_chest
                    .iter()
                    .next()
                    .map(|(item, goal)| {
                        let slot = inventory.current_slot(item)?;
                        let current = player_contents.get(item).cloned()?;
                        let transfer_size =
                            TransferSize::select(current, *goal);
                        Some(Transfer {
                            chest: tile,
                            direction: TransferDirection::PlayerToChest,
                            slot,
                            size: transfer_size,
                        })
                    })
                    .flatten()
            });
        if opt_store_in_empty_slot.is_some() {
            // There's an item the player wants in the inventory, but
            // no chest has some of that item stored.  Open a chest
            // with empty space to store the item.
            return Ok(opt_store_in_empty_slot);
        }

        Ok(None)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.next_transfer(game_state)?.is_none())
    }
}

impl BotGoal for InventoryGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        if self.bounds.len() == 1 {
            let (item_ty, bounds) = self.bounds.iter().next().unwrap();

            let id = &item_ty.item_id;
            match (bounds.min, bounds.max) {
                (Some(1), Some(1)) => format!("Pick up {id}").into(),
                (Some(min), Some(max)) => {
                    format!("Inventory, {id} between {min} and {max}").into()
                }
                (Some(min), None) => {
                    format!("Inventory, {id} at least {min}").into()
                }
                (None, Some(max)) => {
                    format!("Inventory, {id} at most {max}").into()
                }
                (None, None) => format!("Inventory, {id} no change").into(),
            }
        } else {
            "Manage Inventory".into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some(transfer) = self.next_transfer(game_state)? else {
            // The inventory transfer is complete.  Close out the
            // chest menu if it is still open, and return control to
            // the parent goal.
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(chest_menu) = &game_state.chest_menu else {
            // There is no chest open.  Open the chest for the next
            // transfer.
            let goal = ActivateTile::new("Farm", transfer.chest);
            return Ok(goal.into());
        };

        if chest_menu.chest_tile != Some(transfer.chest) {
            // There's a chest open, but it isn't the one we want to
            // be open.  Close it.
            return Ok(MenuCloser::new().into());
        }

        let pixel = match transfer.direction {
            TransferDirection::PlayerToChest => {
                chest_menu.player_item_locations[transfer.slot]
            }
            TransferDirection::ChestToPlayer => {
                chest_menu.chest_item_locations[transfer.slot]
            }
        };
        actions.do_action(GameAction::MouseOverPixel(pixel));
        transfer.size.send_inputs(game_state, actions);

        Ok(BotGoalResult::InProgress)
    }
}

impl TransferSize {
    /// Determine the best transfer size in order to go from a stack
    /// of `current` items down to a a stack of `goal` items.
    pub fn select(current: usize, goal: usize) -> Self {
        assert!(current >= goal);
        if goal == 0 {
            Self::All
        } else if goal <= current / 2 {
            Self::Half
        } else {
            Self::One
        }
    }

    /// Send the inputs required to transfer items between the player
    /// and a chest.
    ///
    /// Before calling this method, the mouse should be positioned
    /// above the item to be transferred.
    pub fn send_inputs(
        &self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) {
        match self {
            TransferSize::All => {
                actions.do_action(GameAction::LeftClick);
            }
            TransferSize::Half => {
                actions.do_action(GameAction::HoldLeftShift);
                if game_state.inputs.holding_left_shift() {
                    actions.do_action(GameAction::RightClick);
                }
            }
            TransferSize::One => {
                if !game_state.inputs.holding_left_shift() {
                    actions.do_action(GameAction::RightClick);
                }
            }
        }
    }
}
