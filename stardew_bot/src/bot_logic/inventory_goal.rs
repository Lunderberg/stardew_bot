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

pub struct InventoryGoal {
    bounds: HashMap<ItemId, Bounds>,

    /// If true, any item not mentioned in `bounds` will be treated as
    /// if it has a maximum of zero items, and will be stored in a
    /// chest.  If false, any item not mentioned in `bounds` will be
    /// treated as if it has `Bounds{min: None, max: None}`, and will
    /// not be transferred into or out of the player's inventory.
    stash_unspecified_items: bool,
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
        }
    }

    pub fn current() -> Self {
        Self {
            bounds: Default::default(),
            stash_unspecified_items: false,
        }
    }

    pub fn empty() -> Self {
        Self {
            bounds: Default::default(),
            stash_unspecified_items: true,
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
}

impl InventoryGoal {
    fn next_transfer(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Transfer>, Error> {
        let iter_chests = || {
            game_state.get_room("Farm").map(|location| {
                location.objects.iter().filter_map(|obj| {
                    obj.kind.as_chest().map(|chest| (obj.tile, chest))
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

        // Items that should be transferred from the player to a
        // chest.  Values are the desired number of items in the
        // player's inventory after the tranfer.
        let player_to_chest: HashMap<ItemId, usize> = player_contents
            .iter()
            .filter_map(|(item, count)| {
                let opt_new_count = if let Some(bound) = self.bounds.get(item) {
                    bound.max.filter(|max| max < count)
                } else if self.stash_unspecified_items {
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
                .iter_slots()
                .enumerate()
                .filter_map(|(slot, opt_item)| {
                    opt_item.map(|item| (slot, item))
                })
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
                .iter_slots()
                .enumerate()
                .filter_map(|(slot, opt_item)| {
                    opt_item.map(|item| (slot, item))
                })
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
        }

        // If there is a preferred chest to load up, open it.
        let opt_open_preferred_chest = preferred_chest
            .iter()
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
            return Ok(opt_open_preferred_chest);
        }

        // If there are items to retrieve from a chest, open that
        // chest.
        let opt_retrieve_from_chest = iter_chests()?
            .flat_map(|(tile, chest)| {
                chest.iter_slots().enumerate().filter_map(
                    move |(slot, opt_item)| {
                        opt_item.map(|item| (tile, slot, item))
                    },
                )
            })
            .find_map(|(tile, slot, item)| {
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
            return Ok(opt_retrieve_from_chest);
        }

        // If there are items to stash in an arbitrary chest, open
        // whichever chest has space.
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
