use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use geometry::Vector;
use itertools::Itertools as _;

use crate::{
    bot_logic::LogicStack, Error, GameAction, ItemIterExt as _,
    ItemLookupExt as _, MenuCloser,
};
use game_state::{
    GameState, Item, ItemCategory, ItemId, ItemSet, Location, WeaponKind,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStackItem},
    ActivateTile, CraftItemGoal, GameStateExt as _, LocationExt as _,
};

const MAX_GP_PER_STAMINA: f32 = 2.5;

pub struct InventoryGoal {
    /// Which room contains the storage chests to be interacted with.
    room: Cow<'static, str>,

    /// Specifies which items should be added/removed to the
    /// inventory.
    bounds: HashMap<ItemId, Bounds>,

    /// If true, any item not mentioned in `bounds` will be treated as
    /// if it has a maximum of zero items, and will be stored in a
    /// chest.  If false, any item not mentioned in `bounds` will be
    /// treated as if it has `Bounds{min: None, max: None}`, and will
    /// not be transferred into or out of the player's inventory.
    stash_unspecified_items: bool,

    /// If present, any item for which this function returns true will
    /// be kept in the inventory, even if the
    /// `stash_unspecified_items` is enabled.
    keep_if: Option<Box<dyn Fn(&Item) -> bool>>,

    /// If present, any item for which this function returns true will
    /// be taken from the storage chests, even if the
    /// `stash_unspecified_items` is enabled.
    take_if: Option<Box<dyn Fn(&Item) -> bool>>,

    /// If true, try to use this many slots to hold items to eat for
    /// stamina recovery.
    stamina_recovery_slots: usize,

    /// Categories of items to be picked up.  If an item is required
    /// for a bundle, it will not be picked up by category.
    with_categories: Vec<ItemCategory>,

    /// If true, keep the best weapon in the inventory.
    with_weapon: bool,

    /// If true, craft any items that are missing.
    craft_missing: bool,

    /// A read may occur after the player's inventory has been updated
    /// but before the game location has been updated.  This will
    /// appear as if the player has insufficient seeds/materials, and
    /// may cause the bot to head to the storage chests to craft
    /// additional materials.  To avoid this, this field tracks the
    /// game tick on which the decision to buy/craft was made.  The
    /// decision is only applied if made again on the following frame.
    tick_decided_to_gather_items: Option<i32>,
}

struct Bounds {
    min: Option<usize>,
    max: Option<usize>,
}

#[derive(Debug)]
struct Transfer {
    chest: Vector<isize>,
    direction: TransferDirection,
    slot: usize,
    size: TransferSize,
}

#[derive(Debug)]
enum TransferDirection {
    PlayerToChest,
    ChestToPlayer,
}

#[derive(Debug)]
pub(super) enum TransferSize {
    /// Ship the entire stack by left-clicking
    All,

    /// Ship half of the stack by shift + right-click
    Half,

    /// Ship one item by right-clicking
    One,
}

impl InventoryGoal {
    pub fn room(self, room: impl Into<Cow<'static, str>>) -> Self {
        let room = room.into();
        Self { room, ..self }
    }

    pub fn new(item: impl Into<Item>) -> Self {
        let item = item.into();
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
            ..Self::current()
        }
    }

    pub fn current() -> Self {
        Self {
            room: "Farm".into(),
            bounds: Default::default(),
            stash_unspecified_items: false,
            keep_if: None,
            take_if: None,
            stamina_recovery_slots: 0,
            with_categories: Vec::new(),
            with_weapon: false,
            craft_missing: false,
            tick_decided_to_gather_items: None,
        }
    }

    pub fn empty() -> Self {
        Self {
            stash_unspecified_items: true,
            ..Self::current()
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

    pub fn with_exactly(mut self, item_set: impl ItemSet) -> Self {
        for item in item_set.iter_item_set() {
            let count = item.count;
            self.bounds.insert(
                item.id,
                Bounds {
                    min: Some(count),
                    max: Some(count),
                },
            );
        }
        self
    }

    pub fn with(mut self, item_set: impl ItemSet) -> Self {
        for item in item_set.iter_item_set() {
            let count = item.count;
            self.bounds.insert(
                item.id,
                Bounds {
                    min: Some(count),
                    max: None,
                },
            );
        }
        self
    }

    pub fn with_category(mut self, category: ItemCategory) -> Self {
        self.with_categories.push(category);
        self
    }

    pub fn with_weapon(self) -> Self {
        Self {
            with_weapon: true,
            ..self
        }
    }

    pub fn craft_missing(self, craft_missing: bool) -> Self {
        Self {
            craft_missing,
            ..self
        }
    }

    pub fn keep_if(self, func: impl Fn(&Item) -> bool + 'static) -> Self {
        Self {
            keep_if: Some(Box::new(func)),
            ..self
        }
    }

    pub fn take_if(self, func: impl Fn(&Item) -> bool + 'static) -> Self {
        Self {
            take_if: Some(Box::new(func)),
            ..self
        }
    }

    pub fn stamina_recovery_slots(self, stamina_recovery_slots: usize) -> Self {
        Self {
            stamina_recovery_slots,
            ..self
        }
    }

    pub fn otherwise_empty(self) -> Self {
        Self {
            stash_unspecified_items: true,
            ..self
        }
    }

    fn get_room<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<&'a Location, Error> {
        let room = game_state.get_room(&self.room)?;
        Ok(room)
    }
}

pub(super) fn best_weapon<'a>(
    iter: impl IntoIterator<Item = &'a Item>,
) -> Option<&'a Item> {
    iter.into_iter()
        .filter(|item| item.as_weapon().is_some())
        .max_by_key(|item| {
            let as_weapon = item.as_weapon().unwrap();
            let kind = match &as_weapon.kind {
                WeaponKind::Club => 2,
                WeaponKind::Sword => 1,
                WeaponKind::Dagger => 0,
            };
            (kind, as_weapon.min_damage)
        })
}

impl InventoryGoal {
    fn next_crafting_action(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStackItem>, Error> {
        if !self.craft_missing {
            return Ok(None);
        }

        let available = self.iter_stored_and_carried(game_state)?.item_counts();
        for (item, bound) in
            self.bounds.iter().sorted_by_key(|(item, _)| &item.item_id)
        {
            let Some(min) = bound.min else {
                continue;
            };
            let current = available.item_count(item);
            let num_missing = min.saturating_sub(current);

            if num_missing == 0 {
                continue;
            }

            let iter_ingredients = || item.iter_recipe().into_iter().flatten();

            let num_craftable = iter_ingredients()
                .map(|(ingredient, count)| {
                    let ingredient_available =
                        available.item_count(&ingredient);
                    ingredient_available / count
                })
                .min()
                .unwrap_or(0);

            let num_to_craft = num_missing.min(num_craftable);
            if num_to_craft == 0 {
                continue;
            }

            let subgoal = InventoryGoal::current()
                .room(self.room.clone())
                .with(iter_ingredients().flat_map(|(ingredient, count)| {
                    available
                        .iter_items_with_quality(
                            ingredient.with_count(num_to_craft * count),
                        )
                        .map(|(id, count)| id.with_count(count))
                }));
            if !subgoal.is_completed(game_state)? {
                return Ok(Some(subgoal.into()));
            }

            let num_in_inventory = game_state.player.inventory.count_item(item);
            let craft = CraftItemGoal::new(
                item.clone().with_count(num_in_inventory + num_to_craft),
            );
            return Ok(Some(craft.into()));
        }

        Ok(None)
    }

    fn next_transfer(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Transfer>, Error> {
        // Iterator of (tile, &Chest)
        let iter_chests = || {
            self.get_room(game_state).map(|location| {
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

        let chest_contents: HashMap<ItemId, usize> = iter_chest_items()?
            .map(|(_, _, item)| (&item.id, item.count))
            .item_counts();

        let preferred_chest: HashMap<ItemId, Vector<isize>> = iter_chests()?
            .flat_map(|(tile, chest)| {
                let has_empty_slot = chest.empty_slot().is_some();
                chest
                    .iter_items()
                    .filter(move |item| has_empty_slot || !item.is_full_stack())
                    .map(move |item| (item.id.clone(), tile))
            })
            .collect();

        let reserved_items: HashMap<ItemId, usize> = {
            let all_items = iter_chest_items()?
                .map(|(_, _, item)| item)
                .chain(inventory.iter_items())
                .item_counts();
            game_state
                .iter_reserved_items()?
                .flat_map(|item| all_items.iter_items_with_quality(item))
                .item_counts()
        };

        let current_stamina_items: HashSet<&ItemId> = inventory
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
            .filter(|item| {
                let num_reserved = reserved_items.item_count(&item.id);
                let num_stored = chest_contents.item_count(&item.id);
                num_stored >= num_reserved
            })
            .filter_map(|item| item.gp_per_stamina().map(|gp| (gp, item)))
            .filter(|(gp, _)| *gp <= MAX_GP_PER_STAMINA)
            .sorted_by(|(lhs_gp, _), (rhs_gp, _)| lhs_gp.total_cmp(rhs_gp))
            .map(|(_, item)| &item.id)
            .take(self.stamina_recovery_slots)
            .collect();

        let opt_current_weapon = best_weapon(
            inventory
                .iter_items()
                .filter(|_| self.with_weapon)
                .filter(|item| item.id != ItemId::SCYTHE),
        )
        .map(|item| &item.id);

        let is_desired_category = |item: &Item| -> bool {
            let explicitly_stored = matches!(
                self.bounds.get(&item.id).map(|bounds| bounds.max),
                Some(Some(0))
            );

            let is_correct_category = item
                .category
                .as_ref()
                .map(|item_category| {
                    self.with_categories.iter().any(|desired_category| {
                        item_category == desired_category
                    })
                })
                .unwrap_or(false);

            let num_reserved = reserved_items.item_count(&item.id);
            let exceeds_num_reserved = item.count > num_reserved;

            is_correct_category && exceeds_num_reserved && !explicitly_stored
        };

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
                let opt_new_count = 'opt_new_count: {
                    if let Some(bound) = self.bounds.get(item) {
                        // The player has more than the desired
                        // amount, should store excess.
                        break 'opt_new_count bound
                            .max
                            .filter(|max| max < count);
                    }

                    if !self.stash_unspecified_items {
                        // No further checks needed, as unspecified
                        // items are to be kept in the inventory.
                        break 'opt_new_count None;
                    }

                    if opt_current_weapon
                        .map(|weapon| item == weapon)
                        .unwrap_or(false)
                    {
                        if *count > 1 {
                            // The player has more than one copy of
                            // the same weapon, so stash the copies.
                            break 'opt_new_count Some(1);
                        } else {
                            // The player has a single instance of the
                            // weapon in their inventory, and should
                            // keep it.
                            break 'opt_new_count None;
                        }
                    }

                    if current_stamina_items.contains(&item) {
                        // This item is for stamina recovery, so keep
                        // it.
                        break 'opt_new_count None;
                    }

                    let full_item = game_state
                        .statics
                        .enrich_item(item.clone().into())
                        .with_count(*count);

                    if is_desired_category(&full_item) {
                        // This item is from a desired category, so keep
                        // it.
                        break 'opt_new_count None;
                    }

                    if let Some(keep_if) = &self.keep_if {
                        if keep_if(&full_item) {
                            // This item passes the check to be kept
                            // in the inventory.
                            break 'opt_new_count None;
                        }
                    }
                    if let Some(take_if) = &self.take_if {
                        if take_if(&full_item) {
                            // This item passes the check to be taken
                            // from the storage chests.
                            break 'opt_new_count None;
                        }
                    }

                    // No exceptions would keep the item in the
                    // player's inventory, so store it.
                    Some(0)
                };

                opt_new_count.map(|new_count| (item.clone(), new_count))
            })
            .collect();

        // Items that should be transferred from a chest to the
        // player.  Values are the desired number of items in the
        // player's inventory after the tranfer.
        let mut chest_to_player: HashMap<ItemId, usize> = self
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

        // Items that pass the `take_if` check should be grabbed out
        // of storage.
        if let Some(take_if) = &self.take_if {
            iter_chest_items()?
                .map(|(_, _, item)| item)
                .filter(|item| take_if(item))
                .for_each(|item| {
                    let count = chest_to_player
                        .entry(item.id.clone())
                        .or_insert_with(|| {
                            player_contents.get(&item.id).cloned().unwrap_or(0)
                        });
                    *count += item.count;
                });
        }

        // If the player has no weapon, wants one, and there's one
        // available to pick up, add it to the list of items to pick
        // up.
        if self.with_weapon && opt_current_weapon.is_none() {
            let opt_stored_weapon = best_weapon(
                iter_chest_items()?
                    .map(|(_, _, item)| item)
                    .filter(|item| item.id != ItemId::SCYTHE),
            )
            .map(|item| &item.id);
            if let Some(stored_weapon) = opt_stored_weapon {
                chest_to_player.insert(stored_weapon.clone(), 1);
            }
        }

        let is_stamina_item = |item: &Item| -> bool {
            // If an item is explicitly set to be stored
            // away, it may not be used as the stamina
            // recovery item.
            let explicitly_stored = matches!(
                self.bounds.get(&item.id).map(|bounds| bounds.max),
                Some(Some(0))
            );
            let cheap_to_eat = item
                .gp_per_stamina()
                .map(|gp| gp < MAX_GP_PER_STAMINA)
                .unwrap_or(false);

            let num_reserved = reserved_items.item_count(
                &ItemId::new(item.id.item_id.clone())
                    .with_quality(item.quality()),
            );
            let exceeds_num_reserved = item.count > num_reserved;

            cheap_to_eat && exceeds_num_reserved && !explicitly_stored
        };

        if let Some(chest) = game_state.chest_menu() {
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
                    let goal = *chest_to_player.get(&item.id)?;
                    let current = player_contents
                        .get(&item.id)
                        .cloned()
                        .or_else(|| player_has_empty_slot.then_some(0))?;
                    let transfer_size = TransferSize::select(
                        item.count,
                        (item.count + current).saturating_sub(goal),
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

            let opt_take_stamina_item = (current_stamina_items.len()
                < self.stamina_recovery_slots
                && player_has_empty_slot)
                .then(|| {
                    chest
                        .chest_items
                        .iter_filled_slots()
                        .filter(|(_, item)| is_stamina_item(item))
                        .min_by(|(_, lhs), (_, rhs)| {
                            let lhs = lhs.gp_per_stamina().unwrap();
                            let rhs = rhs.gp_per_stamina().unwrap();
                            lhs.total_cmp(&rhs)
                        })
                        .map(|(slot, item)| {
                            let goal_number = reserved_items
                                .get(&item.id)
                                .cloned()
                                .unwrap_or(0);
                            let transfer_size =
                                TransferSize::select(item.count, goal_number);
                            Transfer {
                                chest: tile,
                                direction: TransferDirection::ChestToPlayer,
                                slot,
                                size: transfer_size,
                            }
                        })
                })
                .flatten();

            if opt_take_stamina_item.is_some() {
                // There is a chest open, and it contains an item that
                // may be consumed for stamina.  The player doesn't
                // currently have an item that may be consumed for
                // stamina, so it should be taken from the chest.
                return Ok(opt_take_stamina_item);
            }

            let opt_take_category = (!self.with_categories.is_empty())
                .then(|| {
                    chest
                        .chest_items
                        .iter_filled_slots()
                        .filter(|(_, item)| is_desired_category(item))
                        .find_map(|(slot, item)| {
                            let goal_number = reserved_items
                                .get(&item.id)
                                .cloned()
                                .unwrap_or(0);
                            (goal_number < item.count).then(|| {
                                let transfer_size = TransferSize::select(
                                    item.count,
                                    goal_number,
                                );
                                Transfer {
                                    chest: tile,
                                    direction: TransferDirection::ChestToPlayer,
                                    slot,
                                    size: transfer_size,
                                }
                            })
                        })
                })
                .flatten();

            if opt_take_category.is_some() {
                // There is a chest open, and it contains an item that
                // is listed as a desired category.
                return Ok(opt_take_category);
            }
        }

        let opt_open_preferred_chest = preferred_chest
            .iter()
            .filter(|(item, _)| player_to_chest.contains_key(item))
            .min_by_key(|(_, tile)| (tile.down, -tile.right))
            .and_then(|(item, tile)| {
                let current = player_contents.get(item).cloned()?;
                let goal = player_to_chest.get(item).cloned()?;
                let transfer_size = TransferSize::select(current, goal);
                let slot = inventory.current_slot(item)?;
                Some(Transfer {
                    chest: *tile,
                    direction: TransferDirection::PlayerToChest,
                    slot,
                    size: transfer_size,
                })
            });
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
                    (item.count + current).saturating_sub(goal),
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

        let opt_retrieve_stamina_item = (current_stamina_items.len()
            < self.stamina_recovery_slots
            && player_has_empty_slot)
            .then(|| {
                iter_chest_items().map(|iter| {
                    iter.filter(|(_, _, item)| is_stamina_item(item))
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

        let opt_retrieve_category = (!self.with_categories.is_empty()
            && player_has_empty_slot)
            .then(|| {
                iter_chest_items().map(|mut iter| {
                    iter.find(|(_, _, item)| is_desired_category(item)).map(
                        |(tile, slot, _)| Transfer {
                            chest: tile,
                            direction: TransferDirection::ChestToPlayer,
                            slot,
                            size: TransferSize::All,
                        },
                    )
                })
            })
            .transpose()?
            .flatten();
        if opt_retrieve_category.is_some() {
            // The player would like to pick up items from a category,
            // and there are items of that category within a chest.
            // Open the chest to retrieve them.
            return Ok(opt_retrieve_category);
        }

        let opt_store_in_empty_slot = iter_chests()?
            .filter(|(_, chest)| chest.inventory.empty_slot().is_some())
            .min_by_key(|(tile, _)| (tile.down, -tile.right))
            .and_then(|(tile, _)| {
                player_to_chest.iter().next().and_then(|(item, goal)| {
                    let slot = inventory.current_slot(item)?;
                    let current = player_contents.get(item).cloned()?;
                    let transfer_size = TransferSize::select(current, *goal);
                    Some(Transfer {
                        chest: tile,
                        direction: TransferDirection::PlayerToChest,
                        slot,
                        size: transfer_size,
                    })
                })
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
        Ok(self.next_crafting_action(game_state)?.is_none()
            && self.next_transfer(game_state)?.is_none())
    }

    pub fn iter_stored_and_carried<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = &'a Item> + 'a, Error> {
        let iter_current = game_state.player.inventory.iter_items();

        let iter_stored =
            game_state.get_room(self.room.as_ref())?.iter_stored_items();

        Ok(iter_current.chain(iter_stored))
    }

    pub fn total_stored_and_carried<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<HashMap<&'a ItemId, usize>, Error> {
        let total: HashMap<&ItemId, usize> = self
            .iter_stored_and_carried(game_state)?
            .map(|item| (&item.id, item.count))
            .into_grouping_map()
            .sum();

        Ok(total)
    }

    pub fn has_sufficient_stored(
        &self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        let total = self.total_stored_and_carried(game_state)?;

        let can_reach_goal = self.bounds.iter().all(|(item, bound)| {
            bound
                .min
                .map(|min| {
                    let available = total.get(item).cloned().unwrap_or(0);
                    available >= min
                })
                .unwrap_or(true)
        });

        Ok(can_reach_goal)
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
        if let Some(next_crafting_action) =
            self.next_crafting_action(game_state)?
        {
            // In some cases, the update to the player's inventory can
            // occur before the update to the game location.  In that
            // case, the total number of seeds may appear to be less
            // than the required number of seeds.  To avoid running
            // off to the general store, wait until the choice to buy
            // seeds has been made for a few game ticks in a row.
            let tick = game_state.globals.game_tick;
            if let Some(prev_tick) = self.tick_decided_to_gather_items {
                if tick < prev_tick + 2 {
                    return Ok(BotGoalResult::InProgress);
                }
            } else {
                self.tick_decided_to_gather_items = Some(tick);
                return Ok(BotGoalResult::InProgress);
            }

            return Ok(next_crafting_action.into());
        } else {
            self.tick_decided_to_gather_items = None;
        }

        let Some(transfer) = self.next_transfer(game_state)? else {
            // The inventory transfer is complete.  Close out the
            // chest menu if it is still open, and return control to
            // the parent goal.
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }
            return Ok(BotGoalResult::Completed);
        };

        let Some(chest_menu) = game_state.chest_menu() else {
            // Some other menu has been left accidentally open.
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }

            // There is no chest open.  Open the chest for the next
            // transfer.
            let stack = LogicStack::new()
                .then(ActivateTile::new(self.room.clone(), transfer.chest))
                .cancel_if(|game_state| game_state.any_menu_open());
            return Ok(stack.into());
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
        assert!(
            current >= goal,
            "Cannot transfer items to decrease stack size \
             from {current} to {goal}"
        );
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
