use std::{borrow::Cow, collections::HashMap};

use itertools::Itertools as _;

use crate::{
    bot_logic::{bot_logic::LogicStack, MovementGoal},
    game_state::{Chest, Inventory, Item, Key, ObjectKind, Quality, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    ActivateTile,
};

pub struct InventoryGoal {
    bounds: HashMap<ItemType, Bounds>,

    /// If true, any item not mentioned in `bounds` will be treated as
    /// if it has a maximum of zero items, and will be stored in a
    /// chest.  If false, any item not mentioned in `bounds` will be
    /// treated as if it has `Bounds{min: None, max: None}`, and will
    /// not be transferred into or out of the player's inventory.
    stash_unspecified_items: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ItemType {
    item_id: Cow<'static, str>,
    quality: Quality,
}

struct Bounds {
    min: Option<usize>,
    max: Option<usize>,
}

impl InventoryGoal {
    pub fn new(item: Item) -> Self {
        let bounds = [(
            ItemType {
                item_id: item.item_id,
                quality: item.quality,
            },
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

    pub fn empty() -> Self {
        Self {
            bounds: Default::default(),
            stash_unspecified_items: true,
        }
    }
}

trait InventoryExt: Sized {
    fn iter_stacks<'a>(
        &'a self,
        ty: &'a ItemType,
    ) -> impl Iterator<Item = usize> + 'a;

    // fn count(&self, ty: &ItemType) -> usize {
    //     self.iter_stacks(ty).sum()
    // }
    fn contains(&self, ty: &ItemType) -> bool {
        self.iter_stacks(ty).next().is_some()
    }

    fn to_hash_map(&self) -> HashMap<ItemType, usize>;

    fn item_slot(&self, ty: &ItemType) -> Option<usize>;
}
impl InventoryExt for Inventory {
    fn iter_stacks<'a>(
        &'a self,
        ty: &'a ItemType,
    ) -> impl Iterator<Item = usize> + 'a {
        self.items
            .iter()
            .filter_map(|opt_item| opt_item.as_ref())
            .filter(|item| {
                item.item_id == ty.item_id && item.quality == ty.quality
            })
            .map(|item| item.count)
    }

    fn to_hash_map(&self) -> HashMap<ItemType, usize> {
        let mut contents = HashMap::new();
        for item in self.iter_items() {
            let count = contents
                .entry(ItemType {
                    item_id: item.item_id.clone(),
                    quality: item.quality,
                })
                .or_default();
            *count += item.count;
        }
        contents
    }

    fn item_slot(&self, ty: &ItemType) -> Option<usize> {
        self.items
            .iter()
            .enumerate()
            .filter_map(|(i, opt_item)| opt_item.as_ref().map(|item| (i, item)))
            .find(|(_, item)| {
                ty.item_id == item.item_id && ty.quality == item.quality
            })
            .map(|(i, _)| i)
    }
}

impl InventoryGoal {
    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let player_contents = game_state.player.inventory.to_hash_map();

        let within_all_maximums =
            player_contents.iter().all(|(item, count)| {
                if let Some(bound) = self.bounds.get(item) {
                    bound.max.map(|max| *count <= max).unwrap_or(true)
                } else {
                    !self.stash_unspecified_items
                }
            });

        let within_all_minimums = self
            .bounds
            .iter()
            .filter_map(|(item, bound)| bound.min.map(|min| (item, min)))
            .all(|(item, min)| {
                let count = player_contents.get(item).cloned().unwrap_or(0);
                count >= min
            });

        within_all_maximums && within_all_minimums
    }
}

impl BotGoal for InventoryGoal {
    fn description(&self) -> std::borrow::Cow<str> {
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
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            // The inventory transfer is complete.  Close out the
            // chest menu if it is still open, and return control to
            // the parent goal.
            if let Some(chest_menu) = &game_state.chest_menu {
                do_action(GameAction::MouseOverPixel(chest_menu.ok_button));
                do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(BotGoalResult::Completed);
            }
        }

        let player_inventory = &game_state.player.inventory;
        let player_contents: HashMap<ItemType, usize> =
            player_inventory.to_hash_map();

        let farm = game_state.get_room("Farm")?;
        let preferred_chest: HashMap<ItemType, Vector<isize>> = farm
            .objects
            .iter()
            .filter_map(|obj| match &obj.kind {
                ObjectKind::Chest(chest) => Some((chest, obj.tile)),
                _ => None,
            })
            .flat_map(|(chest, tile)| {
                let has_empty_slot = chest.has_empty_slot();
                chest
                    .iter_items()
                    .filter(move |item| !item.is_full_stack() || has_empty_slot)
                    .map(move |item| {
                        (
                            ItemType {
                                item_id: item.item_id.clone(),
                                quality: item.quality,
                            },
                            tile,
                        )
                    })
            })
            .collect();

        if let Some(chest_menu) = &game_state.chest_menu {
            let chest_tile = chest_menu.chest_tile;

            for (item, player_count) in &player_contents {
                let player_count = *player_count;

                let opt_upper_bound = self
                    .bounds
                    .get(item)
                    .map(|bound| bound.max)
                    .unwrap_or(self.stash_unspecified_items.then(|| 0));

                let player_has_too_many = opt_upper_bound
                    .map(|upper_bound| player_count > upper_bound)
                    .unwrap_or(false);

                let can_add_to_chest = chest_menu.chest_items.can_add(
                    &Item::new(item.item_id.clone()).with_quality(item.quality),
                );
                let should_add_to_chest = preferred_chest
                    .get(item)
                    .cloned()
                    .map(|preferred_chest_tile| {
                        preferred_chest_tile == chest_tile
                    })
                    .unwrap_or(true);

                let player_to_chest = player_has_too_many
                    && can_add_to_chest
                    && should_add_to_chest;

                if player_to_chest {
                    // The player has more than the maximum amount of
                    // an item, and the chest already contains some of
                    // that item.  Therefore, put more of the item
                    // into the chest.
                    let slot = player_inventory.item_slot(item).unwrap();
                    let pixel = chest_menu.player_item_locations[slot];
                    do_action(GameAction::MouseOverPixel(pixel));
                    if opt_upper_bound == Some(0) {
                        do_action(GameAction::LeftClick);
                    } else {
                        do_action(GameAction::RightClick);
                    }
                    return Ok(BotGoalResult::InProgress);
                }
            }

            let chest_contents = chest_menu.chest_items.to_hash_map();
            for (item, _) in &chest_contents {
                let player_count =
                    player_contents.get(item).cloned().unwrap_or(0);

                let chest_to_player = {
                    let player_has_too_few = self
                        .bounds
                        .get(item)
                        .and_then(|bound| bound.min)
                        .map(|min| player_count < min)
                        .unwrap_or(false);

                    let can_add_to_player = player_inventory.can_add(
                        &Item::new(item.item_id.clone())
                            .with_quality(item.quality),
                    );
                    player_has_too_few && can_add_to_player
                };

                if chest_to_player {
                    // The player doesn't have enough of this item
                    // type, and has room to hold it.  Therefore, grab
                    // it from the chest.
                    let slot = chest_menu.chest_items.item_slot(item).unwrap();
                    let pixel = chest_menu.chest_item_locations[slot];
                    do_action(GameAction::MouseOverPixel(pixel));
                    do_action(GameAction::RightClick);
                    return Ok(BotGoalResult::InProgress);
                }
            }

            // No need to transfer items to/from this chest, so it can
            // be closed.
            do_action(GameAction::MouseOverPixel(chest_menu.ok_button));
            do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        // There's a chest in the process of opening.  Should wait
        // until it finishes opening.
        let currently_opening_chest =
            game_state.current_room()?.objects.iter().any(|obj| {
                matches!(
                    obj.kind,
                    ObjectKind::Chest(Chest {
                        is_opening: true,
                        ..
                    })
                )
            });
        if currently_opening_chest {
            return Ok(BotGoalResult::InProgress);
        }

        // We still have inventory management to do, and there is no
        // chest currently open.  Therefore, find a chest to open.

        if let Some(chest_tile) =
            preferred_chest.iter().next().map(|(_, tile)| *tile)
        {
            // There's a chest that already contains an item that we
            // want to get rid of, and has space to hold more of that
            // item.
            let goal = ActivateTile::new("Farm", chest_tile);
            return Ok(goal.into());
        }

        let player_has_new_item =
            player_contents.iter().any(|(item, player_count)| {
                let player_count = *player_count;
                if let Some(bound) = self.bounds.get(item) {
                    bound.max.map(|max| player_count > max).unwrap_or(false)
                } else {
                    self.stash_unspecified_items
                }
            });
        if player_has_new_item {
            // There's an item that we want to store, and there is no
            // chest with free space that already has some of that
            // item.  Therefore, pick any chest that has some free
            // space.
            let chest_tile = farm
                .objects
                .iter()
                .find(|obj| match &obj.kind {
                    ObjectKind::Chest(chest) => chest.has_empty_slot(),
                    _ => false,
                })
                .map(|obj| obj.tile)
                .expect("TODO: Handle case where all chests are full");
            let goal = ActivateTile::new("Farm", chest_tile);
            return Ok(goal.into());
        }

        let opt_chest_with_desired_item = self
            .bounds
            .iter()
            .filter_map(|(item, bound)| bound.min.map(|min| (item, min)))
            .filter(|(item, min)| {
                let player_count =
                    player_contents.get(item).cloned().unwrap_or(0);
                player_count < *min
            })
            .map(|(item, _)| item)
            .flat_map(|item| {
                farm.objects.iter().filter(|obj| match &obj.kind {
                    ObjectKind::Chest(chest) => chest.contains(item),
                    _ => false,
                })
            })
            .map(|obj| obj.tile)
            .next();

        if let Some(chest_tile) = opt_chest_with_desired_item {
            let goal = ActivateTile::new("Farm", chest_tile);
            return Ok(goal.into());
        }

        todo!("Handle case where desired item doesn't exist anywhere")

    }
}
