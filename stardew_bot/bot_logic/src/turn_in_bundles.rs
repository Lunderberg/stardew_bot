use std::collections::HashMap;

use game_state::{Bundle, GameState, Item, ItemId, Quality};
use geometry::Vector;
use itertools::Itertools as _;

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, Error, GameStateExt as _, InventoryGoal, ItemIterExt as _,
};

pub struct TurnInBundlesGoal;

trait ItemLookupExt {
    /// Returns the number of items that exactly match the specified
    /// item id.
    fn item_count_with_exact_quality(&self, id: &ItemId) -> usize;

    /// Returns the number of items that match the id, and meet or
    /// exceed the specified item quality.
    fn item_count(&self, id: &ItemId) -> usize;

    /// Returns the items that should be used for the bundle.  Because
    /// the bundle may require items of multiple qualities in order to
    /// be completed, this may require more than one item stack.
    /// (e.g. "10 Wheat" being fulfilled by 5 normal-quality Wheat and
    /// 5 silver-star Wheat.)
    fn items_with_quality<'a>(&self, item: &Item) -> [Item; 4];

    /// Returns an iterator over the items that should be used for the
    /// bundle.  Because the bundle may require items of multiple
    /// qualities in order to be completed, this may require more than
    /// one item stack.  (e.g. "10 Wheat" being fulfilled by 5
    /// normal-quality Wheat and 5 silver-star Wheat.)
    fn iter_items_with_quality<'a>(
        &'a self,
        item: &'a Item,
    ) -> impl Iterator<Item = Item> + 'a;

    /// Remove the specified item stack from the lookup
    fn remove_item(&mut self, id: &ItemId, count: usize);
}
impl ItemLookupExt for HashMap<ItemId, usize> {
    fn item_count(&self, id: &ItemId) -> usize {
        Quality::iter()
            .filter(|quality| quality >= &id.quality)
            .map(|quality| {
                self.item_count_with_exact_quality(
                    &id.clone().with_quality(quality),
                )
            })
            .sum()
    }

    fn items_with_quality<'a>(&self, item: &Item) -> [Item; 4] {
        let mut num_remaining = item.count;
        let mut outputs = [
            Quality::Normal,
            Quality::Silver,
            Quality::Gold,
            Quality::Iridium,
        ]
        .map(|quality| item.id.clone().with_quality(quality).with_count(1));

        for output in &mut outputs {
            let count = self
                .item_count_with_exact_quality(&output.id)
                .min(num_remaining);
            output.count = count;
            num_remaining -= count;
        }

        outputs
    }

    fn iter_items_with_quality<'a>(
        &'a self,
        item: &Item,
    ) -> impl Iterator<Item = Item> + 'a {
        let item_quality = item.quality();
        let mut iter_quality =
            Quality::iter().filter(move |quality| quality >= &item_quality);
        let mut num_remaining = item.count;

        let id = item.id.clone();

        std::iter::from_fn(move || {
            let quality = iter_quality.next()?;
            let id_with_quality = id.clone().with_quality(quality);
            let count = self
                .item_count_with_exact_quality(&id_with_quality)
                .min(num_remaining);
            num_remaining -= count;
            let item_with_quality = id_with_quality.with_count(count);

            Some(item_with_quality)
        })
        .filter(|item_with_quality| item_with_quality.count > 0)
    }

    fn item_count_with_exact_quality(&self, id: &ItemId) -> usize {
        self.get(id).cloned().unwrap_or(0)
    }

    fn remove_item(&mut self, id: &ItemId, count: usize) {
        if let Some(prev) = self.get_mut(id) {
            *prev = prev.saturating_sub(count);
            if *prev == 0 {
                self.remove(id);
            }
        }
    }
}

impl TurnInBundlesGoal {
    pub fn new() -> Self {
        Self
    }

    fn to_turn_in<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = (&'a Bundle, Item)> + 'a, Error> {
        let available = if game_state.player.room_name == "CommunityCenter" {
            game_state.player.inventory.iter_items().item_counts()
        } else {
            game_state.iter_accessible_items()?.item_counts()
        };

        let iter = game_state
            .statics
            .bundles
            .iter()
            .filter_map(|bundle| {
                let flags =
                    game_state.globals.bundles.get(&bundle.bundle_index)?;
                Some((bundle, flags))
            })
            .map(|(bundle, flags)| {
                let num_completed =
                    flags.iter().map(|&b| b as usize).sum::<usize>();

                let num_completable = bundle
                    .iter_items()
                    .zip(flags)
                    .filter(|(item, done)| {
                        **done || available.item_count(&item.id) >= item.count
                    })
                    .count();

                (bundle, flags, num_completed, num_completable)
            })
            .sorted_by_key(|(bundle, _, num_completed, num_completable)| {
                let is_completed = num_completed >= &bundle.num_required;
                let is_completable = num_completable >= &bundle.num_required;
                (
                    !is_completed,
                    !is_completable,
                    bundle.num_bundles_to_unlock(),
                )
            })
            .scan(
                0usize,
                |cumsum, (bundle, flags, num_completed, num_completable)| {
                    let prev_completed_bundles = *cumsum;
                    if num_completable >= bundle.num_required {
                        *cumsum += 1;
                    }
                    Some((bundle, flags, num_completed, prev_completed_bundles))
                },
            )
            .filter(|(bundle, _, num_completed, _)| {
                *num_completed < bundle.num_required
            })
            .filter(|(bundle, _, _, prev_completed_bundles)| {
                *prev_completed_bundles >= bundle.num_bundles_to_unlock()
            })
            .flat_map(move |(bundle, flags, _, _)| {
                bundle
                    .iter_items()
                    .zip(flags)
                    .filter(|(_, done)| !**done)
                    .map(move |(item, _)| (bundle, item))
            })
            .flat_map(move |(bundle, item)| {
                available
                    .items_with_quality(item)
                    .map(move |item| (bundle, item))
            });

        Ok(iter)
    }

    // fn to_turn_in(
    //     &self,
    //     game_state: &GameState,
    // ) -> Result<impl Iterator<Item = (usize, Item)>, Error> {
    //     let mut held = game_state.player.inventory.iter_items().item_counts();

    //     let (mut available, mut open_slots) =
    //         if game_state.player.room_name != "CommunityCenter" {
    //             (
    //                 game_state.iter_accessible_items()?.item_counts(),
    //                 game_state.player.inventory.num_slots(),
    //             )
    //         } else {
    //             (held.clone(), game_state.player.inventory.num_empty_slots())
    //         };

    //     // Track the number of completed bundles, to ensure that we
    //     // only plan to give items to bundles that are in accessible
    //     // rooms.
    //     let mut completed_bundles: Vec<bool> = game_state
    //         .statics
    //         .bundles
    //         .iter()
    //         .map(|bundle| bundle.is_completed(&game_state.globals.bundles))
    //         .collect();

    //     let mut completed_ingredients: HashSet<(i32, usize)> = game_state
    //         .statics
    //         .bundles
    //         .iter()
    //         .filter_map(|bundle| {
    //             let flags =
    //                 game_state.globals.bundles.get(&bundle.bundle_index)?;
    //             let iter = bundle
    //                 .ingredients
    //                 .iter()
    //                 .enumerate()
    //                 .zip(flags.iter())
    //                 .filter(|(_, done)| **done)
    //                 .map(|((i_ingredient, _), _)| {
    //                     (bundle.bundle_index, i_ingredient)
    //                 });
    //             Some(iter)
    //         })
    //         .flatten()
    //         .collect();

    //     let iter = std::iter::from_fn(move || {
    //         let num_bundles_completed =
    //             completed_bundles.iter().map(|&b| b as usize).sum::<usize>();

    //         // First, find any bundles that can be completed.
    //         let opt_completable = || {
    //             game_state
    //                 .statics
    //                 .bundles
    //                 .iter()
    //                 .enumerate()
    //                 .filter(|(i, _)| !completed_bundles[*i])
    //                 .filter(|(_, bundle)| {
    //                     let num_bundles_to_unlock =
    //                         match bundle.community_center_room.as_str() {
    //                             "Crafts Room" => 0,
    //                             "Pantry" | "Fish Tank" => 1,
    //                             "Boiler Room" => 2,
    //                             "Bulletin Board" => 3,
    //                             "Vault" => 4,
    //                             _ => 99,
    //                         };
    //                     num_bundles_completed >= num_bundles_to_unlock
    //                 })
    //                 .find_map(|(i, bundle)| {
    //                     println!(
    //                         "Checking if {} can be completed",
    //                         bundle.name
    //                     );
    //                     let flags = game_state
    //                         .globals
    //                         .bundles
    //                         .get(&bundle.bundle_index)?;
    //                     let ingredients_completed =
    //                         flags.iter().map(|&b| b as usize).sum::<usize>();
    //                     let ingredients_remaining =
    //                         bundle.num_required - ingredients_completed;

    //                     let iter_available_ingredients = || {
    //                         bundle
    //                         .iter_items()
    //                             .enumerate()
    //                         .filter(|item| {
    //                             let have = available.item_count(&item.id);
    //                             let need = item.count;
    //                             have >= need
    //                         })
    //                     };

    //                     if iter_available_ingredients().count() < ingredients_remaining {
    //                         println!("\tNot enough collected, cannot complete");
    //                         return None;
    //                     }

    //                     let items = iter_available_ingredients()
    //                         .take(ingredients_remaining)
    //                         .flat_map(|ingredient| {
    //                             available.iter_items_with_quality(ingredient)
    //                         })
    //                         .map(|item| (item.id, item.count))
    //                         .into_grouping_map()
    //                         .sum();

    //                     let slots_required = items
    //                         .iter()
    //                         .filter(|(id, _)| !held.contains_key(id))
    //                         .count();
    //                     if slots_required > open_slots {
    //                         println!(
    //                             "\tNot enough inventory space, cannot complete"
    //                         );
    //                         return None;
    //                     }

    //                     println!("\tCan complete {}", bundle.name);
    //                     Some((i, true, items))
    //                 })
    //         };

    //         if let Some((i, completes_bundle, items)) = opt_completable() {
    //             if completes_bundle {
    //                 completed_bundles[i] = true;
    //             }

    //             for (id, &count) in items.iter() {
    //                 if held.item_count(id) >= count {
    //                     held.remove_item(id, count);
    //                 } else {
    //                     open_slots -= 1;
    //                 }
    //                 available.remove_item(id, count);
    //             }

    //             let iter_items = items
    //                 .into_iter()
    //                 .map(move |(id, count)| (i, id.with_count(count)));

    //             return Some(iter_items);
    //         }

    //         None
    //     })
    //     .flatten();

    //     Ok(iter)
    // }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.to_turn_in(game_state)?.next().is_none())
    }
}

impl BotGoal for TurnInBundlesGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Turn in bundles".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some(next_turn_in) = self.to_turn_in(game_state)?.next() else {
            return Ok(BotGoalResult::Completed);
        };

        if game_state.player.room_name != "CommunityCenter" {
            let prepare = InventoryGoal::empty().with(
                self.to_turn_in(game_state)?.map(|(_, item)| item.clone()),
            );
            if !prepare.is_completed(game_state)? {
                return Ok(prepare.into());
            }
        }

        if !game_state.any_menu_open() {
            let start =
                ActivateTile::new("CommunityCenter", Vector::new(14, 23));
            return Ok(start.into());
        }

        todo!()
    }
}
