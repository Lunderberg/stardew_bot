use std::collections::HashMap;

use game_state::{Bundle, GameState, Item, ItemId, Menu, Quality};
use itertools::Itertools as _;

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, Error, GameAction, GameStateExt as _, InventoryGoal,
    ItemIterExt as _, MenuCloser,
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

    fn item_count_with_exact_quality(&self, id: &ItemId) -> usize {
        self.get(id).cloned().unwrap_or(0)
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

        let has_catfish_bait = game_state
            .iter_accessible_items()?
            .find_map(|item| item.as_fishing_rod())
            .and_then(|rod| rod.bait.as_ref())
            .filter(|bait| {
                &bait.id == &ItemId::TARGETED_BAIT.with_subtype(ItemId::CATFISH)
            })
            .is_some();

        let num_inventory_slots = game_state.player.inventory.num_slots();

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
                        if **done {
                            return true;
                        }
                        let num_available = available.item_count(&item.id);
                        let num_available = if item.id == ItemId::CATFISH
                            && !has_catfish_bait
                        {
                            num_available.saturating_sub(1)
                        } else {
                            num_available
                        };
                        num_available >= item.count
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
                let num_available = available.item_count(&item.id);
                let num_available =
                    if item.id == ItemId::CATFISH && !has_catfish_bait {
                        num_available.saturating_sub(1)
                    } else {
                        num_available
                    };

                let have_enough = num_available >= item.count;
                available
                    .items_with_quality(item)
                    .into_iter()
                    .filter(move |_| have_enough)
                    .filter(|item| item.count > 0)
                    .map(move |item| (bundle, item))
            });

        Ok(iter)
    }

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
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let Some((next_bundle, next_item)) =
            self.to_turn_in(game_state)?.next()
        else {
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

        let menu = match &game_state.menu {
            Some(Menu::Junimo(junimo)) => junimo,
            Some(_) => {
                let cleanup = MenuCloser::new();
                return Ok(cleanup.into());
            }
            None => {
                let tile = next_bundle
                    .community_center_tile()
                    .expect("Bundle should exist in community center");
                let start = ActivateTile::new("CommunityCenter", tile);
                return Ok(start.into());
            }
        };

        let Some(&bundle_pixel) =
            menu.bundle_buttons.get(&next_bundle.bundle_index)
        else {
            // The bundle isn't in this room, so close the menu.
            let cleanup = MenuCloser::new();
            return Ok(cleanup.into());
        };

        if let Some(active_bundle) = menu.current_active_bundle {
            if active_bundle == next_bundle.bundle_index {
                // The correct bundle is open, so no action is needed.
            } else {
                // This is the correct room, but the wrong bundle.
                // Use the back button to close the current bundle.
                actions.do_action(GameAction::MouseOverPixel(menu.back_button));
                actions.do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            }
        } else {
            // The menu for the room is open, but no bundle is
            // selected.  Therefore, select the bundle.
            actions.do_action(GameAction::MouseOverPixel(bundle_pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        };

        let item_slot = game_state
            .player
            .inventory
            .current_slot(&next_item.id)
            .ok_or_else(|| {
                Error::ExpectedItemInInventory(next_item.id.clone())
            })?;
        let item_pixel = menu.player_item_locations[item_slot];

        actions.do_action(GameAction::HoldLeftShift);
        actions.do_action(GameAction::MouseOverPixel(item_pixel));
        if game_state.inputs.holding_left_shift() {
            actions.do_action(GameAction::LeftClick);
        }
        Ok(BotGoalResult::InProgress)
    }
}
