use game_state::{Bundle, GameState, Item, ItemId, Menu};
use itertools::Itertools as _;

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    ActivateTile, Error, GameAction, GameStateExt as _, InventoryGoal,
    ItemIterExt as _, ItemLookupExt as _, MenuCloser,
};

pub struct TurnInBundlesGoal {
    stop_time: i32,
}

impl TurnInBundlesGoal {
    pub fn new() -> Self {
        Self { stop_time: 1100 }
    }

    fn to_turn_in<'a>(
        &self,
        game_state: &'a GameState,
        include_partial_bundles: bool,
    ) -> Result<impl Iterator<Item = (&'a Bundle, Item)> + 'a, Error> {
        let mut available = if game_state.player.room_name == "CommunityCenter"
        {
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
        if !has_catfish_bait {
            if let Some(worst_quality) = available
                .items_with_quality(&ItemId::CATFISH.with_count(1))
                .into_iter()
                .find(|item| item.count > 0)
                .map(|item| item.quality())
            {
                available.remove_item(
                    &ItemId::CATFISH.with_quality(worst_quality),
                    1,
                );
            }
        }

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
                        num_available >= item.count
                    })
                    .count();

                let is_completed = num_completed >= bundle.num_required;
                let is_completable = num_completable >= bundle.num_required;

                (bundle, flags, num_completed, is_completed, is_completable)
            })
            .filter(|(_, _, _, is_completed, is_completable)| {
                *is_completed || *is_completable || include_partial_bundles
            })
            .sorted_by_key(|(bundle, _, _, is_completed, is_completable)| {
                (
                    !is_completed,
                    !is_completable,
                    bundle.num_bundles_to_unlock(),
                )
            })
            .scan(
                0usize,
                |cumsum, (bundle, flags, num_completed, _, is_completable)| {
                    let prev_completed_bundles = *cumsum;
                    if is_completable {
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

                let have_enough = num_available >= item.count;
                available
                    .items_with_quality(&item)
                    .into_iter()
                    .filter(move |_| have_enough)
                    .filter(|item| item.count > 0)
                    .map(move |item| (bundle, item))
            })
            .take(num_inventory_slots);

        Ok(iter)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.to_turn_in(game_state, false)?.next().is_none())
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
            self.to_turn_in(game_state, true)?.next()
        else {
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }
            return Ok(BotGoalResult::Completed);
        };

        if game_state.player.room_name != "CommunityCenter" {
            if game_state.globals.in_game_time >= self.stop_time {
                return Ok(BotGoalResult::Completed);
            }

            let prepare = InventoryGoal::empty().with_exactly(
                self.to_turn_in(game_state, true)?
                    .map(|(_, item)| item)
                    .item_counts()
                    .into_iter()
                    .map(|(id, count)| id.with_count(count)),
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
