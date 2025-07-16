use itertools::Itertools as _;

use crate::Error;
use game_state::{GameState, ItemCategory};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    InventoryGoal, ShipItemGoal,
};

pub struct ShipMostFishGoal;

impl ShipMostFishGoal {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self
    }
}

impl BotGoal for ShipMostFishGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Ship most fish".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let farm = game_state.get_room("Farm")?;

        let iter_fish = || {
            let iter_held = game_state.player.inventory.iter_items();

            let iter_stored = farm
                .objects
                .iter()
                .filter_map(|obj| obj.kind.as_chest())
                .flat_map(|chest| chest.iter_items());

            iter_held
                .chain(iter_stored)
                .filter(|item| {
                    matches!(item.category, Some(ItemCategory::Fish))
                })
                .filter(|item| {
                    item.gp_per_stamina().map(|gp| gp > 1.75).unwrap_or(false)
                })
        };

        let worst_fish_by_type = iter_fish()
            .map(|item| (&item.id.item_id, item.quality()))
            .into_grouping_map()
            .min();

        let to_sell: Vec<_> = iter_fish()
            .map(|item| (&item.id, item.count))
            .into_grouping_map()
            .sum()
            .into_iter()
            .map(|(item_id, count)| {
                let is_worst = worst_fish_by_type
                    .get(&item_id.item_id)
                    .map(|&worst_quality| worst_quality == item_id.quality)
                    .expect("All fish should be in map");
                let to_sell = if is_worst { count - 1 } else { count };

                (item_id, to_sell)
            })
            .filter(|(_, count)| *count > 0)
            .collect();

        if to_sell.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        let prepare =
            to_sell
                .iter()
                .fold(InventoryGoal::empty(), |inv, (id, count)| {
                    let item = (*id).clone().as_item();
                    inv.with(item.with_count(*count))
                });
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.into());
        }

        // In most cases, checking the player's inventory isn't
        // necessary at this stage, because any fish that should not
        // be sold will moved to the storage chests.  However, if
        // there is insufficient storage space, then the player may
        // still have some fish in the inventory that shouldn't be
        // sold off.
        let player_inventory = game_state.player.inventory.to_hash_map();

        let sell = ShipItemGoal::new(to_sell.into_iter().map(
            |(item, num_to_sell)| {
                let current_num =
                    player_inventory.get(item).cloned().unwrap_or(0);
                let target_num = current_num.saturating_sub(num_to_sell);
                item.clone().as_item().with_count(target_num)
            },
        ));
        if !sell.is_completed(game_state) {
            return Ok(sell.into());
        }

        Ok(BotGoalResult::Completed)
    }
}
