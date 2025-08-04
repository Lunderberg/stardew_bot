use game_state::{GameState, Item, ItemCategory, Quality};
use itertools::Itertools as _;

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    Error, GameStateExt as _, InventoryGoal, ItemIterExt as _,
    ItemLookupExt as _, MenuCloser, ShipItemGoal,
};

pub struct SellForCashGoal {
    target_money: i32,
}

impl SellForCashGoal {
    pub fn new(target_money: i32) -> Self {
        Self { target_money }
    }

    pub fn additional_money(
        &self,
        game_state: &GameState,
    ) -> Result<i32, Error> {
        let wallet = game_state.player.current_money;
        let pass_out_penalty = (((wallet as f32) * 0.1) as i32).min(1000);

        let shipped = game_state
            .globals
            .shipped_items
            .iter_items()
            .map(|item| item.stack_price())
            .sum::<i32>();

        let total = wallet - pass_out_penalty + shipped;
        Ok(self.target_money - total)
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.additional_money(game_state)? < 0)
    }
}

impl BotGoal for SellForCashGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Ship items to reach {} GP", self.target_money).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let additional_money = self.additional_money(game_state)?;
        if additional_money < 0 {
            let cleanup = MenuCloser::new();
            if !cleanup.is_completed(game_state) {
                return Ok(cleanup.into());
            }

            return Ok(BotGoalResult::Completed);
        }

        let fish_price_multiplier = game_state.daily.fish_price_multiplier();

        let all_items = game_state.iter_accessible_items()?.item_counts();
        let reserved_items = game_state
            .iter_reserved_items()?
            .flat_map(|item| all_items.iter_items_with_quality(item))
            .item_counts();
        let items_to_sell: Vec<Item> = all_items
            .into_iter()
            .map(|(id, count)| {
                let num_reserved = reserved_items.item_count(&id);
                (id, count.saturating_sub(num_reserved))
            })
            .filter(|(_, count)| *count > 0)
            .filter_map(|(id, count)| {
                let base_quality = id.clone().with_quality(Quality::Normal);
                let data = game_state.statics.object_data.get(&base_quality)?;

                let price_multiplier = {
                    let category_multiplier = match data.category {
                        ItemCategory::Fish => fish_price_multiplier,
                        _ => 1.0,
                    };
                    let quality_multiplier = id.quality.price_multiplier();
                    category_multiplier * quality_multiplier
                };

                let price = ((data.price as f32) * price_multiplier) as i32;

                Some((id, data, price, count))
            })
            .filter(|(_, _, price, _)| *price > 0)
            .sorted_by_key(|(id, data, price, _)| {
                let from_mines = matches!(
                    data.category,
                    ItemCategory::Gem | ItemCategory::Mineral
                );

                let stamina = (data.edibility > 0).then(|| {
                    ((data.edibility as f32) * id.quality.stamina_multiplier())
                        as i32
                });

                (
                    std::cmp::Reverse(from_mines),
                    stamina.is_none(),
                    stamina.unwrap_or(0) * 1000 / price,
                )
            })
            .scan(0i32, |cumsum, (id, _, price, count)| {
                let prev_sold = *cumsum;
                *cumsum += price * (count as i32);
                Some((id, prev_sold, price, count))
            })
            .filter(|(_, prev_sold, _, _)| *prev_sold < additional_money)
            .map(|(id, prev_sold, price, count)| {
                let count = count
                    .min(((additional_money - prev_sold) / price) as usize);
                id.with_count(count)
            })
            .collect();

        if items_to_sell.is_empty() {
            return Ok(BotGoalResult::Completed);
        }

        let prepare =
            InventoryGoal::empty().with_exactly(items_to_sell.iter().cloned());
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.into());
        }

        let sell = ShipItemGoal::new(
            items_to_sell.into_iter().map(|item| item.with_count(0)),
        );
        if !sell.is_completed(game_state) {
            return Ok(sell.into());
        }

        Ok(BotGoalResult::InProgress)
    }
}
