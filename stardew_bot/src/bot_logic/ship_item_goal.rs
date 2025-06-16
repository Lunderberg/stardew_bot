use itertools::Itertools as _;

use crate::{
    bot_logic::ActivateTile, game_state::Item, Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser,
};

pub struct ShipItemGoal {
    items: Vec<Item>,
}

impl ShipItemGoal {
    pub fn new<Iter>(items: Iter) -> Self
    where
        Iter: IntoIterator,
        <Iter as IntoIterator>::Item: Into<Item>,
    {
        let items = items.into_iter().map(Into::into).collect();
        Self { items }
    }
}

impl BotGoal for ShipItemGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        if self.items.len() <= 2 {
            format!("Ship {}", self.items.iter().format(", ")).into()
        } else {
            "Ship items".into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let opt_index_to_ship = game_state
            .player
            .inventory
            .iter_slots()
            .enumerate()
            .find(|(_, opt_item)| {
                opt_item
                    .map(|item| {
                        self.items
                            .iter()
                            .any(|to_sell| item.is_same_item(to_sell))
                    })
                    .unwrap_or(false)
            })
            .map(|(i, _)| i);

        let Some(index_to_ship) = opt_index_to_ship else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(menu) = &game_state.chest_menu else {
            // We have items to ship, but the menu isn't open.  Time
            // to run to the farm to ship something.
            let farm = game_state.get_room("Farm")?;
            let shipping_bin = farm
                .buildings
                .iter()
                .find(|building| building.kind == "Shipping Bin")
                .expect("TODO: Handle case of missing Shipping Bin")
                .iter_tiles()
                .next()
                .expect("Building has at least one tile");

            let goal = ActivateTile::new("Farm", shipping_bin);
            return Ok(goal.into());
        };

        // We have something to ship, and have the menu open to do so.
        // Time to click the item.
        let pixel = menu.player_item_locations[index_to_ship];
        actions.do_action(GameAction::MouseOverPixel(pixel));
        actions.do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}
