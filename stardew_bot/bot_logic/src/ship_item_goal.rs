use itertools::Itertools as _;

use crate::{ActivateTile, Error, GameAction};
use game_state::{GameState, Item};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MenuCloser, TransferSize,
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

    fn next_transfer(
        &self,
        game_state: &GameState,
    ) -> Option<(usize, TransferSize)> {
        let inventory = &game_state.player.inventory;
        let counts = inventory.to_hash_map();

        for item in &self.items {
            let current = counts.get(item.as_ref()).cloned().unwrap_or(0);
            let goal = item.count;
            if goal < current {
                let Some(slot) = inventory.current_slot(item) else {
                    continue;
                };

                let transfer_size = TransferSize::select(current, goal);

                return Some((slot, transfer_size));
            }
        }
        None
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.next_transfer(game_state).is_none()
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
        let Some((index_to_ship, transfer_size)) =
            self.next_transfer(game_state)
        else {
            let cleanup = MenuCloser::new();
            if cleanup.is_completed(game_state) {
                return Ok(BotGoalResult::Completed);
            } else {
                return Ok(cleanup.into());
            }
        };

        let Some(menu) = game_state.chest_menu() else {
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

        transfer_size.send_inputs(game_state, actions);

        Ok(BotGoalResult::InProgress)
    }
}
