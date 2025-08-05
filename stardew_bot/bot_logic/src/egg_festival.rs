use std::collections::HashSet;

use game_state::{GameState, ItemId};

use crate::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    ActivateTile, BuyFromMerchantGoal, Error, GameAction, GoToRoomGoal,
    LocationExt as _, MenuCloser,
};

pub struct EggFestival;

impl EggFestival {
    pub fn new() -> Self {
        Self
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let current_day = game_state.globals.days_played();
        current_day % (4 * 28) != 13 || game_state.globals.in_game_time >= 1400
    }
}

impl BotGoal for EggFestival {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Egg Festival".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.player.room_name == "Town" {
            // Wait for the event to load on the next frame.
            return Ok(BotGoalResult::InProgress);
        } else if game_state.player.room_name != "Temp" {
            // Go to the town, which will automatically load the
            // temporary festival map.
            let stack = LogicStack::new()
                .then(GoToRoomGoal::new("Town"))
                .cancel_if(|game_state| game_state.player.room_name == "Temp");
            return Ok(stack.into());
        }

        let loc = game_state.current_room()?;
        let event = loc
            .current_event
            .as_ref()
            .ok_or(Error::MissingEventDataDuringEggFestival)?;

        let player_tile = game_state.player.tile();
        let distances = loc
            .pathfinding(&game_state.statics)
            .include_border(true)
            .distances(player_tile);

        let talked_to: HashSet<_> = game_state
            .player
            .friendships
            .iter()
            .filter(|friendship| friendship.talked_today)
            .map(|friendship| friendship.name.clone())
            .collect();

        let opt_talk_tile = event
            .characters
            .iter()
            .filter(|character| !talked_to.contains(&character.name))
            .filter(|character| character.name != event.host_name)
            .map(|character| character.tile())
            .filter(|&tile| distances.is_some(tile))
            .min_by_key(|&tile| {
                distances
                    .get_opt(tile)
                    .expect("Protected by distances.is_some()")
            });

        if let Some(menu) = game_state.dialogue_menu() {
            if menu.responses.len() == 2 {
                let text = if opt_talk_tile.is_none() { "yes" } else { "no" };
                let pixel = menu
                    .responses
                    .iter()
                    .find(|response| response.text == text)
                    .map(|response| response.pixel)
                    .expect("Menu should be a yes/no response");
                actions.do_action(GameAction::MouseOverPixel(pixel));
                actions.do_action(GameAction::LeftClick);
                return Ok(BotGoalResult::InProgress);
            }
            let cleanup = MenuCloser::new();
            return Ok(cleanup.into());
        }

        let opt_closest_egg = event
            .props
            .iter()
            .filter_map(|&tile| {
                let dist = distances.get_opt(tile)?;
                Some((tile, *dist))
            })
            .min_by_key(|(_, dist)| *dist)
            .map(|(tile, _)| tile);
        if let Some(closest_egg) = opt_closest_egg {
            let num_eggs = event.props.len();
            let goal = ActivateTile::new("Temp", closest_egg).cancel_if(
                move |game_state| {
                    let current_eggs = game_state
                        .current_room()
                        .ok()
                        .and_then(|loc| loc.current_event.as_ref())
                        .map(|event| event.props.len())
                        .unwrap_or(0);
                    game_state.menu.is_some() || current_eggs != num_eggs
                },
            );
            return Ok(goal.into());
        }

        let buy_seeds = BuyFromMerchantGoal::new(
            "Shop Festival_EggFestival_Pierre",
            ItemId::STRAWBERRY_SEEDS.with_count(400),
        );
        if !buy_seeds.is_completed(game_state)? {
            return Ok(buy_seeds.into());
        }

        if let Some(tile) = opt_talk_tile {
            let goal = ActivateTile::new("Temp", tile)
                .cancel_if(|game_state| game_state.menu.is_some());
            return Ok(goal.into());
        }

        let host_tile = event
            .characters
            .iter()
            .find(|character| character.name == event.host_name)
            .expect("Event's host should exist somewhere")
            .tile();
        let goal = ActivateTile::new("Temp", host_tile)
            .cancel_if(|game_state| game_state.menu.is_some());
        Ok(goal.into())
    }
}
