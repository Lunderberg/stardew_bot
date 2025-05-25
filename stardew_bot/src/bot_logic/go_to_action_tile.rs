use crate::{game_state::Vector, Error, GameAction, GameState};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    BotError, MovementGoal,
};

pub struct GoToActionTile {
    action: String,
}

impl GoToActionTile {
    #[allow(dead_code)]
    pub fn new(action: impl Into<String>) -> Self {
        Self {
            action: action.into(),
        }
    }

    pub fn adjacent_action_tile(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Vector<isize>>, Error> {
        let player_tile = game_state.player.tile();
        let loc = game_state.current_room()?;
        let opt_tile = loc
            .action_tiles
            .iter()
            .filter(|(tile, _)| tile.manhattan_dist(player_tile) <= 1)
            .filter(|(_, action)| action == &self.action)
            .map(|(tile, _)| tile)
            .next()
            .cloned();

        Ok(opt_tile)
    }

    pub fn is_done(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.adjacent_action_tile(game_state)?.is_some())
    }
}

impl BotGoal for GoToActionTile {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Go to {}", self.action).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        Ok(if self.is_done(game_state)? {
            BotGoalResult::Completed
        } else {
            let (target_room, action_tile) = game_state
                .locations
                .iter()
                .flat_map(|loc| {
                    loc.action_tiles
                        .iter()
                        .map(move |(tile, action)| (loc, tile, action))
                })
                .filter(|(_, _, action)| {
                    action.as_str() == self.action.as_str()
                })
                .map(|(loc, tile, _)| (loc.name.clone(), *tile))
                .next()
                .ok_or_else(|| {
                    BotError::NoTileWithAction(self.action.clone())
                })?;
            let target_position = action_tile.map(|x| x as f32);
            let goal = MovementGoal::new(target_room, target_position)
                .with_tolerance(1.1);
            goal.into()
        })
    }
}
