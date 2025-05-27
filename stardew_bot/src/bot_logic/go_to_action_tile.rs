use crate::{game_state::Vector, Error, GameAction, GameState};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    BotError, MovementGoal,
};

#[derive(Clone)]
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

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        game_state.dialogue_menu.is_some()
    }
}

impl BotGoal for GoToActionTile {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Go to {}", self.action).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        // TODO: Handle these are part of some return-to-default
        // logic, rather than needing each goal to release each
        // button.
        {
            let mut cleanup = false;
            if game_state.inputs.right_mouse_down() {
                do_action(GameAction::ReleaseRightClick.into());
                cleanup = true;
            }
            if cleanup {
                return Ok(BotGoalResult::InProgress);
            }
        }

        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        if let Some(tile) = self.adjacent_action_tile(game_state)? {
            do_action(GameAction::MouseOverTile(tile));
            if tile == game_state.inputs.mouse_tile_location {
                do_action(GameAction::RightClick);
            }
            return Ok(BotGoalResult::InProgress);
        }

        let (target_room, target_position) = game_state
            .locations
            .iter()
            .flat_map(|loc| {
                loc.action_tiles
                    .iter()
                    .map(move |(tile, action)| (loc, tile, action))
            })
            .filter(|(_, _, action)| action.as_str() == self.action.as_str())
            .map(|(loc, tile, _)| (loc.name.clone(), tile.map(|x| x as f32)))
            .reduce(|(lhs_loc, lhs_tile), (rhs_loc, rhs_tile)| {
                if lhs_loc == rhs_loc {
                    (lhs_loc, (lhs_tile + rhs_tile) / 2.0)
                } else {
                    (lhs_loc, lhs_tile)
                }
            })
            .ok_or_else(|| BotError::NoTileWithAction(self.action.clone()))?;
        let target_position = target_position.map(|x| x.round());

        let goal =
            MovementGoal::new(target_room, target_position).with_tolerance(1.1);
        Ok(goal.into())
    }
}
