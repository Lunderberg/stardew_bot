use crate::{game_state::Vector, Error, GameAction, GameState};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    MovementGoal,
};

pub struct ActivateTile {
    room: String,
    tile: Vector<isize>,
    is_activated: bool,
}

impl ActivateTile {
    pub fn new(room: impl Into<String>, tile: Vector<isize>) -> Self {
        let room = room.into();
        Self {
            room,
            tile,
            is_activated: false,
        }
    }
}

impl BotGoal for ActivateTile {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Activate {} in {}", self.tile, self.room).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_activated {
            return Ok(BotGoalResult::Completed);
        }

        let is_within_range =
            game_state.player.tile().manhattan_dist(self.tile) <= 1;
        if is_within_range {
            actions.do_action(GameAction::MouseOverTile(self.tile));
            if !game_state.inputs.right_mouse_down() {
                actions.do_action(GameAction::RightClick);
                self.is_activated = true;
            }
            return Ok(BotGoalResult::InProgress);
        }

        let movement =
            MovementGoal::new(self.room.clone(), self.tile.map(|x| x as f32))
                .with_tolerance(1.4);
        Ok(movement.into())
    }
}
