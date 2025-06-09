use crate::{Error, GameAction, GameState};

use super::bot_logic::{BotGoal, BotGoalResult, BotInterrupt};

pub struct SkipCutscenes;

pub struct SkipCurrentCutscene;

impl SkipCutscenes {
    pub fn new() -> Self {
        Self
    }
}

impl BotInterrupt for SkipCutscenes {
    fn description(&self) -> std::borrow::Cow<str> {
        "Skip Cutscenes".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<Box<dyn BotGoal>>, Error> {
        if game_state.globals.event_up {
            Ok(Some(Box::new(SkipCurrentCutscene)))
        } else {
            Ok(None)
        }
    }
}

impl BotGoal for SkipCurrentCutscene {
    fn description(&self) -> std::borrow::Cow<str> {
        "Skip current cutscene".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if !game_state.globals.event_up {
            return Ok(BotGoalResult::Completed);
        }

        let pixel = game_state.display.event_skip_button().center();
        do_action(GameAction::MouseOverPixel(pixel));
        do_action(GameAction::LeftClick);
        Ok(BotGoalResult::InProgress)
    }
}
