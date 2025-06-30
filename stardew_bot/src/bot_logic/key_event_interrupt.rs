use crate::{game_state::Vector, Error, GameState};

use super::{
    bot_logic::{BotInterrupt, LogicStack},
    ActivateTile, MenuCloser,
};

pub struct KeyEventInterrupt;

impl KeyEventInterrupt {
    pub fn new() -> Self {
        Self
    }
}

impl BotInterrupt for KeyEventInterrupt {
    fn description(&self) -> std::borrow::Cow<str> {
        "Trigger key events".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        // When the community center has been unlocked (entering the
        // town from the bus stop on a sunny day between 9 AM and 5
        // PM, on day 5 or later), enter the community center and
        // trigger the menu.
        if game_state.globals.events_triggered.contains("ccDoorUnlock")
            && !game_state
                .globals
                .events_triggered
                .contains("seenJunimoNote")
        {
            // The bundle locations in the community center are
            // hard-coded, and not defined in terms of tile
            // properties.  Therefore, hard-coding the locations here
            // as well.
            let goal = LogicStack::new()
                .then(ActivateTile::new("CommunityCenter", Vector::new(14, 23)))
                .then(MenuCloser::new());
            return Ok(Some(goal.into()));
        }

        Ok(None)
    }
}
