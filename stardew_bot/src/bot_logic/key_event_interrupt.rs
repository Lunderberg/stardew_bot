use geometry::Vector;

use crate::{Error, GameState};

use super::{
    bot_logic::{BotInterrupt, LogicStack},
    ActivateTile, GoToRoomGoal, MenuCloser, MovementGoal,
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
        if game_state.player.using_tool || game_state.any_menu_open() {
            return Ok(None);
        }

        let check_flag = |name: &str| -> bool {
            game_state.globals.events_triggered.contains(name)
        };

        let current_day = game_state
            .globals
            .stats
            .get("daysPlayed")
            .cloned()
            .unwrap_or(0);

        let community_center_unlocked = check_flag("ccDoorUnlock");
        let can_unlock_community_center = current_day >= 5
            && !game_state.daily.is_raining
            && (800..1300).contains(&game_state.globals.in_game_time);
        if !community_center_unlocked
            && can_unlock_community_center
            && game_state.player.room_name == "Backwoods"
        {
            let stack = LogicStack::new()
                .then(GoToRoomGoal::new("BusStop"))
                .then(GoToRoomGoal::new("Town"))
                .cancel_if(|game_state| {
                    game_state.globals.events_triggered.contains("ccDoorUnlock")
                });
            return Ok(Some(stack.into()));
        }

        // When the community center has been unlocked (entering the
        // town from the bus stop on a sunny day between 8 AM and 1
        // PM, on day 5 or later), enter the community center and
        // trigger the menu.
        if community_center_unlocked && !check_flag("seenJunimoNote") {
            // The bundle locations in the community center are
            // hard-coded, and not defined in terms of tile
            // properties.  Therefore, hard-coding the locations here
            // as well.
            let goal = LogicStack::new()
                .then(ActivateTile::new("CommunityCenter", Vector::new(14, 23)))
                .then(MenuCloser::new());
            return Ok(Some(goal.into()));
        }

        if check_flag("wizardJunimoNote") && !check_flag("canReadJunimoText") {
            return Ok(Some(GoToRoomGoal::new("WizardHouse").into()));
        }

        Ok(None)
    }
}
