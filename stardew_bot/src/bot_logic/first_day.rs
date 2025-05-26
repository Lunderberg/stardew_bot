use crate::{
    game_state::{ObjectKind, Vector},
    Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    ClayFarmingGoal, MovementGoal,
};

pub struct FirstDay;

impl BotGoal for FirstDay {
    fn description(&self) -> std::borrow::Cow<str> {
        "First Day".into()
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

        let current_day = game_state.globals.get_stat("daysPlayed")?;
        if current_day != 1 {
            return Ok(BotGoalResult::Completed);
        }

        // If we are playing StardewValleyExpanded, pick up the
        // freebie ForestSword.  The `class PlacedItem:
        // StardewValley.Object` is added by mods, and is not
        // currently unpacked as I want to keep the bot compatible
        // with vanilla Stardew, so it shows up as an
        // `ObjectKind::Unknown`.
        let can_pick_up_forest_sword = game_state
            .locations
            .iter()
            .find(|loc| loc.name == "Custom_ForestWest")
            .into_iter()
            .flat_map(|loc| loc.objects.iter())
            .find(|obj| {
                matches!(obj.kind, ObjectKind::Unknown)
                    && obj.tile == Vector::new(60, 148)
            })
            .is_some();
        if can_pick_up_forest_sword {
            let tile = Vector::<isize>::new(60, 148);
            let movement = MovementGoal::new(
                "Custom_ForestWest".into(),
                tile.map(|x| x as f32),
            )
            .with_tolerance(1.1);
            if !movement.is_completed(game_state) {
                return Ok(movement.into());
            }

            do_action(GameAction::MouseOverTile(tile));
            if tile == game_state.inputs.mouse_tile_location {
                do_action(GameAction::RightClick);
            }

            return Ok(BotGoalResult::InProgress);
        }

        let clay_farming = ClayFarmingGoal::new()
            .stop_at_time(1200)
            .stop_at_stamina(4.0);
        if !clay_farming.is_completed(game_state) {
            return Ok(clay_farming.into());
        }

        Ok(BotGoalResult::InProgress)
    }
}
