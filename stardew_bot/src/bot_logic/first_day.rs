use dotnet_debugger::env_var_flag;

use crate::{
    game_state::{Item, ObjectKind, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    BuyFromMerchantGoal, ClayFarmingGoal, ForagingGoal, GoToActionTile,
    MovementGoal, RecoverStaminaGoal, SellToMerchantGoal,
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
        let should_pick_up_forest_sword = env_var_flag("GRAB_FOREST_SWORD");
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
        if should_pick_up_forest_sword && can_pick_up_forest_sword {
            let tile = Vector::<isize>::new(60, 148);
            let movement =
                MovementGoal::new("Custom_ForestWest", tile.map(|x| x as f32))
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

        let foraging =
            ForagingGoal::new().stop_at_time(700).stop_with_stamina(30);
        if !foraging.is_completed(game_state) {
            return Ok(foraging.into());
        }

        let clay_farming = ClayFarmingGoal::new()
            .stop_at_time(1200)
            .stop_at_stamina(4.0);
        if !clay_farming.is_completed(game_state) {
            return Ok(clay_farming.into());
        }

        let goal = SellToMerchantGoal::new("Carpenter", Item::CLAY);
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let goal =
            BuyFromMerchantGoal::new("Saloon", Item::SALAD.with_count(5));
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        if game_state.player.current_stamina < 20.0 {
            let goal = RecoverStaminaGoal::new();
            if goal.item_to_eat(game_state).is_some() {
                return Ok(goal.into());
            }
        }

        Ok(BotGoalResult::InProgress)
    }
}
