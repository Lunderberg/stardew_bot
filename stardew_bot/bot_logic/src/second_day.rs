use game_state::{GameState, ItemId};

use crate::{
    CheckAllMail, Error, FishOnceGoal, GoToRoomGoal, InventoryGoal,
    MaintainStaminaGoal, MovementGoal, SelectItemGoal, WaterCropsGoal,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
};

pub struct SecondDay;

impl BotGoal for SecondDay {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Second Day".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let current_day =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);
        if current_day != 2 {
            return Ok(BotGoalResult::Completed);
        }

        let goal = CheckAllMail::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let goal = WaterCropsGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        if player.room_name == "Farm" {
            let preparation = InventoryGoal::empty()
                .stamina_recovery_slots(2)
                .craft_missing(true)
                .with_exactly([
                    ItemId::OAK_CHAIR.with_count(1),
                    ItemId::FURNACE.with_count(1),
                    ItemId::COPPER_ORE.with_count(25),
                    ItemId::COAL.with_count(5),
                    ItemId::PICKAXE.with_count(1),
                ]);
            if !preparation.is_completed(game_state)? {
                return Ok(preparation.into());
            }
        }

        let opt_current_pole = player
            .inventory
            .iter_items()
            .filter(|item| item.as_fishing_rod().is_some())
            .max_by_key(|item| {
                item.as_fishing_rod().unwrap().num_attachment_slots
            });
        let Some(current_pole) = opt_current_pole else {
            return Ok(GoToRoomGoal::new("Beach").into());
        };

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state)? {
            return Ok(goal.into());
        }

        if let Some(bubbles) = crate::search_for_bubbles(game_state, "Beach")? {
            let goal = MovementGoal::new(
                bubbles.bubble_room.clone(),
                bubbles.player_tile.map(|x| x as f32),
            )
            .with_tolerance(0.5);
            if !goal.is_completed(game_state) {
                return Ok(goal.into());
            }

            let select_pole = SelectItemGoal::new(current_pole.id.clone());
            if !select_pole.is_completed(game_state) {
                return Ok(select_pole.into());
            }

            let fish_once =
                FishOnceGoal::new().target_bobber_pos(bubbles.target_bobber);
            return Ok(fish_once.into());
        }

        Ok(BotGoalResult::InProgress)
    }
}
