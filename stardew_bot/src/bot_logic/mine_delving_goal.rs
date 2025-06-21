use std::collections::HashSet;

use crate::{
    bot_logic::{
        ActivateTile, BotError, GameStateExt as _, InventoryGoal, MovementGoal,
        UseItemOnTile,
    },
    game_state::{Item, ObjectKind, Vector},
    Error, GameState,
};

use super::bot_logic::{ActionCollector, BotGoal, BotGoalResult};

pub struct MineDelvingGoal;

impl MineDelvingGoal {
    pub fn new() -> Self {
        Self
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let current_day = game_state
            .globals
            .stats
            .get("daysPlayed")
            .cloned()
            .unwrap_or(0);

        let reached_bottom =
            game_state.globals.lowest_mine_level_reached >= 120;

        Ok(current_day < 5 || reached_bottom)
    }
}

impl BotGoal for MineDelvingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Delve Mines".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state)? {
            return Ok(BotGoalResult::Completed);
        }

        let current_room = game_state.current_room()?;
        if current_room.name == "Farm" || current_room.name == "FarmHouse" {
            let prepare = InventoryGoal::empty()
                .with(Item::PICKAXE)
                .with(Item::WOOD.clone().with_count(50))
                .with(Item::STONE.clone().with_count(1000))
                .with(Item::COPPER_ORE.clone().with_count(1000))
                .with(Item::IRON_ORE.clone().with_count(1000))
                .with(Item::GOLD_ORE.clone().with_count(1000))
                .stamina_recovery_slots(5);

            if !prepare.is_completed(game_state)? {
                return Ok(prepare.into());
            }
        }

        if current_room.name != "Mine"
            && !current_room.name.contains("UndergroundMine")
        {
            let movement = MovementGoal::new(
                "Mine",
                game_state.get_mine_elevator()?.into(),
            )
            .with_tolerance(1000.0);
            return Ok(movement.into());
        }

        if current_room.name == "Mine" {
            // Currently at the top level of the mines

            let prepare = InventoryGoal::empty()
                .room("Mine")
                .with_exactly(Item::STONE.clone().with_count(100))
                .with(Item::PICKAXE)
                .stamina_recovery_slots(2)
                .with_weapon();
            if !prepare.is_completed(game_state)? {
                return Ok(prepare.into());
            }

            let mine_ladder = current_room
                .action_tiles
                .iter()
                .find(|(_, action)| action == "Mine")
                .map(|(tile, _)| tile)
                .cloned()
                .ok_or(BotError::MineLadderNotFound)?;

            let descend = ActivateTile::new("Mine", mine_ladder)
                .cancel_if(|game_state| game_state.player.room_name != "Mine");
            return Ok(descend.into());
        }

        let opt_ladder = current_room
            .objects
            .iter()
            .find(|obj| matches!(obj.kind, ObjectKind::MineLadderDown))
            .map(|obj| obj.tile);
        if let Some(ladder) = opt_ladder {
            let room_name = current_room.name.clone();
            let goal = ActivateTile::new(current_room.name.clone(), ladder)
                .cancel_if(move |game_state| {
                    game_state.player.room_name != room_name
                });
            return Ok(goal.into());
        }

        let iter_stones = || {
            current_room
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::Stone(_)))
                .map(|obj| obj.tile)
        };

        let stones_to_mine: HashSet<Vector<isize>> = iter_stones().collect();

        let player_tile = game_state.player.tile();

        let opt_next_stone = current_room
            .pathfinding()
            .include_border(true)
            .iter_dijkstra(player_tile)
            .map(|(tile, _)| tile)
            .find(|tile| stones_to_mine.contains(&tile));

        if let Some(next_stone) = opt_next_stone {
            let goal = UseItemOnTile::new(
                Item::PICKAXE,
                current_room.name.clone(),
                next_stone,
            );
            return Ok(goal.into());
        }

        todo!("Handle case where no stones are remaining")
    }
}
