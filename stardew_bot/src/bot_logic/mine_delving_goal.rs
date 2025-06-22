use std::collections::{HashMap, HashSet};

use crate::{
    bot_logic::{
        ActivateTile, BotError, GameStateExt as _, InventoryGoal,
        MaintainStaminaGoal, MovementGoal, UseItemOnTile,
    },
    game_state::{Item, ObjectKind, Vector},
    Error, GameAction, GameState,
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

    fn elevator_to_floor(
        &self,
        game_state: &GameState,
        actions: &mut ActionCollector,
        depth: usize,
    ) -> Result<BotGoalResult, Error> {
        let room_name = &game_state.player.room_name;
        let is_in_mines = room_name.starts_with("UndergroundMine");

        if is_in_mines && room_name == &format!("UndergroundMine{depth}") {
            return Ok(BotGoalResult::Completed);
        }

        if let Some(menu) = &game_state.mine_elevator_menu {
            let i_button = depth / 5;
            let pixel =
                menu.buttons.get(i_button).cloned().unwrap_or_else(|| {
                    panic!(
                        "TODO: Requested depth {depth}, \
                         which requires button {i_button}, \
                         but elevator only has {} buttons",
                        menu.buttons.len(),
                    )
                });
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let activate_elevator = if is_in_mines {
            let tile = game_state
                .current_room()?
                .objects
                .iter()
                .find(|obj| matches!(obj.kind, ObjectKind::MineElevator))
                .ok_or(BotError::MineElevatorNotFound)?
                .tile;
            ActivateTile::new(game_state.player.room_name.clone(), tile)
        } else {
            let tile = game_state
                .get_room("Mine")?
                .action_tiles
                .iter()
                .find(|(_, action)| action == "MineElevator")
                .map(|(tile, _)| tile)
                .cloned()
                .ok_or(BotError::MineElevatorNotFound)?;
            ActivateTile::new("Mine", tile)
        };

        Ok(activate_elevator.allow_room_change(false).into())
    }

    fn at_mine_entrance(
        &self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let prepare = InventoryGoal::empty()
            .room("Mine")
            .with_exactly(Item::STONE.clone().with_count(100))
            .with(Item::PICKAXE)
            .stamina_recovery_slots(2)
            .with_weapon();
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.into());
        }

        let elevator_depth =
            game_state.globals.lowest_mine_level_reached.clamp(0, 120) as usize;

        if elevator_depth >= 5 {
            return self.elevator_to_floor(game_state, actions, elevator_depth);
        }

        let mine_ladder = game_state
            .current_room()?
            .action_tiles
            .iter()
            .find(|(_, action)| {
                if elevator_depth == 0 {
                    action == "Mine"
                } else {
                    action == "MineElevator"
                }
            })
            .map(|(tile, _)| tile)
            .cloned()
            .ok_or(BotError::MineLadderNotFound)?;

        let descend =
            ActivateTile::new("Mine", mine_ladder).allow_room_change(false);
        return Ok(descend.into());
    }
}

impl BotGoal for MineDelvingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        "Delve Mines".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
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
            return self.at_mine_entrance(game_state, actions);
        }

        if game_state.dialogue_menu.is_some() {
            // The return-to-surface menu is open, so send a
            // confirmation.
            actions.do_action(GameAction::ConfirmMenu);
            return Ok(BotGoalResult::InProgress);
        }

        let goal = MaintainStaminaGoal::new();
        if !goal.is_completed(game_state) {
            return Ok(goal.into());
        }

        let opt_weapon = game_state
            .player
            .inventory
            .iter_items()
            .find(|item| item.as_weapon().is_some());

        let clearable_tiles: HashMap<Vector<isize>, Option<Item>> =
            current_room
                .objects
                .iter()
                .filter_map(|obj| {
                    let opt_tool = match &obj.kind {
                        ObjectKind::Stone(_) => Some(Item::PICKAXE),
                        ObjectKind::Fiber if opt_weapon.is_some() => {
                            opt_weapon.cloned()
                        }
                        ObjectKind::Mineral(_) => None,
                        _ => {
                            return None;
                        }
                    };
                    Some((obj.tile, opt_tool))
                })
                .collect();
        let player_tile = game_state.player.tile();

        let ladder_up = current_room
            .objects
            .iter()
            .find(|obj| matches!(obj.kind, ObjectKind::MineLadderUp))
            .map(|obj| obj.tile)
            .ok_or(BotError::MineLadderNotFound)?;

        let opt_ladder_down = current_room
            .objects
            .iter()
            .find(|obj| matches!(obj.kind, ObjectKind::MineLadderDown))
            .map(|obj| obj.tile);

        let target_tile = if game_state.player.current_stamina <= 5.0 {
            ladder_up
        } else if let Some(ladder_down) = opt_ladder_down {
            ladder_down
        } else {
            current_room
                .pathfinding()
                .include_border(true)
                .iter_dijkstra(player_tile)
                .map(|(tile, _)| tile)
                .find(|tile| clearable_tiles.contains_key(&tile))
                .expect("Handle case where everything has been cleared")
        };

        let path = current_room
            .pathfinding()
            .stone_clearing_cost(2000)
            .fiber_clearing_cost(500)
            .include_border(true)
            .allow_diagonal(false)
            .path_between(player_tile, target_tile)?;

        let opt_blocked_tile_in_path = path
            .into_iter()
            .find(|tile| clearable_tiles.contains_key(&tile));
        let target_tile = opt_blocked_tile_in_path.unwrap_or(target_tile);

        let opt_tool = clearable_tiles
            .get(&target_tile)
            .map(|opt| opt.as_ref())
            .flatten();

        let room_name = current_room.name.clone();
        if let Some(tool) = opt_tool {
            let goal = UseItemOnTile::new(tool.clone(), room_name, target_tile)
                .allow_room_change(false);
            Ok(goal.into())
        } else {
            let goal = ActivateTile::new(room_name, target_tile)
                .allow_room_change(false);
            Ok(goal.into())
        }
    }
}
