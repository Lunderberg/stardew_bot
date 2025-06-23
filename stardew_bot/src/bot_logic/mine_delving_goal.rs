use std::collections::{HashMap, HashSet};

use crate::{
    bot_logic::{
        ActivateTile, BotError, GameStateExt as _, InventoryGoal,
        MaintainStaminaGoal, MovementGoal, UseItemOnTile,
    },
    game_state::{
        Item, ObjectKind, ResourceClumpKind, SeededRng, StoneKind, Vector,
    },
    Error, GameAction, GameState,
};

use super::{
    best_weapon,
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    CraftItemGoal, ObjectKindExt as _,
};

pub struct MineDelvingGoal;

struct MineSingleLevel {
    mineshaft_level: i32,
}

struct MineNearbyOre {
    dist: f32,
}

struct StonePredictor {
    game_id: u64,
    days_played: u32,
    daily_luck: f64,
    mineshaft_level: i32,
    num_enemies: usize,
    num_stones: usize,
    generated_ladder: bool,
}

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

        let room_name = game_state.player.room_name.clone();
        Ok(activate_elevator
            .cancel_if(move |game_state| {
                game_state.player.room_name != room_name
            })
            .into())
    }

    fn expand_furnace_array(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.player.room_name != "Mine" {
            return Ok(None);
        }

        let offsets: [Vector<isize>; 12] = [
            // Along back wall of mines
            Vector::new(1, 1),
            Vector::new(2, 1),
            Vector::new(3, 1),
            Vector::new(4, 1),
            Vector::new(5, 1),
            Vector::new(6, 1),
            Vector::new(7, 1),
            Vector::new(8, 1),
            // Along right wall of mines
            Vector::new(8, 2),
            Vector::new(8, 3),
            Vector::new(8, 4),
            Vector::new(8, 5),
        ];

        let mine_elevator = game_state.get_mine_elevator()?;
        let walkable = game_state.current_room()?.pathfinding().walkable();

        let opt_build_next = offsets
            .into_iter()
            .map(|offset| mine_elevator + offset)
            .find(|tile| walkable[*tile]);
        let Some(tile) = opt_build_next else {
            return Ok(None);
        };

        let prepare = InventoryGoal::current()
            .room("Mine")
            .with(Item::STONE.clone().with_count(25))
            .with(Item::COPPER_ORE.clone().with_count(20));
        let craft = CraftItemGoal::new(Item::FURNACE);
        let place = UseItemOnTile::new(Item::FURNACE, "Mine", tile);

        if !prepare.has_sufficient_stored(game_state)? {
            return Ok(None);
        }

        let stack = LogicStack::new().then(prepare).then(craft).then(place);

        Ok(Some(stack))
    }

    fn at_mine_entrance(
        &self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if let Some(craft_furnace) = self.expand_furnace_array(game_state)? {
            return Ok(craft_furnace.into());
        }

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

        let descend = ActivateTile::new("Mine", mine_ladder)
            .cancel_if(|game_state| game_state.player.room_name != "Mine");
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

        let mineshaft_level = current_room
            .mineshaft_details
            .as_ref()
            .map(|details| details.mineshaft_level)
            .unwrap_or(0);
        let room_name = current_room.name.clone();
        let goal = MineSingleLevel { mineshaft_level }
            .cancel_if(move |game_state| {
                game_state.player.room_name != room_name
            })
            .with_interrupt(MineNearbyOre::new())
            .with_interrupt(MaintainStaminaGoal::new());
        Ok(goal.into())
    }
}

impl BotGoal for MineSingleLevel {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Mine through level {}", self.mineshaft_level).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let current_room = game_state.current_room()?;
        if current_room
            .mineshaft_details
            .as_ref()
            .map(|details| details.mineshaft_level)
            != Some(self.mineshaft_level)
        {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.dialogue_menu.is_some() {
            // The return-to-surface menu is open, so send a
            // confirmation.
            actions.do_action(GameAction::ConfirmMenu);
            return Ok(BotGoalResult::InProgress);
        }

        let opt_weapon = best_weapon(game_state.player.inventory.iter_items());

        let clearable_tiles: HashMap<Vector<isize>, Option<Item>> = {
            let iter_clearable_obj =
                current_room.objects.iter().filter_map(|obj| {
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
                });
            let iter_clearable_clump = current_room
                .resource_clumps
                .iter()
                .filter_map(|clump| {
                    let tool = match &clump.kind {
                        ResourceClumpKind::MineBoulder => Some(Item::PICKAXE),
                        _ => None,
                    }?;
                    Some(
                        clump
                            .shape
                            .iter_points()
                            .map(move |tile| (tile, Some(tool.clone()))),
                    )
                })
                .flatten();

            iter_clearable_obj.chain(iter_clearable_clump).collect()
        };
        let player_tile = game_state.player.tile();

        let should_go_up = 'go_up: {
            if game_state.player.current_stamina <= 5.0 {
                // Not enough food to restore stamina, go up.
                break 'go_up true;
            }
            if self.mineshaft_level % 5 != 0 {
                // Not a mineshaft level, so keep going.
                break 'go_up false;
            }

            let num_empty_slots = game_state.player.inventory.num_empty_slots();
            if num_empty_slots < 5 {
                break 'go_up true;
            }

            let num_weapons = game_state
                .player
                .inventory
                .iter_items()
                .filter(|item| item.as_weapon().is_some())
                .count();
            if num_weapons > 1 {
                break 'go_up true;
            }

            let inventory = game_state.player.inventory.to_hash_map();
            let get_count = |item: &Item| {
                inventory.get(item.as_ref()).cloned().unwrap_or(0)
            };
            get_count(&Item::COPPER_ORE) > 50 || get_count(&Item::IRON_ORE) > 50
        };

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

        let target_tile = if should_go_up {
            ladder_up
        } else if let Some(ladder_down) = opt_ladder_down {
            ladder_down
        } else {
            let predictor = StonePredictor::new(game_state)?;
            let ladder_stones: HashSet<Vector<isize>> = current_room
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::Stone(_)))
                .map(|obj| obj.tile)
                .filter(|tile| predictor.will_produce_ladder(*tile))
                .collect();

            if ladder_stones.is_empty() {
                current_room
                    .pathfinding()
                    .include_border(true)
                    .iter_dijkstra(player_tile)
                    .map(|(tile, _)| tile)
                    .find(|tile| clearable_tiles.contains_key(&tile))
                    .expect("Handle case where everything has been cleared")
            } else {
                current_room
                    .pathfinding()
                    .include_border(true)
                    .stone_clearing_cost(500)
                    .fiber_clearing_cost(500)
                    .iter_dijkstra(player_tile)
                    .map(|(tile, _)| tile)
                    .find(|tile| ladder_stones.contains(tile))
                    .expect(
                        "Handle case where ladder stone exists, \
                         but is inaccessible",
                    )
            }
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

        if let Some(tool) = opt_tool {
            let room_name = game_state.player.room_name.clone();
            let goal = UseItemOnTile::new(
                tool.clone(),
                room_name.clone(),
                target_tile,
            );
            Ok(goal.into())
        } else {
            let room_name = game_state.player.room_name.clone();
            let goal = ActivateTile::new(room_name.clone(), target_tile);
            Ok(goal.into())
        }
    }
}

impl StonePredictor {
    pub fn new(game_state: &GameState) -> Result<Self, Error> {
        let room = game_state.current_room()?;

        let game_id = game_state.globals.unique_id;
        let daily_luck = game_state.daily.daily_luck;
        let (mineshaft_level, generated_ladder) =
            if let Some(details) = &room.mineshaft_details {
                (details.mineshaft_level, details.generated_ladder)
            } else {
                (0, false)
            };

        let num_enemies = room
            .characters
            .iter()
            .filter(|character| character.health.is_some())
            .count();

        let num_stones = room
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::Stone(_)))
            .count();

        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        Ok(Self {
            game_id,
            days_played,
            daily_luck,
            mineshaft_level,
            generated_ladder,
            num_enemies,
            num_stones,
        })
    }

    pub fn ladder_chance(&self) -> f32 {
        if self.generated_ladder {
            return 0.0;
        }

        let base_chance = 0.02;
        let from_remaining_stones = 1.0 / (self.num_stones.max(1) as f64);
        let from_daily_luck = self.daily_luck / 5.0;
        let from_enemies = if self.num_enemies == 0 { 0.04 } else { 0.0 };

        let chance = base_chance
            + from_remaining_stones
            + from_daily_luck
            + from_enemies;
        chance as f32
    }

    fn generate_rng(&self, tile: Vector<isize>) -> SeededRng {
        let mut rng = SeededRng::from_stardew_seed([
            self.days_played as f64,
            (self.game_id / 2) as f64,
            (tile.right * 1000) as f64,
            tile.down as f64,
            self.mineshaft_level as f64,
        ]);

        // Skip unused value;
        rng.rand_i32();

        rng
    }

    pub fn will_produce_ladder(&self, tile: Vector<isize>) -> bool {
        let mut rng = self.generate_rng(tile);
        rng.rand_float() < self.ladder_chance()
    }
}

impl MineNearbyOre {
    pub fn new() -> Self {
        Self { dist: 4.0 }
    }
}
impl BotInterrupt for MineNearbyOre {
    fn description(&self) -> std::borrow::Cow<str> {
        "Mine nearby ore".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<super::bot_logic::LogicStack>, Error> {
        let loc = game_state.current_room()?;
        if loc.mineshaft_details.is_none() {
            return Ok(None);
        }

        let player_tile = game_state.player.tile();
        let reachable = loc
            .pathfinding()
            .include_border(true)
            .distances(player_tile);

        let opt_nearby_stone = loc.objects.iter().find(|obj| {
            let max_dist = match &obj.kind {
                ObjectKind::Stone(
                    StoneKind::Copper
                    | StoneKind::Iron
                    | StoneKind::Gold
                    | StoneKind::Iridium
                    | StoneKind::Gem
                    | StoneKind::Mystic
                    | StoneKind::Diamond
                    | StoneKind::Ruby
                    | StoneKind::Jade
                    | StoneKind::Amethyst
                    | StoneKind::Topaz
                    | StoneKind::Emerald
                    | StoneKind::Aquamarine,
                )
                | ObjectKind::MineCartCoal
                | ObjectKind::Chest(_)
                | ObjectKind::Mineral(_) => self.dist,
                _ => {
                    return false;
                }
            };

            reachable
                .get(obj.tile)
                .cloned()
                .flatten()
                .map(|dist| (dist as f32) <= max_dist)
                .unwrap_or(false)
        });

        Ok(opt_nearby_stone.map(|obj| {
            if let Some(tool) = obj.kind.get_tool() {
                UseItemOnTile::new(
                    tool,
                    game_state.player.room_name.clone(),
                    obj.tile,
                )
                .into()
            } else {
                ActivateTile::new(game_state.player.room_name.clone(), obj.tile)
                    .into()
            }
        }))
    }
}
