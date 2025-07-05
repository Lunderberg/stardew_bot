use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{
        bot_logic::LogicStackItem, ActivateTile, BotError, GameStateExt as _,
        InventoryGoal, MaintainStaminaGoal, MovementGoal, UseItemOnTile,
    },
    game_state::{
        Item, ItemId, Object, ObjectKind, Quality, ResourceClumpKind,
        SeededRng, StoneKind, Vector,
    },
    Error, GameAction, GameState,
};

use super::{
    best_weapon,
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    AttackNearbyEnemy, CraftItemGoal, LocationExt as _, ObjectKindExt as _,
    OrganizeInventoryGoal,
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
    mining_level: u8,
    daily_luck: f32,
    mineshaft_level: i32,
    num_enemies: usize,
    num_stones: usize,
    generated_ladder: bool,
}

#[derive(Default)]
struct StonePrediction {
    ladder: bool,
    coal: bool,
    gem: bool,
    ore: bool,
    geode: bool,
    omnigeode: bool,
}

const OFFSETS_ELEVATOR_TO_FURNACE: [Vector<isize>; 12] = [
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

fn collect_clearable_tiles(
    game_state: &GameState,
) -> Result<HashMap<Vector<isize>, Option<Item>>, Error> {
    let current_room = game_state.current_room()?;
    let opt_weapon = best_weapon(game_state.player.inventory.iter_items());

    let iter_clearable_obj = current_room.objects.iter().filter_map(|obj| {
        let opt_tool = match &obj.kind {
            ObjectKind::Stone(_) => Some(Item::PICKAXE),
            ObjectKind::Fiber | ObjectKind::MineBarrel
                if opt_weapon.is_some() =>
            {
                opt_weapon.cloned()
            }
            ObjectKind::Mineral(_) => None,

            other if other.is_forage() => None,

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

    let clearable_tiles =
        iter_clearable_obj.chain(iter_clearable_clump).collect();

    Ok(clearable_tiles)
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

    fn start_smelting(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.player.room_name != "Mine" {
            return Ok(None);
        }

        let loc = game_state.current_room()?;
        let (num_loadable_furnaces, num_harvestable_furnaces) = loc
            .objects
            .iter()
            .map(|obj| {
                obj.kind
                    .as_furnace()
                    .map(|furnace| {
                        let can_harvest = furnace.ready_to_harvest;
                        let can_load = !furnace.has_held_item || can_harvest;
                        (can_load, can_harvest)
                    })
                    .unwrap_or((false, false))
            })
            .fold(
                (0usize, 0usize),
                |(num_loadable, num_harvestable), (can_load, can_harvest)| {
                    (
                        num_loadable + (can_load as usize),
                        num_harvestable + (can_harvest as usize),
                    )
                },
            );

        if num_loadable_furnaces == 0 && num_harvestable_furnaces == 0 {
            return Ok(None);
        }

        let available = InventoryGoal::current()
            .room("Mine")
            .total_stored_and_carried(game_state)?;
        let get_available = |item: &ItemId| -> usize {
            available.get(item).cloned().unwrap_or(0)
        };

        let num_to_smelt =
            num_loadable_furnaces.min(get_available(&Item::COAL.id));

        if num_to_smelt == 0 && num_harvestable_furnaces == 0 {
            return Ok(None);
        }

        let num_iron_bars =
            (get_available(&Item::IRON_ORE.id) / 5).min(num_to_smelt);
        let num_to_smelt = num_to_smelt - num_iron_bars;

        let num_copper_bars =
            (get_available(&Item::COPPER_ORE.id) / 5).min(num_to_smelt);

        if num_iron_bars == 0
            && num_copper_bars == 0
            && num_harvestable_furnaces == 0
        {
            return Ok(None);
        }

        let prepare = InventoryGoal::current()
            .room("Mine")
            .with_exactly(
                Item::COAL
                    .clone()
                    .with_count(num_iron_bars + num_copper_bars),
            )
            .with_exactly(
                Item::COPPER_ORE.clone().with_count(num_copper_bars * 5),
            )
            .with_exactly(Item::IRON_ORE.clone().with_count(num_iron_bars * 5));

        let mut iter_smelt = std::iter::empty()
            .chain(std::iter::repeat_n(Item::COPPER_ORE, num_copper_bars))
            .chain(std::iter::repeat_n(Item::IRON_ORE, num_iron_bars));

        let stack = loc
            .objects
            .iter()
            .filter_map(|obj| {
                obj.kind.as_furnace().map(|furnace| (obj.tile, furnace))
            })
            .flat_map(|(tile, furnace)| -> [Option<LogicStackItem>; 2] {
                let can_harvest = furnace.ready_to_harvest;
                let take_complete = can_harvest
                    .then(|| ActivateTile::new("Mine", tile))
                    .map(Into::into);

                let can_load = !furnace.has_held_item || can_harvest;
                let start_new = can_load
                    .then(|| {
                        iter_smelt
                            .next()
                            .map(|ore| UseItemOnTile::new(ore, "Mine", tile))
                    })
                    .flatten()
                    .map(Into::into);
                [take_complete, start_new]
            })
            .flatten()
            .fold(LogicStack::new().then(prepare), |stack, goal| {
                stack.with_item(goal)
            });

        Ok(Some(stack))
    }

    fn expand_furnace_array(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if game_state.player.room_name != "Mine" {
            return Ok(None);
        }

        let mine_elevator = game_state.get_mine_elevator()?;
        let walkable = game_state.current_room()?.pathfinding().walkable();

        let opt_build_next = OFFSETS_ELEVATOR_TO_FURNACE
            .iter()
            .cloned()
            .map(|offset| mine_elevator + offset)
            .find(|tile| walkable[*tile]);
        let Some(tile) = opt_build_next else {
            return Ok(None);
        };

        let has_furnace = game_state
            .player
            .inventory
            .iter_items()
            .any(|item| item.is_same_item(&Item::FURNACE));
        if has_furnace {
            return Ok(Some(
                UseItemOnTile::new(Item::FURNACE, "Mine", tile).into(),
            ));
        }

        let prepare = InventoryGoal::current()
            .room("Mine")
            .with(Item::STONE.clone().with_count(25))
            .with(Item::COPPER_ORE.clone().with_count(20));
        if !prepare.has_sufficient_stored(game_state)? {
            return Ok(None);
        }
        if prepare.is_completed(game_state)? {
            Ok(Some(CraftItemGoal::new(Item::FURNACE).into()))
        } else {
            Ok(Some(prepare.into()))
        }
    }

    fn prefer_mining_copper(
        &self,
        game_state: &GameState,
    ) -> Result<bool, Error> {
        let items = InventoryGoal::empty()
            .room("Mine")
            .total_stored_and_carried(game_state)?;
        let get_count = |item: &Item| -> usize {
            items.get(&item.id).cloned().unwrap_or(0)
        };

        let enough_ore_to_smelt = get_count(&Item::COAL) >= 1
            && (get_count(&Item::COPPER_ORE) >= 5
                && get_count(&Item::IRON_ORE) >= 5);
        let num_furnaces = game_state
            .get_room("Mine")?
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::Furnace(_)))
            .count();
        if enough_ore_to_smelt
            && num_furnaces < OFFSETS_ELEVATOR_TO_FURNACE.len()
        {
            // We have enough ore/coal to smelt a bar, and aren't
            // smelting it.  Therefore, it must be because we don't
            // have enough smelters.  Mine more copper to make more
            // smelters.
            return Ok(true);
        }

        // If there are more iron bars than copper bars, mine more
        // copper.

        let copper_bars =
            get_count(&Item::COPPER_BAR) + get_count(&Item::COPPER_ORE) / 5;
        let iron_bars =
            get_count(&Item::IRON_BAR) + get_count(&Item::IRON_ORE) / 5;

        Ok(copper_bars <= iron_bars)
    }

    fn at_mine_entrance(
        &self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if let Some(start_smelting) = self.start_smelting(game_state)? {
            return Ok(start_smelting.into());
        }

        if let Some(craft_furnace) = self.expand_furnace_array(game_state)? {
            return Ok(craft_furnace.into());
        }

        let prepare = InventoryGoal::empty()
            .room("Mine")
            .with(Item::PICKAXE)
            .stamina_recovery_slots(2)
            .with_weapon()
            .with(Item::COAL.clone().with_count(1000))
            .with(Item::COPPER_BAR.clone().with_count(1000))
            .with(Item::IRON_BAR.clone().with_count(1000))
            .with(Item::GEODE.clone().with_count(1000))
            .with(Item::FROZEN_GEODE.clone().with_count(1000))
            .with(Item::MAGMA_GEODE.clone().with_count(1000))
            .with(Item::OMNI_GEODE.clone().with_count(1000));
        if !prepare.is_completed(game_state)? {
            return Ok(prepare.into());
        }

        let organization = OrganizeInventoryGoal::new(|item| {
            use super::SortedInventoryLocation as Loc;
            if item.as_weapon().is_some() || Item::PICKAXE.is_same_item(&item) {
                Loc::HotBarLeft
            } else if item.edibility > 0 {
                Loc::HotBarRight
            } else if item.is_same_item(&Item::COAL)
                || item.is_same_item(&Item::COPPER_BAR)
                || item.is_same_item(&Item::IRON_BAR)
                || item.is_same_item(&Item::GEODE)
                || item.is_same_item(&Item::FROZEN_GEODE)
                || item.is_same_item(&Item::MAGMA_GEODE)
                || item.is_same_item(&Item::OMNI_GEODE)
            {
                Loc::HotBar
            } else {
                Loc::Hidden
            }
        });
        if !organization.is_completed(game_state) {
            return Ok(organization.into());
        }

        let elevator_depth =
            game_state.globals.lowest_mine_level_reached.clamp(0, 120) as usize;

        if elevator_depth >= 5 {
            let go_to_depth = if self.prefer_mining_copper(game_state)? {
                20
            } else if elevator_depth < 80 {
                elevator_depth
            } else {
                60
            };
            let go_to_depth = go_to_depth.min(elevator_depth);
            return Self::elevator_to_floor(game_state, actions, go_to_depth);
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
            // Bring enough wood to make two storage chests at the
            // mines.  If the mines already have two storage chests,
            // or if the first storage chest contains wood to craft
            // the second, no need to bring wood along.
            let mines = game_state.get_room("Mine")?;

            let currently_stored_in_mines = mines
                .iter_stored_items()
                .map(|item| (&item.id, item.count))
                .into_grouping_map()
                .sum();

            let wood_from_crafted_chests = 50
                * mines
                    .objects
                    .iter()
                    .filter_map(|obj| obj.kind.as_chest())
                    .count();

            let iter_items_to_transfer = [
                Item::WOOD.with_count(
                    100usize.saturating_sub(wood_from_crafted_chests),
                ),
                Item::STONE.with_count(1000),
                Item::COAL.with_count(100),
                Item::COPPER_ORE.with_count(1000),
                Item::COPPER_BAR.with_count(1000),
                Item::IRON_ORE.with_count(1000),
                Item::IRON_BAR.with_count(1000),
                Item::GOLD_ORE.with_count(1000),
            ]
            .into_iter()
            .map(|item| {
                if let Some(num_current) =
                    currently_stored_in_mines.get(&item.id)
                {
                    let new_count = item.count.saturating_sub(*num_current);
                    item.with_count(new_count)
                } else {
                    item
                }
            })
            .filter(|item| item.count > 0);

            let prepare = InventoryGoal::empty()
                .with(Item::PICKAXE)
                .with_exactly(iter_items_to_transfer)
                .stamina_recovery_slots(6)
                .with_weapon();

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

        // If we are returning to the copper levels after having
        // reached iron, then we're here to get more copper ore.
        let prefer_mining_ore = (mineshaft_level < 40
            && game_state.globals.lowest_mine_level_reached >= 40)
            || (mineshaft_level < 80
                && game_state.globals.lowest_mine_level_reached >= 80);
        let mining_dist = if prefer_mining_ore { 16.0 } else { 4.0 };

        let room_name = current_room.name.clone();
        let goal = MineSingleLevel { mineshaft_level }
            .cancel_if(move |game_state| {
                game_state.player.room_name != room_name
            })
            .with_interrupt(
                MineNearbyOre::new().with_search_distance(mining_dist),
            )
            .with_interrupt(MaintainStaminaGoal::new())
            .with_interrupt(AttackNearbyEnemy::new());
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

        let clearable_tiles = collect_clearable_tiles(game_state)?;
        let player_tile = game_state.player.tile();

        let should_go_up = 'go_up: {
            if game_state.player.current_stamina <= 5.0 {
                // Not enough food to restore stamina, go up.
                break 'go_up true;
            }

            if self.mineshaft_level / 5
                < game_state.globals.lowest_mine_level_reached / 5
                && self.mineshaft_level % 10 != 0
            {
                // We are revisiting this level for ore.
                // Therefore, go back up as soon as the
                // MineNearbyOre interrupts are completed.
                break 'go_up true;
            }

            if self.mineshaft_level % 5 != 0 {
                // This is the deepest we've gone, and this level
                // doesn't have an elevator.  Therefore, keep
                // going to the next elevator level.
                break 'go_up false;
            }

            let dist_from_ladder = current_room
                .objects
                .iter()
                .filter(|obj| matches!(obj.kind, ObjectKind::MineLadderUp))
                .map(|obj| obj.tile.manhattan_dist(player_tile))
                .min()
                .unwrap_or(100);
            if dist_from_ladder > 3 {
                // We may have picked up additional items on a level
                // that has an elevator.  In that case, should keep
                // going on the floor, even if the inventory state has
                // changed.
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
            get_count(&Item::COPPER_ORE) >= 5 || get_count(&Item::IRON_ORE) >= 5
        };

        if should_go_up && game_state.mine_elevator_menu.is_some() {
            return MineDelvingGoal::elevator_to_floor(game_state, actions, 0);
        }

        let pathfinding = current_room
            .pathfinding()
            .include_border(true)
            .mine_boulder_clearing_cost(10000)
            .stone_clearing_cost(3000)
            .breakable_clearing_cost(500);

        let distances = pathfinding.distances(player_tile);

        let opt_ladder_down = current_room
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::MineLadderDown))
            .map(|obj| obj.tile)
            .filter(|tile| distances.is_some(*tile))
            .min_by_key(|tile| distances[*tile].unwrap());

        let target_tile = if should_go_up {
            current_room
                .objects
                .iter()
                .filter(|obj| {
                    matches!(
                        obj.kind,
                        ObjectKind::MineLadderUp
                            | ObjectKind::MineLadderDown
                            | ObjectKind::MineElevator
                    )
                })
                .map(|obj| obj.tile)
                .filter(|tile| distances.is_some(*tile))
                .min_by_key(|tile| distances[*tile].unwrap())
                .ok_or(BotError::MineLadderNotFound)?
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

            pathfinding
                .iter_dijkstra(player_tile)
                .map(|(tile, _)| tile)
                .find(|tile| {
                    if ladder_stones.is_empty() {
                        clearable_tiles.contains_key(&tile)
                    } else {
                        ladder_stones.contains(tile)
                    }
                })
                .ok_or(BotError::MineLadderNotFound)?
        };

        let path = pathfinding
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

        let stack = LogicStack::new();
        let stack = if let Some(tool) = opt_tool {
            let room_name = game_state.player.room_name.clone();
            let goal = UseItemOnTile::new(
                tool.clone(),
                room_name.clone(),
                target_tile,
            );
            stack.then(goal)
        } else {
            let room_name = game_state.player.room_name.clone();
            let goal = ActivateTile::new(room_name.clone(), target_tile);
            stack.then(goal)
        };

        let num_objects = current_room.objects.len();
        let stack = stack.cancel_if(move |game_state| {
            // If the number of objects in the room increased, then
            // cancel out and re-plan.  This can occur if a nearby
            // enemy is killed, dropping a ladder.
            let current_num_objects = game_state
                .current_room()
                .map(|room| room.objects.len())
                .unwrap_or(num_objects);
            current_num_objects > num_objects
        });

        Ok(stack.into())
    }
}

impl StonePredictor {
    pub fn new(game_state: &GameState) -> Result<Self, Error> {
        let room = game_state.current_room()?;

        let game_id = game_state.globals.game_id;
        let daily_luck = game_state.daily.daily_luck as f32;
        let mining_level = game_state.player.skills.mining_level();

        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

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

        Ok(Self {
            game_id,
            days_played,
            daily_luck,
            mining_level,
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
        let from_remaining_stones = 1.0 / (self.num_stones.max(1) as f32);
        let from_daily_luck = self.daily_luck / 5.0;
        let from_enemies = if self.num_enemies == 0 { 0.04 } else { 0.0 };

        base_chance + from_remaining_stones + from_daily_luck + from_enemies
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

    pub fn drop_from_stone(
        &self,
        tile: Vector<isize>,
        kind: &StoneKind,
    ) -> StonePrediction {
        let mut prediction = StonePrediction::default();

        let mut rng = self.generate_rng(tile);

        // Skip RNG roll to generate a ladder, only called if the
        // level did not contain a ladder during generation.
        if !self.generated_ladder {
            prediction.ladder = rng.rand_float() < self.ladder_chance();
        }

        match kind {
            StoneKind::Normal => {}
            StoneKind::DoubleStone => {
                // 3 rolls that determine the number of stone
                for _ in 0..3 {
                    rng.rand_i32();
                }

                prediction.coal = rng.rand_float() < 0.08;
                return prediction;
            }
            StoneKind::Copper
            | StoneKind::Iron
            | StoneKind::Gold
            | StoneKind::Iridium => {
                prediction.ore = true;
                return prediction;
            }

            StoneKind::Gem
            | StoneKind::Mystic
            | StoneKind::Diamond
            | StoneKind::Ruby
            | StoneKind::Jade
            | StoneKind::Amethyst
            | StoneKind::Topaz
            | StoneKind::Emerald
            | StoneKind::Aquamarine => {
                prediction.gem = true;
                return prediction;
            }

            StoneKind::Other { name, id } => todo!(
                "Handle stone with name {name}, \
                 id {id} in the StonePredictor"
            ),
        }

        // TODO: If the player has Gemologist profession, skip a roll
        // for the extra gem.  (Roll occurs regardless of whether the
        // stone type can actually drop a gem.)

        // TODO: If the player has unlocked Secret Note drops, skip a
        // roll that determines whether a secret note will be dropped.

        let chance_modifier =
            self.daily_luck / 2.0 + (self.mining_level as f32) * 0.005;

        let geode_chance = 0.022 * (1.0 + chance_modifier);
        prediction.geode = rng.rand_float() < geode_chance;

        if self.mineshaft_level > 20 {
            let omnigeode_chance = 0.005 * (1.0 + chance_modifier);
            prediction.omnigeode = rng.rand_float() < omnigeode_chance;
        }

        prediction
    }
}

impl MineNearbyOre {
    pub fn new() -> Self {
        Self { dist: 4.0 }
    }

    pub fn with_search_distance(self, dist: f32) -> Self {
        Self { dist, ..self }
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
        let pathfinding = loc
            .pathfinding()
            .breakable_clearing_cost(200)
            .stone_clearing_cost(2000)
            .include_border(true);
        let distances = pathfinding.distances(player_tile);

        let opt_weapon = best_weapon(game_state.player.inventory.iter_items());

        let predictor = StonePredictor::new(game_state)?;
        let mut desirable_rocks: HashMap<Vector<isize>, (f32, &ObjectKind)> =
            loc.objects
                .iter()
                .filter_map(|obj| {
                    let dist_multiplier = match &obj.kind {
                        ObjectKind::Stone(stone) => {
                            let prediction =
                                predictor.drop_from_stone(obj.tile, stone);
                            if prediction.omnigeode {
                                2.0
                            } else if prediction.geode {
                                1.5
                            } else if prediction.gem {
                                match stone {
                                    StoneKind::Diamond => 4.0,
                                    _ => 2.0,
                                }
                            } else if prediction.coal {
                                1.5
                            } else if prediction.ore {
                                1.0
                            } else {
                                return None;
                            }
                        }
                        ObjectKind::MineCartCoal => 3.0,

                        ObjectKind::MineBarrel if opt_weapon.is_some() => 0.5,

                        ObjectKind::Chest(_) => 4.0,
                        ObjectKind::Mineral(_) => 1.0,

                        other if other.is_forage() => 1.0,

                        _ => {
                            return None;
                        }
                    };
                    Some((obj.tile, (dist_multiplier, &obj.kind)))
                })
                .collect();

        {
            let mut to_group: HashSet<Vector<isize>> =
                desirable_rocks.iter().map(|(tile, _)| *tile).collect();
            while !to_group.is_empty() {
                let seed = to_group.iter().next().cloned().unwrap();
                to_group.remove(&seed);

                let mut current_group = vec![seed];
                let mut to_visit = vec![seed];
                while let Some(visiting) = to_visit.pop() {
                    for adj in visiting.iter_nearby() {
                        if to_group.contains(&adj) {
                            to_group.remove(&adj);
                            to_visit.push(adj);
                            current_group.push(adj);
                        }
                    }
                }

                if current_group.len() > 1 {
                    let total_multiplier = current_group
                        .iter()
                        .map(|tile| desirable_rocks[tile].0)
                        .sum::<f32>();
                    current_group.into_iter().for_each(|tile| {
                        desirable_rocks.get_mut(&tile).unwrap().0 =
                            total_multiplier;
                    });
                }
            }
        }

        let opt_closest_stone = desirable_rocks
            .iter()
            .filter_map(|(tile, (multiplier, _))| {
                let dist = distances.get(*tile)?.as_ref()?;
                ((*dist as f32) < self.dist * multiplier)
                    .then(|| (*tile, *dist))
            })
            .min_by_key(|(_, dist)| *dist)
            .map(|(tile, _)| tile);

        let Some(target_tile) = opt_closest_stone else {
            return Ok(None);
        };

        let path = pathfinding
            .allow_diagonal(false)
            .path_between(player_tile, target_tile)?;

        let clearable_tiles = collect_clearable_tiles(game_state)?;
        let opt_blocked_tile_in_path = path
            .into_iter()
            .find(|tile| clearable_tiles.contains_key(&tile));
        let target_tile = opt_blocked_tile_in_path.unwrap_or(target_tile);

        let opt_tool = desirable_rocks
            .get(&target_tile)
            .map(|(_, kind)| kind.get_tool())
            .or_else(|| {
                clearable_tiles
                    .get(&target_tile)
                    .map(|opt_tool| opt_tool.clone())
            })
            .unwrap();

        let goal = if let Some(tool) = opt_tool {
            let tool = if tool.id.item_id.starts_with("(W)") {
                if let Some(weapon) = opt_weapon {
                    weapon.clone()
                } else {
                    return Ok(None);
                }
            } else {
                tool.clone()
            };
            UseItemOnTile::new(
                tool,
                game_state.player.room_name.clone(),
                target_tile,
            )
            .into()
        } else {
            ActivateTile::new(game_state.player.room_name.clone(), target_tile)
                .into()
        };

        Ok(Some(goal))
    }
}
