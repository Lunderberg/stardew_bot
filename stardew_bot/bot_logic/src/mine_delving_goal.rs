use std::collections::{HashMap, HashSet};

use geometry::{Direction, Vector};
use itertools::{Either, Itertools as _};

use crate::{
    bot_logic::LogicStackItem, ActivateTile, CraftItemGoal, Error, GameAction,
    GameStateExt as _, InventoryGoal, ItemIterExt as _, MaintainStaminaGoal,
    MovementGoal, UseItemOnTile,
};
use game_state::{
    GameState, ItemCategory, ItemId, MineralKind, ObjectKind, SeededRng,
    StoneKind, TileMap,
};

use super::{
    best_weapon,
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    AttackNearbyEnemy, LocationExt as _, MenuCloser, OrganizeInventoryGoal,
};

pub struct MineDelvingGoal;

struct GoToDepth {
    depth: i32,
}

struct SmeltingPlan {
    old_furnaces: usize,
    new_furnaces: usize,
    copper_bars: usize,
    iron_bars: usize,
    quartz: usize,
    fire_quartz: usize,
}

struct MineSingleLevel {
    mineshaft_level: i32,
}

struct MineNearbyOre {
    dist: f32,

    /// The game tick on which the most recent bomb was used.  Tracked
    /// to avoid placing multiple bombs at once.
    most_recent_bomb: Option<i32>,

    cache: Option<NearbyOreCache>,
}

struct NearbyOreCache {
    /// The number of objects present when this cache was created.
    num_objects: usize,

    /// The number of bombs present when this cache was created.
    num_bombs: usize,

    /// A lookup from tile to the (multiplier,offset) tuple for going
    /// to collect from that tile.
    desirable_rocks: HashMap<Vector<isize>, (f32, f32)>,

    /// A lookup of items that should be collected for a bundle, but
    /// which aren't already stored either in the player's inventory
    /// or in a storage chest.
    missing_items: HashSet<ItemId>,

    /// Tiles that are both clear and reachable.  Used to determine
    /// where bombs can be placed.
    placeable: TileMap<bool>,
}

struct WaitNearBomb;

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

pub struct MineLevelPredictor {
    game_id: u64,
    days_played: u32,
    days_ahead: u32,
}

#[derive(Debug, Default)]
pub struct LevelPrediction {
    pub is_mushroom: bool,
    pub is_monster: bool,
    pub is_slime: bool,
    pub is_dinosaur: bool,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum MiningRegion {
    Copper,
    Iron,
    Gold,
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

// How much copper ore/coal should be reserved for crafting
// cherry bombs, rather than immediately smelted.
const CRAFTABLE_CHERRY_BOMBS: usize = 3;

impl SmeltingPlan {
    fn new(game_state: &GameState) -> Result<Self, Error> {
        let mines = game_state.get_room("Mine")?;

        let mut available = InventoryGoal::current()
            .room("Mine")
            .total_stored_and_carried(game_state)?;

        ItemId::CHERRY_BOMB
            .iter_recipe()
            .into_iter()
            .flatten()
            .for_each(|(ingredient, count)| {
                if let Some(current) = available.get_mut(&ingredient) {
                    *current =
                        current.saturating_sub(CRAFTABLE_CHERRY_BOMBS * count);
                }
            });

        let get_available = |item: &ItemId| -> usize {
            available.get(item).cloned().unwrap_or(0)
        };
        let mut copper_ore = get_available(&ItemId::COPPER_ORE);
        let mut iron_ore = get_available(&ItemId::IRON_ORE);
        let mut stone = get_available(&ItemId::STONE);
        let mut coal = get_available(&ItemId::COAL);
        let mut quartz = get_available(&ItemId::QUARTZ);
        let mut fire_quartz = get_available(&ItemId::FIRE_QUARTZ);

        let mut required_refined_quartz = {
            let num_required = game_state
                .iter_reserved_items()?
                .filter(|(id, _)| *id == &ItemId::REFINED_QUARTZ)
                .map(|(_, count)| count)
                .sum::<usize>();

            let num_at_farm = game_state
                .iter_stored_items("Farm")?
                .filter(|item| item.id == ItemId::REFINED_QUARTZ)
                .map(|item| item.count)
                .sum::<usize>();

            let num_at_mines = get_available(&ItemId::REFINED_QUARTZ);

            num_required.saturating_sub(num_at_farm + num_at_mines)
        };

        let mut num_available_furnaces = 0;
        let mut num_old_furnaces = 0;

        mines
            .objects
            .iter()
            .filter_map(|obj| obj.kind.as_furnace())
            .for_each(|furnace| {
                num_old_furnaces += 1;

                let can_harvest = furnace.ready_to_harvest;
                let can_load = furnace.held_item.is_none() || can_harvest;
                if can_load {
                    num_available_furnaces += 1;
                }
            });
        let mut num_furnace_spaces = OFFSETS_ELEVATOR_TO_FURNACE
            .len()
            .saturating_sub(num_old_furnaces);

        let mut plan = SmeltingPlan {
            old_furnaces: num_old_furnaces,
            new_furnaces: 0,
            copper_bars: 0,
            iron_bars: 0,
            quartz: 0,
            fire_quartz: 0,
        };

        fn limiting_factor<const N: usize>(values: [usize; N]) -> usize {
            values.into_iter().min().unwrap_or(0)
        }

        loop {
            let fire_quartz_to_smelt = limiting_factor([
                fire_quartz,
                coal,
                num_available_furnaces,
                required_refined_quartz,
            ]);
            if fire_quartz_to_smelt > 0 {
                plan.fire_quartz += fire_quartz_to_smelt;
                fire_quartz -= fire_quartz_to_smelt;
                coal -= fire_quartz_to_smelt;
                num_available_furnaces -= fire_quartz_to_smelt;
                required_refined_quartz = required_refined_quartz
                    .saturating_sub(3 * fire_quartz_to_smelt);
            }

            let quartz_to_smelt = limiting_factor([
                quartz,
                coal,
                num_available_furnaces,
                required_refined_quartz,
            ]);
            if quartz_to_smelt > 0 {
                plan.quartz += quartz_to_smelt;
                quartz -= quartz_to_smelt;
                coal -= quartz_to_smelt;
                num_available_furnaces -= quartz_to_smelt;
                required_refined_quartz -= quartz_to_smelt;
            }

            let iron_bars =
                limiting_factor([iron_ore / 5, coal, num_available_furnaces]);
            if iron_bars > 0 {
                plan.iron_bars += iron_bars;
                iron_ore -= 5 * iron_bars;
                coal -= iron_bars;
                num_available_furnaces -= iron_bars;
            }

            let copper_bars =
                limiting_factor([copper_ore / 5, coal, num_available_furnaces]);
            if copper_bars > 0 {
                plan.copper_bars += copper_bars;
                copper_ore -= 5 * copper_bars;
                coal -= copper_bars;
                num_available_furnaces -= copper_bars;
            }

            let new_furnaces = {
                let for_iron = limiting_factor([
                    copper_ore / 20,
                    stone / 25,
                    iron_ore / 5,
                    coal,
                    num_furnace_spaces,
                ]);
                let for_copper = limiting_factor([
                    copper_ore / 25,
                    stone / 25,
                    coal,
                    num_furnace_spaces,
                ]);
                for_iron.max(for_copper)
            };
            if new_furnaces > 0 {
                plan.new_furnaces += new_furnaces;
                num_available_furnaces += new_furnaces;
                copper_ore -= new_furnaces * 20;
                stone -= new_furnaces * 25;
                num_furnace_spaces -= new_furnaces;
            } else {
                break;
            }
        }

        Ok(plan)
    }
}

impl Default for MineDelvingGoal {
    fn default() -> Self {
        Self::new()
    }
}

impl MineDelvingGoal {
    pub fn new() -> Self {
        Self
    }

    pub fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        let currently_in_mines =
            game_state.current_room()?.mineshaft_details.is_some();

        let current_day = game_state
            .globals
            .stats
            .get("daysPlayed")
            .cloned()
            .unwrap_or(0);

        let reached_bottom =
            game_state.globals.lowest_mine_level_reached >= 120;

        Ok(!currently_in_mines && (current_day < 5 || reached_bottom))
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

        if let Some(menu) = game_state.mine_elevator_menu() {
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
                .ok_or(Error::MineElevatorNotFound)?
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
                .ok_or(Error::MineElevatorNotFound)?;
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

        let plan = SmeltingPlan::new(game_state)?;

        let prepare = InventoryGoal::current()
            .room("Mine")
            .craft_missing(true)
            .with(ItemId::FURNACE.with_count(plan.new_furnaces))
            .with(ItemId::COAL.with_count(
                plan.fire_quartz
                    + plan.quartz
                    + plan.iron_bars
                    + plan.copper_bars,
            ))
            .with(ItemId::COPPER_ORE.with_count(plan.copper_bars * 5))
            .with(ItemId::IRON_ORE.with_count(plan.iron_bars * 5))
            .with(ItemId::QUARTZ.with_count(plan.quartz))
            .with(ItemId::FIRE_QUARTZ.with_count(plan.fire_quartz));

        let mut iter_smelt = std::iter::empty()
            .chain(std::iter::repeat_n(ItemId::FIRE_QUARTZ, plan.fire_quartz))
            .chain(std::iter::repeat_n(ItemId::QUARTZ, plan.quartz))
            .chain(std::iter::repeat_n(ItemId::COPPER_ORE, plan.copper_bars))
            .chain(std::iter::repeat_n(ItemId::IRON_ORE, plan.iron_bars));

        let iter_existing_furnaces = loc
            .objects
            .iter()
            .filter_map(|obj| {
                obj.kind.as_furnace().map(|furnace| (obj.tile, furnace))
            })
            .map(|(tile, furnace)| {
                let can_build = false;
                let can_harvest = furnace.ready_to_harvest;
                let can_load = furnace.held_item.is_none() || can_harvest;
                (tile, can_build, can_harvest, can_load)
            });

        let mine_elevator = game_state.get_mine_elevator()?;
        let iter_new_furnaces = OFFSETS_ELEVATOR_TO_FURNACE
            .iter()
            .skip(plan.old_furnaces)
            .take(plan.new_furnaces)
            .map(|offset| mine_elevator + *offset)
            .map(|tile| {
                let can_build = true;
                let can_harvest = false;
                let can_load = true;
                (tile, can_build, can_harvest, can_load)
            });

        let stack = LogicStack::new().then(prepare).then_iter(
            iter_existing_furnaces
                .chain(iter_new_furnaces)
                .flat_map(
                    |(tile, can_build, can_harvest, can_load)|
                      -> [Option<LogicStackItem>; 3] {

                    let build_furnace = can_build.then(|| {
                        UseItemOnTile::new(ItemId::FURNACE, "Mine", tile).into()
                    });

                    let take_complete = can_harvest
                        .then(|| ActivateTile::new("Mine", tile).into());

                    let start_new = can_load
                        .then(|| {
                            iter_smelt.next().map(|ore| {
                                UseItemOnTile::new(ore, "Mine", tile).into()
                            })
                        })
                        .flatten();
                    [build_furnace, take_complete, start_new]
                })
                .flatten(),
        );

        Ok((stack.len() > 1).then_some(stack))
    }

    fn preferred_mining_region(
        &self,
        game_state: &GameState,
    ) -> Result<MiningRegion, Error> {
        let farm = game_state.get_room("Farm")?;
        let mines = game_state.get_room("Mine")?;

        let items = game_state
            .player
            .inventory
            .iter_items()
            .chain(
                [farm, mines]
                    .into_iter()
                    .flat_map(|loc| loc.objects.iter())
                    .filter_map(|obj| match &obj.kind {
                        ObjectKind::Chest(chest) => {
                            Some(Either::Left(chest.inventory.iter_items()))
                        }
                        ObjectKind::CraftingMachine(machine) => {
                            Some(Either::Right(machine.held_item.iter()))
                        }
                        _ => None,
                    })
                    .flatten(),
            )
            .item_counts();
        let get_count =
            |item: &ItemId| -> usize { items.get(item).cloned().unwrap_or(0) };

        let opt_region_for_bundle = game_state
            .iter_reserved_items()?
            .filter(|(id, num_required)| get_count(id) < *num_required)
            .filter_map(|(id, _)| {
                if id == &ItemId::SOLAR_ESSENCE {
                    Some(MiningRegion::Gold)
                } else if id == &ItemId::VOID_ESSENCE {
                    Some(MiningRegion::Gold)
                } else if id == &ItemId::REFINED_QUARTZ {
                    Some(MiningRegion::Gold)
                } else {
                    None
                }
            })
            .min();
        if let Some(region_for_bundle) = opt_region_for_bundle {
            return Ok(region_for_bundle);
        }

        let copper_ore = get_count(&ItemId::COPPER_ORE);
        let copper_bar = get_count(&ItemId::COPPER_BAR);
        let iron_ore = get_count(&ItemId::IRON_ORE);
        let iron_bar = get_count(&ItemId::IRON_BAR);
        let coal = get_count(&ItemId::COAL);

        let copper_ore = copper_ore.saturating_sub(CRAFTABLE_CHERRY_BOMBS * 4);

        let enough_ore_to_smelt =
            coal >= 1 && (copper_ore >= 5 || iron_ore >= 5);
        let num_furnaces = game_state
            .get_room("Mine")?
            .objects
            .iter()
            .filter(|obj| matches!(obj.kind, ObjectKind::CraftingMachine(_)))
            .count();
        let preferred_region = if enough_ore_to_smelt
            && num_furnaces < OFFSETS_ELEVATOR_TO_FURNACE.len()
        {
            // We have enough ore/coal to smelt a bar, and aren't
            // smelting it.  Therefore, it must be because we don't
            // have enough smelters.  Mine more copper to make more
            // smelters.
            MiningRegion::Copper
        } else {
            // If there are more iron bars than copper bars, mine more
            // copper.
            let effective_copper_bars = copper_bar + copper_ore / 5;
            let effective_iron_bars = iron_bar + iron_ore / 5;

            if effective_copper_bars <= effective_iron_bars {
                MiningRegion::Copper
            } else {
                MiningRegion::Iron
            }
        };

        Ok(preferred_region)
    }

    fn before_descending_into_mine(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if let Some(start_smelting) = self.start_smelting(game_state)? {
            return Ok(Some(start_smelting));
        }

        let prepare = InventoryGoal::empty()
            .room("Mine")
            .with(ItemId::PICKAXE)
            .stamina_recovery_slots(2)
            .with_weapon()
            .with(ItemId::CHERRY_BOMB.clone().with_count(1000))
            .with(ItemId::STONE.clone().with_count(1000))
            .with(ItemId::COAL.clone().with_count(1000))
            .with(ItemId::COPPER_ORE.clone().with_count(1000))
            .with(ItemId::COPPER_BAR.clone().with_count(1000))
            .with(ItemId::IRON_ORE.clone().with_count(1000))
            .with(ItemId::IRON_BAR.clone().with_count(1000))
            .with(ItemId::GEODE.clone().with_count(1000))
            .with(ItemId::FROZEN_GEODE.clone().with_count(1000))
            .with(ItemId::MAGMA_GEODE.clone().with_count(1000))
            .with(ItemId::OMNI_GEODE.clone().with_count(1000));

        let prepare = if game_state.globals.in_game_time >= 2200 {
            // If it's close to the end of the day, grab items that
            // should be brought back to the farm for the next day.

            let stored_at_farm =
                game_state.iter_stored_items("Farm")?.item_counts();
            let at_mines = InventoryGoal::empty()
                .room("Mine")
                .iter_stored_and_carried(game_state)?
                .item_counts();

            let iter_reserved = game_state
                .iter_reserved_items()?
                .filter(|(id, count)| {
                    let num_at_farm =
                        stored_at_farm.get(id).cloned().unwrap_or(0);

                    let num_at_mines = at_mines.get(id).cloned().unwrap_or(0);

                    (num_at_farm < *count)
                        && (num_at_farm + num_at_mines >= *count)
                })
                .map(|(id, count)| id.clone().with_count(count));

            let iter_gems = InventoryGoal::empty()
                .room("Mine")
                .iter_stored_and_carried(game_state)?
                .filter(|item| {
                    matches!(
                        item.category,
                        Some(ItemCategory::Gem | ItemCategory::Mineral)
                    )
                })
                .sorted_by_key(|item| std::cmp::Reverse(item.stack_price()))
                .cloned();

            let iter_bring_back = iter_reserved
                .map(|item| (item, true))
                .chain(iter_gems.map(|item| (item, false)))
                .enumerate()
                .filter(|(i, (_, is_reserved))| *is_reserved || (*i < 5))
                .map(|(_, (item, _))| item);

            prepare.with(ItemId::HOE).with(iter_bring_back)
        } else {
            prepare
        };

        if !prepare.is_completed(game_state)? {
            return Ok(Some(prepare.into()));
        }

        let organization = OrganizeInventoryGoal::new(|item| {
            use super::SortedInventoryLocation as Loc;
            if item.as_weapon().is_some() || item == &ItemId::PICKAXE {
                Loc::HotBarLeft
            } else if item.gp_per_stamina().is_some() {
                Loc::HotBarRight
            } else if item == &ItemId::COAL
                || item == &ItemId::COPPER_BAR
                || item == &ItemId::COPPER_ORE
                || item == &ItemId::IRON_BAR
                || item == &ItemId::IRON_ORE
                || item == &ItemId::CHERRY_BOMB
            {
                Loc::HotBar
            } else {
                Loc::Hidden
            }
        });
        if !organization.is_completed(game_state) {
            return Ok(Some(organization.into()));
        }

        Ok(None)
    }

    fn at_mine_entrance(
        &self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        // If there's anything to prepare before going down, do so.
        // The check for the `mine_elevator_menu` is to avoid an edge
        // case that occurs if the furnaces finish smelting at the
        // exact same time that we opened the menu.
        if let Some(prepare) = self.before_descending_into_mine(game_state)? {
            if game_state.mine_elevator_menu().is_some() {
                return Ok(MenuCloser::new().into());
            }
            return Ok(prepare
                .cancel_if(|game_state| game_state.player.room_name != "Mine")
                .into());
        }

        let elevator_depth = {
            let lowest_level =
                game_state.globals.lowest_mine_level_reached.clamp(0, 120);
            (lowest_level / 5) * 5
        };

        let preferred_region = self.preferred_mining_region(game_state)?;
        let go_to_depth = if elevator_depth == 0 {
            1
        } else if elevator_depth < 40 {
            elevator_depth
        } else if preferred_region == MiningRegion::Copper {
            20
        } else if elevator_depth < 80 {
            elevator_depth
        } else if preferred_region == MiningRegion::Iron {
            60
        } else if elevator_depth > 80 && {
            let predictor = MineLevelPredictor::new(game_state);
            (elevator_depth..elevator_depth + 5)
                .any(|level| !predictor.predict(level).is_infested())
        } {
            80
        } else if elevator_depth < 120 {
            elevator_depth
        } else {
            100
        };

        let goal = GoToDepth::new(go_to_depth);
        Ok(goal.into())
    }
}

impl GoToDepth {
    pub fn new(depth: i32) -> Self {
        Self { depth }
    }
}

impl BotGoal for GoToDepth {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!("Go to level {}", self.depth).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let current_room = game_state.current_room()?;

        let is_in_mines = current_room.mineshaft_details.is_some();
        let at_target_depth = current_room
            .mineshaft_details
            .as_ref()
            .map(|mineshaft| mineshaft.mineshaft_level == self.depth)
            .unwrap_or(false);

        if at_target_depth {
            return Ok(BotGoalResult::Completed);
        }

        if let Some(menu) = game_state.mine_elevator_menu() {
            let i_button = (self.depth / 5) as usize;
            let pixel =
                menu.buttons.get(i_button).cloned().unwrap_or_else(|| {
                    panic!(
                        "TODO: Requested depth {}, \
                         which requires button {i_button}, \
                         but elevator only has {} buttons",
                        self.depth,
                        menu.buttons.len(),
                    )
                });
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let activate_tile = if self.depth == 1 {
            let tile = current_room
                .action_tiles
                .iter()
                .find(|(_, action)| action == "Mine")
                .map(|(tile, _)| tile)
                .cloned()
                .ok_or(Error::MineLadderNotFound)?;
            ActivateTile::new("Mine", tile)
        } else if is_in_mines {
            let tile = current_room
                .objects
                .iter()
                .find(|obj| matches!(obj.kind, ObjectKind::MineElevator))
                .ok_or(Error::MineElevatorNotFound)?
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
                .ok_or(Error::MineElevatorNotFound)?;
            ActivateTile::new("Mine", tile)
        };

        let room_name = game_state.player.room_name.clone();
        Ok(activate_tile
            .cancel_if(move |game_state| {
                game_state.player.room_name != room_name
            })
            .into())
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
                ItemId::WOOD.with_count(
                    100usize.saturating_sub(wood_from_crafted_chests),
                ),
                ItemId::STONE.with_count(1000),
                ItemId::COAL.with_count(100),
                ItemId::COPPER_ORE.with_count(1000),
                ItemId::COPPER_BAR.with_count(1000),
                ItemId::IRON_ORE.with_count(1000),
                ItemId::IRON_BAR.with_count(1000),
                ItemId::GOLD_ORE.with_count(1000),
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
                .with(ItemId::PICKAXE)
                .with(ItemId::HOE)
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
        let prefer_mining_ore = mineshaft_level
            < 5 * (game_state.globals.lowest_mine_level_reached / 5);
        let mining_dist = if prefer_mining_ore { 16.0 } else { 4.0 };

        let room_name = current_room.name.clone();
        let goal = MineSingleLevel { mineshaft_level }
            .cancel_if(move |game_state| {
                game_state.player.room_name != room_name
            })
            .with_interrupt(
                MineNearbyOre::new().with_search_distance(mining_dist),
            )
            .with_interrupt(WaitNearBomb::new())
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

        if game_state.dialogue_menu().is_some() {
            // The return-to-surface menu is open, so send a
            // confirmation.
            actions.do_action(GameAction::ConfirmMenu);
            return Ok(BotGoalResult::InProgress);
        }

        let clearable_tiles = game_state.collect_clearable_tiles()?;
        let player_tile = game_state.player.tile();

        let should_go_up = 'go_up: {
            if self.mineshaft_level == 120 {
                // Having reached the bottom, time to go back up.
                break 'go_up true;
            }

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

            let smelting_plan = SmeltingPlan::new(game_state)?;
            smelting_plan.copper_bars > 0 || smelting_plan.iron_bars > 0
        };

        if should_go_up && game_state.mine_elevator_menu().is_some() {
            return MineDelvingGoal::elevator_to_floor(game_state, actions, 0);
        }

        let pathfinding = current_room
            .pathfinding(&game_state.statics)
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

        let has_stone = current_room
            .objects
            .iter()
            .any(|obj| matches!(obj.kind, ObjectKind::Stone(_)));

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
                .ok_or(Error::MineLadderNotFound)?
        } else if let Some(ladder_down) = opt_ladder_down {
            ladder_down
        } else if !has_stone {
            // Must be an infested floor, so walk toward an enemy.
            // Once close enough, the `AttackNearbyEnemy` interrupt
            // will take over.
            let iter_monsters = || {
                current_room
                    .characters
                    .iter()
                    .filter(|character| character.is_monster())
            };

            let num_monsters = iter_monsters().count();

            let player_pos = game_state.player.center_pos();
            let monster_pos = iter_monsters()
                .map(|monster| monster.center_pos())
                .min_by(|a, b| {
                    let dist_a = player_pos.dist2(*a);
                    let dist_b = player_pos.dist2(*b);
                    dist_a.total_cmp(&dist_b)
                })
                .ok_or(Error::NoRemainingMonsterFound)?;
            let goal =
                MovementGoal::new(current_room.name.clone(), monster_pos)
                    .cancel_if(move |game_state| {
                        let current_num_monsters = game_state
                            .current_room()
                            .ok()
                            .into_iter()
                            .flat_map(|room| room.characters.iter())
                            .filter(|character| character.is_monster())
                            .count();
                        current_num_monsters != num_monsters
                    });
            return Ok(goal.into());
        } else {
            let predictor = StonePredictor::new(game_state)?;

            let iter_stones = || {
                current_room
                    .objects
                    .iter()
                    .filter(|obj| matches!(obj.kind, ObjectKind::Stone(_)))
                    .map(|obj| obj.tile)
            };

            let ladder_stones: HashSet<Vector<isize>> = iter_stones()
                .filter(|tile| predictor.will_produce_ladder(*tile))
                .collect();

            let preferred_stones = if ladder_stones.is_empty() {
                // No stone will produce a ladder if mined next.
                // Therefore, prefer to mine stones that will have a
                // high RNG value when rolling for the ladder.
                // Eventually, the `1/num_stones` term will allow the
                // ladder chance to exceed the RNG value for one of
                // the stones.  This will occur faster if we don't
                // break those low-RNG-roll stones ahead of time.
                let num_stones = iter_stones().count();
                iter_stones()
                    .map(|tile| (tile, predictor.ladder_rng_roll(tile)))
                    .sorted_by(|(_, a), (_, b)| b.total_cmp(a))
                    .map(|(tile, _)| tile)
                    .take((num_stones as f32 * 0.8).ceil() as usize)
                    .collect()
            } else {
                ladder_stones
            };

            pathfinding
                .iter_dijkstra(player_tile)
                .map(|(tile, _)| tile)
                .find(|tile| preferred_stones.contains(tile))
                .ok_or(Error::MineLadderNotFound)?
        };

        let path = pathfinding
            .allow_diagonal(false)
            .path_between(player_tile, target_tile)?;

        let opt_blocked_tile_in_path = path
            .into_iter()
            .find(|tile| clearable_tiles.contains_key(tile));
        let target_tile = opt_blocked_tile_in_path.unwrap_or(target_tile);

        let opt_tool = clearable_tiles
            .get(&target_tile)
            .and_then(|opt| opt.as_ref());

        let current_tick = game_state.globals.game_tick;
        let num_objects = current_room.objects.len();
        let num_enemies = current_room.characters.len();
        let cancellation = move |game_state: &GameState| -> bool {
            if game_state.player.using_tool || game_state.any_menu_open() {
                // This action has some state to it, and shouldn't
                // be interrupted.
                return false;
            }

            // If more than half a second has passed, clear out the
            // current movement/tile goals and re-plan.
            let time_passed = game_state.globals.game_tick > current_tick + 30;

            let current_room = game_state.current_room().ok();

            // If the number of objects in the room increased, then
            // cancel out and re-plan.  This can occur if a nearby
            // enemy is killed, dropping a ladder.
            let new_object_appeared = {
                let current_num_objects = current_room
                    .map(|room| room.objects.len())
                    .unwrap_or(num_objects);
                current_num_objects > num_objects
            };

            // If the number of enemies has changed, it may indicate
            // either that an enemy was killed, in which case we may
            // need to path to the newly-dropped item, or that an
            // enemy spawned, in which case we may need to path to the
            // enemy in order to collect useful drops from it.
            let enemy_count_changed = {
                let current_num_enemies = current_room
                    .map(|room| room.characters.len())
                    .unwrap_or(num_enemies);
                current_num_enemies != num_enemies
            };
            time_passed || new_object_appeared || enemy_count_changed
        };

        if let Some(tool) = opt_tool {
            let room_name = game_state.player.room_name.clone();
            let goal = UseItemOnTile::new(
                tool.clone(),
                room_name.clone(),
                target_tile,
            );
            Ok(goal.cancel_if(cancellation).into())
        } else {
            let room_name = game_state.player.room_name.clone();
            let goal = ActivateTile::new(room_name.clone(), target_tile);
            Ok(goal.cancel_if(cancellation).into())
        }
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

    pub fn ladder_rng_roll(&self, tile: Vector<isize>) -> f32 {
        let mut rng = self.generate_rng(tile);
        rng.rand_float()
    }

    pub fn will_produce_ladder(&self, tile: Vector<isize>) -> bool {
        self.ladder_rng_roll(tile) < self.ladder_chance()
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
        Self {
            dist: 4.0,
            cache: None,
            most_recent_bomb: None,
        }
    }

    pub fn with_search_distance(self, dist: f32) -> Self {
        Self { dist, ..self }
    }

    fn generate_cache(game_state: &GameState) -> Result<NearbyOreCache, Error> {
        let loc = game_state.current_room()?;
        let mineshaft_details = loc
            .mineshaft_details
            .as_ref()
            .ok_or_else(|| Error::MineNearbyOreUsedOutsideOfMines)?;
        let level = mineshaft_details.mineshaft_level;

        let level_kind = (level / 40 + 1) as f32;
        let stone_clearing_cost = 3.0 * level_kind;

        let has_weapon = game_state
            .player
            .inventory
            .iter_items()
            .any(|item| item.as_weapon().is_some());

        let has_empty_slot = game_state.player.inventory.has_empty_slot();
        let inventory: HashSet<_> = game_state
            .player
            .inventory
            .iter_items()
            .map(|item| &item.id)
            .collect();

        let predictor = StonePredictor::new(game_state)?;
        let mut desirable_rocks: HashMap<Vector<isize>, (f32, f32)> = loc
            .objects
            .iter()
            .filter_map(|obj| {
                let dist_multiplier = match &obj.kind {
                    ObjectKind::Stone(stone) => {
                        let prediction =
                            predictor.drop_from_stone(obj.tile, stone);
                        let multiplier = if prediction.omnigeode {
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
                        };

                        (multiplier / level_kind, -stone_clearing_cost)
                    }
                    ObjectKind::MineCartCoal => (3.0, 0.0),

                    ObjectKind::MineBarrel if has_weapon => (0.5, 0.0),

                    ObjectKind::Chest(_) => (4.0, 0.0),
                    ObjectKind::Mineral(mineral)
                        if has_empty_slot || {
                            let id = match mineral {
                                MineralKind::Quartz => &ItemId::QUARTZ,
                                MineralKind::EarthCrystal => {
                                    &ItemId::EARTH_CRYSTAL
                                }
                                MineralKind::FrozenTear => &ItemId::FROZEN_TEAR,
                                MineralKind::FireQuartz => &ItemId::FIRE_QUARTZ,
                                MineralKind::Other { id, .. } => id,
                            };
                            inventory.contains(id)
                        } =>
                    {
                        (1.0, 0.0)
                    }

                    other if other.is_forage() => (1.0, 0.0),

                    _ => {
                        return None;
                    }
                };
                Some((obj.tile, dist_multiplier))
            })
            .filter(|(tile, _)| {
                // There's generally only zero or one bombs on the
                // map, so iterate over the bombs rather than making a
                // lookup table.
                loc.bombs.iter().all(|bomb| {
                    let dist2 = bomb.tile().dist2(*tile);
                    dist2 > bomb.dist2()
                })
            })
            .collect();

        let mut to_group: HashSet<Vector<isize>> =
            desirable_rocks.keys().copied().collect();
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
                let total = current_group
                    .iter()
                    .map(|tile| desirable_rocks[tile])
                    .fold(
                        (0.0f32, 0.0f32),
                        |(sum_mult, sum_offset), (mult, offset)| {
                            (sum_mult + mult, sum_offset + offset)
                        },
                    );
                current_group.into_iter().for_each(|tile| {
                    *desirable_rocks.get_mut(&tile).unwrap() = total;
                });
            }
        }

        let placeable = {
            let pathfinding = loc.pathfinding(&game_state.statics);
            let clear = pathfinding.clear();
            let reachable = pathfinding.reachable(game_state.player.tile());
            clear.imap(|tile, &is_clear| is_clear && reachable.is_set(tile))
        };

        let missing_items = {
            let stored_items = game_state
                .player
                .inventory
                .iter_items()
                .chain(game_state.iter_stored_items("Farm")?)
                .chain(game_state.iter_stored_items("Mine")?)
                .item_counts();

            game_state
                .iter_reserved_items()?
                .filter(|(_, num_required)| *num_required < 50)
                .filter(|(id, num_required)| {
                    let num_stored = stored_items.get(id).unwrap_or(&0);
                    num_stored < num_required
                })
                .map(|(id, _)| id.clone())
                .collect()
        };

        Ok(NearbyOreCache {
            num_objects: loc.objects.len(),
            num_bombs: loc.bombs.len(),
            desirable_rocks,
            placeable,
            missing_items,
        })
    }

    fn update_cache(&mut self, game_state: &GameState) -> Result<(), Error> {
        let is_up_to_date = self
            .cache
            .as_ref()
            .map(|cache| -> Result<_, Error> {
                let loc = game_state.current_room()?;
                let up_to_date = cache.num_objects == loc.objects.len()
                    && cache.num_bombs == loc.bombs.len();

                Ok(up_to_date)
            })
            .unwrap_or(Ok(false))?;

        if !is_up_to_date {
            self.cache = Some(Self::generate_cache(game_state)?);
        }
        Ok(())
    }
}

impl BotInterrupt for MineNearbyOre {
    fn description(&self) -> std::borrow::Cow<str> {
        "Mine nearby ore".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let loc = game_state.current_room()?;
        let Some(level) = loc
            .mineshaft_details
            .as_ref()
            .map(|mineshaft| mineshaft.mineshaft_level)
        else {
            return Ok(None);
        };

        self.update_cache(game_state)?;
        let NearbyOreCache {
            desirable_rocks,
            missing_items,
            placeable,
            ..
        } = self.cache.as_ref().expect("Populated by self.update_cache");

        if desirable_rocks.is_empty() {
            return Ok(None);
        }

        let level_kind = (level / 40 + 1) as f32;

        let player_tile = game_state.player.tile();
        let stone_clearing_cost = 3.0 * level_kind;
        let pathfinding = loc
            .pathfinding(&game_state.statics)
            .breakable_clearing_cost(200)
            .stone_clearing_cost((1000.0 * stone_clearing_cost) as u64)
            .include_border(true);
        let distances = pathfinding.distances(player_tile);

        enum TargetKind {
            Object,
            Enemy,
        }

        let iter_nearby_objects = desirable_rocks.iter().filter_map(
            |(tile, (multiplier, offset))| {
                let dist = *distances.get_opt(*tile)?;
                ((dist as f32) + offset < self.dist * multiplier)
                    .then_some((*tile, TargetKind::Object, dist))
            },
        );

        let iter_nearby_enemies = loc
            .characters
            .iter()
            .filter(|character| {
                character
                    .item_drops
                    .iter()
                    .any(|drop| missing_items.contains(drop))
            })
            .map(|character| character.tile())
            .filter_map(|tile| {
                let dist = *distances.get_opt(tile)?;
                let multiplier = 2.0;
                ((dist as f32) < self.dist * multiplier)
                    .then_some((tile, TargetKind::Enemy, dist))
            });

        let opt_target_tile = iter_nearby_objects
            .chain(iter_nearby_enemies)
            .min_by_key(|(tile, _, dist)| (*dist, tile.right, tile.down))
            .map(|(tile, kind, _)| (tile, kind));

        let Some((target_tile, target_kind)) = opt_target_tile else {
            return Ok(None);
        };

        let clearable_tiles = game_state.collect_clearable_tiles()?;

        let opt_blocked_tile_in_path = || -> Result<_, Error> {
            Ok(pathfinding
                .allow_diagonal(false)
                .path_between(player_tile, target_tile)?
                .into_iter()
                .find(|tile| clearable_tiles.contains_key(tile)))
        };

        let craft_cherry_bomb = CraftItemGoal::new(ItemId::CHERRY_BOMB);
        let opt_bomb_tile = || {
            const CHERRY_BOMB_DIST2: isize = 12;
            Some(target_tile)
                .into_iter()
                .filter(|_| craft_cherry_bomb.is_completable(game_state))
                .filter(|_| level_kind > 1.0)
                .flat_map(|tile| tile.iter_dist2(CHERRY_BOMB_DIST2))
                .filter(|&bomb_tile| placeable.is_set(bomb_tile))
                .map(|bomb_tile| {
                    let num_bombed = bomb_tile
                        .iter_dist2(CHERRY_BOMB_DIST2)
                        .filter(|bombed_tile| {
                            desirable_rocks.contains_key(bombed_tile)
                        })
                        .count();
                    (bomb_tile, num_bombed)
                })
                .max_by_key(|(_, num_bombed)| *num_bombed)
                .filter(|(_, num_bombed)| *num_bombed >= 5)
                .map(|(bomb_tile, _)| bomb_tile)
        };
        let (target_tile, target_kind, opt_item): (
            Vector<isize>,
            TargetKind,
            Option<ItemId>,
        ) = if let Some(blocked) = opt_blocked_tile_in_path()? {
            (blocked, TargetKind::Object, None)
        } else if let Some(bomb_tile) = opt_bomb_tile() {
            (bomb_tile, TargetKind::Object, Some(ItemId::CHERRY_BOMB))
        } else {
            (target_tile, target_kind, None)
        };

        let opt_item = opt_item.or_else(|| {
            clearable_tiles
                .get(&target_tile).cloned()
                .unwrap_or(None)
        });

        if opt_item == Some(ItemId::CHERRY_BOMB) {
            // If using a cherry-bomb, prefer to move to the target
            // location prior to crafting.  That way, we avoid
            // crafting bombs that aren't going to be used
            // immediately.
            let movement = MovementGoal::new(
                game_state.player.room_name.clone(),
                target_tile.into(),
            );
            if !movement.is_completed(game_state) {
                return Ok(Some(movement.into()));
            }

            if !craft_cherry_bomb.is_completed(game_state) {
                // Avoid crafting another bomb immediately after
                // placing one down.  If it looks like a good idea,
                // it's more likely that we're in an inconsistent
                // state, where the bomb is no longer in the player's
                // inventory, but isn't yet added to the location's
                // temporary sprites.
                let used_bomb_recently = self
                    .most_recent_bomb
                    .map(|prev_tick| {
                        prev_tick + 15 < game_state.globals.game_tick
                    })
                    .unwrap_or(false);

                let opt_craft =
                    (!used_bomb_recently).then(|| craft_cherry_bomb.into());

                return Ok(opt_craft);
            }

            self.most_recent_bomb = Some(game_state.globals.game_tick);
        }

        let goal: LogicStack = if matches!(target_kind, TargetKind::Enemy) {
            let movement = MovementGoal::new(
                game_state.player.room_name.clone(),
                target_tile.into(),
            );
            if movement.is_completed(game_state) {
                return Ok(None);
            }

            let num_enemies = loc.characters.len();
            let current_tick = game_state.globals.game_tick;
            let cancellation = move |game_state: &GameState| -> bool {
                current_tick + 30 < game_state.globals.game_tick
                    || game_state
                        .current_room()
                        .map(|room| num_enemies != room.characters.len())
                        .unwrap_or(false)
            };
            LogicStack::new().then(movement).cancel_if(cancellation)
        } else if let Some(item) = opt_item {
            let item = if item.item_id.starts_with("(W)") {
                let opt_weapon =
                    best_weapon(game_state.player.inventory.iter_items());
                if let Some(weapon) = opt_weapon {
                    weapon.id.clone()
                } else {
                    return Ok(None);
                }
            } else {
                item.clone()
            };
            UseItemOnTile::new(
                item,
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

impl WaitNearBomb {
    pub fn new() -> Self {
        Self
    }
}

impl BotInterrupt for WaitNearBomb {
    fn description(&self) -> std::borrow::Cow<str> {
        "Wait near bomb".into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let loc = game_state.current_room()?;
        let Some(bomb) = loc.bombs.first() else {
            return Ok(None);
        };

        let player_tile = game_state.player.tile();
        let bomb_tile = bomb.tile();

        let pathfinding = loc.pathfinding(&game_state.statics);

        let player_dist = pathfinding.distances(player_tile);

        let opt_target_tile = bomb
            .within_range(player_tile)
            .then(|| {
                // Within range of the bomb, so find somewhere to run.
                pathfinding
                    .iter_dijkstra(player_tile)
                    .map(|(tile, _)| tile)
                    .find(|tile| !bomb.within_range(*tile))
            })
            .flatten()
            .or_else(|| {
                // Out of range of the bomb, so move around without
                // going too far away.
                let perimeter = bomb.perimeter_dist2();
                Direction::iter_cardinal()
                    .map(|dir| player_tile + dir.offset())
                    .find(|adj| {
                        player_dist.is_some(*adj)
                            && perimeter.contains(&adj.dist2(bomb_tile))
                    })
            });

        let Some(target_tile) = opt_target_tile else {
            return Ok(None);
        };

        let stack = LogicStack::new()
            .then(MovementGoal::new(loc.name.clone(), target_tile.into()))
            .cancel_if(|game_state| {
                game_state
                    .current_room()
                    .map(|loc| loc.bombs.is_empty())
                    .unwrap_or(true)
            });

        Ok(Some(stack))
    }
}

impl MineLevelPredictor {
    pub fn new(game_state: &GameState) -> Self {
        let game_id = game_state.globals.game_id;

        let days_played =
            game_state.globals.get_stat("daysPlayed").unwrap_or(1);

        Self {
            game_id,
            days_played,
            days_ahead: 0,
        }
    }

    pub fn days_ahead(self, days_ahead: u32) -> Self {
        Self { days_ahead, ..self }
    }

    pub fn predict(&self, level: i32) -> LevelPrediction {
        let mine_area = if level < 40 {
            0
        } else if level < 80 {
            40
        } else if level <= 120 {
            80
        } else {
            121
        };

        let day = (self.days_played + self.days_ahead) as f64;

        let mut prediction = LevelPrediction::default();

        {
            let mut rng = SeededRng::from_stardew_seed([
                day,
                (self.game_id / 2) as f64,
                day,
                level as f64,
                (4 * level) as f64,
            ]);

            // Roll to see if it's dark.  (TODO: Skip this roll if
            // dangerous mines are active).
            if rng.rand_float() < 0.3 && level > 2 {
                // Adjust the lighting, conditional on previous check.
                rng.rand_i32();
            }
            // More lighting adjustments.
            rng.rand_i32();

            prediction.is_mushroom =
                rng.rand_float() < 0.035 && mine_area == 80 && level % 5 != 0;
        }

        {
            let mut rng = SeededRng::from_stardew_seed([
                day,
                (self.game_id / 2) as f64,
                (100 * level) as f64,
            ]);

            let is_infested = rng.rand_float() < 0.044
                && level % 5 != 0
                && level % 40 > 5
                && level % 40 < 30
                && level % 40 != 19;

            if is_infested {
                if rng.rand_bool() {
                    prediction.is_monster = true;
                } else {
                    prediction.is_slime = true;
                }

                if mine_area == 121 && level > 126 && rng.rand_bool() {
                    prediction.is_dinosaur = false;
                    prediction.is_monster = false;
                    prediction.is_slime = false;
                }
            }
        }

        prediction
    }
}

impl LevelPrediction {
    pub fn is_normal(&self) -> bool {
        !self.is_mushroom
            && !self.is_monster
            && !self.is_slime
            && !self.is_dinosaur
    }

    pub fn is_infested(&self) -> bool {
        self.is_monster || self.is_slime || self.is_dinosaur
    }
}

impl std::fmt::Display for LevelPrediction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_normal() {
            write!(f, "Normal")
        } else {
            write!(
                f,
                "{}",
                [
                    (self.is_mushroom, "Mushroom"),
                    (self.is_monster, "Monster"),
                    (self.is_slime, "Slime"),
                    (self.is_dinosaur, "Dinosaur"),
                ]
                .into_iter()
                .filter(|(flag, _)| *flag)
                .map(|(_, tag)| tag)
                .format("/")
            )
        }
    }
}
