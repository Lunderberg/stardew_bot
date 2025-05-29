use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use crate::{
    bot_logic::{ActivateTile, MovementGoal},
    game_state::{FloatingItem, ObjectKind, TileMap, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult},
    graph_search::GraphSearch,
};

pub struct ForagingGoal {
    // TODO: Extract these out into a more general handling that can
    // be applied to any goal.
    stop_at_time: Option<i32>,
    stop_with_stamina: Option<i32>,
}

impl ForagingGoal {
    pub fn new() -> Self {
        Self {
            stop_at_time: None,
            stop_with_stamina: None,
        }
    }

    pub fn stop_at_time(self, time: i32) -> Self {
        Self {
            stop_at_time: Some(time),
            ..self
        }
    }

    pub fn stop_with_stamina(self, item_stamina: i32) -> Self {
        Self {
            stop_with_stamina: Some(item_stamina),
            ..self
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let after_stopping_time = self
            .stop_at_time
            .map(|time| game_state.globals.in_game_time >= time)
            .unwrap_or(false);

        let collected_enough_stamina = self
            .stop_with_stamina
            .map(|stamina| {
                let total_stamina_recovery = game_state
                    .player
                    .inventory
                    .iter_items()
                    .filter_map(|item| item.stamina_recovery())
                    .filter(|&stamina| stamina > 0.0)
                    .sum::<f32>();
                total_stamina_recovery > (stamina as f32)
            })
            .unwrap_or(false);

        after_stopping_time || collected_enough_stamina
    }
}

impl BotGoal for ForagingGoal {
    fn description(&self) -> std::borrow::Cow<str> {
        "Foraging".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _do_action: &mut dyn FnMut(GameAction),
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let farm_door = game_state
            .locations
            .iter()
            .find(|loc| loc.name == "FarmHouse")
            .unwrap()
            .warps
            .iter()
            .filter(|warp| warp.target_room == "Farm")
            .map(|warp| warp.target)
            .next()
            .unwrap();
        let farm = game_state
            .locations
            .iter()
            .find(|loc| loc.name == "Farm")
            .unwrap();

        let reachable = farm.find_reachable_tiles(farm_door);

        let player_pos = game_state.player.center_pos();

        let opt_fruit = farm
            .items
            .iter()
            .map(|item| item.tile_pos())
            .filter(|item_pos| item_pos.dist(player_pos) < 5.0)
            .next();
        if let Some(pos) = opt_fruit {
            if pos.dist(player_pos) < 2.0 {
                return Ok(BotGoalResult::InProgress);
            } else {
                return Ok(MovementGoal::new("Farm", pos).into());
            }
        }

        let opt_fruit_tree = farm
            .objects
            .iter()
            .filter(|obj| {
                Direction::iter().any(|dir| reachable[obj.tile + dir.offset()])
            })
            .filter(|obj| match &obj.kind {
                ObjectKind::FruitTree(fruit_tree) => fruit_tree.num_fruit > 0,
                _ => false,
            })
            .map(|obj| obj.tile)
            .next();

        if let Some(fruit_tree) = opt_fruit_tree {
            return Ok(ActivateTile::new("Farm", fruit_tree).into());
        }

        let condensed_map = CondensedMap::build(game_state);
        let goal_forage: HashSet<(usize, Vector<isize>)> = game_state
            .locations
            .iter()
            .enumerate()
            .flat_map(|(i, loc)| loc.objects.iter().map(move |obj| (i, obj)))
            .filter(|(_, obj)| match &obj.kind {
                // TODO: Customize based on which forage is desired.
                ObjectKind::Other(name) => match name.as_str() {
                    "Leek" => true,
                    "Dandelion" => true,
                    _ => false,
                },
                _ => false,
            })
            .map(|(i, obj)| (i, obj.tile))
            .collect();

        let initial = (
            game_state
                .locations
                .iter()
                .enumerate()
                .find(|(_, loc)| loc.name == game_state.player.room_name)
                .map(|(i, _)| i)
                .expect("Player must exist within a room"),
            game_state.player.tile(),
        );
        let opt_closest_forage = condensed_map
            .dijkstra_search(initial)
            .find_map(|((index, tile), _)| {
                Direction::iter()
                    .filter(|dir| dir.is_cardinal())
                    .map(|dir| (index, tile + dir.offset()))
                    .find(|node| goal_forage.contains(node))
            });

        if let Some(closest_forage) = opt_closest_forage {
            let (index, tile) = closest_forage;
            let goal =
                ActivateTile::new(condensed_map.0[index].name.clone(), tile);
            Ok(goal.into())
        } else {
            Ok(BotGoalResult::Completed)
        }
    }
}

struct CondensedMap(Vec<CondensedLocation>);
struct CondensedLocation {
    name: String,
    clear_tiles: TileMap<bool>,
    warps: HashMap<Vector<isize>, (usize, Vector<isize>)>,
}

impl CondensedMap {
    fn build(game_state: &GameState) -> Self {
        let location_index_lookup: HashMap<String, usize> = game_state
            .locations
            .iter()
            .enumerate()
            .map(|(i, loc)| (loc.name.clone(), i))
            .collect();

        let locations = game_state
            .locations
            .iter()
            .map(|loc| {
                let name = loc.name.clone();
                let clear_tiles = loc.collect_clear_tiles();
                let warps = loc
                    .warps
                    .iter()
                    .filter_map(|warp| {
                        location_index_lookup
                            .get(&warp.target_room)
                            .cloned()
                            .map(|target_index| {
                                (warp.location, (target_index, warp.target))
                            })
                    })
                    .collect();
                CondensedLocation {
                    name,
                    clear_tiles,
                    warps,
                }
            })
            .collect();

        Self(locations)
    }
}

impl GraphSearch<(usize, Vector<isize>)> for CondensedMap {
    fn connections_from<'a>(
        &'a self,
        node: &'a (usize, Vector<isize>),
    ) -> impl IntoIterator<Item = ((usize, Vector<isize>), u64)> + 'a {
        let (loc_index, tile) = node;
        let loc = &self.0[*loc_index];
        let within_room = loc
            .clear_tiles
            .connections_from(tile)
            .into_iter()
            .map(move |(next_tile, cost)| ((*loc_index, next_tile), cost));

        let between_rooms = Direction::iter()
            .filter(|dir| dir.is_cardinal())
            .filter_map(move |dir| loc.warps.get(&(*tile + dir.offset())))
            .map(|&node| (node, 2));

        within_room.chain(between_rooms)
    }
}
