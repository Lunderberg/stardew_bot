use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use geometry::{Direction, Vector};

use crate::{ActivateTile, Error, MovementGoal};
use game_state::{GameState, ObjectKind, TileMap};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult},
    graph_search::GraphSearch,
    GameStateExt as _, LocationExt as _, RepairBeachBridgeGoal,
};

pub struct ForagingGoal {
    location: Option<Cow<'static, str>>,

    // TODO: Extract these out into a more general handling that can
    // be applied to any goal.
    stop_at_time: Option<i32>,
    stop_with_stamina: Option<i32>,
}

impl ForagingGoal {
    pub fn new() -> Self {
        Self {
            location: None,
            stop_at_time: None,
            stop_with_stamina: None,
        }
    }

    /// Only forage at the specified location
    pub fn location(self, location: impl Into<Cow<'static, str>>) -> Self {
        Self {
            location: Some(location.into()),
            ..self
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

    fn stamina_collected(game_state: &GameState) -> f32 {
        game_state
            .player
            .inventory
            .iter_items()
            .filter_map(|item| item.stamina_recovery())
            .filter(|&stamina| stamina > 0.0)
            .sum::<f32>()
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let after_stopping_time = self
            .stop_at_time
            .map(|time| game_state.globals.in_game_time >= time)
            .unwrap_or(false);

        let collected_enough_stamina = self
            .stop_with_stamina
            .map(|stamina| {
                Self::stamina_collected(game_state) > (stamina as f32)
            })
            .unwrap_or(false);

        let room_exists = self
            .location
            .as_ref()
            .map(|name| {
                game_state.locations.iter().any(|loc| &loc.name == name)
            })
            .unwrap_or(true);

        after_stopping_time || collected_enough_stamina || !room_exists
    }

    fn next_forageable_in_any_room(
        game_state: &GameState,
    ) -> Result<Option<(String, Vector<isize>)>, Error> {
        let condensed_map = CondensedMap::build(game_state);
        let goal_forage: HashSet<(usize, Vector<isize>)> = game_state
            .locations
            .iter()
            .enumerate()
            .flat_map(|(i, loc)| loc.objects.iter().map(move |obj| (i, obj)))
            .filter(|(_, obj)| match &obj.kind {
                // TODO: Customize based on which forage is desired.
                ObjectKind::Other { name, .. } => match name.as_str() {
                    "Leek" => true,
                    "Dandelion" => true,
                    _ => false,
                },
                _ => false,
            })
            .map(|(i, obj)| (i, obj.tile))
            .collect();

        let initial = (
            condensed_map.get_room_index(&game_state.player.room_name)?,
            game_state.player.tile(),
        );
        let opt_closest_forage = condensed_map
            .dijkstra_search(initial)
            .find_map(|((index, tile), _)| {
                Direction::iter()
                    .filter(|dir| dir.is_cardinal())
                    .map(|dir| (index, tile + dir.offset()))
                    .find(|node| goal_forage.contains(node))
            })
            .map(|(index, tile)| (condensed_map.0[index].name.clone(), tile));

        Ok(opt_closest_forage)
    }

    fn next_farm_forageable(
        game_state: &GameState,
    ) -> Result<Option<BotGoalResult>, Error> {
        if game_state.player.room_name != "Farm" {
            return Ok(None);
        }

        let farm = game_state.get_room("Farm")?;
        let farm_door = game_state.get_farm_door()?;

        let reachable =
            farm.pathfinding(&game_state.statics).reachable(farm_door);

        let player_pos = game_state.player.center_pos();

        let opt_fruit = farm
            .items
            .iter()
            .map(|item| item.tile_pos())
            .filter(|item_pos| item_pos.dist(player_pos) < 5.0)
            .next();
        if let Some(pos) = opt_fruit {
            if pos.dist(player_pos) < 2.0 {
                return Ok(Some(BotGoalResult::InProgress));
            } else {
                return Ok(Some(MovementGoal::new("Farm", pos).into()));
            }
        }

        let opt_fruit_tree = farm
            .objects
            .iter()
            .filter(|obj| {
                Direction::iter()
                    .any(|dir| reachable.is_set(obj.tile + dir.offset()))
            })
            .filter(|obj| match &obj.kind {
                ObjectKind::FruitTree(fruit_tree) => fruit_tree.num_fruit > 0,
                _ => false,
            })
            .map(|obj| obj.tile)
            .next();

        if let Some(fruit_tree) = opt_fruit_tree {
            return Ok(Some(ActivateTile::new("Farm", fruit_tree).into()));
        }

        Ok(None)
    }

    fn next_forageable_in_room(
        game_state: &GameState,
        room_name: &str,
    ) -> Result<Option<Vector<isize>>, Error> {
        let initial = if game_state.player.room_name == room_name {
            game_state.player.tile()
        } else {
            game_state.closest_entrance(room_name)?
        };

        let room = game_state.get_room(room_name)?;
        let distances = room
            .pathfinding(&game_state.statics)
            .include_border(true)
            .distances(initial);

        let opt_closest_forage = room
            .objects
            .iter()
            .filter(|obj| obj.kind.is_forage())
            .map(|obj| obj.tile)
            .filter(|tile| distances.is_some(*tile))
            .min_by_key(|tile| {
                distances[*tile].expect("Guarded by distances.is_some()")
            });

        Ok(opt_closest_forage)
    }

    fn next_forageable<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Result<Option<(String, Vector<isize>)>, Error> {
        if let Some(loc) = &self.location {
            Ok(Self::next_forageable_in_room(game_state, loc)?
                .map(|tile| (loc.to_string(), tile)))
        } else {
            Self::next_forageable_in_any_room(game_state)
        }
    }
}

impl BotGoal for ForagingGoal {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        if let Some(room_name) = &self.location {
            format!("Forage at {room_name}").into()
        } else {
            "Foraging".into()
        }
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let opt_room: Option<&str> =
            self.location.as_ref().map(|loc| loc.as_ref());

        if matches!(opt_room, None | Some("Farm")) {
            if let Some(farm_forage) = Self::next_farm_forageable(game_state)? {
                return Ok(farm_forage);
            }
        }

        if matches!(opt_room, Some("Beach")) {
            let repair_bridge = RepairBeachBridgeGoal::new();
            if !repair_bridge.is_completed(game_state)? {
                return Ok(repair_bridge.into());
            }
        }

        if let Some((room, tile)) = self.next_forageable(game_state)? {
            let stop_with_stamina = self.stop_with_stamina;
            let goal =
                ActivateTile::new(room, tile).cancel_if(move |game_state| {
                    stop_with_stamina
                        .map(|stamina| {
                            Self::stamina_collected(game_state)
                                > (stamina as f32)
                        })
                        .unwrap_or(false)
                });
            return Ok(goal.into());
        }

        Ok(BotGoalResult::Completed)
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
                let clear_tiles = loc.collect_clear_tiles(&game_state.statics);
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

    fn get_room_index(&self, name: &str) -> Result<usize, Error> {
        let index = self
            .0
            .iter()
            .enumerate()
            .find(|(_, loc)| loc.name == name)
            .map(|(i, _)| i)
            .ok_or_else(|| Error::UnknownRoom(name.to_string()))?;

        Ok(index)
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
