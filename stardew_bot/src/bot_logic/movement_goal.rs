use itertools::Itertools as _;

use crate::{
    bot_logic::BotError,
    game_state::{FacingDirection, TileMap, Vector},
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{BotGoal, BotGoalResult, SubGoals},
    graph_search::GraphSearch,
};

/// Epsilon distance (in tiles) to consider a target reached
///
/// For each waypoint, and for the final target location, consider the
/// position to be successfully reached when it is within this
/// distance of the desired location.
const TOLERANCE: f32 = 0.1;

/// Minimum distance (in tiles) to re-plan a route.
///
/// Since the planned route is tracked on a per-tile basis, the player
/// should always be close to the next waypoint.  If something moves
/// the player far from the expected location (e.g. lag, triggering an
/// event, etc), then the route should be replanned.
const REPLAN_THRESHOLD: f32 = 2.0;

pub struct MovementGoal {
    target_room: &'static str,
    target_position: Vector<f32>,
}

pub struct LocalMovementGoal {
    room_name: &'static str,
    position: Vector<f32>,
    waypoints: Vec<Vector<f32>>,
}

pub struct FaceDirectionGoal(pub FacingDirection);

impl MovementGoal {
    pub fn new(
        target_room: &'static str,
        target_position: Vector<f32>,
    ) -> Self {
        Self {
            target_room,
            target_position,
        }
    }

    fn room_to_room_movement(
        &self,
        game_state: &GameState,
    ) -> Result<Option<SubGoals>, Error> {
        let current_room_name = &game_state.player.room_name;
        if self.target_room == current_room_name {
            return Ok(None);
        }

        // TODO: Handle rooms with disconnnected regions.
        //
        // For example, the Backwoods connects to four other rooms,
        // the Farm, the Mountain, the BusStop, and the Tunnel.
        // However, from a given location in the Backwoods, the player
        // can only reach two of those four rooms.  The current
        // algorithm assumes that all exits can be reached from any
        // point in the room.

        // Dijkstra's algorithm to search for connections between rooms

        Ok(None)
    }

    fn within_room_movement(
        &self,
        game_state: &GameState,
    ) -> Result<Option<SubGoals>, Error> {
        let player = &game_state.player;

        let goal_dist = (self.target_position - player.position / 64.0).mag();
        let opt_goals = if goal_dist >= TOLERANCE {
            let goals = SubGoals::new().then(LocalMovementGoal::new(
                self.target_room,
                self.target_position,
            ));
            Some(goals)
        } else {
            None
        };

        Ok(opt_goals)
    }
}

struct TileGraph {
    clear_tiles: TileMap<bool>,
}
impl GraphSearch<Vector<isize>> for TileGraph {
    fn connections_from<'a>(
        &'a self,
        &tile: &'a Vector<isize>,
    ) -> impl IntoIterator<Item = (Vector<isize>, u64)> + 'a {
        Direction::iter()
            .filter(move |dir| {
                let is_clear_tile = |tile: Vector<isize>| {
                    self.clear_tiles.get(tile).cloned().unwrap_or(false)
                };

                let offset = dir.offset();
                if dir.is_cardinal() {
                    is_clear_tile(tile + offset)
                } else {
                    [
                        Vector::new(offset.right, offset.down),
                        Vector::new(0, offset.down),
                        Vector::new(offset.right, 0),
                    ]
                    .into_iter()
                    .all(|offset| is_clear_tile(tile + offset))
                }
            })
            .map(move |dir| {
                let offset = dir.offset();
                let new_tile = tile + offset;
                let additional_distance = if dir.is_cardinal() { 2 } else { 3 };

                (new_tile, additional_distance)
            })
    }

    fn heuristic_between(
        &self,
        node_from: &Vector<isize>,
        node_to: &Vector<isize>,
    ) -> Option<u64> {
        let offset = (*node_from - *node_to).map(|x| x.abs());
        let min = offset.right.min(offset.down) as u64;
        let max = offset.right.max(offset.down) as u64;

        let diagonal_movements = min;
        let cardinal_movements = max - min;
        // Counting the number of half-tiles means that I can stay
        // in integer math.  3/2 as the cost of a diagonal
        // movement is close enough to sqrt(2) for the
        // pathfinding.
        Some(3 * diagonal_movements + 2 * cardinal_movements)
    }
}

impl LocalMovementGoal {
    pub fn new(room_name: &'static str, position: Vector<f32>) -> Self {
        Self {
            room_name,
            position,
            waypoints: Vec::new(),
        }
    }

    fn update_plan(&mut self, game_state: &GameState) -> Result<(), Error> {
        let player = &game_state.player;
        if player.room_name != self.room_name {
            todo!("Plan movement between rooms");
        }

        while let Some(next_waypoint) = self.waypoints.last().cloned() {
            let dist = (next_waypoint - player.position / 64.0).mag();
            if dist < TOLERANCE {
                self.waypoints.pop();
            } else if dist > REPLAN_THRESHOLD {
                self.waypoints.clear();
            } else {
                break;
            }
        }

        let goal_dist = (self.position - player.position / 64.0).mag();
        if self.waypoints.is_empty() && goal_dist > TOLERANCE {
            self.waypoints = self.generate_plan(game_state)?;
        }

        Ok(())
    }

    fn generate_plan(
        &self,
        game_state: &GameState,
    ) -> Result<Vec<Vector<f32>>, Error> {
        let player = &game_state.player;

        let location = game_state
            .locations
            .iter()
            .find(|loc| loc.name == player.room_name)
            .expect("Player must be in a room");
        let clear_tiles = {
            let width = location.shape.right as usize;
            let height = location.shape.down as usize;
            let mut map = TileMap::<bool>::full(true, width, height);
            // TODO: Store the `blocked` tiles as a TileMap
            let iter_blocked = location
                .blocked
                .iter()
                .enumerate()
                .filter(|(_, is_blocked)| **is_blocked)
                .map(|(index, _)| {
                    let i = (index / height) as isize;
                    let j = (index % height) as isize;
                    Vector::new(i, j)
                });

            let iter_water = location
                .water_tiles
                .iter()
                .flat_map(|vec_bool| vec_bool.iter())
                .enumerate()
                .filter(|(_, is_water)| **is_water)
                .map(|(index, _)| {
                    let i = (index / height) as isize;
                    let j = (index % height) as isize;
                    Vector::new(i, j)
                });

            let iter_clumps = location
                .resource_clumps
                .iter()
                .flat_map(|clump| clump.shape.iter_points());

            let iter_bush = location
                .bushes
                .iter()
                .flat_map(|bush| bush.rectangle().iter_points());

            let iter_tree = location.trees.iter().map(|tree| tree.position);

            let iter_litter = location.litter.iter().map(|litter| litter.tile);

            let iter_buildings = location
                .buildings
                .iter()
                .flat_map(|building| building.shape.iter_points());

            std::iter::empty()
                .chain(iter_blocked)
                .chain(iter_water)
                .chain(iter_clumps)
                .chain(iter_bush)
                .chain(iter_tree)
                .chain(iter_litter)
                .chain(iter_buildings)
                .for_each(|tile| {
                    map[tile] = false;
                });

            map
        };

        let player_tile = player.tile();
        let target_tile: Vector<isize> =
            self.position.map(|x| x.round() as isize);

        let search_tiles: Vec<_> = TileGraph { clear_tiles }
            .a_star_search(player_tile, target_tile)
            .take_while_inclusive(|(tile, _)| *tile != target_tile)
            .collect();

        let last = search_tiles
            .last()
            .filter(|(tile, _)| *tile == target_tile)
            .ok_or(BotError::NoRouteToTarget)?;
        let iter_waypoints =
            std::iter::successors(Some(last), |(_, metadata)| {
                metadata
                    .backref
                    .as_ref()
                    .map(|b| b.initial_node)
                    .and_then(|index| search_tiles.get(index))
            })
            .map(|(tile, _)| tile)
            .map(|vec_isize| vec_isize.map(|i| i as f32));

        let waypoints = std::iter::once(self.position)
            .chain(iter_waypoints.skip(1))
            .collect();

        Ok(waypoints)
    }

    pub fn iter_waypoints(
        &self,
    ) -> impl DoubleEndedIterator<Item = Vector<f32>> + '_ {
        self.waypoints.iter().copied()
    }
}

impl BotGoal for MovementGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        Ok(
            if let Some(subgoals) = self.room_to_room_movement(game_state)? {
                subgoals.into()
            } else if let Some(subgoals) =
                self.within_room_movement(game_state)?
            {
                subgoals.into()
            } else {
                BotGoalResult::Completed
            },
        )
    }
}

impl BotGoal for LocalMovementGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let player_position = player.position / 64.0;

        self.update_plan(game_state)?;

        let Some(next_waypoint) = self.waypoints.last().cloned() else {
            return Ok(if player.movement.is_some() {
                BotGoalResult::Action(Some(GameAction::StopMoving))
            } else {
                BotGoalResult::Completed
            });
        };
        let direction = next_waypoint - player_position;

        let dir = Direction::iter()
            .max_by(|dir_a, dir_b| {
                let do_dot_product = |dir: Direction| {
                    let offset = dir.offset().map(|i| i as f32);
                    let offset = offset / offset.mag();
                    offset.dot(direction)
                };
                let dot_a = do_dot_product(*dir_a);
                let dot_b = do_dot_product(*dir_b);
                num::traits::float::TotalOrder::total_cmp(&dot_a, &dot_b)
            })
            .expect("Direction::iter is non-empty");

        Ok(BotGoalResult::Action(Some(GameAction::Move(dir))))
    }
}

impl BotGoal for FaceDirectionGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
    ) -> Result<BotGoalResult, Error> {
        let output = if game_state.player.facing != self.0 {
            let dir = match self.0 {
                FacingDirection::North => Direction::North,
                FacingDirection::East => Direction::East,
                FacingDirection::South => Direction::South,
                FacingDirection::West => Direction::West,
            };
            BotGoalResult::Action(Some(GameAction::Move(dir)))
        } else if game_state.player.movement.is_some() {
            BotGoalResult::Action(Some(GameAction::StopMoving))
        } else {
            BotGoalResult::Completed
        };

        Ok(output)
    }
}
