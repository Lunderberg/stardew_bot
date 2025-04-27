use std::collections::HashMap;

use priority_queue::PriorityQueue;

use crate::{
    bot_logic::BotError,
    game_state::{FacingDirection, TileMap, Vector},
    Direction, Error, GameAction, GameState,
};

use super::bot_logic::{BotGoal, BotGoalResult};

pub struct MoveToLocationGoal {
    room_name: &'static str,
    position: Vector<f32>,
    tolerance: f32,
    replan_threshold: f32,
    waypoints: Vec<Vector<f32>>,
}

pub struct FaceDirectionGoal(pub FacingDirection);

struct SearchItemMetadata {
    prev_dist: isize,
    backref: Option<Vector<isize>>,
    min_dist_remaining: isize,
}

impl MoveToLocationGoal {
    pub fn new(room_name: &'static str, position: Vector<f32>) -> Self {
        Self {
            room_name,
            position,
            tolerance: 0.1,
            replan_threshold: 2.0,
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
            if dist < self.tolerance {
                self.waypoints.pop();
            } else if dist > self.replan_threshold {
                self.waypoints.clear();
            } else {
                break;
            }
        }

        let goal_dist = (self.position - player.position / 64.0).mag();
        if self.waypoints.is_empty() && goal_dist > self.tolerance {
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
        let is_clear_tile = |tile: Vector<isize>| -> bool {
            clear_tiles.get(tile).cloned().unwrap_or(false)
        };

        let player_tile = player.tile();
        let target_tile: Vector<isize> =
            self.position.map(|x| x.round() as isize);

        let heuristic = |tile: Vector<isize>| {
            let offset = (tile - target_tile).map(|x| x.abs());
            let min = offset.right.min(offset.down);
            let max = offset.right.max(offset.down);

            let diagonal_movements = min;
            let cardinal_movements = max - min;
            // Counting the number of half-tiles means that I can stay
            // in integer math.  3/2 as the cost of a diagonal
            // movement is close enough to sqrt(2) for the
            // pathfinding.
            3 * diagonal_movements + 2 * cardinal_movements
        };

        let mut queue =
            PriorityQueue::<Vector<isize>, SearchItemMetadata>::new();
        let mut finished = HashMap::<Vector<isize>, SearchItemMetadata>::new();

        queue.push(
            player_tile,
            SearchItemMetadata {
                prev_dist: 0,
                backref: None,
                min_dist_remaining: heuristic(player_tile),
            },
        );

        while let Some((tile, metadata)) = queue.pop() {
            let dist_to_tile = metadata.prev_dist;
            finished.insert(tile, metadata);

            if tile == target_tile {
                break;
            }

            Direction::iter()
                .filter(|dir| !finished.contains_key(&(tile + dir.offset())))
                .filter(|dir| {
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
                .for_each(|dir| {
                    let new_tile = tile + dir.offset();
                    let additional_distance =
                        if dir.is_cardinal() { 2 } else { 3 };

                    queue.push_increase(
                        new_tile,
                        SearchItemMetadata {
                            prev_dist: dist_to_tile + additional_distance,
                            backref: Some(tile),
                            min_dist_remaining: heuristic(new_tile),
                        },
                    );
                });
        }

        finished
            .get(&target_tile)
            .ok_or(BotError::NoRouteToTarget)?;

        let iter_waypoints = std::iter::successors(Some(target_tile), |tile| {
            finished.get(tile).expect("Broken backref chain").backref
        })
        .map(|vec| vec.map(|i| i as f32));

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

impl BotGoal for MoveToLocationGoal {
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

impl SearchItemMetadata {
    fn min_distance_to_dest(&self) -> isize {
        self.prev_dist + self.min_dist_remaining
    }
}

impl PartialEq for SearchItemMetadata {
    fn eq(&self, other: &Self) -> bool {
        self.min_distance_to_dest() == other.min_distance_to_dest()
    }
}
impl Eq for SearchItemMetadata {}
impl PartialOrd for SearchItemMetadata {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for SearchItemMetadata {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .min_distance_to_dest()
            .cmp(&self.min_distance_to_dest())
    }
}
