use std::borrow::Cow;

use itertools::Itertools as _;

use crate::{
    bot_logic::BotError,
    game_state::{
        FacingDirection, Location, Rectangle, TileMap, Vector, WarpKind,
    },
    Direction, Error, GameAction, GameState,
};

use super::{
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    graph_search::GraphSearch,
    impl_tile_map_graph_search::point_to_point_lower_bound,
    WaitUntilTimeOfDay,
};

/// Epsilon distance (in tiles) to consider a target reached
///
/// For each waypoint, and for the final target location, consider the
/// position to be successfully reached when it is within this
/// distance of the desired location.
///
/// Used as the default tolerance to reach the target location.  Can
/// be overridden for a goal using `MovementGoal::with_tolerance`.
const WAYPOINT_TOLERANCE: f32 = 0.3;

/// Minimum distance (in tiles) to re-plan a route.
///
/// Since the planned route is tracked on a per-tile basis, the player
/// should always be close to the next waypoint.  If something moves
/// the player far from the expected location (e.g. lag, triggering an
/// event, etc), then the route should be replanned.
const REPLAN_THRESHOLD: f32 = 2.0;

pub struct MovementGoal {
    target_room: String,
    target_position: Vector<f32>,
    tolerance: f32,
}

pub struct LocalMovementGoal {
    room_name: String,
    position: Vector<f32>,
    waypoints: Vec<Vector<f32>>,
    tolerance: f32,

    /// The type of warp to be used at the end of the movement, if
    /// any.  Used to determine if the final tile should be clicked
    /// on, and if the player needs to wait until a given time.
    warp_kind: Option<WarpKind>,

    /// The previous position, and the game tick on which it had
    /// updated.  Used to detect if the pathfinding is stuck and
    /// should re-plan.
    previous_position: Option<(Vector<f32>, i32)>,
}

pub struct FaceDirectionGoal(pub FacingDirection);

pub struct StopMovingGoal;

struct ConnectedRoomGraph<'a> {
    locations: &'a [Location],
    in_game_time: i32,
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct RoomSearchNode {
    current_pos: Vector<isize>,
    current_room_index: usize,
}

impl ConnectedRoomGraph<'_> {
    fn get_room_index(&self, room_name: &str) -> Option<usize> {
        self.locations
            .iter()
            .enumerate()
            .find(|(_, loc)| loc.name == room_name)
            .map(|(i, _)| i)
    }
}

impl GraphSearch<RoomSearchNode> for ConnectedRoomGraph<'_> {
    fn connections_from<'a>(
        &'a self,
        node: &'a RoomSearchNode,
    ) -> impl IntoIterator<Item = (RoomSearchNode, u64)> + 'a {
        self.locations
            .get(node.current_room_index)
            .into_iter()
            .flat_map(|loc| {
                let reachable = loc.find_reachable_tiles(node.current_pos);

                loc.warps
                    .iter()
                    .filter(|warp| {
                        // TODO: Handle case where the player has the
                        // key to the town.
                        if let WarpKind::LockedDoor { closes, .. } = &warp.kind
                        {
                            self.in_game_time < *closes
                        } else {
                            true
                        }
                    })
                    .filter(move |warp| {
                        Direction::iter_cardinal()
                            .map(|dir| dir.offset())
                            .chain(Some(Vector::new(0, 0)))
                            .map(|offset| warp.location + offset)
                            .any(|adj_tile| reachable.is_set(adj_tile))
                    })
                    .filter_map(|warp| {
                        if warp.location == node.current_pos {
                            self.get_room_index(&warp.target_room).map(
                                |next_room_index| {
                                    (
                                        RoomSearchNode {
                                            current_pos: warp.target,
                                            current_room_index: next_room_index,
                                        },
                                        1,
                                    )
                                },
                            )
                        } else {
                            let dist = point_to_point_lower_bound(
                                node.current_pos,
                                warp.location,
                            );
                            Some((
                                RoomSearchNode {
                                    current_pos: warp.location,
                                    current_room_index: node.current_room_index,
                                },
                                dist,
                            ))
                        }
                    })
            })
    }
}

impl MovementGoal {
    pub fn new(
        target_room: impl Into<String>,
        target_position: Vector<f32>,
    ) -> Self {
        let target_room = target_room.into();
        Self {
            target_room,
            target_position,
            tolerance: WAYPOINT_TOLERANCE,
        }
    }

    #[allow(dead_code)]
    pub fn with_tolerance(self, tolerance: f32) -> Self {
        Self { tolerance, ..self }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let player = &game_state.player;

        let goal_dist =
            self.target_position.manhattan_dist(player.center_pos());

        let is_correct_room = player.room_name == self.target_room;
        let is_correct_location_within_room = goal_dist < self.tolerance;

        is_correct_room && is_correct_location_within_room
    }

    fn room_to_room_movement(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if self.target_room == game_state.player.room_name {
            return Ok(None);
        }

        // Dijkstra's algorithm to search for connections between rooms

        let graph = ConnectedRoomGraph {
            locations: &game_state.locations,
            in_game_time: game_state.globals.in_game_time,
        };
        let initial = RoomSearchNode {
            current_pos: game_state.player.tile(),
            current_room_index: graph
                .get_room_index(&game_state.player.room_name)
                .unwrap(),
        };
        let target_room_index = graph
            .get_room_index(&self.target_room)
            .ok_or_else(|| BotError::UnknownRoom(self.target_room.clone()))?;

        let search_nodes: Vec<_> = graph
            .dijkstra_search(initial)
            .take_while_inclusive(|(node, _)| {
                node.current_room_index != target_room_index
            })
            .collect();

        let last = search_nodes
            .last()
            .filter(|(node, _)| node.current_room_index == target_room_index)
            .ok_or_else(|| BotError::NoRouteToRoom {
                from_room: game_state.player.room_name.clone(),
                to_room: self.target_room.clone(),
            })?;
        let goals: Vec<_> =
            std::iter::successors(Some(last), |(_, metadata)| {
                metadata
                    .backref
                    .as_ref()
                    .and_then(|backref| search_nodes.get(backref.initial_node))
            })
            .map(|(node, _)| node)
            .tuple_windows()
            .filter(|(a, b)| a.current_room_index == b.current_room_index)
            .map(|(a, _)| {
                let loc = &game_state.locations[a.current_room_index];
                let tile = a.current_pos;
                let warp_kind = loc
                    .warps
                    .iter()
                    .find(|warp| warp.location == tile)
                    .map(|warp| warp.kind.clone());
                let opt_tolerance = match warp_kind {
                    None | Some(WarpKind::Automatic) => None,
                    Some(WarpKind::Door | WarpKind::LockedDoor { .. }) => {
                        Some(1.4)
                    }
                };
                let goal = LocalMovementGoal::new(
                    loc.name.clone(),
                    tile.map(|x| x as f32),
                    warp_kind,
                );
                if let Some(tolerance) = opt_tolerance {
                    goal.with_tolerance(tolerance)
                } else {
                    goal
                }
            })
            .collect();

        let goals = goals.into_iter().rev().collect();

        Ok(Some(goals))
    }

    fn within_room_movement(
        &self,
        game_state: &GameState,
    ) -> Option<LogicStack> {
        let player = &game_state.player;

        Some(
            LogicStack::new().then(
                LocalMovementGoal::new(
                    self.target_room.clone(),
                    self.target_position,
                    None,
                )
                .with_tolerance(self.tolerance),
            ),
        )
        .filter(|_| self.target_room == player.room_name)
        .filter(|_| {
            let goal_dist =
                self.target_position.manhattan_dist(player.center_pos());
            goal_dist >= self.tolerance
        })
    }
}

impl LocalMovementGoal {
    pub fn new(
        room_name: String,
        position: Vector<f32>,
        warp_kind: Option<WarpKind>,
    ) -> Self {
        Self {
            room_name,
            position,
            waypoints: Vec::new(),
            tolerance: WAYPOINT_TOLERANCE,
            warp_kind,
            previous_position: None,
        }
    }

    fn with_tolerance(self, tolerance: f32) -> Self {
        Self { tolerance, ..self }
    }

    fn activate_endpoint(&self) -> bool {
        self.warp_kind
            .as_ref()
            .map(|kind| {
                matches!(kind, WarpKind::Door | WarpKind::LockedDoor { .. })
            })
            .unwrap_or(false)
    }

    fn is_completed(&self, game_state: &GameState) -> bool {
        let player = &game_state.player;

        // TODO: Handle case where the player has the key to the town.
        let door_has_closed = if let Some(WarpKind::LockedDoor {
            closes, ..
        }) = &self.warp_kind
        {
            game_state.globals.in_game_time >= *closes
        } else {
            false
        };

        if door_has_closed {
            // We haven't reached the goal location, but it's too late
            // in the day to get there anyways.
            true
        } else if player.room_name != self.room_name {
            // Reached a warp to another room, so the local movement
            // can be popped from the stack.
            true
        } else if self.activate_endpoint() {
            // Even if we're within the threshold, we still need to
            // click on the target to complete the action.  Therefore,
            // not yet done.
            false
        } else {
            // Check completion by seeing how far we are from the
            // target position.
            let goal_dist = self.position.manhattan_dist(player.center_pos());
            goal_dist < self.tolerance
        }
    }

    fn update_plan(&mut self, game_state: &GameState) -> Result<(), Error> {
        let player = &game_state.player;
        if player.room_name != self.room_name {
            // Reached a warp to another room, so the local movement
            // can be popped from the stack.
            self.waypoints.clear();
            return Ok(());
        }

        while let Some(next_waypoint) = self.waypoints.last().cloned() {
            let dist = next_waypoint.manhattan_dist(player.center_pos());
            if dist < WAYPOINT_TOLERANCE {
                self.waypoints.pop();
            } else if dist > REPLAN_THRESHOLD {
                self.waypoints.clear();
            } else {
                break;
            }
        }

        if self.waypoints.is_empty() {
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

        let player_pos = player.center_pos();
        let player_tile = player.tile();

        let bounds = Rectangle {
            top_left: Vector::zero(),
            shape: location.shape,
        };

        if !bounds.contains(player_tile) {
            // During screen transitions, the player's current room is
            // updated before the player's X/Y position is updated.
            // If the memory-read occurs between these two times, then
            // the player may appear to be out-of-bounds, causing an
            // error during pathfinding.
            //
            // TODO: Find a stronger check to determine if a screen
            // transition is in progress, as the current check
            // wouldn't catch cases where the player's pre-update
            // location places them in an inaccessible part of the
            // post-update room.
            return Ok(Vec::new());
        }

        let target_tile: Vector<isize> =
            self.position.map(|x| x.round() as isize);

        let waypoints = location
            .pathfinding()
            .include_border(
                self.tolerance >= 1.0 || !bounds.contains(target_tile),
            )
            .path_between(player_tile, target_tile)?
            .into_iter()
            .map(|tile| tile.map(|x| x as f32))
            .rev()
            .filter(|pos| pos.manhattan_dist(player_pos) > WAYPOINT_TOLERANCE)
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
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let goal: BotGoalResult = if self.is_completed(game_state) {
            BotGoalResult::Completed
        } else if let Some(subgoals) = self.room_to_room_movement(game_state)? {
            subgoals.into()
        } else if let Some(subgoals) = self.within_room_movement(game_state) {
            subgoals.into()
        } else {
            unreachable!(
                "Unless the MovementGoal is completed, \
                 should always be able to produced either \
                 a room-to-room or within-room movement.  \
                 However, could not make a path to {}/{} \
                 from current location of {}/{}/{}",
                self.target_room,
                self.target_position,
                game_state.player.room_name,
                game_state.player.tile(),
                game_state.player.center_pos(),
            )
        };

        Ok(goal)
    }
    fn description(&self) -> Cow<'static, str> {
        format!(
            "X-room move to {} in {}",
            self.target_position, self.target_room
        )
        .into()
    }
}

impl BotGoal for LocalMovementGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;

        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        if game_state.dialogue_menu.is_some() {
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        if self
            .previous_position
            .map(|(prev_pos, _)| player.center_pos().dist(prev_pos) > 0.001)
            .unwrap_or(true)
        {
            self.previous_position =
                Some((player.center_pos(), game_state.globals.game_tick));
        }
        if self
            .previous_position
            .map(|(_, prev_tick)| game_state.globals.game_tick - prev_tick > 30)
            .unwrap_or(false)
        {
            // Something has gone wrong, as the pathfinding isn't able
            // to make progress for over half a second.  Clear the
            // waypoints and re-plan.
            self.waypoints.clear();
        }

        self.update_plan(game_state)?;

        let next_waypoint =
            self.waypoints.last().cloned().unwrap_or(self.position);

        let player_position = player.center_pos();
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
        let target_tile = self.position.as_tile();

        actions.do_action(GameAction::Move(dir));
        actions.do_action(GameAction::MouseOverTile(target_tile));

        let must_open_door = self.activate_endpoint()
            && player_position.manhattan_dist(self.position) < 1.5;

        if must_open_door
            && game_state.inputs.mouse_tile_location == target_tile
        {
            match self.warp_kind {
                Some(WarpKind::LockedDoor { opens, .. })
                    if game_state.globals.in_game_time < opens =>
                {
                    let goal = WaitUntilTimeOfDay::new(opens);
                    return Ok(goal.into());
                }
                _ => {
                    actions.do_action(GameAction::RightClick);
                }
            }
        }

        Ok(BotGoalResult::InProgress)
    }

    fn description(&self) -> Cow<'static, str> {
        format!(
            "Move {} tiles to {} in {}",
            self.waypoints.len(),
            self.position,
            self.room_name
        )
        .into()
    }
}

impl FaceDirectionGoal {
    pub fn new(dir: FacingDirection) -> Self {
        Self(dir)
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let facing_correct_direction = game_state.player.facing == self.0;
        let mouse_over_correct_tile = game_state.player.tile()
            + self.0.offset()
            == game_state.inputs.mouse_tile_location;

        facing_correct_direction && mouse_over_correct_tile
    }
}

impl BotGoal for FaceDirectionGoal {
    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if self.is_completed(game_state) {
            return Ok(BotGoalResult::Completed);
        }

        let facing_tile = game_state.player.tile() + self.0.offset();
        actions.do_action(GameAction::MouseOverTile(facing_tile));

        if game_state.player.facing != self.0 {
            let dir = match self.0 {
                FacingDirection::North => Direction::North,
                FacingDirection::East => Direction::East,
                FacingDirection::South => Direction::South,
                FacingDirection::West => Direction::West,
            };
            actions.do_action(GameAction::Move(dir));
        }

        Ok(BotGoalResult::InProgress)
    }

    fn description(&self) -> Cow<'static, str> {
        format!("Turn to face {}", self.0).into()
    }
}

impl BotGoal for StopMovingGoal {
    fn description(&self) -> Cow<'static, str> {
        "Stop moving".into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        Ok(if game_state.player.movement.is_some() {
            // The player is currently moving.  The return-to-default
            // logic will stop the player's movement, so long as no
            // goal triggers additional movement.  Mark this goal as
            // InProgress to prevent any other goals from continuing
            // the ongoing motion.
            BotGoalResult::InProgress
        } else {
            // The player is not moving.  Therefore, the goal of
            // becoming stationary has been achieved.
            BotGoalResult::Completed
        })
    }
}
