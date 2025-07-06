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
    graph_search::{GraphSearch, SearchNodeMetadata},
    impl_tile_map_graph_search::point_to_point_lower_bound,
    GameStateExt as _, WaitUntilTimeOfDay,
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

pub struct MovementGoal {
    target_room: String,
    target_position: Vector<f32>,
    tolerance: f32,
}

pub struct GoToRoomGoal {
    target_room: String,
}

pub struct LocalMovementGoal {
    target_room: String,
    target_position: Vector<f32>,
    tolerance: f32,

    /// A lookup specifying the direction to move, given the tile on
    /// which the player is standing.
    direction_map: Option<TileMap<Direction>>,

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
            .flat_map(move |loc| {
                let reachable = loc.pathfinding().reachable(node.current_pos);

                let is_on_warp = loc
                    .warps
                    .iter()
                    .any(|warp| node.current_pos == warp.location);

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
                    .filter(|warp| warp.requires_friendship.is_none())
                    .filter(move |warp| {
                        Direction::iter_cardinal()
                            .map(|dir| dir.offset())
                            .chain(Some(Vector::new(0, 0)))
                            .map(|offset| warp.location + offset)
                            .any(|adj_tile| reachable.is_set(adj_tile))
                    })
                    .filter_map(move |warp| {
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
                            .filter(|_| dist > 5 || !is_on_warp)
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

    fn room_to_room_iter_dijkstra<'a>(
        &'a self,
        game_state: &'a GameState,
        graph: &'a ConnectedRoomGraph<'a>,
    ) -> Result<
        (
            usize,
            impl Iterator<Item = (RoomSearchNode, SearchNodeMetadata)> + 'a,
        ),
        Error,
    > {
        // Dijkstra's algorithm to search for connections between rooms

        let initial = RoomSearchNode {
            current_pos: game_state.player.tile(),
            current_room_index: graph
                .get_room_index(&game_state.player.room_name)
                .unwrap(),
        };
        let target_room_index = graph
            .get_room_index(&self.target_room)
            .ok_or_else(|| BotError::UnknownRoom(self.target_room.clone()))?;

        let iter = graph.dijkstra_search(initial).take_while_inclusive(
            move |(node, _)| node.current_room_index != target_room_index,
        );

        Ok((target_room_index, iter))
    }

    fn room_to_room_movement(
        &self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        if self.target_room == game_state.player.room_name {
            return Ok(None);
        }

        let graph = ConnectedRoomGraph {
            locations: &game_state.locations,
            in_game_time: game_state.globals.in_game_time,
        };

        // Dijkstra's algorithm to search for connections between rooms
        let (target_room_index, iter_search) =
            self.room_to_room_iter_dijkstra(game_state, &graph)?;
        let search_nodes: Vec<_> = iter_search.collect();

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

    pub(crate) fn closest_entrance(
        game_state: &GameState,
        target_room: &str,
    ) -> Result<Vector<isize>, Error> {
        let goal = MovementGoal::new(target_room, Vector::new(0.0, 0.0));

        let graph = ConnectedRoomGraph {
            locations: &game_state.locations,
            in_game_time: game_state.globals.in_game_time,
        };

        let (target_room_index, mut iter_search) =
            goal.room_to_room_iter_dijkstra(game_state, &graph)?;

        let entrance = iter_search
            .find(|(node, _)| node.current_room_index == target_room_index)
            .map(|(node, _)| node.current_pos)
            .ok_or_else(|| BotError::NoRouteToRoom {
                from_room: game_state.player.room_name.clone(),
                to_room: target_room.to_string(),
            })?;

        Ok(entrance)
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

impl GoToRoomGoal {
    pub fn new(target_room: impl Into<String>) -> Self {
        let target_room = target_room.into();
        Self { target_room }
    }
}

impl LocalMovementGoal {
    pub fn new(
        room_name: String,
        position: Vector<f32>,
        warp_kind: Option<WarpKind>,
    ) -> Self {
        Self {
            target_room: room_name,
            target_position: position,
            direction_map: None,
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
        } else if player.room_name != self.target_room {
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
            let goal_dist =
                self.target_position.manhattan_dist(player.center_pos());
            goal_dist < self.tolerance
        }
    }

    fn generate_plan(
        &mut self,
        game_state: &GameState,
    ) -> Result<TileMap<Direction>, Error> {
        let player = &game_state.player;
        if player.room_name != self.target_room {
            return Err(BotError::IncorrectRoomToGenerateLocalMovementPlan {
                current_room: player.room_name.clone(),
                expected_room: self.target_room.clone(),
            }
            .into());
        }

        let location = game_state.current_room()?;
        let player_tile = player.tile();

        let room_bounds = location.bounds();

        if !room_bounds.contains(player_tile) {
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
            return Err(BotError::PlayerIsOutOfBounds {
                pos: player.center_pos(),
                room_bounds,
            }
            .into());
        }

        let target_tile: Vector<isize> =
            self.target_position.map(|x| x.round() as isize);

        let map = location.pathfinding().direction_map(target_tile);

        Ok(map)
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

impl BotGoal for GoToRoomGoal {
    fn description(&self) -> Cow<'static, str> {
        format!("Go to '{}'", self.target_room).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        _actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        if game_state.player.room_name == self.target_room {
            Ok(BotGoalResult::Completed)
        } else {
            let entrance = game_state.closest_entrance(&self.target_room)?;
            let goal =
                MovementGoal::new(self.target_room.clone(), entrance.into());
            Ok(goal.into())
        }
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
            // direction map and re-plan.
            self.direction_map = None;
        }

        if self.direction_map.is_none() {
            self.direction_map = Some(self.generate_plan(game_state)?);
        }

        let map = self
            .direction_map
            .as_ref()
            .expect("Just populated direction map");

        let player_tile = player.tile();
        let player_position = player.center_pos();
        let target_tile = self.target_position.as_tile();

        let dir = if player_tile == target_tile {
            let offset = self.target_position - player.center_pos();
            offset.closest_direction()
        } else {
            let core_dir = map.get(player_tile).cloned().ok_or_else(|| {
                BotError::PlayerIsOutOfBounds {
                    pos: player_position,
                    room_bounds: map.bounds(),
                }
            })?;

            let tile_offset = player_position - player_tile.map(|x| x as f32);
            let on_west_edge = tile_offset.right < -0.2;
            let on_east_edge = tile_offset.right > 0.2;
            let on_north_edge = tile_offset.down < -0.2;
            let on_south_edge = tile_offset.down > 0.2;

            match core_dir {
                Direction::North if on_west_edge => Direction::NorthEast,
                Direction::North if on_east_edge => Direction::NorthWest,
                Direction::South if on_west_edge => Direction::SouthEast,
                Direction::South if on_east_edge => Direction::SouthWest,
                Direction::East if on_north_edge => Direction::SouthEast,
                Direction::East if on_south_edge => Direction::NorthEast,
                Direction::West if on_north_edge => Direction::SouthWest,
                Direction::West if on_south_edge => Direction::NorthWest,
                other => other,
            }
        };

        actions.do_action(GameAction::Move(dir));
        actions.do_action(GameAction::MouseOverTile(target_tile));

        let must_open_door = self.activate_endpoint()
            && player_position.manhattan_dist(self.target_position) < 1.5;

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
            "Move to {} within {}",
            self.target_position, self.target_room
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
