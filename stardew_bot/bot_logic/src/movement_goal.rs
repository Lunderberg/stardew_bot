use std::{borrow::Cow, collections::HashSet};

use geometry::{Direction, Vector};
use itertools::Itertools as _;

use crate::{
    Error, GameAction, MenuCloser, SelectItemGoal, SwingWithAnimationCancel,
};
use game_state::{
    FacingDirection, FurnitureKind, GameState, Item, ItemId, Location, Object,
    ObjectKind, StaticState, TileMap, WarpKind,
};

use super::{
    GameStateExt as _, LocationExt as _, WaitUntilTimeOfDay,
    bot_logic::{ActionCollector, BotGoal, BotGoalResult, LogicStack},
    graph_search::{GraphSearch, SearchNodeMetadata},
    impl_tile_map_graph_search::point_to_point_lower_bound,
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
    clear_breakables: bool,
}

pub struct GoToRoomGoal {
    target_room: String,
    clear_breakables: bool,
}

pub struct LocalMovementGoal {
    target_room: String,
    target_position: Vector<f32>,
    tolerance: f32,

    /// If true and if the player has a suitable weapon, objects that
    /// are breakable without using stamina (e.g. fiber, barrels in
    /// the mine) will be broken in order to follow a shorter path.
    ///
    /// Otherwise, these objects will be walked around instead.
    clear_breakables: bool,

    /// A lookup specifying the direction to move, given the tile on
    /// which the player is standing.
    direction_map: Option<TileMap<Direction>>,

    /// A lookup of breakable objects, to avoid per-frame iteration
    /// over all objects in the location.
    breakable_objects: Option<HashSet<Vector<isize>>>,

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

/// Use a chair to move 2 tiles without advancing in-game time
///
/// While getting into or out of a chair, time is paused.  A human
/// player can take advantage of this by setting up chains of chairs,
/// hopping from one to the next.  A bot can do even better, by
/// placing down and picking up the same chair.
///
/// To use this goal, the bot must have a chair in their inventory.
/// The goal will place the chair down with the specified
/// location/orientation, jump into the chair, jump out of the chair,
/// then pick up the chair.  During this time, interrupts should be
/// suppressed, so that the bot doesn't accidentally leave the chair
/// behind.
struct ChairJumpGoal {
    /// The tile on which the chair should be placed.  Must be
    /// currently open, and adjacent to the tile the player is on.
    chair_tile: Vector<isize>,

    /// The direction the chair should be facing when placed.
    facing: FacingDirection,

    /// The tick on which the chair was placed down.  If we have just
    /// placed the chair down, we may get an inconsistent view of the
    /// game state, where the chair no longer exists in the player's
    /// inventory, but hasn't yet been updated on the current
    /// location.  To prevent this, wait a few frames to make sure the
    /// chair shows up.
    chair_placement_tick: Option<i32>,
}

struct ConnectedRoomGraph<'a> {
    locations: &'a [Location],
    statics: &'a StaticState,
    in_game_time: i32,
    events: &'a HashSet<String>,
    clear_breakables: bool,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct RoomSearchNode {
    current_pos: Vector<isize>,
    current_room_index: usize,
    warp_index: Option<usize>,
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
                let reachable = loc
                    .pathfinding(self.statics)
                    .clear_breakables(self.clear_breakables)
                    .reachable(node.current_pos);

                let is_on_warp = loc
                    .warps
                    .iter()
                    .any(|warp| node.current_pos == warp.location);

                loc.warps
                    .iter()
                    .enumerate()
                    .filter(|(_, warp)| {
                        // TODO: Handle case where the player has the
                        // key to the town.
                        match &warp.kind {
                            WarpKind::Automatic | WarpKind::Door => true,
                            WarpKind::LockedDoor { closes, .. } => {
                                self.in_game_time < *closes
                            }
                            WarpKind::Minecart { event, .. } => {
                                self.events.contains(event)
                            }
                        }
                    })
                    .filter(|(_, warp)| warp.requires_friendship.is_none())
                    .filter(move |(_, warp)| {
                        Direction::iter_cardinal()
                            .map(|dir| dir.offset())
                            .chain(Some(Vector::new(0, 0)))
                            .map(|offset| warp.location + offset)
                            .any(|adj_tile| reachable.is_set(adj_tile))
                    })
                    .filter_map(move |(warp_index, warp)| {
                        if warp.location == node.current_pos {
                            let next_room_index =
                                self.get_room_index(&warp.target_room)?;
                            let node = RoomSearchNode {
                                current_pos: warp.target,
                                current_room_index: next_room_index,
                                warp_index: Some(warp_index),
                            };
                            Some((node, 1))
                        } else {
                            let dist = point_to_point_lower_bound(
                                node.current_pos,
                                warp.location,
                            );
                            let node = RoomSearchNode {
                                current_pos: warp.location,
                                current_room_index: node.current_room_index,
                                warp_index: Some(warp_index),
                            };
                            Some((node, dist))
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
            clear_breakables: false,
        }
    }

    #[allow(dead_code)]
    pub fn with_tolerance(self, tolerance: f32) -> Self {
        Self { tolerance, ..self }
    }

    pub fn clear_breakables(self, clear_breakables: bool) -> Self {
        Self {
            clear_breakables,
            ..self
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        let player = &game_state.player;

        let goal_dist =
            self.target_position.chessboard_dist(player.center_pos());

        let is_correct_room = player.room_name == self.target_room;
        let is_correct_location_within_room = goal_dist < self.tolerance;

        is_correct_room && is_correct_location_within_room
    }

    fn can_clear_breakables(&self, game_state: &GameState) -> bool {
        self.clear_breakables
            && game_state
                .player
                .inventory
                .iter_items()
                .any(|item| item.as_weapon().is_some())
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
            warp_index: None,
        };
        let target_room_index = graph
            .get_room_index(&self.target_room)
            .ok_or_else(|| Error::UnknownRoom(self.target_room.clone()))?;

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
            statics: &game_state.statics,
            in_game_time: game_state.globals.in_game_time,
            events: &game_state.globals.events_triggered,
            clear_breakables: self.can_clear_breakables(game_state),
        };

        // Dijkstra's algorithm to search for connections between rooms
        let (target_room_index, iter_search) =
            self.room_to_room_iter_dijkstra(game_state, &graph)?;
        let search_nodes: Vec<_> = iter_search.collect();

        let last = search_nodes
            .last()
            .filter(|(node, _)| node.current_room_index == target_room_index)
            .ok_or_else(|| Error::NoRouteToRoom {
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
            .filter(|(a, b)| a.current_room_index != b.current_room_index)
            .map(|(b, a)| {
                let loc = &game_state.locations[a.current_room_index];
                let tile = a.current_pos;

                let warp_kind =
                    b.warp_index.map(|index| loc.warps[index].kind.clone());

                let opt_tolerance = match warp_kind {
                    None | Some(WarpKind::Automatic) => None,
                    Some(
                        WarpKind::Door
                        | WarpKind::LockedDoor { .. }
                        | WarpKind::Minecart { .. },
                    ) => Some(1.4),
                };
                let goal = LocalMovementGoal::new(
                    loc.name.clone(),
                    tile.map(|x| x as f32),
                    warp_kind,
                )
                .clear_breakables(self.clear_breakables);
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
        clear_breakables: bool,
    ) -> Result<Vector<isize>, Error> {
        let goal = MovementGoal::new(target_room, Vector::new(0.0, 0.0));

        let graph = ConnectedRoomGraph {
            locations: &game_state.locations,
            statics: &game_state.statics,
            in_game_time: game_state.globals.in_game_time,
            events: &game_state.globals.events_triggered,
            clear_breakables,
        };

        let (target_room_index, mut iter_search) =
            goal.room_to_room_iter_dijkstra(game_state, &graph)?;

        let entrance = iter_search
            .find(|(node, _)| node.current_room_index == target_room_index)
            .map(|(node, _)| node.current_pos)
            .ok_or_else(|| Error::NoRouteToRoom {
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
                .with_tolerance(self.tolerance)
                .clear_breakables(self.clear_breakables),
            ),
        )
        .filter(|_| self.target_room == player.room_name)
        .filter(|_| {
            let goal_dist =
                self.target_position.chessboard_dist(player.center_pos());
            goal_dist >= self.tolerance
        })
    }
}

impl GoToRoomGoal {
    pub fn new(target_room: impl Into<String>) -> Self {
        let target_room = target_room.into();
        Self {
            target_room,
            clear_breakables: false,
        }
    }

    pub fn is_completed(&self, game_state: &GameState) -> bool {
        self.target_room == game_state.player.room_name
    }

    pub fn clear_breakables(self, clear_breakables: bool) -> Self {
        Self {
            clear_breakables,
            ..self
        }
    }
}

fn object_is_breakable(obj: &Object) -> bool {
    // TODO: Extract a helper function, so this function and
    // `Pathfinding::movement_cost_component` can share the
    // same definition of "breakable" objects.
    matches!(obj.kind, ObjectKind::Fiber | ObjectKind::MineBarrel)
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
            breakable_objects: None,
            clear_breakables: false,
        }
    }

    fn with_tolerance(self, tolerance: f32) -> Self {
        Self { tolerance, ..self }
    }

    pub fn clear_breakables(self, clear_breakables: bool) -> Self {
        Self {
            clear_breakables,
            ..self
        }
    }

    fn activate_endpoint(&self) -> bool {
        self.warp_kind
            .as_ref()
            .map(|kind| {
                matches!(
                    kind,
                    WarpKind::Door
                        | WarpKind::LockedDoor { .. }
                        | WarpKind::Minecart { .. }
                )
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
                self.target_position.chessboard_dist(player.center_pos());
            goal_dist < self.tolerance
        }
    }

    fn can_clear_breakables(&self, game_state: &GameState) -> bool {
        self.clear_breakables && Self::preferred_weapon(game_state).is_some()
    }

    fn preferred_weapon(game_state: &GameState) -> Option<&Item> {
        let player = &game_state.player;
        player
            .selected_item()
            .filter(|item| item.as_weapon().is_some())
            .or_else(|| {
                player
                    .inventory
                    .iter_items()
                    .find(|item| item.as_weapon().is_some())
            })
    }

    fn generate_plan(
        &mut self,
        game_state: &GameState,
    ) -> Result<TileMap<Direction>, Error> {
        let player = &game_state.player;
        if player.room_name != self.target_room {
            return Err(Error::IncorrectRoomToGenerateLocalMovementPlan {
                current_room: player.room_name.clone(),
                expected_room: self.target_room.clone(),
            });
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
            return Err(Error::PlayerIsOutOfBounds {
                pos: player.center_pos(),
                room_bounds,
            });
        }

        let target_tile: Vector<isize> =
            self.target_position.map(|x| x.round() as isize);

        let map = location
            .pathfinding(&game_state.statics)
            .clear_breakables(self.can_clear_breakables(game_state))
            .direction_map(target_tile);

        Ok(map)
    }

    fn collect_breakables(
        &self,
        game_state: &GameState,
    ) -> Result<HashSet<Vector<isize>>, Error> {
        let location = game_state.current_room()?;
        let breakables = location
            .objects
            .iter()
            .filter(|obj| object_is_breakable(obj))
            .map(|obj| obj.tile)
            .collect();
        Ok(breakables)
    }

    fn choose_chair_jump(
        &self,
        game_state: &GameState,
    ) -> Result<Option<ChairJumpGoal>, Error> {
        let player = &game_state.player;

        let player_tile = player.tile();
        let target_tile = self.target_position.as_tile();

        let map = self.direction_map.as_ref().expect(
            "Should populate direction map before calling choose_chair_jump",
        );

        if player_tile == target_tile {
            // We're already on the target tile, so at best the chair
            // jump would move us to the center of the target tile.
            return Ok(None);
        }

        let Some(placement_dir) = map.get(player_tile) else {
            return Ok(None);
        };
        let chair_tile = player_tile + placement_dir.offset();
        if chair_tile == target_tile {
            // Using a chair jump would overshoot the target tile.
            return Ok(None);
        }

        let Some(stand_up_dir) = map.get(chair_tile) else {
            return Ok(None);
        };
        let facing = match stand_up_dir {
            Direction::North => FacingDirection::North,
            Direction::East => FacingDirection::East,
            Direction::South => FacingDirection::South,
            Direction::West => FacingDirection::West,
            // Since the chair can only face in cardinal directions,
            // preferred movement along a diagonal needs to be
            // replaced with movement in a cardinal direction.  This
            // should only occur in cases where either cardinal
            // direction is valid, so for now I'll pick an arbitrary
            // one of the two.  If this becomes an issue, may need to
            // track and check for a preferred direction between the
            // two options.
            Direction::NorthEast | Direction::NorthWest => {
                FacingDirection::North
            }
            Direction::SouthEast | Direction::SouthWest => {
                FacingDirection::South
            }
        };

        let goal = ChairJumpGoal {
            chair_tile,
            facing,
            chair_placement_tick: None,
        };

        if goal.find_held_chair(game_state).is_none()
            || !goal.is_allowed(game_state)?
        {
            return Ok(None);
        }

        // The player has a chair, and both the chair tile and the
        // final tile are unoccupied.  Therefore, the chair jump can
        // be started.
        Ok(Some(goal))
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
                MovementGoal::new(self.target_room.clone(), entrance.into())
                    .clear_breakables(self.clear_breakables);
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

        let opt_minecart_pixel = game_state.dialogue_menu().and_then(|menu| {
            let WarpKind::Minecart { destination, .. } =
                self.warp_kind.as_ref()?
            else {
                return None;
            };

            menu.responses
                .iter()
                .find(|response| &response.text == destination)
                .map(|response| response.pixel)
        });
        if let Some(pixel) = opt_minecart_pixel {
            actions.do_action(GameAction::MouseOverPixel(pixel));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        // Many items (e.g. geodes/artifacts) trigger a dialogue menu
        // when first picked up.  Close these menus when they occur.
        //
        // TODO: Distinguish between a fully-loaded menu that has no
        // responses (the popups) and a partially-loaded menu that
        // doesn't yet have a response (minecart menu).  Currently,
        // this code may prematurely close minecart menus while they
        // are in the process of opening.
        let cleanup = MenuCloser::new();
        if game_state
            .dialogue_menu()
            .map(|menu| menu.responses.is_empty())
            .unwrap_or(true)
            && !cleanup.is_completed(game_state)
        {
            return Ok(cleanup.into());
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

        if let Some(chair_jump) = self.choose_chair_jump(game_state)? {
            return Ok(chair_jump.prevent_interrupts().into());
        }

        let dir = if player_tile == target_tile {
            let offset = self.target_position - player.center_pos();
            offset.closest_direction()
        } else {
            let core_dir = map.get(player_tile).cloned().ok_or_else(|| {
                Error::PlayerIsOutOfBounds {
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

        if self.clear_breakables
            && player_tile != target_tile
            && let Some(core_dir) = map.get(player_tile).cloned()
            && let Some(weapon) = Self::preferred_weapon(game_state)
        {
            'breakable: {
                // We are allowed to clear out breakables, and have the
                // tools required to do so.
                let next_tile = player_tile + core_dir.offset();
                if self.breakable_objects.is_none() {
                    self.breakable_objects =
                        Some(self.collect_breakables(game_state)?);
                }
                let breakables =
                    self.breakable_objects.as_mut().expect("Just populated");

                if !breakables.contains(&next_tile) {
                    break 'breakable;
                }

                // Last time we checked, there was a breakable object
                // on the next tile, but it may have been destroyed
                // since then.  If it no longer exists, we can bail
                // out.
                let still_exists =
                    game_state.current_room()?.objects.iter().any(|obj| {
                        object_is_breakable(obj) && obj.tile == next_tile
                    });
                if !still_exists {
                    breakables.remove(&next_tile);
                    break 'breakable;
                }

                actions.do_action(GameAction::MouseOverTile(next_tile));

                let select_item = SelectItemGoal::new(weapon.id.clone());
                if !select_item.is_completed(game_state) {
                    return Ok(select_item.into());
                }

                if game_state.inputs.mouse_tile_location != next_tile {
                    // Wait for the mouse to be over the correct tile
                    // before starting the swing.
                    return Ok(BotGoalResult::InProgress);
                }

                return Ok(SwingWithAnimationCancel::new()
                    .prevent_interrupts()
                    .into());
            }
        }

        // Most of the time, keep the mouse hovering over the target
        // tile, to give a visual indication of where the bot is
        // moving.
        actions.do_action(GameAction::MouseOverTile(target_tile));

        let must_open_door = self.activate_endpoint()
            && player_position.chessboard_dist(self.target_position) < 1.5;

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

impl ChairJumpGoal {
    fn find_placed_chair(
        &self,
        game_state: &GameState,
    ) -> Result<Option<FacingDirection>, Error> {
        let location = game_state.current_room()?;
        let placed_chair: Option<FacingDirection> = location
            .furniture
            .iter()
            .filter(|furniture| furniture.shape.top_left == self.chair_tile)
            .find_map(|furniture| {
                if let FurnitureKind::Chair(facing) = furniture.kind {
                    Some(facing)
                } else {
                    None
                }
            });
        Ok(placed_chair)
    }

    fn find_held_chair<'a>(
        &self,
        game_state: &'a GameState,
    ) -> Option<(&'a ItemId, FacingDirection)> {
        game_state.player.inventory.iter_items().find_map(|item| {
            item.as_chair().map(|chair| (&item.id, chair.facing))
        })
    }

    /// Returns true if the player is too close to the chair's future
    /// location to place it down.
    ///
    /// The player is too close is their bounding box (0.75 x 0.5
    /// tile) overlaps with the chair's bounding box (1.0 x 1.0 tile).
    fn too_close_to_chair_tile(&self, game_state: &GameState) -> bool {
        let delta =
            game_state.player.center_pos() - self.chair_tile.map(|x| x as f32);
        delta.right.abs() <= 0.875 && delta.down.abs() <= 0.75
    }

    /// Returns true if the player is too far from the chair's future
    /// location to jump into it.
    fn too_far_from_chair_tile(&self, game_state: &GameState) -> bool {
        let delta =
            game_state.player.center_pos() - self.chair_tile.map(|x| x as f32);
        delta.right.abs() >= 1.0 || delta.down.abs() >= 1.0
    }

    fn is_allowed(&self, game_state: &GameState) -> Result<bool, Error> {
        let location = game_state.current_room()?;

        if self.too_far_from_chair_tile(game_state)
            || self.too_close_to_chair_tile(game_state)
        {
            return Ok(false);
        }

        let stand_up_tile = self.chair_tile + self.facing.offset();
        let jump_is_blocked = location
            .iter_obstructed_tiles(&game_state.statics)
            .chain(location.characters.iter().map(|character| character.tile()))
            .any(|blocked| {
                blocked == self.chair_tile || blocked == stand_up_tile
            });
        if jump_is_blocked {
            return Ok(false);
        }

        let chair_placement_allowed =
            location.no_furniture.is_unset(self.chair_tile);
        if !chair_placement_allowed {
            return Ok(false);
        }

        // TODO: Unpack the no-furniture map on buildings, as some
        // tiles do allow chair placement.  (The stairs on the
        // farmhouse prevent placement, but the top of the stairs
        // allows placement.)
        let overlaps_building = location
            .buildings
            .iter()
            .any(|building| building.shape.contains(self.chair_tile));
        if overlaps_building {
            return Ok(false);
        }

        let blocked_but_walkable_tile = location
            .objects
            .iter()
            .filter(|obj| obj.tile == self.chair_tile)
            .any(|obj| match &obj.kind {
                ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => true,
                ObjectKind::HoeDirt(dirt) => dirt.crop.is_some(),
                _ => false,
            });
        if blocked_but_walkable_tile {
            return Ok(false);
        }

        let too_close_to_warp = location
            .warps
            .iter()
            .filter(|warp| matches!(warp.kind, WarpKind::Automatic))
            .map(|warp| warp.location)
            .any(|tile| {
                self.chair_tile.chessboard_dist(tile) <= 1
                    || stand_up_tile.chessboard_dist(tile) <= 1
            });
        if too_close_to_warp {
            return Ok(false);
        }

        Ok(true)
    }
}

impl BotGoal for ChairJumpGoal {
    fn description(&self) -> Cow<'static, str> {
        format!("Chair jump over {} to {}", self.chair_tile, self.facing).into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let player = &game_state.player;
        let player_tile = player.tile();

        if player_tile.chessboard_dist(self.chair_tile) > 1 {
            actions.log("Too far from chair tile");
            // Something has gone wrong in the setup, so bail out and
            // let something else figure out what to do.
            return Ok(BotGoalResult::Completed);
        }

        if player.is_getting_up {
            // Already started the stand-up animation, now need to
            // wait for it to complete.
            actions.log("Waiting for stand-up animation to complete");
            return Ok(BotGoalResult::InProgress);
        }

        if player.is_sitting {
            // Currently sitting down, so whatever else we do
            // afterwards, the first step is to stand up.  Uses a
            // left-click instead of a right-click, so that any
            // accidental double clicks would result in picking the
            // chair up (the intended next step), rather than sitting
            // back down.
            actions.log("Standing up");
            actions.do_action(GameAction::MouseOverTile(player_tile));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        let placed_chair = self.find_placed_chair(game_state)?;

        let goal_tile = self.chair_tile + self.facing.offset();

        if let Some(facing) = placed_chair
            && facing == self.facing
            && player_tile != goal_tile
        {
            // The chair is in place, facing the correct direction,
            // and we have not yet completed the chair jump.
            // Therefore, sit on the chair.
            actions.log(format!(
                "Sitting down (player at {player_tile}, not {goal_tile}"
            ));
            actions.do_action(GameAction::MouseOverTile(self.chair_tile));
            actions.do_action(GameAction::RightClick);
            return Ok(BotGoalResult::InProgress);
        }

        if placed_chair.is_some() {
            // The chair is placed down, but we aren't going to sit
            // down in it.  This can either occur because we have
            // already stood up from the chair, or because it happens
            // to be placed in the wrong orientation.  Either way, we
            // should pick up the chair.
            actions.log("Picking up chair");
            actions.do_action(GameAction::MouseOverTile(self.chair_tile));
            actions.do_action(GameAction::LeftClick);
            return Ok(BotGoalResult::InProgress);
        }

        if player_tile == goal_tile {
            actions.log("Successfully completed chair jump");
            // We've reached the goal tile, and the chair is no longer
            // placed down, so the chair jump is completed.
            return Ok(BotGoalResult::Completed);
        }

        let opt_chair_item = self.find_held_chair(game_state);
        let Some((chair_item_id, chair_facing)) = opt_chair_item else {
            if let Some(tick) = self.chair_placement_tick
                && game_state.globals.game_tick < tick + 2
            {
                actions.log("Waiting for chair to appear in GameLocation");
                // The chair was just placed down, but the most recent
                // memory read didn't find it in the GameLocation.
                // Wait a few game ticks to see if it shows up.
                return Ok(BotGoalResult::InProgress);
            } else {
                actions.log("Bailout, no chair with which to chair jump");
                // The chair is neither placed down, nor in the inventory,
                // so there's not much we can do at this point.  Can't
                // really chair jump without a chair, after all.
                return Ok(BotGoalResult::Completed);
            }
        };

        if self.too_close_to_chair_tile(game_state) {
            actions.log("Bailout, too close to chair tile");
            return Ok(BotGoalResult::Completed);
        }

        if self.too_far_from_chair_tile(game_state) {
            actions.log("Bailout, too far from chair tile");
            return Ok(BotGoalResult::Completed);
        }

        let select_chair = SelectItemGoal::new(chair_item_id.clone());
        if !select_chair.is_completed(game_state) {
            actions.log("Selecting chair from inventory");
            return Ok(select_chair.into());
        }

        actions.do_action(GameAction::MouseOverTile(self.chair_tile));

        // The chair is selected, with the mouse in the correct
        // location to place it down.  We should either rotate it
        // (right click) or place it down (left click), depending on
        // whether it is facing the correct direction for the jump.
        if chair_facing == self.facing {
            if game_state.inputs.mouse_tile_location == self.chair_tile {
                actions.log("Placing down chair");
                self.chair_placement_tick = Some(game_state.globals.game_tick);
                actions.do_action(GameAction::LeftClick);
            } else {
                actions.log("Waiting for mouse movement to be applied");
            }
            return Ok(BotGoalResult::InProgress);
        }

        // Some resource clumps, such as the stones on the farm,
        // can trigger a popup message on a right-click.  This
        // depends on whether the player is facing the resource
        // clump, and can take priority over furniture rotation.
        //
        // I've seen this occur when aligned with the upper row of
        // the stones, facing left or right, and attempting to
        // rotate a chair being placed just above the stone.
        //
        //         Setup 1                  Setup 2
        //
        //            C                        C
        //          P S S                    S S P
        //            S S                    S S
        //
        // For now, avoid rotating a chair while facing any
        // resource clump.
        let mut has_clump = [false; 4];
        game_state
            .current_room()?
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points())
            .for_each(|tile| {
                has_clump
                    .iter_mut()
                    .zip(FacingDirection::VALUES)
                    .filter(|(_, dir)| player_tile + dir.offset() == tile)
                    .for_each(|(b, _)| {
                        *b = true;
                    });
            });
        let player_facing_clump = FacingDirection::VALUES
            .iter()
            .position(|dir| *dir == player.facing)
            .map(|i| has_clump[i])
            .expect("Player must be facing a valid direction");
        if player_facing_clump {
            let safe_dir = FacingDirection::VALUES
                .into_iter()
                .zip(has_clump)
                .find(|(_, b)| !b)
                .map(|(dir, _)| dir)
                .expect(
                    "Player somehow surrounded \
                     on all four sides by resource clumps",
                );
            return Ok(FaceDirectionGoal(safe_dir).into());
        }

        // We have the chair, but cannot set it down yet because it is
        // facing the wrong direction.  Right-click in order to rotate
        // the chair until it is facing the desired direction for this
        // jump.
        actions.log("Rotating chair before placing down");
        actions.do_action(GameAction::RightClick);

        Ok(BotGoalResult::InProgress)
    }
}
