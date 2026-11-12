use geometry::{Direction, TileSet, Vector};
use itertools::Itertools as _;

use crate::Error;
use game_state::{
    FacingDirection, Location, ObjectKind, ResourceClumpKind, StaticState,
    TileMap, WarpKind,
};

#[derive(Debug, Clone)]
pub struct Pathfinding<'a> {
    statics: &'a StaticState,
    location: &'a Location,

    /// The allowed movement types for the player to make.
    kind: MovementKind,

    /// If true, results will allow the final tile of a path to be on
    /// an unreachable tile.  If false, the final tile of a path must
    /// not be blocked.
    ///
    /// Including the border is useful when identifying tiles on which
    /// a tool may be used, where the player only needs to stand next
    /// to the final tile.  Excluding the border is useful when
    /// identifying tiles on which the player may stand.
    include_border: bool,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// small stone debris, with an additional penalty as specified.
    /// If `None`, do not allow walking through tiles that contain
    /// small stones.
    clear_stone: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain 2x2
    /// boulders on the farm, with an additional penalty as specified.
    /// If `None`, do not allow walking through tiles that contain 2x2
    /// boulders.
    ///
    /// This only applies to boulders outside of the mines, in the
    /// mines, which requires an iron pickaxe.  See
    /// `clear_mine_boulders` for clearing of boulders on the farm,
    /// which can be broken using a regular pickaxe.
    clear_boulders: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain 2x2
    /// boulders in the mines, with an additional penalty as
    /// specified.  If `None`, do not allow walking through tiles that
    /// contain 2x2 boulders.
    ///
    /// This only applies to boulders in the mines, which can be
    /// broken using a regular pickaxe.  See `clear_boulders` for
    /// clearing of boulders on the farm, which requires an iron
    /// pickaxe.
    clear_mine_boulders: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain wood
    /// debris/twigs, with an additional penalty as specified.  If
    /// `None`, do not allow walking through tiles that contain wood
    /// debris/twigs.
    clear_wood: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain 2x2
    /// stumps, with an additional penalty as specified.  If `None`,
    /// do not allow walking through tiles that contain 2x2 stumps.
    clear_stumps: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// breakable objects (fiber/weeds on the farm, barrels in the
    /// mines), with an additional penalty as specified.  If `None`,
    /// do not allow walking through tiles that contain breakable
    /// objects.
    clear_breakables: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// forageable objects (flowers/leeks/etc), with an additional
    /// penalty as specified.  If `None`, do not allow walking through
    /// tiles that contain forageable objects.
    clear_forage: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// trees, with an additional penalty as specified.  If `None`, do
    /// not allow walking through tiles that contain trees.
    clear_trees: Option<u64>,

    /// The additional cost associated with moving through a tile that
    /// contains grass.
    grass_penalty: u64,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// craftable objects (e.g. scarecrows, sprinklers), with an
    /// additional penalty as specified.  If `None`, do not allow
    /// walking through tiles that contain craftable objects.
    clear_craftables: Option<u64>,

    /// If true, treat NPCs as obstacles to be avoided.  If false,
    /// ignore NPCs while pathfinding.
    avoid_npcs: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum MovementKind {
    /// Only allow movement by in cardinal directions.
    Cardinal,

    /// Allow movement along unblocked diagonals.  This requires both
    /// off-diagonal tiles to be clear, as the player's bounding box
    /// will intersect with those tiles during the movement.  For
    /// example, to move from NW to SE, the tiles to NE and SW must be
    /// clear.
    UnblockedDiagonals,

    /// Allow movement by walking along unblocked diagonals, and by
    /// performing chair jumps.
    ChairJump,
}

#[derive(Debug, Clone)]
pub struct MovementCost {
    /// Additional traversal costs for the tile.  Should be relative
    /// to the cost of walking across a single tile, which is defined
    /// to be 1000.
    ///
    /// If `None`, the tile will be used for the path.
    pub map: TileMap<Option<u64>>,

    /// True if a chair can be placed down on the tile, false
    /// otherwise.
    pub chairable: TileMap<bool>,

    /// True if the tile can be walked on.  Used to determine
    /// valid exit locations when standing up from a chair.
    pub walkable: TileMap<bool>,

    /// The allowed movement types for the player to make.
    pub kind: MovementKind,

    /// If true, results will allow the final tile of a path to be on
    /// an unreachable tile.  If false, the final tile of a path must
    /// not be blocked.
    ///
    /// Including the border is useful when identifying tiles on which
    /// a tool may be used, where the player only needs to stand next
    /// to the final tile.  Excluding the border is useful when
    /// identifying tiles on which the player may stand.
    pub include_border: bool,
}

/// Movement that may be taken by the bot as part of pathfinding
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PathStep {
    /// The chair jump that should be performed.  This is the
    /// preferred method of movement, as in-game time doesn't pass
    /// when sitting down or standing up.
    pub chair_jump: Option<ChairJump>,

    /// The direction that may be walked.  This is a backup, either if
    /// a chair jump cannot be performed (e.g. not at correct location
    /// within tile) or if no chair jump is available for that tile
    /// (e.g. cannot place chair at any nearby location).
    pub walk_dir: Direction,
}

/// A chair-jump that should be performed as part of moving between
/// two locations.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChairJump {
    /// The tile on which the chair should be placed.  Must be
    /// currently open, and adjacent to the tile the player is on.
    pub chair_tile: Vector<isize>,

    /// The direction the chair should be facing when placed.
    pub facing: FacingDirection,
}

/// Tracking for points during A* search.
///
/// Implements `Eq` and `Ord`, with comparisons based solely on the
/// `dist_plus_heuristic` and `tile` fields.  This allows them to be
/// used as entries in a `std::collection::BTreeSet`, ordered by
/// priority, with the tile's location as a tie-breaker.
struct AStarEntry {
    /// The distance from the initial point to the current tile, plus
    /// the heurstic from the current tile to the goal tile.
    dist_plus_heuristic: u64,
    tile: Vector<isize>,
    dist: u64,
    dir: Option<Direction>,
    should_propagate: bool,
}

/// Tracking for points during Dijkstra search.
///
/// Implements `Eq` and `Ord`, with comparisons based solely on the
/// `dist_plus_heuristic` and `tile` fields.  This allows them to be
/// used as entries in a `std::collection::BTreeSet`, ordered by
/// priority, with the tile's location as a tie-breaker.
#[derive(Debug, PartialEq, Eq)]
struct DijkstraEntry {
    /// The tile
    tile: Vector<isize>,

    /// The distance from the initial point to the current tile.
    dist: u64,

    /// The step taken to reach this tile.  This is always the step
    /// that the player would need to take, regardless of whether
    /// using forward iteration starting at the player's location, or
    /// using backward iteration starting at the goal location.
    step: Option<PathStep>,

    /// If true, then the player can stand on this tile, and so paths
    /// may contain this as an intermediate.  If false, then the
    /// player cannot stand on this tile.  This may only be true when
    /// `include_border` is set, as that allows the search to
    /// terminate at a tile that the player may interact with, but may
    /// not stand on.
    should_propagate: bool,
}

/// The direction in which movement options should be searched.
///
/// If walking between two points, forward searches (player to goal)
/// and reverse searches (goal to player) are equivalent.  However,
/// chair jumps allow movement in both cardinal and diagonal
/// directions when sitting down, but only allow movement in cardinal
/// directions when standing up.  Therefore, the iteration must be
/// aware of whether the search is starting from the player or
/// starting from the goal.
pub enum SearchDirection {
    /// Iterate forward, from the player's position to a goal location.
    Forward,

    /// Iterate backwards, from a goal location toward the player's location.
    Reverse,
}

/// Internal implementation, provide a lower bound on the distance
/// between two tiles.
fn heuristic_between_points(a: Vector<isize>, b: Vector<isize>) -> u64 {
    let offset = (a - b).map(|x| x.unsigned_abs() as u64);

    let min = offset.right.min(offset.down);
    let max = offset.right.max(offset.down);

    let diagonal_movements = min;
    let cardinal_movements = max - min;
    1414 * diagonal_movements + 1000 * cardinal_movements
}

impl<'a> Pathfinding<'a> {
    pub fn new(location: &'a Location, statics: &'a StaticState) -> Self {
        Pathfinding {
            location,
            statics,
            kind: MovementKind::UnblockedDiagonals,
            clear_stone: None,
            clear_boulders: None,
            clear_mine_boulders: None,
            clear_wood: None,
            clear_stumps: None,
            clear_breakables: None,
            clear_forage: None,
            clear_trees: None,
            grass_penalty: 100,
            clear_craftables: None,
            include_border: false,
            avoid_npcs: true,
        }
    }

    pub fn forbid_diagonals(self) -> Self {
        Self {
            kind: MovementKind::Cardinal,
            ..self
        }
    }

    pub fn allow_chair_jump(self) -> Self {
        Self {
            kind: MovementKind::ChairJump,
            ..self
        }
    }

    pub fn stone_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_stone: Some(cost),
            ..self
        }
    }
    pub fn mine_boulder_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_mine_boulders: Some(cost),
            ..self
        }
    }
    pub fn wood_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_wood: Some(cost),
            ..self
        }
    }
    #[allow(dead_code)]
    pub fn stump_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_stumps: Some(cost),
            ..self
        }
    }
    pub fn clear_breakables(self, clear_breakables: bool) -> Self {
        Self {
            clear_breakables: clear_breakables.then(|| 0),
            ..self
        }
    }
    pub fn breakable_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_breakables: Some(cost),
            ..self
        }
    }
    #[allow(dead_code)]
    pub fn forage_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_forage: Some(cost),
            ..self
        }
    }
    pub fn tree_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_trees: Some(cost),
            ..self
        }
    }
    pub fn do_not_clear_trees(self) -> Self {
        Self {
            clear_trees: None,
            ..self
        }
    }

    pub fn grass_movement_cost(self, grass_penalty: u64) -> Self {
        Self {
            grass_penalty,
            ..self
        }
    }

    pub fn craftable_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_craftables: Some(cost),
            ..self
        }
    }

    pub fn ignoring_small_obstacles(self) -> Self {
        Self {
            clear_stone: Some(0),
            clear_wood: Some(0),
            clear_breakables: Some(0),
            clear_forage: Some(0),
            clear_trees: Some(0),
            clear_craftables: Some(0),
            grass_penalty: 0,
            ..self
        }
    }

    pub fn ignoring_obstacles(self) -> Self {
        Self {
            clear_boulders: Some(0),
            clear_mine_boulders: Some(0),
            clear_wood: Some(0),
            clear_stumps: Some(0),
            ..self.ignoring_small_obstacles()
        }
    }

    pub fn avoid_npcs(self, avoid_npcs: bool) -> Self {
        Self { avoid_npcs, ..self }
    }

    pub fn include_border(self, include_border: bool) -> Self {
        Self {
            include_border,
            ..self
        }
    }

    /// An iterator of (tile,movement_cost)
    ///
    /// Each element specifies a tile, and an update to the movement
    /// cost required for passing through a tile.  If any update is
    /// `None`, then that tile is impassable.
    fn movement_cost_component(
        &self,
    ) -> impl Iterator<Item = (Vector<isize>, Option<u64>)> + '_ {
        let loc = &self.location;

        // Handle tiles that are not passable, even with an additional
        // movement cost.
        let iter_blocked = loc
            .blocked
            .iter()
            .filter(|(_, b)| **b)
            .map(|(tile, _)| tile);

        let iter_bush = loc.iter_bush_tiles();

        let iter_furniture = loc
            .furniture
            .iter()
            .filter(|piece| !piece.is_walkable())
            .flat_map(|piece| piece.shape.iter_points());

        let iter_buildings = loc.iter_building_tiles();

        let iter_npcs = loc
            .characters
            .iter()
            .chain(
                loc.current_event
                    .iter()
                    .flat_map(|event| event.characters.iter()),
            )
            .filter(|_| self.avoid_npcs)
            .filter(|character| !character.is_monster())
            .map(|character| character.tile());

        let iter_unwalkable = std::iter::empty()
            .chain(iter_blocked)
            .chain(iter_bush)
            .chain(iter_furniture)
            .chain(iter_buildings)
            .chain(iter_npcs)
            .map(|tile| (tile, None));

        // Handle tiles that may be passable, but with an additional
        // movement cost.
        let iter_clumps = loc.resource_clumps.iter().flat_map(|clump| {
            let opt_cost = match &clump.kind {
                ResourceClumpKind::Stump => self.clear_stumps,
                ResourceClumpKind::Boulder => self.clear_boulders,
                ResourceClumpKind::MineBoulder => self.clear_mine_boulders,
                _ => None,
            };
            clump.shape.iter_points().map(move |tile| (tile, opt_cost))
        });

        let iter_obj = loc.objects.iter().filter_map(|obj| {
            let opt_cost = match &obj.kind {
                ObjectKind::Stone(_) => self.clear_stone,
                ObjectKind::Wood => self.clear_wood,
                ObjectKind::Fiber | ObjectKind::MineBarrel => {
                    self.clear_breakables
                }
                ObjectKind::Tree(tree) if tree.growth_stage > 0 => {
                    self.clear_trees
                }
                ObjectKind::Grass => Some(self.grass_penalty),

                ObjectKind::CraftingMachine(_)
                | ObjectKind::Sprinkler(_)
                | ObjectKind::Scarecrow => self.clear_craftables,

                other if other.is_walkable(self.statics) => {
                    // Can walk on this tile without penalty (e.g. a
                    // rug on the floor)
                    return None;
                }
                other if other.is_forage() => self.clear_forage,
                _ => None,
            };

            Some((obj.tile, opt_cost))
        });

        std::iter::empty()
            .chain(iter_unwalkable)
            .chain(iter_clumps)
            .chain(iter_obj)
    }

    fn movement_cost(&self) -> MovementCost {
        let map = self.raw_movement_cost();

        let mut walkable = self.location.blocked.map(|&b| !b);
        self.location
            .iter_obstructed_tiles(&self.statics)
            .chain(
                self.location
                    .warps
                    .iter()
                    .filter(|warp| matches!(warp.kind, WarpKind::Automatic))
                    .map(|warp| warp.location)
                    .flat_map(|tile| {
                        Direction::VALUES
                            .into_iter()
                            .map(|dir| dir.offset())
                            .chain([Vector::zero()])
                            .map(move |offset| tile + offset)
                    }),
            )
            .for_each(|tile| {
                if let Some(allows_stand_up) = walkable.get_mut(tile) {
                    *allows_stand_up = false;
                }
            });

        let mut chairable = walkable.clone();
        std::iter::empty()
            .chain(self.location.no_furniture.iter_true())
            .chain(self.location.buildings.iter().flat_map(|building| {
                let below_farmhouse_door = (building.kind == "Farmhouse")
                    .then(|| building.shape.top_left + Vector::new(5, 5));
                building.shape.iter_points().chain(below_farmhouse_door)
            }))
            .chain(
                self.location
                    .objects
                    .iter()
                    .filter(|obj| match &obj.kind {
                        ObjectKind::ArtifactSpot | ObjectKind::SeedSpot => true,
                        ObjectKind::HoeDirt(dirt) => dirt.crop.is_some(),
                        _ => false,
                    })
                    .map(|obj| obj.tile),
            )
            .for_each(|tile| {
                if let Some(allows_chair) = chairable.get_mut(tile) {
                    *allows_chair = false;
                }
            });

        MovementCost {
            map,
            kind: self.kind,
            include_border: self.include_border,
            walkable,
            chairable,
        }
    }

    fn raw_movement_cost(&self) -> TileMap<Option<u64>> {
        // Collect all the tile costs into a single TileMap
        let mut map = self.location.blocked.map(|_| Some(0));

        self.movement_cost_component().for_each(|(tile, opt_cost)| {
            if map.in_bounds(tile) {
                map[tile] = match (map[tile], opt_cost) {
                    (None, _) | (_, None) => None,
                    (Some(a), Some(b)) => {
                        // Add the two costs together, as some objects can
                        // coexist on the same tile.  For example, a stone
                        // that has grass underneath it.
                        Some(a + b)
                    }
                }
            };
        });

        map
    }

    /// Check if a single tile is walkable.  If checking several
    /// tiles, may be faster to use `Pathfinding.walkable` instead.
    #[allow(dead_code)]
    pub fn tile_is_walkable(&self, tile: Vector<isize>) -> bool {
        self.location.blocked.in_bounds(tile)
            && self
                .movement_cost_component()
                .filter(|(component_tile, _)| component_tile == &tile)
                .all(|(_, opt_cost)| opt_cost.is_some())
    }

    pub fn walkable(&self) -> TileMap<bool> {
        self.movement_cost().walkable()
    }

    pub fn clear(&self) -> TileMap<bool> {
        self.movement_cost().clear()
    }

    pub fn reachable(&self, initial: Vector<isize>) -> TileMap<bool> {
        self.movement_cost().reachable(initial)
    }

    pub fn distances_forward(
        &self,
        initial: impl TileSet,
    ) -> TileMap<Option<u64>> {
        self.distances(initial, SearchDirection::Forward)
    }

    pub fn distances_reverse(
        &self,
        goal: impl TileSet,
    ) -> TileMap<Option<u64>> {
        self.distances(goal, SearchDirection::Reverse)
    }

    fn distances(
        &self,
        tiles: impl TileSet,
        search_dir: SearchDirection,
    ) -> TileMap<Option<u64>> {
        let mut distances = self.location.blocked.map(|_| None);

        self.movement_cost()
            .iter_dijkstra(tiles, search_dir)
            .for_each(|(tile, dist)| {
                assert!(distances[tile].is_none());
                distances[tile] = Some(dist);
            });

        distances
    }

    pub fn scaled_distances(
        &self,
        initial: impl TileSet,
    ) -> TileMap<Option<u64>> {
        let mut distances = self.location.blocked.map(|_| None);

        self.iter_dijkstra_entries(initial, SearchDirection::Forward)
            .for_each(|entry| {
                assert!(distances[entry.tile].is_none());
                distances[entry.tile] = Some(entry.dist);
            });

        distances
    }

    /// Given a target location, return the direction to travel from
    ///
    /// Interrupts are often location-dependent, such as triggering
    /// when the player is within range of a forageable item, and so
    /// `LocalMovementGoal` is often interrupted.  When resumed, the
    /// player may be in a different location than when interrupted,
    /// so any waypoint-based method may have out-of-date waypoints.
    /// Using a direction map allows the local pathfinding to resume
    /// without interruption when this occurs.
    pub fn direction_map(&self, target: impl TileSet) -> TileMap<PathStep> {
        let mut map: TileMap<Option<PathStep>> =
            self.location.blocked.map(|_| None);

        for entry in
            self.iter_dijkstra_entries(target, SearchDirection::Reverse)
        {
            if let Some(step) = entry.step {
                map[entry.tile] = Some(step);
            }
        }

        map.imap(|tile, opt_step| {
            opt_step.unwrap_or_else(|| {
                let offset = target
                    .iter()
                    .map(|target_tile| target_tile - tile)
                    .min_by_key(|offset| offset.mag2())
                    .expect("Must have at least one target tile");
                PathStep {
                    walk_dir: offset.closest_direction(),
                    chair_jump: None,
                }
            })
        })
    }

    pub fn path_between(
        &self,
        initial: Vector<isize>,
        goal: impl TileSet,
    ) -> Result<Vec<Vector<isize>>, Error> {
        self.movement_cost()
            .path_between(initial, goal, &self.location.name)
    }

    fn iter_dijkstra_entries(
        &self,
        initial: impl TileSet,
        search_dir: SearchDirection,
    ) -> impl Iterator<Item = DijkstraEntry> {
        self.movement_cost()
            .iter_dijkstra_entries(initial, search_dir)
    }

    /// Starting from the specified tiles, iterate through all
    /// reachable tiles.
    ///
    /// This should be used when starting at the player's location and
    /// searching forward.  For starting at the goal location, use
    /// `iter_dijkstra_reverse`.
    pub fn iter_dijkstra_forward<'b, 'c, 'd>(
        &'b self,
        initial: impl TileSet,
    ) -> impl Iterator<Item = (Vector<isize>, u64)> {
        self.movement_cost()
            .iter_dijkstra(initial, SearchDirection::Forward)
    }

    /// Starting from the specified tiles, iterate through all
    /// reachable tiles.
    ///
    /// This should be used when starting at the goal location and
    /// searching backward toward the player.  For starting at the
    /// player location, use `iter_dijkstra_forward`.
    pub fn iter_dijkstra_reverse<'b, 'c, 'd>(
        &'b self,
        goal: impl TileSet,
    ) -> impl Iterator<Item = (Vector<isize>, u64)> {
        self.movement_cost()
            .iter_dijkstra(goal, SearchDirection::Reverse)
    }
}

impl MovementCost {
    pub fn walkable(&self) -> TileMap<bool> {
        self.map.map(|opt_cost| opt_cost.is_some())
    }

    pub fn clear(&self) -> TileMap<bool> {
        self.map.map(|opt_cost| opt_cost == &Some(0))
    }

    pub fn reachable(&self, initial: Vector<isize>) -> TileMap<bool> {
        let walkable = self.walkable();

        let mut reachable = walkable.map(|_| false);
        if reachable.in_bounds(initial) {
            reachable[initial] = true;
        }

        let mut to_visit = vec![initial];
        while let Some(tile) = to_visit.pop() {
            for dir in Direction::iter_cardinal() {
                let adj = tile + dir.offset();
                if reachable.in_bounds(adj) && !reachable[adj] {
                    if walkable[adj] {
                        // The player may walk through this tile.
                        reachable[adj] = true;
                        to_visit.push(adj);
                    } else if self.include_border {
                        // The player may interact with this tile, but
                        // may not walk through it.
                        reachable[adj] = true;
                    }
                }
            }
        }

        reachable
    }

    fn path_between(
        &self,
        initial: Vector<isize>,
        goal: impl TileSet,
        room_name: &str,
    ) -> Result<Vec<Vector<isize>>, Error> {
        let cost_map = &self.map;

        let mut best: TileMap<Option<(u64, Option<Direction>)>> =
            TileMap::empty(cost_map.width() + 2, cost_map.height() + 2);

        let get_heuristic = |tile: Vector<isize>| -> u64 {
            goal.iter()
                .map(|goal_tile| heuristic_between_points(goal_tile, tile))
                .min()
                .unwrap()
        };

        let mut to_visit = std::collections::BTreeSet::<AStarEntry>::new();
        to_visit.insert(AStarEntry {
            dist_plus_heuristic: get_heuristic(initial),
            tile: initial,
            dist: 0,
            dir: None,
            should_propagate: true,
        });
        // Offset between the actual tile position and the tile
        // position in the `best` map, because often the goal tile map
        // be one unit outside of the map boundary (e.g. Warp
        // locations to adjacent screens).
        let best_tile_offset = Vector::<isize>::new(1, 1);

        let mut opt_final_tile: Option<Vector<isize>> = None;

        while let Some(visiting) = to_visit.pop_first() {
            assert!(
                best.in_bounds(visiting.tile + best_tile_offset),
                "Tile {} is out of bounds for location '{}'",
                visiting.tile,
                room_name,
            );
            let is_best = best[visiting.tile + best_tile_offset]
                .map(|(prev, _)| visiting.dist_plus_heuristic <= prev)
                .unwrap_or(true);
            if !is_best {
                continue;
            }

            let tile = visiting.tile;
            best[tile + best_tile_offset] =
                Some((visiting.dist_plus_heuristic, visiting.dir));

            let tolerance = if visiting.should_propagate { 1 } else { 0 };
            if goal.iter().any(|goal_tile| {
                visiting.tile.chessboard_dist(goal_tile) <= tolerance
            }) {
                opt_final_tile = Some(visiting.tile);
                break;
            }

            let iter_dir = Direction::iter()
                .filter(|_| visiting.should_propagate)
                .filter(|dir| match self.kind {
                    MovementKind::Cardinal => dir.is_cardinal(),
                    _ => true,
                });

            for dir in iter_dir {
                let offset = dir.offset();
                let new_tile = tile + offset;

                let is_walkable = cost_map.is_some(new_tile);
                let is_valid_path = if dir.is_cardinal() {
                    is_walkable
                } else {
                    is_walkable
                        && cost_map.is_some(tile + Vector::new(0, offset.down))
                        && cost_map.is_some(tile + Vector::new(offset.right, 0))
                };

                if !(is_valid_path || (self.include_border && !is_walkable)) {
                    // The player may not move to this tile, and we
                    // are looking for locations where the player may
                    // stand, not places where the player can reach
                    // with a tool.  Therefore, no further processing
                    // required for this Direction.
                    continue;
                }

                let tile_dist = if dir.is_cardinal() { 1000 } else { 1414 };
                let additional_cost =
                    cost_map.get(new_tile).copied().flatten().unwrap_or(0);

                let new_dist = visiting.dist + tile_dist + additional_cost;
                let new_heuristic = get_heuristic(new_tile);
                let new_dist_plus_heuristic = new_dist + new_heuristic;

                let is_best = best[new_tile + best_tile_offset]
                    .map(|(prev, _)| new_dist_plus_heuristic < prev)
                    .unwrap_or(true);

                if is_best {
                    // This is the best known route to access the new tile.
                    best[new_tile + best_tile_offset] =
                        Some((new_dist_plus_heuristic, Some(dir)));

                    to_visit.insert(AStarEntry {
                        dist_plus_heuristic: new_dist_plus_heuristic,
                        tile: new_tile,
                        dist: new_dist,
                        dir: Some(dir),
                        should_propagate: is_valid_path,
                    });
                }
            }
        }

        let final_tile =
            opt_final_tile.ok_or_else(|| Error::NoRouteToTargets {
                room: room_name.to_string(),
                start: initial,
                goals: goal.iter().collect(),
            })?;

        let mut path: Vec<_> =
            std::iter::successors(Some(final_tile), |&path_tile| {
                let dir = best[path_tile + best_tile_offset]?.1?;
                Some(path_tile - dir.offset())
            })
            .collect();

        path.reverse();

        Ok(path)
    }

    fn iter_dijkstra_entries(
        self,
        initial_or_goal: impl TileSet,
        search_dir: SearchDirection,
    ) -> impl Iterator<Item = DijkstraEntry> {
        let mut finished: TileMap<bool> = self.map.map(|_| false);

        let mut to_visit = std::collections::BTreeSet::<DijkstraEntry>::new();
        for tile in initial_or_goal.iter() {
            to_visit.insert(DijkstraEntry {
                dist: 0,
                tile,
                step: None,
                should_propagate: true,
            });
        }

        std::iter::from_fn(move || -> Option<DijkstraEntry> {
            loop {
                let entry = to_visit.pop_first()?;

                if finished.in_bounds(entry.tile) {
                    if finished[entry.tile] {
                        continue;
                    } else {
                        finished[entry.tile] = true;
                    }
                }

                let iter_walking = Direction::iter()
                    .filter(|dir| match self.kind {
                        MovementKind::Cardinal => dir.is_cardinal(),
                        _ => true,
                    })
                    .filter(|_| entry.should_propagate)
                    .filter(|dir| finished.is_unset(entry.tile + dir.offset()))
                    .filter(|dir| {
                        let offset = dir.offset();

                        let is_walkable = self.map.is_some(entry.tile + offset);
                        let is_accessible = is_walkable
                            && (dir.is_cardinal()
                                || (self.map.is_some(
                                    entry.tile + Vector::new(0, offset.down),
                                ) && self.map.is_some(
                                    entry.tile + Vector::new(offset.right, 0),
                                )));
                        is_accessible || (self.include_border && !is_walkable)
                    })
                    .map(|dir| {
                        let new_tile = entry.tile + dir.offset();

                        let tile_dist =
                            if dir.is_cardinal() { 1000 } else { 1414 };
                        let additional_cost =
                            self.map.get_opt(new_tile).cloned().unwrap_or(0);
                        let new_dist = entry.dist + tile_dist + additional_cost;

                        let step = PathStep {
                            chair_jump: None,
                            walk_dir: match search_dir {
                                SearchDirection::Forward => dir,
                                SearchDirection::Reverse => dir.opposite(),
                            },
                        };

                        DijkstraEntry {
                            dist: new_dist,
                            tile: new_tile,
                            step: Some(step),
                            should_propagate: self.map.is_some(new_tile),
                        }
                    });

                let iter_chair_jump = Direction::VALUES
                    .into_iter()
                    .cartesian_product(FacingDirection::VALUES)
                    .filter(|_| matches!(self.kind, MovementKind::ChairJump))
                    .filter(|(sit_down_dir, stand_up_dir)| {
                        sit_down_dir.opposite() != *stand_up_dir
                    })
                    .map(|(sit_down_dir, stand_up_dir)| {
                        let (chair_tile, next_tile) = match search_dir {
                            SearchDirection::Forward => {
                                let initial_tile = entry.tile;
                                let chair_tile =
                                    initial_tile + sit_down_dir.offset();
                                let stand_up_tile =
                                    chair_tile + stand_up_dir.offset();
                                (chair_tile, stand_up_tile)
                            }
                            SearchDirection::Reverse => {
                                let stand_up_tile = entry.tile;
                                let chair_tile =
                                    stand_up_tile - stand_up_dir.offset();
                                let initial_tile =
                                    chair_tile - sit_down_dir.offset();
                                (chair_tile, initial_tile)
                            }
                        };
                        let chair_jump = ChairJump {
                            chair_tile,
                            facing: stand_up_dir,
                        };
                        let step = PathStep {
                            chair_jump: Some(chair_jump),
                            walk_dir: sit_down_dir,
                        };
                        let tile_dist = match sit_down_dir {
                            Direction::North
                            | Direction::East
                            | Direction::South
                            | Direction::West => {
                                // Best case scenario, can just move
                                // in a straight line.
                                100
                            }

                            Direction::NorthWest | Direction::NorthEast => {
                                // When jumping out of a chair, the
                                // player is centered in the stand-up
                                // tile along the X axis, but is along
                                // the top of the tile in Y.
                                // Therefore, can immediately start a
                                // NW/NE chair jump without any
                                // movement in-between.
                                //
                                // But it would be silly to going left
                                // and right, so giving it just a
                                // little deprioritization relative to
                                // cardinal directions.
                                105
                            }
                            Direction::SouthWest | Direction::SouthEast => {
                                // However, a chair-jump going SW/SE
                                // requires first walking in that
                                // direction.  Therefore, to avoid
                                // these unless extra useful.
                                201
                            }
                        };

                        let new_dist = entry.dist + tile_dist;
                        DijkstraEntry {
                            tile: next_tile,
                            dist: new_dist,
                            step: Some(step),
                            should_propagate: true,
                        }
                    })
                    .filter(|next| self.map.is_some(next.tile))
                    .filter(|next| {
                        let chair_jump = next
                            .step
                            .and_then(|step| step.chair_jump)
                            .expect("Constructed as chair jump");

                        self.walkable.is_set(
                            chair_jump.chair_tile + chair_jump.facing.offset(),
                        ) && self.chairable.is_set(chair_jump.chair_tile)
                    });

                std::iter::empty()
                    .chain(iter_walking)
                    .chain(iter_chair_jump)
                    .for_each(|entry| {
                        to_visit.insert(entry);
                    });

                if !finished.in_bounds(entry.tile) {
                    continue;
                }

                break Some(entry);
            }
        })
    }

    pub fn iter_dijkstra(
        self,
        tiles: impl TileSet,
        iter_dir: SearchDirection,
    ) -> impl Iterator<Item = (Vector<isize>, u64)> {
        self.iter_dijkstra_entries(tiles, iter_dir)
            .map(|entry| (entry.tile, entry.dist / 1000))
    }
}

impl std::cmp::PartialEq for AStarEntry {
    fn eq(&self, other: &Self) -> bool {
        self.dist_plus_heuristic == other.dist_plus_heuristic
            && self.tile == other.tile
    }
}
impl std::cmp::Eq for AStarEntry {}

impl std::cmp::PartialOrd for AStarEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for AStarEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.dist_plus_heuristic
            .cmp(&other.dist_plus_heuristic)
            .then_with(|| self.tile.right.cmp(&other.tile.right))
            .then_with(|| self.tile.down.cmp(&other.tile.down))
    }
}

impl std::cmp::PartialOrd for DijkstraEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl std::cmp::Ord for DijkstraEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.dist
            .cmp(&other.dist)
            .then_with(|| self.tile.right.cmp(&other.tile.right))
            .then_with(|| self.tile.down.cmp(&other.tile.down))
    }
}
