use itertools::Itertools as _;

use crate::{
    bot_logic::BotError,
    game_state::{Location, ObjectKind, ResourceClumpKind, TileMap, Vector},
    Direction, Error,
};

use std::collections::BinaryHeap;

#[derive(Debug, Clone)]
pub struct Pathfinding<'a> {
    location: &'a Location,

    allow_diagonal: bool,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// small stone debris, with an additional penalty as specified.
    /// If `None`, do not allow walking through tiles that contain
    /// small stones.
    clear_stone: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain 2x2
    /// boulders, with an additional penalty as specified.  If `None`,
    /// do not allow walking through tiles that contain 2x2 boulders.
    clear_boulders: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain wood
    /// debris/twigs, with an additional penalty as specified.  If
    /// `None`, do not allow walking through tiles that contain wood
    /// debris/twigs.
    clear_wood: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// fiber/weeds, with an additional penalty as specified.  If
    /// `None`, do not allow walking through tiles that contain
    /// fiber/weeds.
    clear_fiber: Option<u64>,

    /// If `Some(cost)`, allow walking through tiles that contain
    /// trees, with an additional penalty as specified.  If `None`, do
    /// not allow walking through tiles that contain trees.
    clear_trees: Option<u64>,

    /// The additional cost associated with moving through a tile that
    /// contains grass.
    grass_penalty: u64,

    /// If true, results will allow the final tile of a path to be on
    /// an unreachable tile.  If false, the final tile of a path must
    /// not be blocked.
    ///
    /// Including the border is useful when identifying tiles on which
    /// a tool may be used, where the player only needs to stand next
    /// to the final tile.  Excluding the border is useful when
    /// identifying tiles on which the player may stand.
    include_border: bool,
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
#[derive(PartialEq, Eq)]
struct DijkstraEntry {
    /// The distance from the initial point to the current tile.
    dist: u64,
    tile: Vector<isize>,
    should_propagate: bool,
}

/// Internal implementation, provide a lower bound on the distance
/// between two tiles.
fn heuristic_between_points(a: Vector<isize>, b: Vector<isize>) -> u64 {
    let offset = (a - b).map(|x| x.abs() as u64);

    let min = offset.right.min(offset.down);
    let max = offset.right.max(offset.down);

    let diagonal_movements = min;
    let cardinal_movements = max - min;
    1414 * diagonal_movements + 1000 * cardinal_movements
}

mod detail {
    use super::*;
    pub trait Goal: Sized + Copy {
        fn iter(self) -> impl Iterator<Item = Vector<isize>>;
    }
    impl Goal for Vector<isize> {
        fn iter(self) -> impl Iterator<Item = Vector<isize>> {
            std::iter::once(self)
        }
    }
    impl<Iter> Goal for Iter
    where
        Iter: Copy,
        Iter: IntoIterator,
        <Iter as IntoIterator>::Item: Into<Vector<isize>>,
    {
        fn iter(self) -> impl Iterator<Item = Vector<isize>> {
            self.into_iter().map(Into::into)
        }
    }
}

impl Location {
    pub fn pathfinding(&self) -> Pathfinding {
        let clear_boulders = self.mineshaft_details.as_ref().map(|_| 10000);
        Pathfinding {
            location: self,
            allow_diagonal: true,
            clear_stone: None,
            clear_boulders,
            clear_wood: None,
            clear_fiber: None,
            clear_trees: None,
            grass_penalty: 100,
            include_border: false,
        }
    }
}

impl Pathfinding<'_> {
    pub fn allow_diagonal(self, allow_diagonal: bool) -> Self {
        Self {
            allow_diagonal,
            ..self
        }
    }
    pub fn stone_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_stone: Some(cost),
            ..self
        }
    }
    pub fn wood_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_wood: Some(cost),
            ..self
        }
    }
    pub fn fiber_clearing_cost(self, cost: u64) -> Self {
        Self {
            clear_fiber: Some(cost),
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

        let iter_water = loc.iter_water_tiles();

        let iter_bush = loc.iter_bush_tiles();

        let iter_furniture = loc
            .furniture
            .iter()
            .filter(|piece| !piece.is_walkable())
            .flat_map(|piece| piece.shape.iter_points());

        let iter_buildings = loc.iter_building_tiles();

        let iter_unwalkable = std::iter::empty()
            .chain(iter_blocked)
            .chain(iter_water)
            .chain(iter_bush)
            .chain(iter_furniture)
            .chain(iter_buildings)
            .map(|tile| (tile, None));

        // Handle tiles that may be passable, but with an additional
        // movement cost.
        let iter_clumps = loc.resource_clumps.iter().flat_map(|clump| {
            let opt_cost = match &clump.kind {
                ResourceClumpKind::MineBoulder => self.clear_boulders,
                _ => None,
            };
            clump.shape.iter_points().map(move |tile| (tile, opt_cost))
        });

        let iter_obj = loc.objects.iter().filter_map(|obj| {
            let opt_cost = match &obj.kind {
                ObjectKind::Stone(_) => self.clear_stone,
                ObjectKind::Wood => self.clear_wood,
                ObjectKind::Fiber => self.clear_fiber,
                ObjectKind::Tree(_) => self.clear_trees,
                ObjectKind::Grass => Some(self.grass_penalty),
                other if other.is_walkable() => {
                    // Can walk on this tile without penalty (e.g. a
                    // rug on the floor)
                    return None;
                }
                _ => None,
            };

            Some((obj.tile, opt_cost))
        });

        std::iter::empty()
            .chain(iter_unwalkable)
            .chain(iter_clumps)
            .chain(iter_obj)
    }

    fn movement_cost(&self) -> TileMap<Option<u64>> {
        // Collect all the tile costs into a single TileMap
        let mut map = self.location.blocked.map(|_| Some(0));

        self.movement_cost_component().for_each(|(tile, opt_cost)| {
            map[tile] = match (map[tile], opt_cost) {
                (None, _) | (_, None) => None,
                (Some(a), Some(b)) => {
                    // Add the two costs together, as some objects can
                    // coexist on the same tile.  For example, a stone
                    // that has grass underneath it.
                    Some(a + b)
                }
            };
        });

        map
    }

    /// Check if a single tile is walkable.  If checking several
    /// tiles, may be faster to use `Pathfinding.walkable` instead.
    pub fn tile_is_walkable(&self, tile: Vector<isize>) -> bool {
        self.location.blocked.in_bounds(tile)
            && self
                .movement_cost_component()
                .filter(|(component_tile, _)| component_tile == &tile)
                .all(|(_, opt_cost)| opt_cost.is_some())
    }

    pub fn walkable(&self) -> TileMap<bool> {
        self.movement_cost().map(|opt_cost| opt_cost.is_some())
    }

    pub fn reachable(&self, initial: Vector<isize>) -> TileMap<bool> {
        let walkable = self.walkable();

        let mut reachable = walkable.map(|_| false);
        reachable[initial] = true;

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

    pub fn distances(&self, initial: Vector<isize>) -> TileMap<Option<u64>> {
        let mut distances = self.location.blocked.map(|_| None);

        self.iter_dijkstra(initial).for_each(|(tile, dist)| {
            assert!(distances[tile].is_none());
            distances[tile] = Some(dist);
        });

        distances
    }

    pub fn path_between(
        &self,
        initial: Vector<isize>,
        goal: impl detail::Goal,
    ) -> Result<Vec<Vector<isize>>, Error> {
        let cost_map = self.movement_cost();

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

        while let Some(visiting) = to_visit.pop_first() {
            assert!(
                best.in_bounds(visiting.tile + best_tile_offset),
                "Tile {} is out of bounds for location '{}'",
                visiting.tile,
                self.location.name,
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

            let iter_dir = Direction::iter()
                .filter(|_| visiting.should_propagate)
                .filter(|dir| self.allow_diagonal || dir.is_cardinal());

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
                let additional_cost = cost_map
                    .get(new_tile)
                    .map(Clone::clone)
                    .flatten()
                    .unwrap_or(0);

                let new_dist = visiting.dist + tile_dist + additional_cost;
                let new_dist_plus_heuristic =
                    new_dist + get_heuristic(new_tile);

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

            let is_finished = goal.iter().any(|goal_tile| {
                best.in_bounds(goal_tile + best_tile_offset)
                    && best[goal_tile + best_tile_offset].is_some()
            });
            if is_finished {
                break;
            }
        }

        let goal_tile = goal
            .iter()
            .find(|&goal_tile| {
                best.in_bounds(goal_tile + best_tile_offset)
                    && best[goal_tile + best_tile_offset].is_some()
            })
            .ok_or_else(|| BotError::NoRouteToTargets {
                room: self.location.name.clone(),
                start: initial,
                goals: goal.iter().collect(),
            })?;

        let mut path: Vec<_> =
            std::iter::successors(Some(goal_tile), |&path_tile| {
                let dir = best[path_tile + best_tile_offset]?.1?;
                Some(path_tile - dir.offset())
            })
            .collect();

        path.reverse();

        Ok(path)
    }

    pub fn iter_dijkstra(
        &self,
        initial: impl detail::Goal,
    ) -> impl Iterator<Item = (Vector<isize>, u64)> + '_ {
        let walkable = self.walkable();

        let mut finished: TileMap<bool> = walkable.map(|_| false);

        let mut to_visit = std::collections::BTreeSet::<DijkstraEntry>::new();
        for initial in initial.iter() {
            to_visit.insert(DijkstraEntry {
                dist: 0,
                tile: initial,
                should_propagate: true,
            });
        }

        std::iter::from_fn(move || -> Option<(Vector<isize>, u64)> {
            let DijkstraEntry {
                dist,
                tile,
                should_propagate,
            } = loop {
                let entry = to_visit.pop_first()?;

                if !finished[entry.tile] {
                    break entry;
                }
            };
            finished[tile] = true;

            let iter_dir = Direction::iter()
                .filter(|dir| self.allow_diagonal || dir.is_cardinal())
                .filter(|_| should_propagate);

            for dir in iter_dir {
                let offset = dir.offset();
                let new_tile = tile + offset;

                let is_walkable = walkable.is_set(new_tile);
                let is_accessible = if dir.is_cardinal() {
                    is_walkable
                } else {
                    is_walkable
                        && walkable.is_set(tile + Vector::new(0, offset.down))
                        && walkable.is_set(tile + Vector::new(offset.right, 0))
                };

                if finished.in_bounds(new_tile)
                    && !finished[new_tile]
                    && (is_accessible || (self.include_border && !is_walkable))
                {
                    to_visit.insert(DijkstraEntry {
                        dist: dist + 1,
                        tile: new_tile,
                        should_propagate: is_accessible,
                    });
                }
            }

            Some((tile, dist))
        })
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
