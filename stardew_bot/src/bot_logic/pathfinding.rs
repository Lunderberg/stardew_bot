use crate::{
    bot_logic::BotError,
    game_state::{Location, ObjectKind, TileMap, Vector},
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
        Pathfinding {
            location: self,
            allow_diagonal: true,
            clear_stone: None,
            clear_wood: None,
            clear_fiber: None,
            clear_trees: None,
            grass_penalty: 100,
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

    fn movement_cost(&self) -> TileMap<Option<u64>> {
        let loc = &self.location;
        let mut map = loc
            .blocked
            .map(|&blocked| if blocked { None } else { Some(0) });

        let iter_water = loc.iter_water_tiles();

        let iter_clumps = loc
            .resource_clumps
            .iter()
            .flat_map(|clump| clump.shape.iter_points());

        let iter_bush = loc.iter_bush_tiles();

        let iter_furniture = loc
            .furniture
            .iter()
            .filter(|piece| !piece.is_walkable())
            .flat_map(|piece| piece.shape.iter_points());

        let iter_buildings = loc.iter_building_tiles();

        // Handle tiles that are not passable, even with an additional
        // movement cost.
        std::iter::empty()
            .chain(iter_water)
            .chain(iter_clumps)
            .chain(iter_bush)
            .chain(iter_furniture)
            .chain(iter_buildings)
            .for_each(|tile| {
                map[tile] = None;
            });

        // Handle tiles that may be passable, but with an additional
        // movement cost.
        for obj in &loc.objects {
            let opt_cost = match &obj.kind {
                ObjectKind::Stone => self.clear_stone,
                ObjectKind::Wood => self.clear_wood,
                ObjectKind::Fiber => self.clear_fiber,
                ObjectKind::Tree(_) => self.clear_trees,
                ObjectKind::Grass => Some(self.grass_penalty),
                other if other.is_walkable() => {
                    // Can walk on this tile without penalty (e.g. a
                    // rug on the floor)
                    continue;
                }
                _ => None,
            };

            map[obj.tile] = match (map[obj.tile], opt_cost) {
                (None, _) | (_, None) => None,
                (Some(a), Some(b)) => {
                    // Add the two costs together, as some objects can
                    // coexist on the same tile.  For example, a stone
                    // that has grass underneath it.
                    Some(a + b)
                }
            };
        }

        map
    }

    pub fn reachable(&self, initial: Vector<isize>) -> TileMap<bool> {
        let cost = self.movement_cost();

        let mut reachable = cost.map(|_| false);
        reachable[initial] = true;

        let mut to_visit = vec![initial];
        while let Some(tile) = to_visit.pop() {
            for dir in Direction::iter_cardinal() {
                let adj = tile + dir.offset();
                if reachable.in_bounds(adj)
                    && !reachable[adj]
                    && cost[adj].is_some()
                {
                    reachable[adj] = true;
                    to_visit.push(adj);
                }
            }
        }

        reachable
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
                .map(|(prev, _)| prev <= visiting.dist_plus_heuristic)
                .unwrap_or(true);
            if !is_best {
                continue;
            }

            let tile = visiting.tile;
            best[tile + best_tile_offset] =
                Some((visiting.dist_plus_heuristic, visiting.dir));

            Direction::iter()
                .filter(|dir| self.allow_diagonal || dir.is_cardinal())
                .filter(|dir| {
                    let is_walkable = |check_tile: Vector<isize>| -> bool {
                        if cost_map.in_bounds(check_tile) {
                            cost_map[check_tile].is_some()
                        } else {
                            goal.iter().any(|goal_tile| goal_tile == check_tile)
                        }
                    };

                    let offset = dir.offset();
                    if dir.is_cardinal() {
                        is_walkable(tile + offset)
                    } else {
                        [
                            Vector::new(offset.right, offset.down),
                            Vector::new(0, offset.down),
                            Vector::new(offset.right, 0),
                        ]
                        .into_iter()
                        .all(|offset| is_walkable(tile + offset))
                    }
                })
                .map(|dir| {
                    let new_tile = tile + dir.offset();
                    let tile_dist = if dir.is_cardinal() { 1000 } else { 1414 };
                    let additional_cost = if cost_map.in_bounds(new_tile) {
                        cost_map[new_tile].expect(
                            "Should only have walkable tiles at this point",
                        )
                    } else {
                        0
                    };

                    let new_dist = visiting.dist + tile_dist + additional_cost;
                    AStarEntry {
                        dist_plus_heuristic: new_dist + get_heuristic(new_tile),
                        tile: new_tile,
                        dist: new_dist,
                        dir: Some(dir),
                    }
                })
                .for_each(|new_entry| {
                    let is_best = best[new_entry.tile + best_tile_offset]
                        .map(|(prev, _)| new_entry.dist_plus_heuristic < prev)
                        .unwrap_or(true);

                    if is_best {
                        best[new_entry.tile + best_tile_offset] = Some((
                            new_entry.dist_plus_heuristic,
                            new_entry.dir,
                        ));
                        to_visit.insert(new_entry);
                    }
                });

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
        let walkable = self.movement_cost().map(|opt_cost| opt_cost.is_some());

        let mut finished: TileMap<bool> = walkable.map(|_| false);

        let mut to_visit = std::collections::BTreeSet::<DijkstraEntry>::new();
        for initial in initial.iter() {
            to_visit.insert(DijkstraEntry {
                dist: 0,
                tile: initial,
            });
        }

        std::iter::from_fn(move || -> Option<(Vector<isize>, u64)> {
            let DijkstraEntry { dist, tile } = loop {
                let entry = to_visit.pop_first()?;

                if !finished[entry.tile] {
                    break entry;
                }
            };

            finished[tile] = true;

            Direction::iter()
                .filter(|dir| self.allow_diagonal || dir.is_cardinal())
                .filter(|dir| {
                    let is_walkable = |check_tile: Vector<isize>| -> bool {
                        walkable.get(check_tile).cloned().unwrap_or(false)
                    };

                    let offset = dir.offset();
                    if dir.is_cardinal() {
                        is_walkable(tile + offset)
                    } else {
                        [
                            Vector::new(offset.right, offset.down),
                            Vector::new(0, offset.down),
                            Vector::new(offset.right, 0),
                        ]
                        .into_iter()
                        .all(|offset| is_walkable(tile + offset))
                    }
                })
                .for_each(|dir| {
                    let new_tile = tile + dir.offset();

                    if !finished[new_tile] {
                        to_visit.insert(DijkstraEntry {
                            dist: dist + 1,
                            tile: new_tile,
                        });
                    }
                });

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
