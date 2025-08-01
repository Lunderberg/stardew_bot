use geometry::{Direction, Vector};

use game_state::TileMap;

use super::graph_search::GraphSearch;

pub(crate) fn point_to_point_lower_bound(
    node_from: Vector<isize>,
    node_to: Vector<isize>,
) -> u64 {
    let offset = (node_from - node_to).map(|x| x.unsigned_abs() as u64);
    let min = offset.right.min(offset.down);
    let max = offset.right.max(offset.down);

    let diagonal_movements = min;
    let cardinal_movements = max - min;
    // Counting the number of half-tiles means that I can stay
    // in integer math.  3/2 as the cost of a diagonal
    // movement is close enough to sqrt(2) for the
    // pathfinding.
    3 * diagonal_movements + 2 * cardinal_movements
}

impl GraphSearch<Vector<isize>> for TileMap<bool> {
    fn connections_from<'a>(
        &'a self,
        &tile: &'a Vector<isize>,
    ) -> impl IntoIterator<Item = (Vector<isize>, u64)> + 'a {
        Direction::iter()
            .filter(move |dir| {
                let is_clear_tile = |tile: Vector<isize>| self.is_set(tile);

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
        Some(point_to_point_lower_bound(*node_from, *node_to))
    }
}
