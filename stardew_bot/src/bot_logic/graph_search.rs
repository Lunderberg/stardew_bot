use std::cmp::Reverse;
use std::collections::HashSet;
use std::hash::Hash;

use priority_queue::PriorityQueue;

/// The externally-exposed representation of a graph.
pub trait GraphSearch<T: Eq + Hash> {
    /// Given a node, return all nodes directly excessible from that
    /// node, along with the cost associated with each edge.
    fn connections_from<'a>(
        &'a self,
        node: &'a T,
    ) -> impl IntoIterator<Item = (T, u64)> + 'a;

    /// Provide a lower bound for the distance between two nodes
    ///
    /// Used for A* search.  If no such heuristic can be generated,
    /// return 0 to fall back to using Dijkstra's.  If None, implies
    /// that it's impossible to reach the target node from the
    /// specified point.
    fn heuristic_between(&self, _node_from: &T, _node_to: &T) -> Option<u64> {
        Some(0)
    }

    /// Iterate through nodes of the graph, starting at `initial`
    ///
    /// Result iterators contain the node, and metadata about that
    /// node.  Backreferences in the metadata may be used to generate
    /// the path to a node.  Indices in the backreferences refer to
    /// the order in which the iterator produced the nodes.
    #[allow(dead_code)]
    fn dijkstra_search(
        &self,
        initial: T,
    ) -> SearchIter<T, Self, DijkstraHeuristic>
    where
        T: Clone,
    {
        self.search_with_explicit_heuristic(initial, DijkstraHeuristic)
    }

    /// Iterate through nodes of the graph, starting at `initial`,
    /// using heuristic to visit nodes that are closer to the target
    /// node.
    ///
    /// Result iterators contain the node, and metadata about that
    /// node.  Backreferences in the metadata may be used to generate
    /// the path to a node.  Indices in the backreferences refer to
    /// the order in which the iterator produced the nodes.
    ///
    /// If the `DynamicGraph.heuristic_between` method returns the
    /// default `Some(0)`, the results from this method are identical
    /// to the results from `DynamicGraph.dijkstra_search`.
    fn a_star_search(
        &self,
        initial: T,
        target: T,
    ) -> SearchIter<T, Self, GoalNodeHeuristic<T, Self>>
    where
        T: Clone,
    {
        self.search_with_explicit_heuristic(
            initial,
            GoalNodeHeuristic {
                goal: target,
                graph: self,
            },
        )
    }

    fn search_with_explicit_heuristic<H>(
        &self,
        initial: T,
        heuristic: H,
    ) -> SearchIter<T, Self, H>
    where
        T: Clone,
        H: Heuristic<T>,
    {
        let search_queue = heuristic
            .lower_bound_to_dest(&initial)
            .map(|heuristic_to_dest| {
                (
                    initial,
                    InternalInfo {
                        node_index: None,
                        initial_to_node: 0,
                        heuristic: heuristic_to_dest,
                        backref: None,
                    },
                )
            })
            .into_iter()
            .collect();

        SearchIter {
            search_queue,
            finished: HashSet::new(),
            graph: self,
            heuristic,
        }
    }
}

/// The result structure of producing a path.
#[derive(Debug, Clone)]
pub struct SearchNodeMetadata {
    /// The distance required to travel from the initial node to this
    /// node.
    #[allow(dead_code)]
    pub initial_to_node: u64,

    /// An estimated lower-bound on the distance remaining from this
    /// node to the target node.  For a dijkstra search, this value
    /// will always be zero.
    #[allow(dead_code)]
    pub heuristic: u64,

    /// The edge that was followed to reach this node, along the
    /// fastest path from the initial node.  Only the initial node may
    /// have previous_edge: None.
    pub backref: Option<GraphEdge>,
}

/// Internal structure for path-finding.  Implements Ord based on the
/// sum of src_to_pos and heuristic_to_dest.
struct InternalInfo {
    node_index: Option<usize>,
    initial_to_node: u64,
    heuristic: u64,
    backref: Option<GraphEdge>,
}

/// A backwards edge, pointing towards the initial node of the search.
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct GraphEdge {
    /// Index into a vector of nodes, where all elements of that
    /// vector have the fastest path known.  This can be used to
    /// reconstruct the fastest path, by tracing these backreferences
    /// until reaching the initial node.
    pub initial_node: usize,

    /// The distance required to travel along an edge.  This value is
    /// produced by the `DynamicGraph::connections_from` method.
    pub edge_weight: u64,
}

pub trait Heuristic<T> {
    fn lower_bound_to_dest(&self, from_node: &T) -> Option<u64>;
}
pub struct DijkstraHeuristic;

pub struct GoalNodeHeuristic<'a, T, Graph: ?Sized> {
    goal: T,
    graph: &'a Graph,
}

impl<T> Heuristic<T> for DijkstraHeuristic {
    fn lower_bound_to_dest(&self, _: &T) -> Option<u64> {
        Some(0)
    }
}
impl<Func, T> Heuristic<T> for Func
where
    Func: Fn(&T) -> Option<u64>,
{
    fn lower_bound_to_dest(&self, from_node: &T) -> Option<u64> {
        self(from_node)
    }
}
impl<'a, T, Graph> Heuristic<T> for GoalNodeHeuristic<'a, T, Graph>
where
    T: Eq + Hash,
    Graph: GraphSearch<T> + ?Sized,
{
    fn lower_bound_to_dest(&self, from_node: &T) -> Option<u64> {
        self.graph.heuristic_between(from_node, &self.goal)
    }
}

pub struct SearchIter<
    'a,
    T: Eq + Hash + Clone,
    Graph: GraphSearch<T> + ?Sized,
    H: Heuristic<T>,
> {
    search_queue: PriorityQueue<T, InternalInfo>,
    finished: HashSet<T>,
    graph: &'a Graph,
    heuristic: H,
}

impl<
        'a,
        T: Eq + Hash + Clone,
        Graph: GraphSearch<T> + ?Sized,
        H: Heuristic<T>,
    > Iterator for SearchIter<'a, T, Graph, H>
{
    type Item = (T, SearchNodeMetadata);

    fn next(&mut self) -> Option<Self::Item> {
        let (node, mut info) = self.search_queue.pop()?;

        let initial_to_node = info.initial_to_node;
        let node_index = self.finished.len();
        info.node_index = Some(node_index);
        self.finished.insert(node.clone());

        let heuristic = &mut self.heuristic;
        let finished = &mut self.finished;
        let search_queue = &mut self.search_queue;

        self.graph
            .connections_from(&node)
            .into_iter()
            .filter(|(new_node, _)| !finished.contains(new_node))
            .filter_map(|(new_node, edge_weight)| {
                let heuristic_to_dest =
                    heuristic.lower_bound_to_dest(&new_node)?;
                let new_info = InternalInfo {
                    node_index: None,
                    initial_to_node: initial_to_node + edge_weight,
                    heuristic: heuristic_to_dest,
                    backref: Some(GraphEdge {
                        initial_node: node_index,
                        edge_weight,
                    }),
                };
                Some((new_node, new_info))
            })
            .for_each(|(node, info)| {
                search_queue.push_increase(node, info);
            });

        Some((node, info.into()))
    }
}

impl PartialEq for InternalInfo {
    fn eq(&self, rhs: &Self) -> bool {
        self.priority().eq(&rhs.priority())
    }
}
impl Eq for InternalInfo {}

impl PartialOrd for InternalInfo {
    fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(rhs))
    }
}

impl Ord for InternalInfo {
    fn cmp(&self, rhs: &Self) -> std::cmp::Ordering {
        self.priority().cmp(&rhs.priority())
    }
}

impl InternalInfo {
    fn priority(&self) -> impl Eq + Ord {
        (
            Reverse(self.initial_to_node + self.heuristic),
            self.initial_to_node,
        )
    }
}

impl From<InternalInfo> for SearchNodeMetadata {
    fn from(info: InternalInfo) -> Self {
        Self {
            initial_to_node: info.initial_to_node,
            heuristic: info.heuristic,
            backref: info.backref,
        }
    }
}
