use game_state::GameState;
use geometry::Vector;

use crate::{Error, GameAction};

use super::{
    bot_logic::{
        ActionCollector, BotGoal, BotGoalResult, BotInterrupt, LogicStack,
    },
    LocationExt as _, MovementGoal,
};

pub struct CollectNearbyItems {
    search_radius: f32,
    cluster_search_radius: f32,
    cluster_radius: f32,
    min_items_in_cluster: usize,
}

struct WalkTowardDebris {
    search_radius_squared: f32,
    goal_dist_squared: f32,
}

impl CollectNearbyItems {
    pub fn new() -> Self {
        Self {
            search_radius: 2.10,
            cluster_search_radius: 10.0,
            cluster_radius: 2.5,
            min_items_in_cluster: 3,
        }
    }

    #[allow(dead_code)]
    pub fn with_search_radius(self, search_radius: f32) -> Self {
        Self {
            search_radius,
            ..self
        }
    }

    fn iter_items_within_radius(
        game_state: &GameState,
        center: Vector<f32>,
        max_dist: f32,
    ) -> Result<impl Iterator<Item = Vector<f32>> + '_, Error> {
        let max_dist2 = max_dist * max_dist;

        let inventory = &game_state.player.inventory;
        let iter = game_state
            .current_room()?
            .items
            .iter()
            .filter(|item| inventory.can_add(item))
            .map(|item| item.position / 64.0)
            .filter(move |pos| {
                let dist2 = center.dist2(*pos);
                dist2 <= max_dist2
            });

        Ok(iter)
    }

    fn nearby_cluster(
        &self,
        game_state: &GameState,
    ) -> Result<Option<Vector<f32>>, Error> {
        let (sum_pos, count) = Self::iter_items_within_radius(
            game_state,
            game_state.player.center_pos(),
            self.cluster_search_radius,
        )?
        .fold((Vector::<f32>::zero(), 0usize), |(sum, count), pos| {
            (sum + pos, count + 1)
        });
        if count < self.min_items_in_cluster {
            // Not enough items near the player to form a cluster.
            return Ok(None);
        }

        let item_centroid = sum_pos / (count as f32);

        let closest_item = Self::iter_items_within_radius(
            game_state,
            game_state.player.center_pos(),
            self.cluster_search_radius,
        )?
        .min_by(|a, b| {
            let a_dist2 = item_centroid.dist2(*a);
            let b_dist2 = item_centroid.dist2(*b);
            a_dist2.total_cmp(&b_dist2)
        })
        .expect("Already checked that there's items near the player");

        let (sum_cluster, num_in_cluster) = Self::iter_items_within_radius(
            game_state,
            closest_item,
            self.cluster_radius,
        )?
        .fold((Vector::<f32>::zero(), 0usize), |(sum, count), pos| {
            (sum + pos, count + 1)
        });
        if num_in_cluster < self.min_items_in_cluster {
            // The items around the player are not clustered together.
            return Ok(None);
        }

        let cluster_centroid = sum_cluster / (num_in_cluster as f32);

        let reachable = game_state
            .current_room()?
            .pathfinding(&game_state.statics)
            .include_border(true)
            .reachable(game_state.player.tile());
        if !reachable.is_set(cluster_centroid.map(|x| x.round() as isize)) {
            // The items are near the player, but are not accessible.
            return Ok(None);
        }

        Ok(Some(cluster_centroid))
    }
}

impl BotInterrupt for CollectNearbyItems {
    fn description(&self) -> std::borrow::Cow<str> {
        format!("Collect items within {} tiles", self.search_radius).into()
    }

    fn check(
        &mut self,
        game_state: &GameState,
    ) -> Result<Option<LogicStack>, Error> {
        let goal_dist = 1.55;
        let goal = WalkTowardDebris {
            search_radius_squared: self.search_radius * self.search_radius,
            goal_dist_squared: goal_dist * goal_dist,
        };

        if !goal.is_completed(game_state)? {
            return Ok(Some(goal.into()));
        }

        if let Some(cluster) =
            self.nearby_cluster(game_state)?.filter(|cluster| {
                // If the cluster is right on top of the player, the
                // movement goal will terminate immediately.
                // Therefore, skip these cases.
                let dist = game_state.player.center_pos().dist(*cluster);
                dist > self.search_radius
            })
        {
            let goal =
                MovementGoal::new(game_state.player.room_name.clone(), cluster)
                    .with_tolerance(goal_dist);
            return Ok(Some(goal.into()));
        }

        Ok(None)
    }
}

impl WalkTowardDebris {
    fn is_completed(&self, game_state: &GameState) -> Result<bool, Error> {
        Ok(self.iter_offsets(game_state)?.next().is_none())
    }

    fn iter_offsets<'a>(
        &'a self,
        game_state: &'a GameState,
    ) -> Result<impl Iterator<Item = Vector<f32>> + 'a, Error> {
        let inventory = &game_state.player.inventory;

        let player_pos = game_state.player.center_pos();

        let iter = game_state
            .current_room()?
            .items
            .iter()
            .filter(|item| inventory.can_add(item))
            .map(move |item| item.position / 64.0 - player_pos)
            .filter(|offset| {
                let dist2 = offset.mag2();
                self.goal_dist_squared < dist2
                    && dist2 < self.search_radius_squared
            });

        Ok(iter)
    }
}

impl BotGoal for WalkTowardDebris {
    fn description(&self) -> std::borrow::Cow<'static, str> {
        format!(
            "Walk toward debris within {} tiles",
            self.search_radius_squared
        )
        .into()
    }

    fn apply(
        &mut self,
        game_state: &GameState,
        actions: &mut ActionCollector,
    ) -> Result<BotGoalResult, Error> {
        let opt_nearby_item = self
            .iter_offsets(game_state)?
            .min_by(|a, b| a.mag2().total_cmp(&b.mag2()));

        if let Some(offset) = opt_nearby_item {
            actions.do_action(GameAction::Move(offset.closest_direction()));
            return Ok(BotGoalResult::InProgress);
        }

        Ok(BotGoalResult::Completed)
    }
}
