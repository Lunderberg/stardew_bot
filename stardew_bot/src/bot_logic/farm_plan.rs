use crate::{
    bot_logic::GameStateExt as _,
    game_state::{Rectangle, Vector},
    Error, GameState,
};

pub struct FarmPlan {
    initial_plot: Rectangle<isize>,
    tree_farm: Rectangle<isize>,
}

impl FarmPlan {
    pub fn plan(game_state: &GameState) -> Result<FarmPlan, Error> {
        let farm_door = game_state.get_farm_door()?;
        let initial_plot = {
            let top_right = farm_door + Vector::new(3, 5);
            // 60 seeds for the first day, plus an extra row for any
            // carrot seeds.
            let num_columns = 11;
            let num_rows = 6;
            Rectangle::new(
                top_right - Vector::new(num_columns - 1, 0),
                Vector::new(num_columns, num_rows),
            )
        };

        let tree_farm = {
            let tree_top_right = initial_plot.top_right() + Vector::new(-1, 12);

            let num_tree_rows = 10;
            let num_tree_columns = 10;

            let shape =
                Vector::new(num_tree_columns * 2 - 1, num_tree_rows * 2 - 1);

            Rectangle::new(
                tree_top_right - Vector::new(shape.right - 1, 0),
                shape,
            )
        };

        Ok(Self {
            initial_plot,
            tree_farm,
        })
    }

    pub fn is_planned_tree(&self, tile: Vector<isize>) -> bool {
        self.tree_farm.contains(tile)
            && (self.tree_farm.top_left - tile).map(|x| x % 2) == Vector::zero()
    }

    pub fn iter_planned_trees(&self) -> impl Iterator<Item = Vector<isize>> {
        let shape = self.tree_farm.shape;
        let top_left = self.tree_farm.top_left;
        (0..shape.right)
            .step_by(2)
            .rev()
            .flat_map(move |di| {
                (0..shape.down)
                    .step_by(2)
                    .map(move |dj| Vector::new(di, dj))
            })
            .map(move |offset| top_left + offset)
    }

    pub fn iter_initial_plot(
        &self,
    ) -> impl Iterator<Item = Vector<isize>> + '_ {
        self.initial_plot.iter_points()
    }
}
