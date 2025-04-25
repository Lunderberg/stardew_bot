use std::any::Any;

use ratatui::{
    layout::Constraint,
    style::Color,
    symbols::Marker,
    text::Text,
    widgets::{
        canvas::{
            Canvas, Context as CanvasContext, Points,
            Rectangle as CanvasRectangle,
        },
        Block, Cell, Row, Table, Widget as _,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::{
    bot_logic::MoveToLocationGoal,
    game_state::{LitterKind, Location, Rectangle, ResourceClumpKind, Vector},
    BotLogic, Error, GameState,
};

pub struct PathfindingUI;

struct DrawableGameLocation<'a> {
    room: &'a Location,
    bot_logic: &'a BotLogic,
    draw_marker: Marker,
    draw_area: ratatui::layout::Rect,
    player_position: Vector<f32>,
}

impl<'a> DrawableGameLocation<'a> {
    fn render(&self, buf: &mut ratatui::prelude::Buffer) {
        Canvas::default()
            .block(Block::new().title(self.room.name.as_ref()))
            .marker(self.draw_marker)
            .x_bounds(self.x_bounds())
            .y_bounds(self.y_bounds())
            .paint(|ctx| {
                self.paint_blocked_tiles(ctx);
                self.paint_water_tiles(ctx);
                self.paint_buildings(ctx);
                self.paint_grass(ctx);
                self.paint_resource_clumps(ctx);
                self.paint_bushes(ctx);
                self.paint_trees(ctx);
                self.paint_litter(ctx);

                ctx.layer();

                self.paint_waypoints(ctx);

                ctx.layer();

                ctx.print(
                    self.player_position.right as f64,
                    (self.room.shape.down as f64)
                        - (self.player_position.down as f64),
                    "x",
                );
            })
            .render(self.draw_area, buf)
    }

    fn to_draw_coordinates(&self, loc: Vector<f64>) -> (f64, f64) {
        let height = self.room.shape.down as f64;
        let y = height - loc.down;
        (loc.right, y)
    }

    fn to_draw_rectangle(
        &self,
        rect: Rectangle<isize>,
        color: Color,
    ) -> CanvasRectangle {
        let (left_x, top_y) =
            self.to_draw_coordinates(rect.top_left.map(|i| i as f64));
        let bottom_y = top_y - (rect.shape.down as f64) + 1.0;

        CanvasRectangle {
            x: left_x,
            y: bottom_y,
            width: (rect.shape.right as f64) - 1.0,
            height: (rect.shape.down as f64) - 1.0,
            color,
        }
    }

    fn x_bounds(&self) -> [f64; 2] {
        let (x, _) =
            self.to_draw_coordinates(self.player_position.map(Into::into));
        self.compute_bounds(
            self.room.shape.right as usize,
            self.draw_area.width.into(),
            x,
        )
    }

    fn y_bounds(&self) -> [f64; 2] {
        let (_, y) =
            self.to_draw_coordinates(self.player_position.map(Into::into));
        self.compute_bounds(
            self.room.shape.down as usize,
            self.draw_area.height.into(),
            y,
        )
    }

    fn compute_bounds(
        &self,
        game_extent_in_tiles: usize,
        view_extent_in_terminal: usize,
        position: f64,
    ) -> [f64; 2] {
        let game_tiles_per_terminal_tile = match self.draw_marker {
            Marker::Block => 1,
            Marker::HalfBlock => 2,
            _ => 1,
        };

        let game_extent = game_extent_in_tiles as f64;
        let view_extent =
            (view_extent_in_terminal * game_tiles_per_terminal_tile) as f64;

        if view_extent > game_extent {
            [0.0, view_extent]
        } else if position < view_extent / 2.0 {
            [0.0, view_extent]
        } else if position > game_extent - view_extent / 2.0 {
            [game_extent - view_extent, game_extent]
        } else {
            [position - view_extent / 2.0, position + view_extent / 2.0]
        }
    }

    fn paint_blocked_tiles(&self, ctx: &mut CanvasContext) {
        let Vector {
            right: width,
            down: height,
        } = self.room.shape;
        let width = width as usize;
        let height = height as usize;
        assert!(width * height == self.room.blocked.len());

        let blocked = self
            .room
            .blocked
            .iter()
            .enumerate()
            .filter(|(_, is_blocked)| **is_blocked)
            .map(|(index, _)| {
                let i = index / height;
                let j = index % height;
                let x = i as f64;
                let y = (height - j) as f64;
                (x, y)
            })
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &blocked,
            color: Color::Red,
        });
    }

    fn paint_water_tiles(&self, ctx: &mut CanvasContext) {
        let Vector {
            right: width,
            down: height,
        } = self.room.shape;
        let width = width as usize;
        let height = height as usize;

        let Some(tiles) = &self.room.water_tiles else {
            return;
        };

        assert!(width * height == tiles.len());
        let points = tiles
            .iter()
            .enumerate()
            .filter(|(_, is_water)| **is_water)
            .map(|(index, _)| {
                let i = index / height;
                let j = index % height;
                let x = i as f64;
                let y = (height - j) as f64;
                (x, y)
            })
            .collect::<Vec<_>>();

        ctx.draw(&Points {
            coords: &points,
            color: Color::Blue,
        });
    }

    fn paint_buildings(&self, ctx: &mut CanvasContext) {
        self.room
            .buildings
            .iter()
            .map(|building| {
                self.to_draw_rectangle(building.shape, Color::Rgb(45, 20, 0))
            })
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_resource_clumps(&self, ctx: &mut CanvasContext) {
        self.room
            .resource_clumps
            .iter()
            .map(|clump| {
                self.to_draw_rectangle(
                    clump.shape,
                    match clump.kind {
                        ResourceClumpKind::Stump => Color::Yellow,
                        ResourceClumpKind::Boulder => Color::DarkGray,
                        ResourceClumpKind::Meterorite => Color::Magenta,
                        ResourceClumpKind::MineBoulder => Color::DarkGray,
                    },
                )
            })
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_bushes(&self, ctx: &mut CanvasContext) {
        self.room
            .bushes
            .iter()
            .map(|bush| self.to_draw_rectangle(bush.rectangle(), Color::Green))
            .for_each(|rect| ctx.draw(&rect));
    }

    fn paint_trees(&self, ctx: &mut CanvasContext) {
        let trees = self
            .room
            .trees
            .iter()
            .map(|tree| {
                self.to_draw_coordinates(tree.position.map(|x| x as f64))
            })
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &trees,
            color: Color::Rgb(133, 74, 5),
        });
    }

    fn paint_grass(&self, ctx: &mut CanvasContext) {
        let grass = self
            .room
            .grass
            .iter()
            .map(|grass_pos| {
                self.to_draw_coordinates(grass_pos.map(|x| x as f64))
            })
            .collect::<Vec<_>>();
        ctx.draw(&Points {
            coords: &grass,
            color: Color::Rgb(10, 80, 10),
        });
    }

    fn paint_litter(&self, ctx: &mut CanvasContext) {
        for (litter_kind, color) in [
            (LitterKind::Stone, Color::DarkGray),
            (LitterKind::Wood, Color::Rgb(97, 25, 0)),
            (LitterKind::Fiber, Color::LightGreen),
        ] {
            let litter = self
                .room
                .litter
                .iter()
                .filter(|obj| obj.kind == litter_kind)
                .map(|obj| self.to_draw_coordinates(obj.tile.map(|x| x as f64)))
                .collect::<Vec<_>>();
            ctx.draw(&Points {
                coords: &litter,
                color,
            });
        }
    }

    fn paint_waypoints(&self, ctx: &mut CanvasContext) {
        let points: Vec<_> = self
            .bot_logic
            .current_goal()
            .and_then(|goal| {
                <dyn Any>::downcast_ref::<MoveToLocationGoal>(goal)
            })
            .into_iter()
            .flat_map(|move_goal| move_goal.iter_waypoints())
            .map(|loc| loc.as_type::<f64>())
            .map(|loc| self.to_draw_coordinates(loc))
            .collect();

        ctx.draw(&Points {
            coords: &points,
            color: Color::Red,
        });
    }
}

impl WidgetWindow<Error> for PathfindingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Pathfinding".into()
    }

    fn draw<'a>(
        &'a mut self,
        globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let game_state = globals
            .get::<GameState>()
            .expect("Generated/updated in top-level GUI update");

        let bot_logic = globals
            .get::<BotLogic>()
            .expect("Generated/updated in top-level GUI update");

        let current_location: &str = &game_state.player.room_name;
        let position = game_state.player.position / 64.0;

        let (left_column, draw_area) = area.split_from_left(30);

        let (top_area, table_area) = left_column.split_from_top(2);

        let longest_name = game_state
            .locations
            .iter()
            .map(|loc| loc.name.len())
            .max()
            .unwrap_or(0);

        let loc_rows = game_state.locations.iter().map(|loc| {
            let name = Cell::new(loc.name.as_str());
            let shape = Cell::new(format!("{}", loc.shape));
            Row::new([name, shape])
        });

        let table = Table::new(
            loc_rows,
            [
                Constraint::Min(longest_name as u16),
                Constraint::Min(10),
                Constraint::Min(longest_name as u16),
                Constraint::Percentage(100),
            ],
        );

        table.render(table_area, buf);

        let top_text = Text::raw(format!(
            "Current: {} ({:.1}, {:.1})",
            current_location, position.right, position.down,
        ));
        top_text.render(top_area, buf);

        let opt_current_room = game_state
            .locations
            .iter()
            .find(|loc| loc.name == current_location);
        if let Some(current_room) = opt_current_room {
            DrawableGameLocation {
                room: current_room,
                bot_logic,
                draw_marker: Marker::Block,
                draw_area,
                player_position: position,
            }
            .render(buf);
        }
    }
}
