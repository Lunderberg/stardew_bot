use geometry::{Rectangle, Vector};
use itertools::Itertools as _;
use ratatui::{
    style::Color,
    symbols::Marker,
    text::Text,
    widgets::{
        canvas::{
            Canvas, Context as CanvasContext, Points,
            Rectangle as CanvasRectangle,
        },
        Block, Widget as _,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::{Error, GameState};
use game_state::{
    FurnitureKind, Location, ObjectKind, ResourceClumpKind, StoneKind, TileMap,
};

pub struct LocationDisplay;

struct DrawableGameLocation<'a> {
    room: &'a Location,
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
                self.paint_diggable_tiles(ctx);
                self.paint_blocked_tiles(ctx);
                self.paint_water_tiles(ctx);
                self.paint_buildings(ctx);
                self.paint_resource_clumps(ctx);
                self.paint_bushes(ctx);
                self.paint_objects(ctx);
                self.paint_furniture(ctx);

                ctx.layer();

                {
                    let player_coords = self.to_draw_coordinates(
                        self.player_position.map(|x| x as f64),
                    );
                    // Hack to reset the foreground color before
                    // printing the 'x' for a player.  Otherwise, the
                    // 'x' will be printed in whichever color was
                    // previously used for that tile.
                    //
                    // TODO: Remove use of the
                    // ratatui::widgets::canvas::Canvas altogether,
                    // since its fp64-representation is overkill for
                    // printing a tiled map.
                    ctx.draw(&Points {
                        coords: &[player_coords],
                        color: Color::White,
                    });
                    ctx.print(player_coords.0, player_coords.1, "x");
                }
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

    fn paint_bool_map(
        ctx: &mut CanvasContext,
        map: &TileMap<bool>,
        color: Color,
    ) {
        let height = map.height();

        let tiles = map
            .iter()
            .filter(|(_, is_blocked)| **is_blocked)
            .map(|(loc, _)| {
                let height = height as f64;
                (loc.right as f64, height - loc.down as f64)
            })
            .collect::<Vec<_>>();

        ctx.draw(&Points {
            coords: &tiles,
            color,
        });
    }

    fn paint_blocked_tiles(&self, ctx: &mut CanvasContext) {
        Self::paint_bool_map(ctx, &self.room.blocked, Color::Red);
    }

    fn paint_diggable_tiles(&self, ctx: &mut CanvasContext) {
        Self::paint_bool_map(ctx, &self.room.diggable, Color::Rgb(20, 20, 20));
    }

    fn paint_water_tiles(&self, ctx: &mut CanvasContext) {
        let Vector {
            right: width,
            down: height,
        } = self.room.shape;
        let width = width as usize;
        let height = height as usize;

        let tiles = &self.room.water_tiles;

        assert!(tiles.width() == width);
        assert!(tiles.height() == height);
        let points = tiles
            .iter()
            .filter(|(_, is_water)| **is_water)
            .map(|(tile, _)| {
                let i = tile.right;
                let j = tile.down;
                let x = i as f64;
                let y = ((height as isize) - j) as f64;
                (x, y)
            })
            .collect::<Vec<_>>();

        ctx.draw(&Points {
            coords: &points,
            color: Color::Blue,
        });
    }

    fn paint_buildings(&self, ctx: &mut CanvasContext) {
        let points: Vec<_> = self
            .room
            .buildings
            .iter()
            .flat_map(|building| building.iter_tiles())
            .map(|point| self.to_draw_coordinates(point.map(|x| x as f64)))
            .collect();

        ctx.draw(&Points {
            coords: &points,
            color: Color::Rgb(45, 20, 0),
        });
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
                        ResourceClumpKind::Meteorite => Color::Magenta,
                        ResourceClumpKind::MineBoulder => Color::DarkGray,
                        ResourceClumpKind::GiantCrop(_) => {
                            Color::Rgb(41, 192, 80)
                        }
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

    fn paint_objects(&self, ctx: &mut CanvasContext) {
        self.room
            .objects
            .iter()
            .filter_map(|obj| {
                let color = match &obj.kind {
                    ObjectKind::Stone(StoneKind::Copper) => {
                        Some(Color::DarkGray)
                    }
                    ObjectKind::Stone(_) => Some(Color::DarkGray),
                    ObjectKind::Mineral(_) => Some(Color::DarkGray),
                    ObjectKind::Wood => Some(Color::Rgb(97, 25, 0)),
                    ObjectKind::Fiber => Some(Color::LightGreen),
                    ObjectKind::PotOfGold => Some(Color::Yellow),
                    ObjectKind::Tree(_) | ObjectKind::FruitTree(_) => {
                        Some(Color::Rgb(133, 74, 5))
                    }
                    ObjectKind::Grass => Some(Color::Rgb(10, 80, 10)),

                    ObjectKind::Torch => None,
                    ObjectKind::Sprinkler(_) => None,
                    ObjectKind::Scarecrow => None,
                    ObjectKind::ArtifactSpot => None,
                    ObjectKind::SeedSpot => None,
                    ObjectKind::Chest(_) => None,
                    ObjectKind::CraftingMachine(_) => None,
                    ObjectKind::MineLadderDown => Some(Color::Rgb(140, 20, 20)),
                    ObjectKind::MineBarrel
                    | ObjectKind::MineLadderUp
                    | ObjectKind::MineHoleDown
                    | ObjectKind::MineElevator
                    | ObjectKind::MineCartCoal => None,
                    ObjectKind::Other { .. } => None,
                    ObjectKind::HoeDirt(_) => Some(Color::Rgb(40, 40, 40)),
                    ObjectKind::Unknown => None,
                }?;
                let coordinates =
                    self.to_draw_coordinates(obj.tile.map(|x| x as f64));
                Some((color, coordinates))
            })
            .into_group_map()
            .into_iter()
            .for_each(|(color, coords)| {
                ctx.draw(&Points {
                    coords: &coords,
                    color,
                });
            });
    }

    fn paint_furniture(&self, ctx: &mut CanvasContext) {
        self.room
            .furniture
            .iter()
            .flat_map(|piece| {
                let color = match &piece.kind {
                    FurnitureKind::Rug => Color::Rgb(11, 13, 58),
                    FurnitureKind::Bed => Color::Rgb(75, 131, 186),
                    _ => Color::Rgb(253, 196, 0),
                };
                piece
                    .shape
                    .iter_points()
                    .map(|tile| {
                        self.to_draw_coordinates(tile.map(|x| x as f64))
                    })
                    .map(move |coord| (color, coord))
            })
            .into_group_map()
            .into_iter()
            .for_each(|(color, coords)| {
                ctx.draw(&Points {
                    coords: &coords,
                    color,
                });
            });
    }
}

impl WidgetWindow<Error> for LocationDisplay {
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

        let current_location: &str = &game_state.player.room_name;
        let position = game_state.player.center_pos();

        let (left_column, draw_area) = area.split_from_left(15);

        let (top_area, _table_area) = left_column.split_from_top(3);

        let top_text = Text::raw(format!(
            "Current: {}\n({:.1}, {:.1})",
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
                draw_marker: Marker::Block,
                draw_area,
                player_position: position,
            }
            .render(buf);
        }
    }
}
