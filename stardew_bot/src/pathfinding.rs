use dotnet_debugger::{
    symbolic_expr::SymbolicGraph, CachedReader, SymbolicType,
};
use itertools::Itertools as _;
use ratatui::{
    layout::Constraint,
    style::Color,
    symbols::Marker,
    text::Text,
    widgets::{
        canvas::{Canvas, Points},
        Block, Cell, Row, Table, Widget as _,
    },
};
use tui_utils::{extensions::SplitRect as _, WidgetWindow};

use crate::Error;

pub struct PathfindingUI {
    current_location: String,
    position: Position,
    locations: Vec<GameLocation>,
}

/// A region of the game.  Contains information extracted from the
/// `StardewValley.GameLocation` objects.
struct GameLocation {
    /// The unique name of the room.  Used as a lookup to refer to
    /// this location.
    name: String,

    /// The size of the room.
    shape: Rectangle,

    /// Tiles that connect to other rooms.  Standing on a warp moves
    /// the player to the room/location specified by the warp.
    warps: Vec<Warp>,

    resource_clumps: Vec<ResourceClump>,

    water_tiles: Option<(usize, usize, Vec<bool>)>,
}

struct Warp {
    location: Tile,
    target: Tile,
    target_room: String,
}

struct ResourceClump {
    location: Position,
    shape: Rectangle,
    kind: ResourceClumpKind,
}

enum ResourceClumpKind {
    Stump,
    Boulder,
    Meterorite,
    MineBoulder,
}

struct Rectangle {
    width: usize,
    height: usize,
}

struct Tile {
    right: isize,
    down: isize,
}

struct Position {
    right: f32,
    down: f32,
}

impl PathfindingUI {
    pub fn new(reader: CachedReader) -> Result<Self, Error> {
        let location_list = "StardewValley.Game1.game1\
                             ._locations\
                             .as<\
                               System.Collections.Generic\
                               .List`1<StardewValley.GameLocation>\
                             >()";

        let current_location = "StardewValley.Game1._player\
                                .currentLocationRef\
                                .locationName\
                                .value";

        let player_x = "StardewValley.Game1._player.position.Field.value.X";
        let player_y = "StardewValley.Game1._player.position.Field.value.Y";

        let res = {
            let mut graph = SymbolicGraph::new();
            [
                format!("{location_list}._size").as_str(),
                current_location,
                player_x,
                player_y,
            ]
            .into_iter()
            .try_for_each(|expr| -> Result<_, Error> {
                let expr = graph.parse(expr, reader)?;
                graph.mark_output(expr);
                Ok(())
            })?;
            let vm = graph.compile(reader)?;
            let res = vm.evaluate(reader)?;

            res
        };
        let num_locations = res[0].unwrap().as_usize()?;
        let current_location = res[1].unwrap().read_string_ptr(&reader)?;
        let player_x: f32 = res[2].unwrap().try_into()?;
        let player_y: f32 = res[3].unwrap().try_into()?;

        /// Helper struct to hold the per-location fields that are
        /// read out for every location.  Fields within a location
        /// that depend on one of these values are read out
        /// later. (e.g. Reading the warps once it is known how many
        /// warps are present.)
        struct StaticLocationFields {
            index: usize,
            name: String,
            shape: Rectangle,
            num_warps: usize,
            num_resource_clumps: usize,
            water_tiles_shape: Option<(usize, usize)>,
        }

        const FIELDS_PER_LOCATION: usize = 7;

        let static_location_fields: Vec<StaticLocationFields> = {
            let mut graph = SymbolicGraph::new();

            let location_list =
                graph.parse(&format!("{location_list}._items"), reader)?;
            (0..num_locations).for_each(|i| {
                let location = graph.access_index(location_list, i);

                let name = graph.access_field(location, "name.value");

                let (width, height) = {
                    let field =
                        graph.access_field(location, "map.m_layers._items");
                    let field = graph.access_index(field, 0);
                    let field = graph.access_field(field, "m_layerSize");

                    let width = graph.access_field(field, "Width");
                    let height = graph.access_field(field, "Height");

                    (width, height)
                };

                let num_warps =
                    graph.access_field(location, "warps.count.value");

                let num_resource_clumps =
                    graph.access_field(location, "resourceClumps.list._size");

                let water_tiles = {
                    let field = graph.access_field(location, "waterTiles");
                    let field = graph.downcast(
                        field,
                        SymbolicType {
                            namespace: Some("StardewValley".into()),
                            name: "WaterTiles".into(),
                            generics: Vec::new(),
                        },
                    );
                    graph.access_field(field, "waterTiles")
                };
                let water_tiles_width = graph.array_extent(water_tiles, 0);
                let water_tiles_height = graph.array_extent(water_tiles, 1);

                let fields: [_; FIELDS_PER_LOCATION] = [
                    name,
                    width,
                    height,
                    num_warps,
                    num_resource_clumps,
                    water_tiles_width,
                    water_tiles_height,
                ];
                fields.into_iter().for_each(|expr| {
                    graph.mark_output(expr);
                });
            });

            let vm = graph.compile(reader)?;
            let values = vm.evaluate(reader)?;

            values
                .chunks_exact(FIELDS_PER_LOCATION)
                .enumerate()
                .map(|(index, chunk)| {
                    let mut next = {
                        let mut iter_chunk = chunk.iter();
                        move || iter_chunk.next().unwrap()
                    };

                    let name = next().unwrap().read_string_ptr(&reader)?;
                    let shape = {
                        let width = next().unwrap().as_usize()?;
                        let height = next().unwrap().as_usize()?;
                        Rectangle { width, height }
                    };

                    let num_warps = next().unwrap().as_usize()?;

                    let num_resource_clumps = next().unwrap().as_usize()?;

                    let water_tiles_shape = {
                        let width = next();
                        let height = next();
                        if width.is_some() && height.is_some() {
                            let width = width.unwrap().as_usize()?;
                            let height = height.unwrap().as_usize()?;

                            Some((width, height))
                        } else {
                            None
                        }
                    };

                    Ok(StaticLocationFields {
                        index,
                        name,
                        shape,
                        num_warps,
                        num_resource_clumps,
                        water_tiles_shape,
                    })
                })
                .collect::<Result<Vec<_>, Error>>()?
        };

        let mut iter_values_within_location = {
            let mut graph = SymbolicGraph::new();

            let location_list =
                graph.parse(&format!("{location_list}._items"), reader)?;

            static_location_fields.iter().for_each(|loc| {
                let location = graph.access_index(location_list, loc.index);

                let warps = graph.access_field(
                    location,
                    "warps.array.value.elements._items",
                );

                for i_warp in 0..loc.num_warps {
                    let warp = {
                        let element = graph.access_index(warps, i_warp);
                        graph.access_field(element, "value")
                    };

                    ["x", "y", "targetX", "targetY", "targetName"]
                        .into_iter()
                        .for_each(|name| {
                            let field = graph.access_field(warp, name);
                            let field = graph.access_field(field, "value");
                            graph.mark_output(field);
                        });
                }

                let resource_clumps =
                    graph.access_field(location, "resourceClumps.list._items");

                for i_clump in 0..loc.num_resource_clumps {
                    let clump = graph.access_index(resource_clumps, i_clump);

                    let right = graph.access_field(clump, "netTile.value.X");
                    let down = graph.access_field(clump, "netTile.value.Y");
                    let width = graph.access_field(clump, "width.value");
                    let height = graph.access_field(clump, "height.value");

                    let kind_index =
                        graph.access_field(clump, "parentSheetIndex.value");

                    [right, down, width, height, kind_index]
                        .into_iter()
                        .for_each(|expr| {
                            graph.mark_output(expr);
                        });
                }

                let water_tiles =
                    graph.access_field(location, "waterTiles.waterTiles");
                if let Some((width, height)) = loc.water_tiles_shape {
                    (0..width).cartesian_product(0..height).for_each(
                        |(i, j)| {
                            let element =
                                graph.access_indices(water_tiles, vec![i, j]);
                            let element =
                                graph.access_field(element, "isWater");
                            graph.mark_output(element);
                        },
                    );
                }
            });
            let vm = graph.compile(reader)?;
            let values = vm.evaluate(reader)?;
            values.into_iter()
        };

        let mut next_value = || {
            iter_values_within_location.next().expect(
                "Should have generated sufficient values for all fields",
            )
        };

        let locations = static_location_fields
            .into_iter()
            .map(|loc| -> Result<_, Error> {
                let warps = (0..loc.num_warps)
                    .map(|_| -> Result<_, Error> {
                        let x = next_value().unwrap();
                        let y = next_value().unwrap();
                        let target_x = next_value().unwrap();
                        let target_y = next_value().unwrap();
                        let target_name = next_value().unwrap();
                        let location = {
                            let right = x.as_isize()?;
                            let down = y.as_isize()?;
                            Tile { right, down }
                        };
                        let target = {
                            let right = target_x.as_isize()?;
                            let down = target_y.as_isize()?;
                            Tile { right, down }
                        };
                        let target_room =
                            target_name.read_string_ptr(&reader)?;
                        Ok(Warp {
                            location,
                            target,
                            target_room,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let resource_clumps = (0..loc.num_resource_clumps)
                    .map(|_| -> Result<_, Error> {
                        let location = {
                            let right = next_value().unwrap().try_into()?;
                            let down = next_value().unwrap().try_into()?;
                            Position { right, down }
                        };
                        let shape = {
                            let width = next_value().unwrap().as_usize()?;
                            let height = next_value().unwrap().as_usize()?;
                            Rectangle { width, height }
                        };

                        let kind: ResourceClumpKind = {
                            let index: i32 =
                                next_value().unwrap().try_into()?;
                            index.try_into()?
                        };

                        Ok(ResourceClump {
                            location,
                            shape,
                            kind,
                        })
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let water_tiles = loc
                    .water_tiles_shape
                    .map(|(width, height)| -> Result<_, Error> {
                        let is_water = (0..width)
                            .cartesian_product(0..height)
                            .map(|(_i, _j)| next_value().unwrap().try_into())
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok((width, height, is_water))
                    })
                    .transpose()?;

                Ok(GameLocation {
                    name: loc.name,
                    shape: loc.shape,
                    warps,
                    resource_clumps,
                    water_tiles,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            locations,
            current_location,
            position: Position {
                right: player_x,
                down: player_y,
            },
        })
    }
}

impl WidgetWindow<Error> for PathfindingUI {
    fn title(&self) -> std::borrow::Cow<str> {
        "Pathfinding".into()
    }

    fn draw<'a>(
        &'a mut self,
        _globals: &'a tui_utils::TuiGlobals,
        area: ratatui::layout::Rect,
        buf: &mut ratatui::prelude::Buffer,
    ) {
        let (left_column, draw_area) = area.split_from_left(30);

        let (top_area, table_area) = left_column.split_from_top(2);

        let longest_name = self
            .locations
            .iter()
            .map(|loc| loc.name.len())
            .max()
            .unwrap_or(0);

        let loc_rows = self.locations.iter().map(|loc| {
            let name = Cell::new(loc.name.as_str());
            let shape = Cell::new(format!(
                "({}, {})",
                loc.shape.width, loc.shape.height
            ));
            Row::new([name, shape])
        });

        let water_rows = self.locations.iter().flat_map(|loc| {
            let name = Cell::new(loc.name.as_str());
            loc.water_tiles
                .as_ref()
                .into_iter()
                .flat_map(|(width, height, is_water)| {
                    assert!(width * height == is_water.len());
                    is_water
                        .iter()
                        .enumerate()
                        .filter(|(_, is_water)| **is_water)
                        .map(move |(index, _)| {
                            let i = index / width;
                            let j = index % width;
                            Cell::new(format!(
                                "({i},{j}) of ({width},{height})"
                            ))
                        })
                })
                .map(move |right| Row::new([name.clone(), right]))
        });

        let warp_rows = self.locations.iter().flat_map(|loc| {
            let name = Cell::new(loc.name.as_str());

            loc.warps.iter().flat_map(move |warp| {
                [
                    Row::new([
                        name.clone(),
                        Cell::new("x"),
                        Cell::new(format!("{}", warp.location.right)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("y"),
                        Cell::new(format!("{}", warp.location.down)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_x"),
                        Cell::new(format!("{}", warp.target.right)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_y"),
                        Cell::new(format!("{}", warp.target.down)),
                    ]),
                    Row::new([
                        name.clone(),
                        Cell::new("target_name"),
                        Cell::new(format!("{}", warp.target_room)),
                    ]),
                ]
            })
        });

        let rows = std::iter::empty()
            .chain(loc_rows)
            .chain(water_rows)
            .chain(warp_rows);

        let table = Table::new(
            rows,
            [
                Constraint::Min(longest_name as u16),
                Constraint::Min(10),
                Constraint::Min(longest_name as u16),
                Constraint::Percentage(100),
            ],
        );

        table.render(table_area, buf);

        let top_text = Text::raw(format!(
            "Current: {} ({}, {}) ({}, {})",
            self.current_location,
            self.position.right,
            self.position.down,
            self.position.right / 64.0,
            self.position.down / 64.0,
        ));
        top_text.render(top_area, buf);

        let current_room = self
            .locations
            .iter()
            .find(|loc| loc.name == self.current_location);
        if let Some(loc) = current_room {
            Canvas::default()
                .block(Block::new().title(loc.name.as_ref()))
                .marker(Marker::HalfBlock)
                .x_bounds([0.0, loc.shape.width as f64])
                .y_bounds([0.0, loc.shape.height as f64])
                .paint(|ctx| {
                    if let Some((width, height, tiles)) = &loc.water_tiles {
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

                    {
                        loc.resource_clumps
                            .iter()
                            .map(|clump| ratatui::widgets::canvas::Rectangle {
                                x: clump.location.right as f64,
                                y: (loc.shape.height as f64)
                                    - (clump.location.down as f64),
                                width: clump.shape.width as f64,
                                height: clump.shape.height as f64,
                                color: match clump.kind {
                                    ResourceClumpKind::Stump => Color::Yellow,
                                    ResourceClumpKind::Boulder => {
                                        Color::DarkGray
                                    }
                                    ResourceClumpKind::Meterorite => {
                                        Color::Magenta
                                    }
                                    ResourceClumpKind::MineBoulder => {
                                        Color::Gray
                                    }
                                },
                            })
                            .for_each(|rect| ctx.draw(&rect));
                    }
                })
                .render(draw_area, buf);
        }
    }
}

impl TryFrom<i32> for ResourceClumpKind {
    type Error = Error;

    fn try_from(value: i32) -> Result<Self, Self::Error> {
        match value {
            600 | 602 => Ok(Self::Stump),
            672 => Ok(Self::Boulder),
            622 => Ok(Self::Meterorite),
            752 | 754 | 756 | 758 => Ok(Self::MineBoulder),

            other => Err(Error::UnrecognizedResourceClump(other)),
        }
    }
}
