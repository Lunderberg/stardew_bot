use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::{Rectangle, Vector};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DisplayState {
    pub viewport: Rectangle<isize>,
    pub zoom_level: f32,
}

impl DisplayState {
    pub(crate) fn def_read_display_state(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_display_state",
            |x: isize,
             y: isize,
             width: isize,
             height: isize,
             zoom_level: f32| {
                let viewport = Rectangle {
                    top_left: Vector::new(x, y),
                    shape: Vector::new(width, height),
                };
                DisplayState {
                    viewport,
                    zoom_level,
                }
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_display_state() {
                let viewport = StardewValley.Game1.viewport;
                let viewport_loc = viewport.Location;
                let x = viewport_loc.X;
                let y = viewport_loc.Y;

                let viewport_size = viewport.Size;
                let width = viewport_size.Width;
                let height = viewport_size.Height;

                let zoom_level = {
                    let base = StardewValley.Game1
                        .game1
                        .instanceOptions
                        .baseZoomLevel;
                    let modifier = StardewValley.Game1
                        .game1
                        .zoomModifier;
                    base*modifier
                };

                new_display_state(
                    x, y, width, height,
                    zoom_level,
                )
            }
        })?;

        Ok(func)
    }

    pub fn tile_to_pixels(&self, tile: Vector<isize>) -> Rectangle<isize> {
        let top_left = (tile * 64 - self.viewport.top_left).map(|x| {
            let x = x as f32;
            let x = x * self.zoom_level;
            
            x as isize
        });

        let size = (64.0 * self.zoom_level) as isize;
        let shape = Vector::new(size, size);
        Rectangle { top_left, shape }
    }

    pub fn center_pixel_of_tile(&self, tile: Vector<isize>) -> Vector<isize> {
        let tile_rect = self.tile_to_pixels(tile);
        tile_rect.center()
    }

    /// The location of the "Skip" button for cutscenes
    ///
    /// It doesn't look like there is an explicit skippable button
    /// with pixel locations used for cutscenes.  Instead, the bounds
    /// of the button are dynamically generated as needed, based on
    /// the size of the view window.
    pub fn event_skip_button(&self) -> Rectangle<isize> {
        let scale = 4;
        let width = 22 * scale;
        let height = 15 * scale;

        let top_left = Vector::new(
            self.viewport.width() - width - 8,
            self.viewport.height() - 64,
        );
        let shape = Vector::new(width, height);

        Rectangle { top_left, shape }
    }
}
