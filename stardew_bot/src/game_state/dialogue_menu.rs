use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::{Rectangle, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct DialogueMenu {
    pub pixel_location: Rectangle<isize>,
}

impl DialogueMenu {
    pub(crate) fn def_read_dialogue_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_dialogue_menu",
            |x: isize, y: isize, width: isize, height: isize| DialogueMenu {
                pixel_location: Rectangle {
                    top_left: Vector::new(x, y),
                    shape: Vector::new(width, height),
                },
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_dialogue_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.DialogueBox>();

                let x = menu.x;
                let y = menu.y;
                let width = menu.width;
                let height = menu.height;

                if menu.is_some(){
                    new_dialogue_menu(x,y,width,height)
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
