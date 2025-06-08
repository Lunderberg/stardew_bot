use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};

use crate::Error;

use super::{Rectangle, Vector};

#[derive(RustNativeObject, Debug, Clone)]
pub struct MailMenu {
    pub pixel_location: Rectangle<isize>,
}

impl MailMenu {
    pub(crate) fn def_read_mail_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_mail_menu",
            |x: isize, y: isize, width: isize, height: isize| MailMenu {
                pixel_location: Rectangle {
                    top_left: Vector::new(x, y),
                    shape: Vector::new(width, height),
                },
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_mail_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.LetterViewerMenu>();

                let x = menu.xPositionOnScreen;
                let y = menu.yPositionOnScreen;
                let width = menu.width;
                let height = menu.height;

                if menu.is_some(){
                    new_mail_menu(
                        x, y,
                        width, height,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
