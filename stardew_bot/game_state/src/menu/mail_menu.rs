use dsl::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::{Rectangle, Vector};

use crate::Error;

use super::Menu;

#[derive(RustNativeObject, Debug, Clone)]
pub struct MailMenu {
    pub pixel_location: Rectangle<isize>,
    pub current_page: usize,
    pub num_pages: usize,
}

impl MailMenu {
    pub(crate) fn def_read_mail_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_mail_menu",
            |x: isize,
             y: isize,
             width: isize,
             height: isize,
             current_page: usize,
             num_pages: usize| {
                Menu::Mail(MailMenu {
                    pixel_location: Rectangle {
                        top_left: Vector::new(x, y),
                        shape: Vector::new(width, height),
                    },
                    current_page,
                    num_pages,
                })
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

                let current_page = menu.page;
                let num_pages = menu.mailMessage._size.prim_cast::<usize>();

                if menu.is_some(){
                    new_mail_menu(
                        x, y,
                        width, height,
                        current_page,
                        num_pages,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
