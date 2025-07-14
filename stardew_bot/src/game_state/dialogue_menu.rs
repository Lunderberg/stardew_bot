use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::{Rectangle, Vector};

use crate::Error;

#[derive(RustNativeObject, Debug, Clone)]
pub struct DialogueMenu {
    pub pixel_location: Rectangle<isize>,
    pub responses: Vec<Response>,
}

#[derive(RustNativeObject, Debug, Clone)]
pub struct Response {
    pub pixel: Vector<isize>,
    pub text: String,
}

impl DialogueMenu {
    pub(crate) fn def_read_dialogue_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_response",
            |pixel: &Vector<isize>, text: &str| Response {
                pixel: pixel.clone(),
                text: text.to_string(),
            },
        )?;

        graph.named_native_function(
            "new_dialogue_menu",
            |x: isize,
             y: isize,
             width: isize,
             height: isize,
             responses: &Vec<Response>| DialogueMenu {
                pixel_location: Rectangle {
                    top_left: Vector::new(x, y),
                    shape: Vector::new(width, height),
                },
                responses: responses.clone(),
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

                let num_responses = menu
                    .responseCC
                    ._size
                    .prim_cast::<usize>();
                let responses = (0..num_responses)
                    .map(|i| {
                        let pixel = center_of_gui_rect(
                            menu
                                .responseCC
                                ._items[i]
                        );
                        let text = menu
                            .responses[i]
                            .responseKey
                            .read_string();

                        new_response(pixel, text)
                    })
                    .collect();

                if menu.is_some(){
                    new_dialogue_menu(
                        x, y, width, height,
                        responses,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }

    pub fn response_pixel(&self, text: &str) -> Option<Vector<isize>> {
        self.responses
            .iter()
            .find(|response| response.text == text)
            .map(|response| response.pixel)
    }
}
