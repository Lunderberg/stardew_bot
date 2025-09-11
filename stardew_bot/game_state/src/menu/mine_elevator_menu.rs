use dsl::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::Vector;

use crate::Error;

use super::Menu;

#[derive(RustNativeObject, Debug, Clone)]
pub struct MineElevatorMenu {
    pub buttons: Vec<Vector<isize>>,
}

impl MineElevatorMenu {
    pub(crate) fn def_read_mine_elevator_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_mine_elevator_menu",
            |buttons: &Vec<Vector<isize>>| {
                Menu::MineElevator(MineElevatorMenu {
                    buttons: buttons.clone(),
                })
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_mine_elevator_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.MineElevatorMenu>();

                let elevators = menu.elevators;
                let num_buttons = elevators
                    ._size
                    .prim_cast::<usize>();
                let elevator_buttons = (0..num_buttons)
                    .map(|i_button| elevators._items[i_button])
                    .map(|button| center_of_gui_rect(button))
                    .collect();

                if menu.is_some(){
                    new_mine_elevator_menu(
                        elevator_buttons
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
