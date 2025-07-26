use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::Vector;

use crate::{Error, Item};

#[derive(RustNativeObject, Debug, Clone)]
pub struct GeodeMenu {
    pub player_item_locations: Vec<Vector<isize>>,
    pub held_item: Option<Item>,
    pub crack_geode_button: Vector<isize>,
    pub is_cracking_geode: bool,
    pub ok_button: Vector<isize>,
}

impl GeodeMenu {
    pub(crate) fn def_read_geode_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_geode_menu",
            |player_item_locations: &Vec<Vector<isize>>,
             held_item: Option<&Item>,
             crack_geode_button: &Vector<isize>,
             is_cracking_geode: bool,
             ok_button: &Vector<isize>| GeodeMenu {
                player_item_locations: player_item_locations.clone(),
                held_item: held_item.cloned(),
                crack_geode_button: crack_geode_button.clone(),
                is_cracking_geode,
                ok_button: ok_button.clone(),
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_geode_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.GeodeMenu>();

                let num_player_slots = menu
                    .inventory
                    .capacity
                    .prim_cast::<usize>();
                let player_item_locations = (0..num_player_slots)
                        .map(|i| menu.inventory.inventory._items[i])
                        .map(|tile| center_of_gui_rect(tile))
                        .collect();

                let held_item = read_item(menu._heldItem);

                let crack_geode_button = center_of_gui_rect(
                    menu.geodeSpot
                );

                let is_cracking_geode = menu
                    .geodeSpot
                    .item
                    .is_some();

                let ok_button = center_of_gui_rect(
                    menu.okButton
                );

                if menu.is_some(){
                    new_geode_menu(
                        player_item_locations,
                        held_item,
                        crack_geode_button,
                        is_cracking_geode,
                        ok_button,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
