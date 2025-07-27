use std::collections::HashMap;

use dotnet_debugger::{RustNativeObject, SymbolicGraph, SymbolicValue};
use geometry::Vector;

use crate::{Error, Item};

use super::Menu;

#[derive(RustNativeObject, Debug, Clone)]
pub struct JunimoMenu {
    pub bundle_buttons: HashMap<i32, Vector<isize>>,
    pub current_active_bundle: Option<i32>,
    pub player_item_locations: Vec<Vector<isize>>,
    pub held_item: Option<Item>,
    pub present_button: Option<Vector<isize>>,
    pub back_button: Vector<isize>,
    pub exit_button: Vector<isize>,
}

#[derive(RustNativeObject, Debug, Clone)]
struct RawButton {
    bundle_index: i32,
    pixel: Vector<isize>,
}

impl JunimoMenu {
    pub(crate) fn def_read_junimo_menu(
        graph: &mut SymbolicGraph,
    ) -> Result<SymbolicValue, Error> {
        graph.named_native_function(
            "new_raw_button",
            |bundle_index: i32, pixel: &Vector<isize>| RawButton {
                bundle_index,
                pixel: *pixel,
            },
        )?;

        graph.named_native_function(
            "new_junimo_menu",
            |bundle_buttons: &Vec<RawButton>,
             current_active_bundle: Option<i32>,
             player_item_locations: &Vec<Vector<isize>>,
             held_item: Option<&Item>,
             present_button: Option<&Vector<isize>>,
             back_button: &Vector<isize>,
             exit_button: &Vector<isize>| {
                let bundle_buttons = bundle_buttons
                    .iter()
                    .map(|button| (button.bundle_index, button.pixel))
                    .collect();
                Menu::Junimo(JunimoMenu {
                    bundle_buttons,
                    current_active_bundle,
                    player_item_locations: player_item_locations.clone(),
                    held_item: held_item.cloned(),
                    present_button: present_button.cloned(),
                    back_button: *back_button,
                    exit_button: *exit_button,
                })
            },
        )?;

        let func = graph.parse(stringify! {
            fn read_junimo_menu() {
                let menu = StardewValley.Game1
                    ._activeClickableMenu
                    .as::<StardewValley.Menus.JunimoNoteMenu>();

                let bundle_buttons = {
                    let list = menu.bundles;
                    let count = list
                        ._size
                        .prim_cast::<usize>();
                    (0..count)
                        .map(|i| list._items[i])
                        .map(|entry| {
                            let bundle_index = entry.bundleIndex;
                            let pixel = center_of_gui_rect(entry);
                            new_raw_button (
                                bundle_index,
                                pixel,
                            )
                        })
                        .collect()
                };

                let current_active_bundle = if (
                    menu.specificBundlePage
                        && menu.currentPageBundle.is_some()
                ) {
                    menu
                        .currentPageBundle
                        .bundleIndex
                } else {
                    None
                };

                let player_item_locations = {
                    let inventory = menu.inventory;
                    let num_player_slots = inventory
                        .capacity
                        .prim_cast::<usize>();

                    (0..num_player_slots)
                        .map(|i| inventory.inventory._items[i])
                        .map(|tile| center_of_gui_rect(tile))
                        .collect()
                };

                let held_item = {
                    let item = menu.heldItem.as::<StardewValley.Item>();
                    if item.is_some() {
                        read_item(item)
                    } else {
                        None
                    }
                };

                let present_button = if menu.presentButton.is_some() {
                    center_of_gui_rect(menu.presentButton)
                } else {
                    None
                };
                let back_button = center_of_gui_rect(menu.backButton);
                let exit_button = center_of_gui_rect(menu.upperRightCloseButton);

                if menu.is_some(){
                    new_junimo_menu(
                        bundle_buttons,
                        current_active_bundle,
                        player_item_locations,
                        held_item,
                        present_button,
                        back_button,
                        exit_button,
                    )
                } else {
                    None
                }
            }
        })?;

        Ok(func)
    }
}
