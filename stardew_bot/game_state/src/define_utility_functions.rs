use dotnet_debugger::SymbolicGraph;
use geometry::{Rectangle, Vector};

use crate::Error;

pub fn define_utility_functions(
    graph: &mut SymbolicGraph,
) -> Result<(), Error> {
    graph.named_native_function(
        "new_vector_isize",
        |right: isize, down: isize| Vector::<isize> { right, down },
    )?;

    graph.named_native_function(
        "new_rectangle",
        |right: isize,
         down: isize,
         width: isize,
         height: isize|
         -> Rectangle<isize> {
            Rectangle {
                top_left: Vector { right, down },
                shape: Vector {
                    right: width,
                    down: height,
                },
            }
        },
    )?;

    graph.named_native_function(
        "center_of_rectangle",
        |left: isize,
         top: isize,
         width: isize,
         height: isize|
         -> Vector<isize> {
            Vector::new(left + width / 2, top + height / 2)
        },
    )?;

    graph.parse(stringify! {
        fn center_of_gui_rect(obj) {
            let bounds = obj.bounds;
            center_of_rectangle(
                bounds.X,
                bounds.Y,
                bounds.Width,
                bounds.Height,
            )
        }
    })?;

    graph.parse(stringify! {
        fn get_item_locations(inventory_menu) {
            let num_slots = inventory_menu
                .capacity
                .prim_cast::<usize>();

            let item_locations = (0..num_slots)
                .map(|i| inventory_menu.inventory._items[i])
                .map(|tile| center_of_gui_rect(tile))
                .collect();

            item_locations
        }
    })?;

    Ok(())
}
