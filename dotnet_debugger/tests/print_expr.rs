use dotnet_debugger::{SymbolicGraph, SymbolicValue};

fn check_printed_expr(
    expected: &str,
    graph_builder: impl Fn(&mut SymbolicGraph) -> SymbolicValue,
) {
    let mut graph = SymbolicGraph::new();
    let value = graph_builder(&mut graph);

    let printed = format!("{}", graph.print(&value)).replace('\u{200B}', "");

    assert_eq!(printed, expected);
}

#[test]
fn print_static_field() {
    check_printed_expr("class_name.field_name", |graph| {
        graph.static_field("class_name", "field_name")
    });
}

#[test]
fn print_array_access() {
    check_printed_expr(
        "class_name.field_name[\
         123.prim_cast::<usize>()]",
        |graph| {
            let obj = graph.static_field("class_name", "field_name");
            graph.access_index(obj, 123)
        },
    );
}

#[test]
fn print_multi_dim_array_access() {
    check_printed_expr(
        "class_name.field_name[\
         123.prim_cast::<usize>(), \
         456.prim_cast::<usize>()]",
        |graph| {
            let obj = graph.static_field("class_name", "field_name");
            graph.access_indices(obj, [123, 456])
        },
    );
}

#[test]
fn print_array_length() {
    check_printed_expr("class_name.field_name.len()", |graph| {
        let obj = graph.static_field("class_name", "field_name");
        graph.num_array_elements(obj)
    });
}

#[test]
fn print_multi_dim_array_extent() {
    check_printed_expr("class_name.field_name.extent(0)", |graph| {
        let obj = graph.static_field("class_name", "field_name");
        graph.array_extent(obj, 0)
    });
}
