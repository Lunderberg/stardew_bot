use dotnet_debugger::SymbolicGraph;

#[test]
fn print_expanded_graph() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    graph.mark_output(item);

    let printed = format!("{}", graph.printer().expand_all_expressions());
    let expected = "\
    [0] <- class_name.field_name\n\
    [1] <- [0].subfield\n\
    [2] <- [1].active_index\n\
    [3] <- [1].list\n\
    [4] <- [3]._items\n\
    [5] <- [2].prim_cast::<usize>()\n\
    [6] (output #0) <- [4][[5]]\n\
    ";

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_compact_graph() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    graph.mark_output(item);

    let printed = format!("{graph}");
    let expected = "\
    [1] <- class_name.field_name.subfield\n\
    [6] (output #0) <- [1].list._items[[1].active_index.prim_cast::<usize>()]\n\
    ";

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}
