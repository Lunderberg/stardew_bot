use dotnet_debugger::{SymbolicGraph, SymbolicType, SymbolicValue};

fn check_printed_expr(
    expected: &str,
    graph_builder: impl Fn(&mut SymbolicGraph) -> SymbolicValue,
) {
    let mut graph = SymbolicGraph::new();
    let value = graph_builder(&mut graph);

    let printed = format!("{}", graph.print(value));

    assert_eq!(printed, expected);
}

#[test]
fn print_static_field_with_zwsp() {
    let mut graph = SymbolicGraph::new();
    let value = graph.static_field("class_name", "field_name");
    let printed = format!(
        "{}",
        graph.print(value).insert_zero_width_space_at_breakpoint()
    );
    let expected = "class_name\u{200B}.field_name";
    assert_eq!(printed, expected);
}

#[test]
fn print_downcast_with_zwsp() {
    let mut graph = SymbolicGraph::new();

    let value = {
        let obj = graph.static_field("class_name", "field_name");
        let field = graph.access_field(obj, "subfield");
        graph.downcast(
            field,
            SymbolicType {
                full_name: "other_class".into(),
                generics: vec!["arg1".into(), "arg2".into()],
            },
        )
    };
    let printed = format!(
        "{}",
        graph.print(value).insert_zero_width_space_at_breakpoint()
    );
    let expected = "class_name\u{200B}\
                    .field_name\u{200B}\
                    .subfield\
                    .as::<\
                    other_class<\u{200B}arg1, \u{200B}arg2>\
                    >()";
    assert_eq!(printed, expected);
}

#[test]
fn expression_printing_ignores_unreachable_nodes() {
    // When printing a specific expression, only inputs that
    // contribute to that expression should be printed.
    check_printed_expr(
        "class_name.static_field_name.instance_field_name",
        |graph| {
            let obj = graph.static_field("class_name", "static_field_name");
            let other_field =
                graph.access_field(obj, "other_instance_field_name");
            graph.access_field(other_field, "x");
            graph.access_field(other_field, "y");
            graph.access_field(obj, "instance_field_name")
        },
    )
}
