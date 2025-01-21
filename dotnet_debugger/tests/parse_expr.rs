use dotnet_debugger::{RuntimePrimType, SymbolicGraph};

fn require_identical_graph(
    string: &str,
    graph_builder: impl Fn(&mut SymbolicGraph),
) {
    let parsed = {
        let mut graph = SymbolicGraph::new();
        graph.parse(string).unwrap();
        graph
    };

    let expected = {
        let mut graph = SymbolicGraph::new();
        graph_builder(&mut graph);
        graph
    };

    // TODO: Improve this method so that it doesn't require operations
    // to appear in the same order.  Maybe walk backwards from the
    // outputs, tracking which expressions are re-used (since the
    // presence or absence of re-use has semantic meaning).
    assert!(parsed.is_equivalent_to(&expected));
}

#[test]
fn parse_static_field() {
    require_identical_graph("class_name.field_name", |graph| {
        graph.static_field("class_name", "field_name");
    });
}

#[test]
fn parse_downcast() {
    require_identical_graph(
        "class_name.field_name.as::<namespace.class_name2>()",
        |graph| {
            let obj = graph.static_field("class_name", "field_name");
            graph.downcast(obj, "namespace.class_name2");
        },
    );
}

#[test]
fn parse_field_access() {
    // Technically, it's ambiguous whether this is a class named
    // "class_name", with static field "static_field", and subfields
    // "a.b.c", or a class named "b" inside namespace
    // "class_name.static_field.a" with static field "c".  To keep the
    // parsing independent of the types that exist in the remote
    // process, this ambiguity is resolved later.
    require_identical_graph("class_name.static_field.a.b.c", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        let a = graph.access_field(obj, "a");
        let b = graph.access_field(a, "b");
        graph.access_field(b, "c");
    });
}

#[test]
fn parse_static_index_access() {
    require_identical_graph("class_name.static_field[123]", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.access_index(obj, 123);
    });
}

#[test]
fn parse_dynamic_index_access() {
    require_identical_graph(
        "class_name.static_field[class2.field2]",
        |graph| {
            let obj = graph.static_field("class_name", "static_field");
            let index = graph.static_field("class2", "field2");
            graph.access_index(obj, index);
        },
    );
}

#[test]
fn parse_multi_dim_array_access() {
    require_identical_graph(
        "class_name.static_field[123, 456, 789]",
        |graph| {
            let obj = graph.static_field("class_name", "static_field");
            graph.access_indices(obj, [123, 456, 789]);
        },
    );
}

#[test]
fn parse_num_array_elements() {
    require_identical_graph("class_name.static_field.len()", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.num_array_elements(obj);
    });
}

#[test]
fn parse_array_extent_static_dim() {
    require_identical_graph("class_name.static_field.extent(0)", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.array_extent(obj, 0);
    });
}

#[test]
fn parse_array_extent_dynamic_dim() {
    require_identical_graph(
        "class_name.static_field.extent(class2.field2)",
        |graph| {
            let obj = graph.static_field("class_name", "static_field");
            let dim = graph.static_field("class2", "field2");
            graph.array_extent(obj, dim);
        },
    );
}

#[test]
fn parse_array_prim_cast() {
    // TODO: Parse prim_cast
    require_identical_graph("class_name.static_field.as::<Ptr>()", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.prim_cast(obj, RuntimePrimType::Ptr);
    });
}

// #[test]
// fn parse_array_ptr_cast() {
//     // TODO: Parse prim_cast
//     require_identical_graph(
//         "class_name.static_field.prim_cast::<Ptr>().ptr_cast::<class2>()",
//         |graph| {
//             let obj = graph.static_field("class_name", "static_field");
//             let ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
//             graph.pointer_cast(ptr, "class2");
//         },
//     );
// }
