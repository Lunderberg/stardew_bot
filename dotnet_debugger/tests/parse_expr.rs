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

    println!("----------- Parsed -------------\n{parsed}");
    println!("----------- Expected -------------\n{expected}");

    assert!(parsed
        .graph_comparison(&expected)
        .order_dependent(true)
        .compare_names(true)
        .apply());
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
    require_identical_graph("class_name.static_field.as::<Ptr>()", |graph| {
        let obj = graph.static_field("class_name", "static_field");
        graph.prim_cast(obj, RuntimePrimType::Ptr);
    });
}

#[test]
fn parse_shared_expression() {
    // If an expression is shared, it will be printed as a variable
    // binding, followed by usage of that binding.  These anonymous
    // variables consist of an underscore followed by an instruction
    // index.  This pattern should be recognized, and should not
    // produce named nodes in the parsed graph.
    require_identical_graph(
        "let _0 = class_name.field_name;\n\
         let _1 = _0.active_slot;\n\
         _0.items[_1]\
         ",
        |graph| {
            let inventory = graph.static_field("class_name", "field_name");
            let index = graph.access_field(inventory, "active_slot");
            let items = graph.access_field(inventory, "items");
            graph.access_index(items, index);
        },
    );
}

#[test]
fn parse_named_expression() {
    // An expression may have an explicit name.  When parsing a
    // variable name, any name that is not an anonymous variable
    // should result in a named node in the parsed graph.
    require_identical_graph(
        "let _0 = class_name.field_name;\n\
         let index = _0.active_slot;\n\
         _0.items[index]\
         ",
        |graph| {
            let inventory = graph.static_field("class_name", "field_name");
            let index = graph.access_field(inventory, "active_slot");
            graph.name(index, "index").unwrap();
            let items = graph.access_field(inventory, "items");
            graph.access_index(items, index);
        },
    );
}

#[test]
fn parse_named_expressions_with_same_name() {
    // Variable names are for human readability, and may be repeated
    // within an expression.  To avoid ambiguity when printing, these
    // duplicate variable names are prefixed with `_{i}_`.  When
    // parsing, this prefix should be stripped from the variable name.
    require_identical_graph(
        "let _0_obj = class_name.static_field;\n\
         let _1_obj = _0_obj.instance_field0;\n\
         let _2_obj = _1_obj.instance_field1;\n\
         _2_obj.instance_field2\
         ",
        |graph| {
            let obj = graph.static_field("class_name", "static_field");
            graph.name(obj, "obj").unwrap();
            let obj = graph.access_field(obj, "instance_field0");
            graph.name(obj, "obj").unwrap();
            let obj = graph.access_field(obj, "instance_field1");
            graph.name(obj, "obj").unwrap();
            graph.access_field(obj, "instance_field2");
        },
    );
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

#[test]
fn parse_top_level_function() {
    require_identical_graph("pub fn main() { 42 }", |graph| {
        let func = graph.function_def(vec![], 42.into());
        graph.name(func, "main").unwrap();
        graph.mark_extern_func(func).unwrap();
    });
}

#[test]
fn parse_function_with_arg() {
    require_identical_graph(
        "let arr = class_name.static_field;
         fn main(index: usize) { arr[index] }",
        |graph| {
            let arr = graph.static_field("class_name", "static_field");
            graph.name(arr, "arr").unwrap();
            let index = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(index, "index").unwrap();
            let item = graph.access_index(arr, index);
            let func = graph.function_def(vec![index], item);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_function_with_multiple_returns() {
    require_identical_graph(
        "let arr1 = class_name.static_field1;
         let arr2 = class_name.static_field2;
         fn main(index: usize) {
               (arr1[index], arr2[index])
        }",
        |graph| {
            let arr1 = graph.static_field("class_name", "static_field1");
            graph.name(arr1, "arr1").unwrap();
            let arr2 = graph.static_field("class_name", "static_field2");
            graph.name(arr2, "arr2").unwrap();
            let index = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(index, "index").unwrap();
            let item1 = graph.access_index(arr1, index);
            let item2 = graph.access_index(arr2, index);

            let tuple = graph.tuple(vec![item1, item2]);
            let func = graph.function_def(vec![index], tuple);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_addition() {
    require_identical_graph("fn main(a: usize, b: usize) { a+b }", |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let sum = graph.add(a, b);
        let func = graph.function_def(vec![a, b], sum);
        graph.name(func, "main").unwrap();
    });
}

#[test]
fn parse_multiplication() {
    require_identical_graph("fn main(a: usize, b: usize) { a*b }", |graph| {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();
        let prod = graph.mul(a, b);
        let func = graph.function_def(vec![a, b], prod);
        graph.name(func, "main").unwrap();
    });
}

#[test]
fn parse_repeated_addition() {
    require_identical_graph(
        "fn main(a: usize, b: usize) { a+b+a+b }",
        |graph| {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let sum = graph.add(a, b);
            let sum = graph.add(sum, a);
            let sum = graph.add(sum, b);

            let func = graph.function_def(vec![a, b], sum);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_repeated_multiplication() {
    require_identical_graph(
        "fn main(a: usize, b: usize) { a*b*a*b }",
        |graph| {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let prod = graph.mul(a, b);
            let prod = graph.mul(prod, a);
            let prod = graph.mul(prod, b);

            let func = graph.function_def(vec![a, b], prod);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_mixed_operators_with_precedence() {
    require_identical_graph(
        "fn main(a: usize, b: usize) { a*b + b*a }",
        |graph| {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let lhs = graph.mul(a, b);
            let rhs = graph.mul(b, a);
            let sum = graph.add(lhs, rhs);

            let func = graph.function_def(vec![a, b], sum);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_mixed_operators_with_parentheses() {
    require_identical_graph(
        "fn main(a: usize, b: usize) { a + (a+b)*(b+a) }",
        |graph| {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let lhs = graph.add(a, b);
            let rhs = graph.add(b, a);
            let prod = graph.mul(lhs, rhs);
            let sum = graph.add(a, prod);

            let func = graph.function_def(vec![a, b], sum);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_mixed_operators_with_leading_parentheses() {
    require_identical_graph(
        "fn main(a: usize, b: usize) { (a+b)*(b+a) }",
        |graph| {
            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let lhs = graph.add(a, b);
            let rhs = graph.add(b, a);
            let prod = graph.mul(lhs, rhs);

            let func = graph.function_def(vec![a, b], prod);
            graph.name(func, "main").unwrap();
        },
    );
}

#[test]
fn parse_function_call() {
    require_identical_graph(
        "
        fn multiply(lhs: usize, rhs: usize) { lhs*rhs }
        fn main(a: usize, b: usize) { multiply(a+b, b+a) }
        ",
        |graph| {
            let lhs = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(lhs, "lhs").unwrap();
            let rhs = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(rhs, "rhs").unwrap();
            let prod = graph.mul(lhs, rhs);
            let multiply = graph.function_def(vec![lhs, rhs], prod);
            graph.name(multiply, "multiply").unwrap();

            let a = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(a, "a").unwrap();
            let b = graph.function_arg(RuntimePrimType::NativeUInt);
            graph.name(b, "b").unwrap();

            let lhs = graph.add(a, b);
            let rhs = graph.add(b, a);
            let call = graph.function_call(multiply, vec![lhs, rhs]);

            let main = graph.function_def(vec![a, b], call);
            graph.name(main, "main").unwrap();
        },
    );
}

#[test]
fn parse_fragment_using_global_variable() {
    // Named global variable may be used within a graph.parse() call.
    require_identical_graph(
        "
        let var = 1+2;
        pub fn main() { var + 3 }
        ",
        |graph| {
            let var = graph.add(1, 2);
            graph.name(var, "var").unwrap();

            graph.parse("pub fn main() { var + 3 }").unwrap();
        },
    );
}

#[test]
fn parse_static_range() {
    require_identical_graph("(0..42)", |graph| {
        graph.range(42);
    });
}

#[test]
fn parse_dynamic_range() {
    require_identical_graph("(0..5+10)", |graph| {
        let extent = graph.add(5, 10);
        graph.range(extent);
    });
}

#[test]
fn non_zero_start_of_range_produces_error() {
    let mut graph = SymbolicGraph::new();
    let res = graph.parse("(1..42)");
    assert!(res.is_err());
}
