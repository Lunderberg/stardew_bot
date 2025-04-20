use dotnet_debugger::{RuntimePrimType, SymbolicGraph};
use indoc::indoc;
use paste::paste;

#[test]
fn print_expanded_graph() {
    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{}", graph.printer().expand_all_expressions());
    let expected = indoc! {"
        let _0 = class_name.field_name;
        let _1 = _0.subfield;
        let _2 = _1.active_index;
        let _3 = _1.list;
        let _4 = _3._items;
        let _5 = _4[_2];
        pub fn main() { _5 }"
    };

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
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
       let _1 = class_name.field_name.subfield;
       let _5 = _1.list._items[_1.active_index];
       pub fn main() { _5 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

macro_rules! paren_test {
    ($left_op: ident,
     $right_op: ident,
     $expected_left_first:expr,
     $expected_right_first:expr $(,)?
    ) => {
        paste! {
            #[test]
            fn [<
                paren_with_lhs_ $left_op _then_rhs_ $right_op
                >] () {
                let mut graph = SymbolicGraph::new();

                let a = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(a, "a").unwrap();
                let b = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(b, "b").unwrap();
                let c = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(c, "c").unwrap();

                let partial = graph.$left_op(a, b);
                let ret = graph.$right_op(partial, c);

                let func = graph.function_def(vec![a, b, c], ret);
                graph.name(func, "main").unwrap();
                graph.mark_extern_func(func).unwrap();

                let printed = format!("{graph}");
                let expected = $expected_left_first;
                let expected = format!(
                    "pub fn main(a: usize, b: usize, c: usize) {{ {expected} }}"
                );

                println!("-------------- Expected --------------\n{expected}");
                println!("-------------- Actual   --------------\n{printed}");

                assert_eq!(printed, expected);
            }


            #[test]
            fn [<
                paren_with_rhs_ $left_op _then_lhs_ $right_op
                >] () {
                let mut graph = SymbolicGraph::new();

                let a = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(a, "a").unwrap();
                let b = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(b, "b").unwrap();
                let c = graph.function_arg(RuntimePrimType::NativeUInt);
                graph.name(c, "c").unwrap();

                let partial = graph.$right_op(b, c);
                let ret = graph.$left_op(a, partial);

                let func = graph.function_def(vec![a, b, c], ret);
                graph.name(func, "main").unwrap();
                graph.mark_extern_func(func).unwrap();

                let printed = format!("{graph}");
                let expected = $expected_right_first;
                let expected = format!(
                    "pub fn main(a: usize, b: usize, c: usize) {{ {expected} }}"
                );

                println!("-------------- Expected --------------\n{expected}");
                println!("-------------- Actual   --------------\n{printed}");

                assert_eq!(printed, expected);
            }
        }
    };
}

paren_test!(mul, add, "a*b + c", "a*(b + c)");
paren_test!(add, mul, "(a + b)*c", "a + b*c");
paren_test!(add, equal, "a + b == c", "a + (b == c)");
paren_test!(equal, add, "(a == b) + c", "a == b + c");
paren_test!(boolean_and, boolean_or, "(a && b) || c", "a && (b || c)");
