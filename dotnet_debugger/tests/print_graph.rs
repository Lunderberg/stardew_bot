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

#[test]
fn print_named_object() {
    // An object may be given an explicit name.  That name should be
    // used when printing the SymbolicGraph.

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let obj = class_name.field_name.subfield;
        let _5 = obj.list._items[obj.active_index];
        pub fn main() { _5 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_named_intermediate_object() {
    // An intermediate expression may be given an explicit name.  Even
    // though the compact view would usually display these as inline
    // expressions, named intermediates should always be displayed.

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    graph.name(active_index, "index").unwrap();
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
       let obj = class_name.field_name.subfield;
       let index = obj.active_index;
       let _5 = obj.list._items[index];
       pub fn main() { _5 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_with_duplicate_names() {
    // Names are intended for ease of readability, and are not
    // required to be unique.  When printing, any non-unique names are
    // prefixed to ensure uniqueness.
    //
    // TODO: Strip out the prefix when parsing, so that the name
    //
    // TODO: Raise an error if the name provided would conflict with
    //       an reserved name.  (Reserving all names that start with
    //       an underscore, then a number.)

    let mut graph = SymbolicGraph::new();
    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    graph.name(subfield, "obj").unwrap();
    let active_index = graph.access_field(subfield, "active_index");
    graph.name(active_index, "obj").unwrap();
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, active_index);
    let func = graph.function_def(vec![], item);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let _1_obj = class_name.field_name.subfield;
        let _2_obj = _1_obj.active_index;
        let _5 = _1_obj.list._items[_2_obj];
        pub fn main() { _5 }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_nullary_function_definition() {
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

#[test]
fn print_unary_function_definition() {
    let mut graph = SymbolicGraph::new();

    let index = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(index, "index").unwrap();

    let obj = graph.static_field("class_name", "field_name");
    let subfield = graph.access_field(obj, "subfield");
    let array = graph.access_field(subfield, "list._items");
    let item = graph.access_index(array, index);

    let func = graph.function_def(vec![index], item);
    graph.name(func, "main").unwrap();

    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let _4 = class_name.field_name.subfield.list._items;
        pub fn main(index: usize) { _4[index] }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_nested_function() {
    let mut graph = SymbolicGraph::new();

    let arg_outer = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(arg_outer, "arg_outer").unwrap();

    let arg_inner = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(arg_inner, "arg_inner").unwrap();
    let sum = graph.add(arg_outer, arg_inner);
    graph.name(sum, "sum").unwrap();

    let res_inner = graph.mul(sum, 10);
    graph.name(res_inner, "res_inner").unwrap();
    let func_inner = graph.function_def(vec![arg_inner], res_inner);
    graph.name(func_inner, "func_inner").unwrap();

    let res_outer = graph.function_call(func_inner, vec![1000.into()]);
    graph.name(res_outer, "res_outer").unwrap();
    let func_outer = graph.function_def(vec![arg_outer], res_outer);
    graph.name(func_outer, "func_outer").unwrap();

    let res_main = graph.function_call(func_outer, vec![42.into()]);
    graph.name(res_main, "res_main").unwrap();
    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        fn func_outer(arg_outer: usize) {
            fn func_inner(arg_inner: usize) {
                let sum = arg_outer + arg_inner;
                let res_inner = sum*10;
                res_inner
            }
            let res_outer = func_inner(1000);
            res_outer
        }
        let res_main = func_outer(42);
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_reduce_with_named_reduction() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(b, "b").unwrap();

    let sum = graph.add(a, b);
    let reduction = graph.function_def(vec![a, b], sum);
    graph.name(reduction, "reduction").unwrap();

    let iterator = graph.range(10);
    let res_main = graph.reduce(0, iterator, reduction);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        fn reduction(a: usize, b: usize) { a + b }
        let res_main = (0..10).reduce(0, reduction);
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_reduce_with_anonymous_reduction() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(b, "b").unwrap();

    let sum = graph.add(a, b);
    let reduction = graph.function_def(vec![a, b], sum);

    let iterator = graph.range(10);
    let res_main = graph.reduce(0, iterator, reduction);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (0..10).reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_reduce_with_named_reduction_including_named_subexpressions() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(b, "b").unwrap();

    let prod = graph.mul(a, b);
    graph.name(prod, "prod").unwrap();
    let sum = graph.add(prod, b);
    let reduction = graph.function_def(vec![a, b], sum);
    graph.name(reduction, "reduction").unwrap();

    let iterator = graph.range(10);
    let res_main = graph.reduce(0, iterator, reduction);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        fn reduction(a: usize, b: usize) {
            let prod = a*b;
            prod + b
        }
        let res_main = (0..10).reduce(0, reduction);
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_reduce_with_anonymous_reduction_including_named_subexpressions() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(b, "b").unwrap();

    let prod = graph.mul(a, b);
    graph.name(prod, "prod").unwrap();
    let sum = graph.add(prod, b);
    let reduction = graph.function_def(vec![a, b], sum);

    let iterator = graph.range(10);
    let res_main = graph.reduce(0, iterator, reduction);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (0..10).reduce(0, |a: usize, b: usize| {
            let prod = a*b;
            prod + b
        });
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_map() {
    let mut graph = SymbolicGraph::new();

    let map = {
        let i = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(i, "i").unwrap();
        let square = graph.mul(i, i);

        graph.function_def(vec![i], square)
    };

    let reduction = {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();
        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        graph.function_def(vec![a, b], sum)
    };

    let iterator = graph.range(10);
    let mapped = graph.map(iterator, map);
    let res_main = graph.reduce(0, mapped, reduction);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (0..10).map(|i: usize| { i*i }).reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_if_else() {
    let mut graph = SymbolicGraph::new();
    let a = graph.if_else(true, 20, 30);
    graph.name(a, "a").unwrap();
    let b = graph.if_else(false, 2, 3);
    graph.name(b, "b").unwrap();

    let res_main = graph.add(a, b);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let a = if true { 20 } else { 30 };
        let b = if false { 2 } else { 3 };
        let res_main = a + b;
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_comparisons() {
    let mut graph = SymbolicGraph::new();

    let a = graph.equal(5, 10);
    let b = graph.not_equal(5, 10);
    let c = graph.less_than(5, 10);
    let d = graph.greater_than(5, 10);
    let e = graph.less_than_or_equal(5, 10);
    let f = graph.greater_than_or_equal(5, 10);

    let res_main = graph.tuple(vec![a, b, c, d, e, f]);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (5 == 10, 5 != 10, 5 < 10, 5 > 10, 5 <= 10, 5 >= 10);
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_div_mod() {
    let mut graph = SymbolicGraph::new();

    let div = graph.div(5, 3);
    graph.name(div, "div").unwrap();
    let modulo = graph.modulo(5, 3);
    graph.name(modulo, "modulo").unwrap();

    let mul = graph.mul(div, 3);
    let sum = graph.add(mul, modulo);
    let validation = graph.equal(sum, 5);
    graph.name(validation, "validation").unwrap();

    let res_main = graph.tuple(vec![div, modulo, validation]);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let div = 5/3;
        let modulo = 5%3;
        let validation = div*3 + modulo == 5;
        let res_main = (div, modulo, validation);
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_iterator_filter() {
    let mut graph = SymbolicGraph::new();

    let iter = graph.range(100);

    let filter = {
        let i = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(i, "i").unwrap();

        let rem = graph.modulo(i, 2);
        let eq = graph.equal(rem, 0);
        graph.function_def(vec![i], eq)
    };
    let iter = graph.filter(iter, filter);

    let map = {
        let j = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(j, "j").unwrap();

        let prod = graph.mul(j, j);
        graph.function_def(vec![j], prod)
    };
    let iter = graph.map(iter, map);

    let reduce = {
        let a = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(a, "a").unwrap();

        let b = graph.function_arg(RuntimePrimType::NativeUInt);
        graph.name(b, "b").unwrap();

        let sum = graph.add(a, b);
        graph.function_def(vec![a, b], sum)
    };
    let res_main = graph.reduce(0, iter, reduce);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (0..100)\
                    .filter(|i: usize| { i%2 == 0 })\
                    .map(|j: usize| { j*j })\
                    .reduce(0, |a: usize, b: usize| { a + b });
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_iterator_collect() {
    let mut graph = SymbolicGraph::new();

    let iter = graph.range(100);
    let res_main = graph.collect(iter);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = (0..100).collect();
        pub fn main() { res_main }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_none() {
    let mut graph = SymbolicGraph::new();

    let else_branch = graph.none();
    let res_main = graph.if_else(true, 100, else_branch);
    graph.name(res_main, "res_main").unwrap();

    let func_main = graph.function_def(vec![], res_main);
    graph.name(func_main, "main").unwrap();
    graph.mark_extern_func(func_main).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        let res_main = if true { 100 } else { None };
        pub fn main() { res_main }"
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

#[test]
fn print_boolean_or() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let ret = graph.boolean_or(a, b);
    let func = graph.function_def(vec![a, b], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool) { a || b }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_boolean_and() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let ret = graph.boolean_and(a, b);
    let func = graph.function_def(vec![a, b], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool) { a && b }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_boolean_not() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let ret = graph.boolean_not(a);
    let func = graph.function_def(vec![a], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();
    let printed = format!("{graph}");
    let expected = indoc! {"
    pub fn main(a: bool) { !a }"};

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_repeated_boolean_or() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let c = graph.function_arg(RuntimePrimType::Bool);
    graph.name(c, "c").unwrap();
    let ret = graph.boolean_or(a, b);
    let ret = graph.boolean_or(ret, c);
    let func = graph.function_def(vec![a, b, c], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool, c: bool) { a || b || c }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_repeated_boolean_and() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let c = graph.function_arg(RuntimePrimType::Bool);
    graph.name(c, "c").unwrap();
    let ret = graph.boolean_and(a, b);
    let ret = graph.boolean_and(ret, c);
    let func = graph.function_def(vec![a, b, c], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool, c: bool) { a && b && c }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_boolean_or_with_not() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let not_a = graph.boolean_not(a);
    let ret = graph.boolean_or(not_a, b);
    let func = graph.function_def(vec![a, b], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool) { !a || b }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn print_boolean_and_with_not() {
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let not_a = graph.boolean_not(a);
    let ret = graph.boolean_and(not_a, b);
    let func = graph.function_def(vec![a, b], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    // Boolean NOT has a higher precedence than boolean AND, so this is
    // parsed as "(!a) && b"
    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool) { !a && b }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn printing_and_with_or_requires_parentheses() {
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let c = graph.function_arg(RuntimePrimType::Bool);
    graph.name(c, "c").unwrap();

    let partial = graph.boolean_and(a, b);
    let ret = graph.boolean_or(partial, c);

    let func = graph.function_def(vec![a, b, c], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool, c: bool) { (a && b) || c }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}

#[test]
fn printing_or_with_and_requires_parentheses() {
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    let mut graph = SymbolicGraph::new();

    let a = graph.function_arg(RuntimePrimType::Bool);
    graph.name(a, "a").unwrap();
    let b = graph.function_arg(RuntimePrimType::Bool);
    graph.name(b, "b").unwrap();
    let c = graph.function_arg(RuntimePrimType::Bool);
    graph.name(c, "c").unwrap();

    let partial = graph.boolean_or(a, b);
    let ret = graph.boolean_and(partial, c);

    let func = graph.function_def(vec![a, b, c], ret);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let printed = format!("{graph}");
    let expected = indoc! {"
        pub fn main(a: bool, b: bool, c: bool) { (a || b) && c }"
    };

    println!("-------------- Expected --------------\n{expected}");
    println!("-------------- Actual   --------------\n{printed}");

    assert_eq!(printed, expected);
}
