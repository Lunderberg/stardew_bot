use dsl::{
    Error, GraphComparisonExt as _, SymbolicGraph, SymbolicGraphCSE as _,
    SymbolicValue,
};

fn compare_against_expected<Before, Expected>(
    build_before: Before,
    build_expected: Expected,
) where
    Before: Fn(&mut SymbolicGraph) -> Result<SymbolicValue, Error>,
    Expected: Fn(&mut SymbolicGraph) -> Result<SymbolicValue, Error>,
{
    let mut before = SymbolicGraph::new();
    build_before(&mut before).unwrap();

    println!("Before:\n{before}\n");

    let mut expected = SymbolicGraph::new();
    build_expected(&mut expected).unwrap();

    println!("Expected:\n{expected}\n");

    let after = before.eliminate_common_subexpressions().unwrap();
    println!("After:\n{after}\n");

    assert!(after.graph_comparison(&expected).apply());
}

#[test]
fn cse_integer_addition() {
    compare_against_expected(
        |graph| {
            let a = graph.add(5, 15);
            let b = graph.add(5, 15);
            Ok(graph.mul(a, b))
        },
        |graph| {
            let a = graph.add(5, 15);
            Ok(graph.mul(a, a))
        },
    );
}

#[test]
fn cse_static_fields() {
    compare_against_expected(
        |graph| {
            let arr = graph.parse("class_name.static_field.member.values")?;
            let index =
                graph.parse("class_name.static_field.member.active_item")?;
            let out = graph.access_index(arr, index);
            Ok(out)
        },
        |graph| {
            let member = graph.parse("class_name.static_field.member")?;
            let arr = graph.access_field(member, "values");
            let index = graph.access_field(member, "active_item");
            let out = graph.access_index(arr, index);
            Ok(out)
        },
    )
}

// #[test]
// fn cse_equivalent_unary_functions() {
//     compare_against_expected(
//         |graph| {
//             graph.parse(
//                 "
//                 fn func1(a: usize) { a+42 }
//                 fn func2(a: usize) { a+42 }
//                 pub fn main() { func1(5) + func2(15) }
//                 ",
//             )
//         },
//         |graph| {
//             graph.parse(
//                 "
//                 fn func(a: usize) { a+42 }
//                 pub fn main() { func(5) + func(15) }
//                 ",
//             )
//         },
//     )
// }

// #[test]
// fn cse_equivalent_binary_functions() {
//     compare_against_expected(
//         |graph| {
//             graph.parse(
//                 "
//                 fn func1(a: usize, b: usize) { a+b }
//                 fn func2(a: usize, b: usize) { a+b }
//                 pub fn main() { func1(5,10) + func2(15,20) }
//                 ",
//             )
//         },
//         |graph| {
//             graph.parse(
//                 "
//                 fn func(a: usize, b: usize) { a+b }
//                 fn func(a: usize, b: usize) { a+b }
//                 pub fn main() { func(5,10) + func(15,20) }
//                 ",
//             )
//         },
//     )
// }
