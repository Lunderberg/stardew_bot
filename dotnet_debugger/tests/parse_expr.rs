use dotnet_debugger::SymbolicGraph;

#[test]
fn non_zero_start_of_range_produces_error() {
    let mut graph = SymbolicGraph::new();
    let res = graph.parse("(1..42)");
    assert!(res.is_err());
}

#[test]
fn error_parsing_boolean_and_with_or() {
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    let mut graph = SymbolicGraph::new();
    let res = graph.parse(stringify! {
        pub fn main(a: bool, b: bool, c: bool) { a && b || c }
    });
    assert!(res.is_err());
}

#[test]
fn error_parsing_boolean_or_with_and() {
    // Boolean OR and AND have incomparable precedences.  While OR can
    // be chained together as (a || b || c), and AND can be chained
    // together as (a && b && c), they require parentheses when mixed
    // together.
    let mut graph = SymbolicGraph::new();
    let res = graph.parse(stringify! {
        pub fn main(a: bool, b: bool, c: bool) { a || b && c }
    });
    assert!(res.is_err());
}
