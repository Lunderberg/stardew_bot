use dotnet_debugger::{RuntimePrimType, SymbolicGraph};

#[test]
fn eval_integer_literal() {
    let mut graph = SymbolicGraph::new();
    let func = graph.function_def(vec![], 42.into());
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(42));
}

#[test]
fn eval_integer_addition() {
    let mut graph = SymbolicGraph::new();
    let sum = graph.add(5, 7);
    let func = graph.function_def(vec![], sum);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(5 + 7));
}

#[test]
fn eval_integer_multiplication() {
    let mut graph = SymbolicGraph::new();
    let prod = graph.mul(5, 7);
    let func = graph.function_def(vec![], prod);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(5 * 7));
}

#[test]
fn eval_nested_integer_expression() {
    let mut graph = SymbolicGraph::new();
    let lhs = graph.add(2, 3);
    let rhs = graph.add(5, 7);
    let prod = graph.mul(lhs, rhs);
    let func = graph.function_def(vec![], prod);
    graph.name(func, "main").unwrap();
    graph.mark_extern_func(func).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some((2 + 3) * (5 + 7)));
}

#[test]
fn eval_function_call() {
    let mut graph = SymbolicGraph::new();

    let lhs = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(lhs, "lhs").unwrap();
    let rhs = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(rhs, "rhs").unwrap();
    let prod = graph.mul(lhs, rhs);
    let multiply = graph.function_def(vec![lhs, rhs], prod);
    graph.name(multiply, "multiply").unwrap();

    let lhs = graph.add(3, 5);
    let rhs = graph.add(7, 11);
    let call = graph.function_call(multiply, vec![lhs, rhs]);

    let main = graph.function_def(vec![], call);
    graph.name(main, "main").unwrap();
    graph.mark_extern_func(main).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<usize>(0).unwrap(),
        Some((3 + 5) * (7 + 11))
    );
}
