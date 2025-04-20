use dotnet_debugger::{bytecode::ExposedNativeFunction, SymbolicGraph};

#[test]
fn constant_folded_integers() {
    let native_func = ExposedNativeFunction::from_closure(|| 0usize);

    let mut graph = SymbolicGraph::new();

    let func = graph.raw_native_function(native_func);

    let mut value = graph.function_call(func, vec![]);
    for _ in 0..100 {
        value = graph.add(value, 1);
    }
    let main_func = graph.function_def(vec![], value);
    graph.name(main_func, "main").unwrap();
    graph.mark_extern_func(main_func).unwrap();

    let vm = graph.compile(None).unwrap();

    println!("------------- VM ------------\n{vm}");

    // 1. Function call
    // 2. Addition
    // 3. Return
    assert!(vm.num_instructions() == 3);
    assert!(vm.stack_size() <= 2);
}

#[test]
fn register_reuse() {
    let native_func = ExposedNativeFunction::from_closure(|| 0usize);

    let mut graph = SymbolicGraph::new();

    let func = graph.raw_native_function(native_func);

    let mut value = graph.function_call(func, vec![]);
    for _ in 0..100 {
        value = graph.add(value, 1);
    }
    let main_func = graph.function_def(vec![], value);
    graph.name(main_func, "main").unwrap();
    graph.mark_extern_func(main_func).unwrap();

    let vm = graph.compiler().disable_optimizations().compile().unwrap();

    println!("------------- VM ------------\n{vm}");

    assert!(
        vm.num_instructions() >= 100,
        "Validity of this test requires \
         constant folding to be disabled"
    );
    assert!(
        vm.stack_size() <= 2,
        "Register re-use should avoid repeated addition \
         from requiring one register per computation. "
    );
}
