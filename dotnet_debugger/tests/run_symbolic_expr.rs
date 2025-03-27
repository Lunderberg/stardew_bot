use dotnet_debugger::{RuntimePrimType, RustNativeObject, SymbolicGraph};

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

#[test]
fn eval_native_function_call() {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize, b: usize| a * b);

    let lhs = graph.add(3, 5);
    let rhs = graph.add(7, 11);
    let call = graph.function_call(func, vec![lhs, rhs]);
    let output = graph.add(call, 13);

    let main = graph.function_def(vec![], output);
    graph.name(main, "main").unwrap();
    graph.mark_extern_func(main).unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<usize>(0).unwrap(),
        Some((3 + 5) * (7 + 11) + 13)
    );
}

#[test]
fn eval_nested_function_call() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            fn func_outer(arg_outer: usize) {
                fn func_inner(arg_inner: usize) {
                    let sum = arg_outer + arg_inner;
                    let res_inner = sum*10;
                    res_inner
                }
                let res_outer = func_inner(1000);
                res_outer
            }
            pub fn main() {
                let res_main = func_outer(42);
                res_main
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();
    let expected = (42 + 1000) * 10;

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(expected));
}

#[test]
fn parse_and_eval_native_function_call() {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize, b: usize| a * b);
    graph.name(func, "func").unwrap();

    graph
        .parse("pub fn main() { func(3+5, 7+11) + 13 }")
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<usize>(0).unwrap(),
        Some((3 + 5) * (7 + 11) + 13)
    );
}

#[test]
fn collecting_into_vector() {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(|| Vec::<usize>::new());
    graph.name(init_vector, "init_vector").unwrap();

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector").unwrap();

    graph
        .parse(
            "
            pub fn main() {
               let vec = init_vector();
               let vec = push_vector(vec, 1);
               let vec = push_vector(vec, 2);
               let vec = push_vector(vec, 3);
               vec
            }",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let vec = results
        .get_any(0)
        .unwrap()
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec, &vec![1, 2, 3]);
}

#[test]
fn sum_of_integers_in_vm_function() {
    let mut graph = SymbolicGraph::new();
    graph
        .parse(
            "
            pub fn main() {
                (0..42)
                   .reduce(0, |a: usize, b:usize| { a+b })
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(42 * 41 / 2));
}

#[test]
fn reduce_integers_into_constant_value() {
    let mut graph = SymbolicGraph::new();
    graph
        .parse(
            "
            pub fn main() {
                (0..42)
                   .reduce(0, |a: usize, b:usize| { 123 })
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(123));
}

#[test]
fn sum_of_integers_in_native_function() {
    let mut graph = SymbolicGraph::new();

    let reduction = graph.native_function(|a: usize, b: usize| a + b);
    graph.name(reduction, "reduction").unwrap();

    graph
        .parse(
            "
            pub fn main() {
                (0..42).reduce(0, reduction)
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(42 * 41 / 2));
}

#[test]
fn collect_integers_into_vector() {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(|| Vec::<usize>::new());
    graph.name(init_vector, "init_vector").unwrap();

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector").unwrap();

    graph
        .parse(
            "
            pub fn main() {
                (0..5).reduce(
                    init_vector(),
                    |arr, i: usize| { push_vector(arr, i*i) }
                )
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let vec = results
        .get_any(0)
        .unwrap()
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec, &vec![0, 1, 4, 9, 16]);
}

#[test]
fn collect_two_vectors_of_integers() {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(|| Vec::<usize>::new());
    graph.name(init_vector, "init_vector").unwrap();

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector").unwrap();

    graph
        .parse(
            "
            pub fn main() {
                let a = (0..5).reduce(
                    init_vector(),
                    |arr, i: usize| { push_vector(arr, i) }
                );
                let b = (0..5).reduce(
                    init_vector(),
                    |arr, i: usize| { push_vector(arr, i*i) }
                );
                (a,b)
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 2);
    let vec_a = results
        .get_any(0)
        .unwrap()
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    let vec_b = results
        .get_any(1)
        .unwrap()
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec_a, &vec![0, 1, 2, 3, 4]);
    assert_eq!(vec_b, &vec![0, 1, 4, 9, 16]);
}

#[test]
fn collect_vector_of_rust_native_objects() {
    let mut graph = SymbolicGraph::new();

    #[derive(Debug, Clone, PartialEq)]
    struct MyObj(usize, usize);

    impl RustNativeObject for MyObj {}

    let make_obj = graph.native_function(|a: usize, b: usize| MyObj(a, b));
    graph.name(make_obj, "make_obj").unwrap();

    let init_vector = graph.native_function(|| Vec::<MyObj>::new());
    graph.name(init_vector, "init_vector").unwrap();

    let push_vector =
        graph.native_function(|vec: &mut Vec<MyObj>, element: &MyObj| {
            vec.push(element.clone());
        });
    graph.name(push_vector, "push_vector").unwrap();

    graph
        .parse(
            "
            pub fn main() {
                (0..5).reduce(
                    init_vector(),
                    |arr, i: usize| {
                         let obj = make_obj(i, i*i);
                         push_vector(arr, obj)
                    }
                )
            }
            ",
        )
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let vec = results
        .get_any(0)
        .unwrap()
        .unwrap()
        .downcast_ref::<Vec<MyObj>>()
        .unwrap();
    assert_eq!(
        vec,
        &vec![
            MyObj(0, 0),
            MyObj(1, 1),
            MyObj(2, 4),
            MyObj(3, 9),
            MyObj(4, 16),
        ]
    );
}

#[test]
fn eval_length_zero_reductions() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            pub fn main() {
                (0..0).reduce(0, |a: usize, b: usize| {a+b+1})
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    let expected = 0;

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(expected));
}

#[test]
fn eval_nested_reductions() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            fn outer_reduction(a:usize, b:usize) {
                fn inner_reduction(c: usize,d: usize) {
                    c + d + b
                }
                a + (0..b).reduce(0, inner_reduction)
            }
            pub fn main() {
                (0..10).reduce(0, outer_reduction)
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    let expected = {
        let mut outer_sum = 0;
        for i in 0..10 {
            let mut inner_sum = 0;
            for j in 0..i {
                inner_sum += i + j;
            }
            outer_sum += inner_sum;
        }
        outer_sum
    };

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(expected));
}

#[test]
fn eval_map_reduce() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            pub fn main() {
                (0..10)
                    .map(|i| i*i)
                    .reduce(0, |a,b| a+b)
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    let expected = (0..10).map(|i| i * i).sum();

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(expected));
}

#[test]
fn eval_if_else() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            pub fn main() {
                let x = 5+5;
                let a = if true {
                    x*2
                } else {
                    x*3
                };
                let b = if false {
                    2
                } else {
                    3
                };
                a+b
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    let expected = 23;

    assert_eq!(results.len(), 1);
    assert_eq!(results.get_as::<usize>(0).unwrap(), Some(expected));
}

#[test]
fn eval_comparisons() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            pub fn main() {
                (
                    5==10,
                    5!=10,
                    5<10,
                    5>10,
                    5<=10,
                    5>=10,
                )
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 6);
    assert_eq!(results.get_as::<bool>(0).unwrap(), Some(5 == 10));
    assert_eq!(results.get_as::<bool>(1).unwrap(), Some(5 != 10));
    assert_eq!(results.get_as::<bool>(2).unwrap(), Some(5 < 10));
    assert_eq!(results.get_as::<bool>(3).unwrap(), Some(5 > 10));
    assert_eq!(results.get_as::<bool>(4).unwrap(), Some(5 <= 10));
    assert_eq!(results.get_as::<bool>(5).unwrap(), Some(5 >= 10));
}

#[test]
fn eval_div_mul_mod() {
    let mut graph = SymbolicGraph::new();

    graph
        .parse(stringify! {
            pub fn main() {
                let numerator = 5;
                let denominator = 3;
                let div = numerator / denominator;
                let modulo = numerator % denominator;
                let validation = div*denominator + modulo == numerator;
                (
                    div,
                    modulo,
                    validation,
                )
            }
        })
        .unwrap();

    let vm = graph.compile(None).unwrap();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 3);
    assert_eq!(
        results.get_as::<usize>(0).unwrap(),
        Some(5usize.div_euclid(3))
    );
    assert_eq!(
        results.get_as::<usize>(1).unwrap(),
        Some(5usize.rem_euclid(3))
    );
    assert_eq!(results.get_as::<bool>(2).unwrap(), Some(true));
}
