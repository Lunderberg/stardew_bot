use dotnet_debugger::{
    Error, RuntimePrimType, RustNativeObject, SymbolicGraph,
};

#[test]
fn eval_integer_literal() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let func = graph.function_def(vec![], 42.into());
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    assert_eq!(result, 42);
    Ok(())
}

#[test]
fn eval_integer_addition() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let sum = graph.add(5, 7);
    let func = graph.function_def(vec![], sum);
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 5 + 7);
    Ok(())
}

#[test]
fn eval_integer_subtraction() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let sum = graph.sub(7, 5);
    let func = graph.function_def(vec![], sum);
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 7 - 5);
    Ok(())
}

#[test]
fn eval_integer_multiplication() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let prod = graph.mul(5usize, 7usize);
    let func = graph.function_def(vec![], prod);
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 5 * 7);
    Ok(())
}

#[test]
fn eval_fp32_multiplication() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let lhs = graph.prim_cast(5, RuntimePrimType::F32);
    let rhs = graph.prim_cast(7, RuntimePrimType::F32);
    let prod = graph.mul(lhs, rhs);
    let div = graph.div(prod, 2usize);
    let func = graph.function_def(vec![], div);
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: f32 = vm.local_eval()?.try_into()?;
    assert_eq!(result, (5.0 * 7.0) / 2.0);
    Ok(())
}

#[test]
fn eval_nested_integer_expression() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    let lhs = graph.add(2usize, 3usize);
    let rhs = graph.add(5usize, 7usize);
    let prod = graph.mul(lhs, rhs);
    let func = graph.function_def(vec![], prod);
    graph.name(func, "main")?;
    graph.mark_extern_func(func)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, (2 + 3) * (5 + 7));
    Ok(())
}

#[test]
fn eval_function_call() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let lhs = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(lhs, "lhs")?;
    let rhs = graph.function_arg(RuntimePrimType::NativeUInt);
    graph.name(rhs, "rhs")?;
    let prod = graph.mul(lhs, rhs);
    let multiply = graph.function_def(vec![lhs, rhs], prod);
    graph.name(multiply, "multiply")?;

    let lhs = graph.add(3usize, 5usize);
    let rhs = graph.add(7usize, 11usize);
    let call = graph.function_call(multiply, vec![lhs, rhs]);

    let main = graph.function_def(vec![], call);
    graph.name(main, "main")?;
    graph.mark_extern_func(main)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, (3 + 5) * (7 + 11));
    Ok(())
}

#[test]
fn eval_function_call_with_unused_parameters() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            let i = 10;
            fn func(j: usize) {
                i
            }
            func(42)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 10);
    Ok(())
}

#[test]
fn eval_native_function_call() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize, b: usize| a * b);

    let lhs = graph.add(3usize, 5usize);
    let rhs = graph.add(7usize, 11usize);
    let call = graph.function_call(func, vec![lhs, rhs]);
    let output = graph.add(call, 13usize);

    let main = graph.function_def(vec![], output);
    graph.name(main, "main")?;
    graph.mark_extern_func(main)?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, (3 + 5) * (7 + 11) + 13);
    Ok(())
}

#[test]
fn eval_nested_function_call() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
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
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    let expected = (42 + 1000) * 10;

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn parse_and_eval_native_function_call() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize, b: usize| a * b);
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() { func(3+5, 7+11) + 13 }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, (3 + 5) * (7 + 11) + 13);
    Ok(())
}

#[test]
fn native_function_call_accepting_prim_option() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|opt_value: Option<usize>| {
        if let Some(value) = opt_value {
            value * value
        } else {
            100
        }
    });
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() {
            func(None) + func(3)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 109);
    Ok(())
}

#[test]
fn native_function_call_accepting_native_option() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(RustNativeObject, PartialEq, Debug)]
    struct RustObj(usize, usize);

    let new_obj = graph.native_function(|a: usize, b: usize| RustObj(a, b));
    graph.name(new_obj, "new_obj")?;

    let func = graph.native_function(|opt_obj: Option<&RustObj>| {
        opt_obj.map(|obj| obj.0 + obj.1).unwrap_or(42)
    });
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() {
            func(None) + func(new_obj(5,10))
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = 42 + 5 + 10;

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn collecting_into_vector() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(Vec::<usize>::new);
    graph.name(init_vector, "init_vector")?;

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector")?;

    graph.parse(stringify! {
        pub fn main() {
           let vec = init_vector();
           let vec = push_vector(vec, 1);
           let vec = push_vector(vec, 2);
           let vec = push_vector(vec, 3);
           vec
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;
    let vec = results
        .get_any(0)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec, &vec![1, 2, 3]);
    Ok(())
}

#[test]
fn sum_of_integers_in_vm_function() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    graph.parse(stringify! {
        pub fn main() {
            (0..42)
               .reduce(0, |a: usize, b:usize| { a+b })
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 42 * 41 / 2);
    Ok(())
}

#[test]
fn reduce_integers_into_constant_value() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();
    graph.parse(stringify! {
        pub fn main() {
            (0..42)
               .reduce(0, |a: usize, b:usize| { 123 })
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 123);
    Ok(())
}

#[test]
fn sum_of_integers_in_native_function() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let reduction = graph.native_function(|a: usize, b: usize| a + b);
    graph.name(reduction, "reduction")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..42).reduce(0, reduction)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;
    assert_eq!(result, 42 * 41 / 2);
    Ok(())
}

#[test]
fn collect_integers_into_vector() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(Vec::<usize>::new);
    graph.name(init_vector, "init_vector")?;

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..5).reduce(
                init_vector(),
                |arr, i: usize| { push_vector(arr, i*i) }
            )
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;
    let vec = results
        .get_any(0)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec, &vec![0, 1, 4, 9, 16]);
    Ok(())
}

#[test]
fn collect_two_vectors_of_integers() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let init_vector = graph.native_function(Vec::<usize>::new);
    graph.name(init_vector, "init_vector")?;

    let push_vector =
        graph.native_function(|vec: &mut Vec<usize>, element: usize| {
            vec.push(element);
        });
    graph.name(push_vector, "push_vector")?;

    graph.parse(stringify! {
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
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    assert_eq!(results.len(), 2);
    let vec_a = results
        .get_any(0)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    let vec_b = results
        .get_any(1)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();
    assert_eq!(vec_a, &vec![0, 1, 2, 3, 4]);
    assert_eq!(vec_b, &vec![0, 1, 4, 9, 16]);
    Ok(())
}

#[test]
fn collect_vector_of_rust_native_objects() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(Debug, Clone, PartialEq, RustNativeObject)]
    struct MyObj(usize, usize);

    let make_obj = graph.native_function(|a: usize, b: usize| MyObj(a, b));
    graph.name(make_obj, "make_obj")?;

    let init_vector = graph.native_function(Vec::<MyObj>::new);
    graph.name(init_vector, "init_vector")?;

    let push_vector =
        graph.native_function(|vec: &mut Vec<MyObj>, element: &MyObj| {
            vec.push(element.clone());
        });
    graph.name(push_vector, "push_vector")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..5).reduce(
                init_vector(),
                |arr, i: usize| {
                     let obj = make_obj(i, i*i);
                     push_vector(arr, obj)
                }
            )
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;
    let vec = results
        .get_any(0)?
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
    Ok(())
}

#[test]
fn eval_length_zero_reductions() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..0).reduce(0, |a: usize, b: usize| {a+b+1})
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = 0;

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_nested_reductions() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn outer_reduction(a:usize, b:usize) {
            fn inner_reduction(c: usize,d: usize) {
                c + d + b
            }
            a + (0..b).reduce(0, inner_reduction)
        }
        pub fn main() {
            (0..10).reduce(0, outer_reduction)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

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

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_nested_reductions_with_enclosed_variables() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn outer_reduction(a:usize, b:usize) {
            let b2 = b*2;
            fn inner_reduction(c: usize,d: usize) {
                if d%2 == 0 {
                    let sum = c;
                    let sum = sum + b2;
                    let sum = sum + b2;
                    sum
                } else {
                    let sum = c;
                    let d2 = d*2;
                    let sum = sum + d2;
                    let sum = sum + d2;
                    sum
                }
            }
            a + (0..b).reduce(0, inner_reduction)
        }
        pub fn main() {
            (0..10).reduce(0, outer_reduction)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..10).fold(0, |a: usize, b: usize| {
        let b2 = b * 2;
        a + (0..b).fold(0, |c: usize, d: usize| {
            if d % 2 == 0 {
                let sum = c;
                let sum = sum + b2;
                
                sum + b2
            } else {
                let sum = c;
                let d2 = d * 2;
                let sum = sum + d2;
                
                sum + d2
            }
        })
    });

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_nested_reductions_with_native_function() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(RustNativeObject)]
    struct Obj(usize);

    let new_obj = graph.native_function(|| Obj(0));
    graph.name(new_obj, "new_obj")?;

    let increment_obj = graph.native_function(|obj: &mut Obj, step: usize| {
        obj.0 += step;
    });
    graph.name(increment_obj, "increment_obj")?;

    let unwrap_obj = graph.native_function(|obj: &Obj| obj.0);
    graph.name(unwrap_obj, "unwrap_obj")?;

    graph.parse(stringify! {
        fn inner_reduction(obj, j:usize) {
            let j_mod_2 = j%2;
            let cond = j_mod_2 == 0;
            if cond {
                increment_obj(obj, j*j)
            } else {
                increment_obj(obj, j)
            }
        }

        fn outer_reduction(obj, i:usize) {
            let obj = increment_obj(obj, i);
            (0..i).reduce(obj, inner_reduction)
        }
        pub fn main() {
            let initial = new_obj();
            let obj = (0..10).reduce(initial, outer_reduction);
            unwrap_obj(obj)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..10)
        .map(|i| {
            i + (0..i)
                .map(|j| if j % 2 == 0 { j * j } else { j })
                .sum::<usize>()
        })
        .sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_nested_reductions_with_native_function_as_last_expression(
) -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(RustNativeObject)]
    struct Obj(usize);

    let new_obj = graph.native_function(|| Obj(0));
    graph.name(new_obj, "new_obj")?;

    let increment_obj = graph.native_function(|obj: &mut Obj, step: usize| {
        obj.0 += step;
    });
    graph.name(increment_obj, "increment_obj")?;

    let unwrap_obj = graph.native_function(|obj: &Obj| obj.0);
    graph.name(unwrap_obj, "unwrap_obj")?;

    graph.parse(stringify! {
        fn inner_reduction(obj, j:usize) {
            let j_mod_2 = j%2;
            let cond = j_mod_2 == 0;
            let value = if cond {
                let j_squared = j*j;
                j_squared
            } else {
                j
            };
            let after_increment = increment_obj(obj, value);
            after_increment
        }

        fn outer_reduction(obj, i:usize) {
            (0..i).reduce(obj, inner_reduction)
        }
        pub fn main() {
            let initial = new_obj();
            let obj = (0..10).reduce(initial, outer_reduction);
            unwrap_obj(obj)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..10)
        .flat_map(|i| 0..i)
        .map(|j| if j % 2 == 0 { j * j } else { j })
        .sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_map_reduce() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| i*i)
                .reduce(0, |a,b| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..10).map(|i| i * i).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_if_else() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
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
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = 23;

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_comparisons() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
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
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    assert_eq!(results.len(), 6);
    assert_eq!(results.get_as::<bool>(0)?, Some(5 == 10));
    assert_eq!(results.get_as::<bool>(1)?, Some(5 != 10));
    assert_eq!(results.get_as::<bool>(2)?, Some(5 < 10));
    assert_eq!(results.get_as::<bool>(3)?, Some(5 > 10));
    assert_eq!(results.get_as::<bool>(4)?, Some(5 <= 10));
    assert_eq!(results.get_as::<bool>(5)?, Some(5 >= 10));
    Ok(())
}

#[test]
fn eval_div_mul_mod() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
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
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    assert_eq!(results.len(), 3);
    assert_eq!(results.get_as::<usize>(0)?, Some(5usize.div_euclid(3)));
    assert_eq!(results.get_as::<usize>(1)?, Some(5usize.rem_euclid(3)));
    assert_eq!(results.get_as::<bool>(2)?, Some(true));
    Ok(())
}

#[test]
fn eval_conditional_reduction() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn reduction(a: usize, b:usize) {
            let condition = b%2==0;
            let c = b*10;
            if condition {
                a + 2*c
            } else {
                a + 3*c
            }
        }
        pub fn main() {
            let reduced = (0..100).reduce(0, reduction);
            reduced
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..100)
        .map(|b| {
            let c = b * 10;
            if b % 2 == 0 {
                2 * c
            } else {
                3 * c
            }
        })
        .sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_conditional_reduction_with_no_change_in_if_branch() -> Result<(), Error>
{
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn reduction(a: usize, b:usize) {
            let condition = b%2==0;
            if condition {
                a
            } else {
                a+b
            }
        }
        pub fn main() {
            let reduced = (0..100).reduce(0, reduction);
            reduced
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..100).filter(|b| b % 2 != 0).sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_conditional_reduction_with_no_change_in_else_branch(
) -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn reduction(a: usize, b:usize) {
            let condition = b%2==0;
            if condition {
                a+b
            } else {
                a
            }
        }
        pub fn main() {
            let reduced = (0..100).reduce(0, reduction);
            reduced
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..100).filter(|b| b % 2 == 0).sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_iterator_filter() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .filter(|i: usize| i%2==0)
                .map(|i: usize| i*i)
                .reduce(0, |a:usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..100)
        .filter(|i| i % 2 == 0)
        .map(|i| i * i)
        .sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn eval_iterator_collect() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .collect()
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;
    let vec = results
        .get_any(0)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();

    let expected: Vec<usize> = (0..100).collect();

    assert_eq!(vec, &expected);
    Ok(())
}

#[test]
fn eval_iterator_filter_collect() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .filter(|i:usize| i%2==0)
                .collect()
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    let expected: Vec<usize> = (0..100).filter(|i| i % 2 == 0).collect();

    assert_eq!(results.get_obj::<Vec<usize>>(0).unwrap(), Some(&expected));
    Ok(())
}

#[test]
fn eval_iterator_map_collect() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .map(|i:usize| i*i)
                .collect()
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    let expected: Vec<usize> = (0..100).map(|i| i * i).collect();

    assert_eq!(results.get_obj::<Vec<usize>>(0).unwrap(), Some(&expected));
    Ok(())
}

#[test]
fn eval_iterator_chain_collect() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        let iter_doubles = (0..10).map(|i| i*2);
        let iter_squares = (0..10).map(|j| j*j);
        let res_main = iter_doubles.chain(iter_squares).collect();
        pub fn main() { res_main }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;
    let vec = results
        .get_any(0)?
        .unwrap()
        .downcast_ref::<Vec<usize>>()
        .unwrap();

    let expected: Vec<usize> = {
        let iter_doubles = (0..10).map(|i| i * 2);
        let iter_squares = (0..10).map(|i| i * i);
        iter_doubles.chain(iter_squares).collect()
    };

    assert_eq!(vec, &expected);
    Ok(())
}

#[test]
fn eval_iterator_map_collect_native_obj() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(Debug, PartialEq, RustNativeObject)]
    struct MyObj(usize, usize);

    let make_obj = graph.native_function(|a: usize, b: usize| MyObj(a, b));
    graph.name(make_obj, "make_obj")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .map(|i:usize| make_obj(i, i*i))
                .collect()
        }
    })?;

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    let expected: Vec<_> = (0..100).map(|i| MyObj(i, i * i)).collect();

    assert_eq!(results.len(), 1);

    let actual = results.get_obj::<Vec<MyObj>>(0)?;
    assert_eq!(actual, Some(&expected));
    Ok(())
}

#[test]
fn reduction_with_last_usage_of_var() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            let x = (0..10)
                .reduce(0, |a:usize, b:usize| a+1);
            (0..10)
                .map(|i:usize| {

                    let y = x+x;
                    let z = y+y;
                    i*z
                })
                .reduce(0, |a:usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected = (0..10)
        .map(|i| {
            let x = 5 + 5;
            let y = x + x;
            let z = y + y;
            i * z
        })
        .sum::<usize>();

    assert_eq!(result, expected);
    Ok(())
}

// Currently, there is no way to mark that the `Vec<usize>` must be
// re-initialized for each iteration of the outer loop.  Because it
// does not depend on any function parameter, it is initialized as
// part of the global scope.
#[ignore = "Known failing test"]
#[test]
fn reduce_into_vec_of_vecs() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(Debug, Clone, PartialEq, RustNativeObject)]
    struct RustObj(Vec<usize>);

    let new_obj =
        graph.native_function(|vec: &Vec<usize>| RustObj(vec.clone()));
    graph.name(new_obj, "new_obj")?;

    let new_vec_usize = graph.native_function(Vec::<usize>::new);
    graph.name(new_vec_usize, "new_vec_usize")?;

    let collect_into_vec_usize = graph
        .native_function(|vec: &mut Vec<usize>, item: usize| vec.push(item));
    graph.name(collect_into_vec_usize, "collect_into_vec_usize")?;

    let new_vec_obj = graph.native_function(Vec::<RustObj>::new);
    graph.name(new_vec_obj, "new_vec_obj")?;

    let collect_into_vec_obj =
        graph.native_function(|vec: &mut Vec<RustObj>, item: &RustObj| {
            vec.push(item.clone())
        });
    graph.name(collect_into_vec_obj, "collect_into_vec_obj")?;

    graph.parse(stringify! {
        pub fn main() {

            (0..3)
                .reduce(
                    new_vec_obj(),
                    |vec_obj, i: usize| {
                        let vec_usize = (0..i)
                            .reduce(
                                new_vec_usize(),
                                |vec_usize, j: usize| {
                                    collect_into_vec_usize(vec_usize, j)
                                }
                            );
                        let rust_obj = new_obj(vec_usize);
                        collect_into_vec_obj(vec_obj, rust_obj)
                    }
                )
        }
    })?;

    println!("Graph: {graph}");

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    let expected: Vec<RustObj> =
        (0..3).map(|i| RustObj((0..i).collect())).collect();

    assert_eq!(results.get_obj::<Vec<RustObj>>(0)?, Some(&expected));
    Ok(())
}

#[test]
fn collect_into_vec_of_vecs() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    #[derive(RustNativeObject, Debug, Clone, PartialEq)]
    struct RustObj(Vec<usize>);

    let new_obj =
        graph.native_function(|vec: &Vec<usize>| RustObj(vec.clone()));
    graph.name(new_obj, "new_obj")?;

    let new_vec_usize = graph.native_function(Vec::<usize>::new);
    graph.name(new_vec_usize, "new_vec_usize")?;

    let collect_into_vec_usize = graph
        .native_function(|vec: &mut Vec<usize>, item: usize| vec.push(item));
    graph.name(collect_into_vec_usize, "collect_into_vec_usize")?;

    let new_vec_obj = graph.native_function(Vec::<RustObj>::new);
    graph.name(new_vec_obj, "new_vec_obj")?;

    let collect_into_vec_obj =
        graph.native_function(|vec: &mut Vec<RustObj>, item: &RustObj| {
            vec.push(item.clone())
        });
    graph.name(collect_into_vec_obj, "collect_into_vec_obj")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..3)
                .map(|i| {
                    let vec_usize = (0..i).collect();
                    new_obj(vec_usize)
                })
                .collect()
        }
    })?;

    println!("Graph: {graph}");

    let vm = graph.compile(None)?;
    let results = vm.local_eval()?;

    let expected: Vec<RustObj> =
        (0..3).map(|i| RustObj((0..i).collect())).collect();

    assert_eq!(results.get_obj::<Vec<RustObj>>(0)?, Some(&expected));
    Ok(())
}

#[test]
fn reduce_with_extent_none() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|| -> Option<usize> { None });
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() {
            let extent = func();
            (0..extent)
                .reduce(42, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = 42;

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn reduction_with_numeric_conditional_in_map() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize| -> Option<usize> {
        (a % 2 == 0).then_some(a)
    });
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| {
                    let j = i+1;
                    let cond = j*j > 10;
                    if cond {
                        j
                    } else {
                        42
                    }
                })
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = (0..10)
        .map(|i| i + 1)
        .map(|i| if i * i > 10 { i } else { 42 })
        .sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn reduction_with_none_check_in_map() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let func = graph.native_function(|a: usize| -> Option<usize> {
        (a % 2 == 0).then_some(a)
    });
    graph.name(func, "func")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| {
                    let b = func(i);
                    if b.is_some() {
                        b
                    } else {
                        42
                    }
                })
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize =
        (0..10).map(|i| if i % 2 == 0 { i } else { 42 }).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn boolean_and() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| {
                    if 3<i && i<7 {
                        10
                    } else {
                        1
                    }
                })
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize =
        (0..10).map(|i| if 3 < i && i < 7 { 10 } else { 1 }).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn boolean_or() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| {
                    if 3<i || i<7 {
                        10
                    } else {
                        1
                    }
                })
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize =
        (0..10).map(|i| if 3 < i || i < 7 { 10 } else { 1 }).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn boolean_not() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(|i| {
                    if !(3<i) {
                        10
                    } else {
                        1
                    }
                })
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = (0..10).map(|i| if 3 >= i { 10 } else { 1 }).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn pass_string_object_to_native_function() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let format_int = graph.native_function(|value: usize| format!("{value}"));
    graph.name(format_int, "format_int")?;

    let parse_and_square = graph.native_function(|value: &String| {
        let as_int: usize = value.parse().unwrap();
        as_int * as_int
    });
    graph.name(parse_and_square, "parse_and_square")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(format_int)
                .map(parse_and_square)
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;
    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = (0..10).map(|i| i * i).sum();

    assert_eq!(result, expected);
    Ok(())
}

#[test]
fn pass_str_reference_to_native_function() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    let format_int = graph.native_function(|value: usize| format!("{value}"));
    graph.name(format_int, "format_int")?;

    let parse_and_square = graph.native_function(|value: &str| {
        let as_int: usize = value.parse().unwrap();
        as_int * as_int
    });
    graph.name(parse_and_square, "parse_and_square")?;

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .map(format_int)
                .map(parse_and_square)
                .reduce(0, |a: usize, b:usize| a+b)
        }
    })?;

    let vm = graph.compile(None)?;

    let result: usize = vm.local_eval()?.try_into()?;
    let expected: usize = (0..10).map(|i| i * i).sum();

    assert_eq!(result, expected);

    Ok(())
}

#[test]
fn multiple_functions_in_one_vm() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main1() {
            100
        }

        pub fn main2() {
            200
        }
    })?;

    let vm = graph.compile(None)?;
    let result1: usize = vm.get_function("main1")?.evaluate()?.try_into()?;
    let result2: usize = vm.get_function("main2")?.evaluate()?.try_into()?;

    assert_eq!(result1, 100);
    assert_eq!(result2, 200);

    Ok(())
}

#[test]
fn other_functions_do_not_impact_performance() -> Result<(), Error> {
    let with_slow_function = {
        let mut graph = SymbolicGraph::new();

        graph.named_native_function("generate_num_iter", || 1000usize)?;

        graph.parse(stringify! {
            let res_fast = 100;
            let res_slow = (0..generate_num_iter())
                    .reduce(0, |a,b| a+b);

            pub fn fast() {
                res_fast
            }

            pub fn slow() {
                res_slow
            }
        })?;

        let vm = graph.compile(None)?;
        let res = vm.get_function("fast")?.evaluate()?;
        res.num_instructions_evaluated()
    };

    let without_slow_function = {
        let mut graph = SymbolicGraph::new();

        graph.parse(stringify! {
            let res_fast = 100;

            pub fn fast() {
                res_fast
            }
        })?;

        let vm = graph.compile(None)?;
        let res = vm.get_function("fast")?.evaluate()?;
        res.num_instructions_evaluated()
    };

    assert_eq!(with_slow_function, without_slow_function);

    Ok(())
}

#[test]
fn use_same_reduction_function_in_multiple_locations() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn sum_to_n_squared(i: usize) {
            let j = i*i;
            let initial = 0;
            (0..j).reduce(initial, |a,b| {
                let reduced = a+b;
                reduced
            })
        }

        pub fn main() {
            let lhs = sum_to_n_squared(5);
            let rhs = sum_to_n_squared(10);
            let sum = lhs + rhs;
            sum
        }
    })?;

    let vm = graph.compile(None)?;

    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = [5, 10].into_iter().flat_map(|i| 0..i * i).sum();

    assert_eq!(result, expected);

    Ok(())
}

#[test]
fn reduction_with_enclosed_variable_from_inlined_function() -> Result<(), Error>
{
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        fn sum_to_n_squared(i: usize) {
            (0..3).reduce(0, |a, b| a + i)
        }

        pub fn main() {
            let lhs = sum_to_n_squared(5);
            let rhs = sum_to_n_squared(10);
            let sum = lhs + rhs;
            sum
        }
    })?;

    let vm = graph.compile(None)?;

    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = 3 * 5 + 3 * 10;

    assert_eq!(result, expected);

    Ok(())
}

#[test]
fn reduction_with_identical_subexpr_in_condition_and_branch(
) -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.parse(stringify! {
        pub fn main() {
            (0..100)
                .reduce(0, |cumsum, n| {
                    let i = n / 10;
                    let j = n % 10;

                    if i+j < 15 {
                        let i = n / 10;
                        let j = n % 10;
                        cumsum + i*j
                    } else {
                        cumsum
                    }
                })
        }
    })?;

    let vm = graph.compile(None)?;

    let result: usize = vm.local_eval()?.try_into()?;

    let expected: usize = (0..100)
        .filter(|n| {
            let i = n / 10;
            let j = n % 10;
            i + j < 15
        })
        .map(|n| {
            let i = n / 10;
            let j = n % 10;
            i * j
        })
        .sum();

    assert_eq!(result, expected);

    Ok(())
}

#[test]
fn use_global_var_from_multiple_public_functions() -> Result<(), Error> {
    let mut graph = SymbolicGraph::new();

    graph.named_native_function("launder", |x: usize| x)?;

    graph.parse(stringify! {
        let upper_bound = launder(10);
        let limit = launder(50);
        let offset = launder(20);
        let iter = (0..upper_bound);

        pub fn main1() {
            iter
                .map(|i| i+offset)
                .reduce(0usize, |a,b| a+b)
        }

        pub fn main2() {
            iter
                .filter(|square| square < limit)
                .reduce(0usize, |a,b| a+b+offset)
        }
    })?;

    let vm = graph.compile(None)?;

    let result1: usize = vm.get_function("main1")?.evaluate()?.try_into()?;
    let expected1: usize = (0..10).map(|i| i + 20).sum();
    assert_eq!(result1, expected1);

    let result2: usize = vm.get_function("main2")?.evaluate()?.try_into()?;
    let expected2: usize =
        (0..10).filter(|&i| i < 50).fold(0, |a, b| a + b + 20);
    assert_eq!(result2, expected2);

    Ok(())
}

#[test]
fn conditional_with_unchanged_native_obj_in_else_branch() -> Result<(), Error> {
    #[derive(RustNativeObject)]
    struct RustObj(usize);

    let mut graph = SymbolicGraph::new();

    graph.named_native_function("initialize", || RustObj(0usize))?;
    graph.named_native_function("incremented", |obj: &RustObj| {
        RustObj(obj.0 + 1)
    })?;

    graph.parse(stringify! {
        pub fn main() {
            (0..10)
                .reduce(initialize(), |value,i| {
                    let a = if i%2 == 0 {
                        incremented(value)
                    } else {
                        value
                    };
                    incremented(a)
                })
        }
    })?;

    let vm = graph.compile(None)?;

    let result: usize = vm.local_eval()?.get_obj::<RustObj>(0)?.unwrap().0;
    let expected = 15usize;
    assert_eq!(result, expected);

    Ok(())
}
