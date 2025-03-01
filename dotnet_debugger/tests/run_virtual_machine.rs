#[allow(unused_imports)]
use dotnet_debugger::{
    bytecode::{
        native_function::WrappedNativeFunction,
        virtual_machine::{FunctionIndex, InstructionIndex, StackIndex},
        NativeFunction,
    },
    Error, Instruction, RuntimePrimValue, RustNativeObject, StackValue, VMArg,
    VirtualMachine,
};

#[test]
fn addition() {
    let instructions = vec![Instruction::Add {
        lhs: VMArg::Const(1usize.into()),
        rhs: VMArg::Const(2usize.into()),
        output: StackIndex(0),
    }];

    let vm = VirtualMachine::builder(instructions).build();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get(0).and_then(|val| val.as_prim()),
        Some(RuntimePrimValue::NativeUInt(3))
    );
}

#[test]
fn triangular_number() {
    let max_value = 5usize;

    let instructions = vec![
        // Initialize cumulative sum
        Instruction::Copy {
            value: VMArg::Const(0usize.into()),
            output: StackIndex(0),
        },
        // Initialize loop variable
        Instruction::Copy {
            value: VMArg::Const(max_value.into()),
            output: StackIndex(1),
        },
        // Begining of loop (instruction 2).  Increment the cumulative sum
        // and by the loop variable.
        Instruction::Add {
            lhs: VMArg::SavedValue(StackIndex(0)),
            rhs: VMArg::SavedValue(StackIndex(1)),
            output: StackIndex(0),
        },
        // Decrement the loop variable, then check if has reached zero.
        Instruction::Sub {
            lhs: VMArg::SavedValue(StackIndex(1)),
            rhs: VMArg::Const(1usize.into()),
            output: StackIndex(1),
        },
        Instruction::GreaterThan {
            lhs: VMArg::SavedValue(StackIndex(1)),
            rhs: VMArg::Const(0usize.into()),
            output: StackIndex(2),
        },
        // Check if the loop should continue
        Instruction::ConditionalJump {
            cond: VMArg::SavedValue(StackIndex(2)),
            dest: InstructionIndex(2),
        },
    ];

    let vm = VirtualMachine::builder(instructions).build().simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);

    let expected = (0..=max_value).sum::<usize>();
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(expected))
    );
}

#[test]
fn run_native_function() {
    let instructions = vec![
        Instruction::Mul {
            lhs: VMArg::Const(2usize.into()),
            rhs: VMArg::Const(3usize.into()),
            output: StackIndex(1),
        },
        Instruction::Mul {
            lhs: VMArg::Const(5usize.into()),
            rhs: VMArg::Const(7usize.into()),
            output: StackIndex(2),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
            output: StackIndex(0),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_raw_native_function(
            |args: &[&mut Option<StackValue>]|
                         -> Result<Option<StackValue>,Error> {
                assert_eq!(args.len(), 2);
                let lhs: usize = args[0].as_ref().unwrap().try_into()?;
                let rhs: usize = args[1].as_ref().unwrap().try_into()?;
                Ok(Some(StackValue::Prim(RuntimePrimValue::NativeUInt(lhs+rhs))))
        })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(2 * 3 + 5 * 7))
    );
}

#[test]
fn run_wrapped_nullary_native_function() {
    let instructions = vec![Instruction::NativeFunctionCall {
        index: FunctionIndex(0),
        first_arg: StackIndex(0),
        num_args: 0,
        output: StackIndex(0),
    }];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|| -> usize { 42 })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(42))
    );
}

#[test]
fn run_wrapped_unary_native_function() {
    let instructions = vec![
        Instruction::Copy {
            value: VMArg::Const(7usize.into()),
            output: StackIndex(0),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(0),
            num_args: 1,
            output: StackIndex(0),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|&val: &usize| -> usize { val * val })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(49))
    );
}

#[test]
fn run_wrapped_binary_native_function() {
    let instructions = vec![
        Instruction::Mul {
            lhs: VMArg::Const(2usize.into()),
            rhs: VMArg::Const(3usize.into()),
            output: StackIndex(1),
        },
        Instruction::Mul {
            lhs: VMArg::Const(5usize.into()),
            rhs: VMArg::Const(7usize.into()),
            output: StackIndex(2),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
            output: StackIndex(0),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|&lhs: &usize, &rhs: &usize| -> usize {
            lhs + rhs
        })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(2 * 3 + 5 * 7))
    );
}

#[test]
fn rust_function_returning_rust_object() {
    struct RustObj {
        a: usize,
        b: usize,
    }
    impl RustNativeObject for RustObj {}

    let instructions = vec![
        Instruction::Mul {
            lhs: VMArg::Const(2usize.into()),
            rhs: VMArg::Const(3usize.into()),
            output: StackIndex(1),
        },
        Instruction::Mul {
            lhs: VMArg::Const(5usize.into()),
            rhs: VMArg::Const(7usize.into()),
            output: StackIndex(2),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
            output: StackIndex(0),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|&a: &usize, &b: &usize| RustObj { a, b })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let StackValue::Any(obj) = results.get(0).unwrap() else {
        panic!("Should produce rust-native output")
    };
    assert_eq!(obj.type_id(), std::any::TypeId::of::<RustObj>());

    let obj = obj.downcast_ref::<RustObj>().unwrap();
    assert_eq!(obj.a, 2 * 3);
    assert_eq!(obj.b, 5 * 7);
}

#[test]
fn rust_function_accepting_rust_object() {
    struct RustObj {
        a: usize,
        b: usize,
    }
    impl RustNativeObject for RustObj {}

    let instructions = vec![
        Instruction::Mul {
            lhs: VMArg::Const(2usize.into()),
            rhs: VMArg::Const(3usize.into()),
            output: StackIndex(1),
        },
        Instruction::Mul {
            lhs: VMArg::Const(5usize.into()),
            rhs: VMArg::Const(7usize.into()),
            output: StackIndex(2),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
            output: StackIndex(3),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(1),
            first_arg: StackIndex(3),
            num_args: 1,
            output: StackIndex(0),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|&a: &usize, &b: &usize| RustObj { a, b })
        .with_native_function(|obj: &RustObj| obj.a + obj.b)
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results.get_as::<RuntimePrimValue>(0).unwrap(),
        Some(RuntimePrimValue::NativeUInt(2 * 3 + 5 * 7))
    );
}

#[test]
fn rust_function_accepting_mutable_rust_object() {
    struct RustObj {
        a: usize,
        b: usize,
    }
    impl RustNativeObject for RustObj {}

    let instructions = vec![
        Instruction::Mul {
            lhs: VMArg::Const(2usize.into()),
            rhs: VMArg::Const(3usize.into()),
            output: StackIndex(1),
        },
        Instruction::Mul {
            lhs: VMArg::Const(5usize.into()),
            rhs: VMArg::Const(7usize.into()),
            output: StackIndex(2),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
            output: StackIndex(0),
        },
        Instruction::Mul {
            lhs: VMArg::Const(11usize.into()),
            rhs: VMArg::Const(13usize.into()),
            output: StackIndex(1),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(1),
            first_arg: StackIndex(0),
            num_args: 2,
            output: StackIndex(2),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|&a: &usize, &b: &usize| RustObj { a, b })
        .with_native_function(|obj: &mut RustObj, &c: &usize| {
            obj.a += c;
        })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let StackValue::Any(obj) = results.get(0).unwrap() else {
        panic!("Should produce rust-native output")
    };
    assert_eq!(obj.type_id(), std::any::TypeId::of::<RustObj>());

    let obj = obj.downcast_ref::<RustObj>().unwrap();
    assert_eq!(obj.a, 2 * 3 + 11 * 13);
    assert_eq!(obj.b, 5 * 7);
}

#[test]
fn rust_function_collecting_triangular_numbers() {
    let max_value = 5usize;

    let instructions = vec![
        // Initialize vector
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(0),
            num_args: 0,
            output: StackIndex(0),
        },
        // Initialize cumulative sum
        Instruction::Copy {
            value: VMArg::Const(0usize.into()),
            output: StackIndex(1),
        },
        // Initialize loop variable
        Instruction::Copy {
            value: VMArg::Const(0usize.into()),
            output: StackIndex(2),
        },
        // Begining of loop (instruction 3).  Increment the cumulative sum
        // and by the loop variable.
        Instruction::Add {
            lhs: VMArg::SavedValue(StackIndex(1)),
            rhs: VMArg::SavedValue(StackIndex(2)),
            output: StackIndex(1),
        },
        Instruction::NativeFunctionCall {
            index: FunctionIndex(1),
            first_arg: StackIndex(0),
            num_args: 2,
            output: StackIndex(1000),
        },
        // Increment the loop variable, then check if has reached the maximum.
        Instruction::Add {
            lhs: VMArg::SavedValue(StackIndex(2)),
            rhs: VMArg::Const(1usize.into()),
            output: StackIndex(2),
        },
        Instruction::LessThan {
            lhs: VMArg::SavedValue(StackIndex(2)),
            rhs: VMArg::Const(max_value.into()),
            output: StackIndex(3),
        },
        // Check if the loop should continue
        Instruction::ConditionalJump {
            cond: VMArg::SavedValue(StackIndex(3)),
            dest: InstructionIndex(3),
        },
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|| -> Vec<usize> {
            println!("Constructing Vec<usize>");
            Vec::new()
        })
        .with_native_function(|vec: &mut Vec<usize>, value: usize| {
            println!("Appending {value} to a Vec<usize>");
            vec.push(value);
        })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    let StackValue::Any(obj) = results.get(0).unwrap() else {
        panic!("Should produce rust-native output")
    };
    assert_eq!(obj.type_id(), std::any::TypeId::of::<Vec<usize>>());

    let vec = obj.downcast_ref::<Vec<usize>>().unwrap();
    assert_eq!(vec, &vec![0, 1, 3, 6, 10]);
}
