use dotnet_debugger::{
    bytecode::virtual_machine::{FunctionIndex, InstructionIndex, StackIndex},
    Error, Instruction, RuntimePrimValue, VMArg, VirtualMachine,
};

#[test]
fn addition() {
    let instructions = vec![
        Instruction::LoadToRegister(VMArg::Const(1usize.into())),
        Instruction::Add(VMArg::Const(2usize.into())),
        Instruction::SaveValue(StackIndex(0)),
    ];

    let vm = VirtualMachine::builder(instructions).build();
    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0], Some(RuntimePrimValue::NativeUInt(3)));
}

#[test]
fn triangular_number() {
    let max_value = 5usize;

    let instructions = vec![
        // Initialize cumulative sum
        Instruction::LoadToRegister(VMArg::Const(0usize.into())),
        Instruction::SaveValue(StackIndex(0)),
        // Initialize loop variable
        Instruction::LoadToRegister(VMArg::Const(max_value.into())),
        Instruction::SaveValue(StackIndex(1)),
        // Begining of loop (instruction 4).  Load the cumulative sum
        // and increment by the loop variable.
        Instruction::LoadToRegister(VMArg::SavedValue(StackIndex(1))),
        Instruction::Add(VMArg::SavedValue(StackIndex(0))),
        Instruction::SaveValue(StackIndex(0)),
        // Load the loop variable and decrement its value.
        Instruction::LoadToRegister(VMArg::SavedValue(StackIndex(1))),
        Instruction::Sub(VMArg::Const(1usize.into())),
        Instruction::SaveValue(StackIndex(1)),
        // Check if the loop should continue
        Instruction::GreaterThan(VMArg::Const(0usize.into())),
        Instruction::ConditionalJump(InstructionIndex(4)),
    ];

    let vm = VirtualMachine::builder(instructions).build().simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);

    let expected = (0..=max_value).sum::<usize>();
    assert_eq!(results[0], Some(RuntimePrimValue::NativeUInt(expected)));
}

#[test]
fn run_native_function() {
    let instructions = vec![
        Instruction::LoadToRegister(VMArg::Const(2usize.into())),
        Instruction::Mul(VMArg::Const(3usize.into())),
        Instruction::SaveValue(StackIndex(1)),
        Instruction::LoadToRegister(VMArg::Const(5usize.into())),
        Instruction::Mul(VMArg::Const(7usize.into())),
        Instruction::SaveValue(StackIndex(2)),
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
        },
        Instruction::SaveValue(StackIndex(0)),
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_raw_native_function(
            |args: &mut [Option<RuntimePrimValue>]|
                         -> Result<Option<RuntimePrimValue>,Error> {
                assert_eq!(args.len(), 2);
                let lhs = match args[0].unwrap() {
                    RuntimePrimValue::NativeUInt(val) => val,
                    other => panic!(
                        "Unexpected value {other} of type {}",
                        other.runtime_type()
                    ),
                };
                let rhs = match args[1].unwrap() {
                    RuntimePrimValue::NativeUInt(val) => val,
                    other => panic!(
                        "Unexpected value {other} of type {}",
                        other.runtime_type()
                    ),
                };
                Ok(Some(RuntimePrimValue::NativeUInt(lhs+rhs)))
        })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results[0],
        Some(RuntimePrimValue::NativeUInt(2 * 3 + 5 * 7))
    );
}

#[test]
fn run_wrapped_nullary_native_function() {
    let instructions = vec![
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(0),
            num_args: 0,
        },
        Instruction::SaveValue(StackIndex(0)),
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|| -> usize { 42 })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0], Some(RuntimePrimValue::NativeUInt(42)));
}

#[test]
fn run_wrapped_nullary_unary_function() {
    let instructions = vec![
        Instruction::LoadToRegister(VMArg::Const(7usize.into())),
        Instruction::SaveValue(StackIndex(0)),
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(0),
            num_args: 1,
        },
        Instruction::SaveValue(StackIndex(0)),
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|val: usize| -> usize { val * val })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(results[0], Some(RuntimePrimValue::NativeUInt(49)));
}

#[test]
fn run_wrapped_binary_native_function() {
    let instructions = vec![
        Instruction::LoadToRegister(VMArg::Const(2usize.into())),
        Instruction::Mul(VMArg::Const(3usize.into())),
        Instruction::SaveValue(StackIndex(1)),
        Instruction::LoadToRegister(VMArg::Const(5usize.into())),
        Instruction::Mul(VMArg::Const(7usize.into())),
        Instruction::SaveValue(StackIndex(2)),
        Instruction::NativeFunctionCall {
            index: FunctionIndex(0),
            first_arg: StackIndex(1),
            num_args: 2,
        },
        Instruction::SaveValue(StackIndex(0)),
    ];

    let vm = VirtualMachine::builder(instructions)
        .with_native_function(|lhs: usize, rhs: usize| -> usize { lhs + rhs })
        .build()
        .simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);
    assert_eq!(
        results[0],
        Some(RuntimePrimValue::NativeUInt(2 * 3 + 5 * 7))
    );
}
