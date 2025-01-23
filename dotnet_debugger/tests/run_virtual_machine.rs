use dotnet_debugger::{
    bytecode::virtual_machine::{InstructionIndex, StackIndex},
    Instruction, RuntimePrimValue, VMArg, VirtualMachine,
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
