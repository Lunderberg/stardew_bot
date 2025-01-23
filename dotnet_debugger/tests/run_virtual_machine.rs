use dotnet_debugger::{Instruction, RuntimePrimValue, VMArg, VirtualMachine};

#[test]
fn addition() {
    let instructions = vec![
        Instruction::LoadToRegister(VMArg::Const(1usize.into())),
        Instruction::Add(VMArg::Const(2usize.into())),
        Instruction::SaveValue { index: 0 },
    ];

    let vm = VirtualMachine::new(instructions, 1);
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
        Instruction::SaveValue { index: 0 },
        // Initialize loop variable
        Instruction::LoadToRegister(VMArg::Const(max_value.into())),
        Instruction::SaveValue { index: 1 },
        // Begining of loop (instruction 4).  Load the cumulative sum
        // and increment by the loop variable.
        Instruction::LoadToRegister(VMArg::SavedValue { index: 1 }),
        Instruction::Add(VMArg::SavedValue { index: 0 }),
        Instruction::SaveValue { index: 0 },
        // Load the loop variable and decrement its value.
        Instruction::LoadToRegister(VMArg::SavedValue { index: 1 }),
        Instruction::Sub(VMArg::Const(1usize.into())),
        Instruction::SaveValue { index: 1 },
        // Check if the loop should continue
        Instruction::GreaterThan(VMArg::Const(0usize.into())),
        Instruction::ConditionalJump { dest: 4 },
    ];

    let vm = VirtualMachine::new(instructions, 1).simplify();

    let results = vm.local_eval().unwrap();

    assert_eq!(results.len(), 1);

    let expected = (0..=max_value).sum::<usize>();
    assert_eq!(results[0], Some(RuntimePrimValue::NativeUInt(expected)));
}
