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
