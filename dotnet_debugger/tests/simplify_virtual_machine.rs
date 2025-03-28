use dotnet_debugger::{
    bytecode::virtual_machine::StackIndex, Instruction, VirtualMachine,
};

fn compare_to_expected(
    before: impl IntoIterator<Item = Instruction>,
    expected: impl IntoIterator<Item = Instruction>,
) {
    let before = VirtualMachine::builder()
        .with_instructions(before.into_iter().collect())
        .build();
    let expected = VirtualMachine::builder()
        .with_instructions(expected.into_iter().collect())
        .build();

    println!("------------- Before ------------\n{before}");

    let after = before.simplify();
    println!("------------- After ------------\n{after}");
    println!("------------- Expected ------------\n{expected}");

    after.assert_equal(&expected);
}

#[test]
fn condense_temporary_indices() {
    compare_to_expected(
        [
            Instruction::Add {
                lhs: 1.into(),
                rhs: 2.into(),
                output: StackIndex(1000),
            },
            Instruction::Add {
                lhs: StackIndex(1000).into(),
                rhs: 3.into(),
                output: StackIndex(0),
            },
        ],
        [
            Instruction::Add {
                lhs: 1.into(),
                rhs: 2.into(),
                output: StackIndex(1),
            },
            Instruction::Add {
                lhs: StackIndex(1).into(),
                rhs: 3.into(),
                output: StackIndex(0),
            },
        ],
    );
}
