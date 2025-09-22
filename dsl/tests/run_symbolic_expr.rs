use paste::paste;

use dsl::{Error, SymbolicGraph, SymbolicGraphCompile as _};

mod common;
use common::generate_tests;

macro_rules! test_vm {
    ($test_name:ident
       $(, ignore = $reason:literal)?
       $(, ignore_int = $reason_int:literal)?
       $(, ignore_vm = $reason_vm:literal)?
       $(,)?
    ) => {
        paste! {
            $( #[ignore = $reason] )?
            $( #[ignore = $reason_vm] )?
            #[test]
            fn [< $test_name _vm >]() -> Result<(),Error> {
                let builder = |graph:&SymbolicGraph| {
                    Ok(graph.compiler()
                        .disable_optimizations()
                        .compile()?)
                };
                common::$test_name(builder)
            }
        }
    };
}

macro_rules! test_opt_vm {
    ($test_name:ident
       $(, ignore = $reason:literal)?
       $(, ignore_int = $reason_int:literal)?
       $(, ignore_vm = $reason_vm:literal)?
       $(,)?
    ) => {
        paste! {
            $( #[ignore = $reason] )?
            $( #[ignore = $reason_vm] )?
            #[test]
            fn [< $test_name _opt_vm >]() -> Result<(),Error> {
                let builder = |graph:&SymbolicGraph| {
                    Ok(graph.compiler().compile()?)
                };
                common::$test_name(builder)
            }
        }
    };
}

macro_rules! test_interpreter {
    ($test_name:ident
       $(, ignore = $reason:literal)?
       $(, ignore_int = $reason_int:literal)?
       $(, ignore_vm = $reason_vm:literal)?
       $(,)?
    ) => {
        paste! {
            $( #[ignore = $reason] )?
            $( #[ignore = $reason_int] )?
            #[test]
            fn [< $test_name _interpreter >]() -> Result<(),Error> {
                let builder = |graph:&SymbolicGraph| {
                    Ok(graph.compiler()
                        .disable_optimizations()
                        .interpreter()?)
                };
                common::$test_name(builder)
            }
        }
    };
}

macro_rules! test_opt_interpreter {
    ($test_name:ident
       $(, ignore = $reason:literal)?
       $(, ignore_int = $reason_int:literal)?
       $(, ignore_vm = $reason_vm:literal)?
       $(,)?
    ) => {
        paste! {
            $( #[ignore = $reason] )?
            $( #[ignore = $reason_int] )?
            #[test]
            fn [< $test_name _opt_interpreter >]() -> Result<(),Error> {
                let builder = |graph:&SymbolicGraph| {
                    Ok(graph.compiler().interpreter()?)
                };
                common::$test_name(builder)
            }
        }
    };
}

generate_tests! {test_vm}
generate_tests! {test_opt_vm}
generate_tests! {test_interpreter}
generate_tests! {test_opt_interpreter}
