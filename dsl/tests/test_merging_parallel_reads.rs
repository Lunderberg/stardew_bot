use dsl::{
    ir::Error,
    optimize::{ConstantFold, MergeParallelReads},
    GraphComparisonExt as _, GraphRewrite as _, SymbolicGraph,
    SymbolicGraphRewrite as _, SymbolicValue,
};

trait NormalizeReturn {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error>;
}
impl NormalizeReturn for () {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        Ok(None)
    }
}
impl NormalizeReturn for SymbolicValue {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        Ok(Some(self))
    }
}
impl NormalizeReturn for Result<SymbolicValue, Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self.map(Some)
    }
}
impl NormalizeReturn for Result<(), Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self.map(|_| None)
    }
}
impl NormalizeReturn for Result<Option<SymbolicValue>, Error> {
    fn normalize_return(self) -> Result<Option<SymbolicValue>, Error> {
        self
    }
}

fn check_before_expected<BuilderFunc1, Res1, BuilderFunc2, Res2>(
    before: BuilderFunc1,
    expected: BuilderFunc2,
) where
    BuilderFunc1: Fn(&mut SymbolicGraph) -> Res1,
    Res1: NormalizeReturn,
    BuilderFunc2: Fn(&mut SymbolicGraph) -> Res2,
    Res2: NormalizeReturn,
{
    let before = {
        let mut graph = SymbolicGraph::new();
        before(&mut graph).normalize_return().unwrap();
        graph
    };

    let expected = {
        let mut graph = SymbolicGraph::new();
        expected(&mut graph).normalize_return().unwrap();
        graph
    };

    println!(
        "----------- Before -------------\n{}",
        before
            .printer()
            .expand_all_expressions()
            .number_all_expressions()
    );

    let rewriter = MergeParallelReads.then(ConstantFold).apply_recursively();

    let after = before.rewrite(rewriter).unwrap();

    println!(
        "----------- After -------------\n{}",
        after
            .printer()
            .expand_all_expressions()
            .number_all_expressions()
    );

    println!(
        "----------- Expected -------------\n{}",
        expected
            .printer()
            .expand_all_expressions()
            .number_all_expressions()
    );

    assert!(after.graph_comparison(&expected).apply());
}

#[test]
fn merge_two_reads() {
    check_before_expected(
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr) {
                    let a_bytes = a_ptr.read_bytes(8);
                    let a_value = a_bytes.cast_bytes::<usize>(0);

                    let b_bytes = b_ptr.read_bytes(8);
                    let b_value = b_bytes.cast_bytes::<usize>(0);

                    a_value + b_value
                }
            })
        },
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);

                    a_value + b_value
                }
            })
        },
    );
}

#[test]
fn merge_three_reads() {
    check_before_expected(
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let a_bytes = a_ptr.read_bytes(8);
                    let a_value = a_bytes.cast_bytes::<usize>(0);

                    let b_bytes = b_ptr.read_bytes(8);
                    let b_value = b_bytes.cast_bytes::<usize>(0);

                    let c_bytes = c_ptr.read_bytes(8);
                    let c_value = c_bytes.cast_bytes::<usize>(0);

                    a_value + b_value + c_value
                }
            })
        },
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8, c_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);
                    let c_value = bytes.cast_bytes::<usize>(16);

                    a_value + b_value + c_value
                }
            })
        },
    );
}

#[test]
fn merge_two_reads_with_already_merged_read() {
    check_before_expected(
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let ab_bytes = read_bytes(a_ptr, 8, b_ptr, 8);
                    let a_value = ab_bytes.cast_bytes::<usize>(0);
                    let b_value = ab_bytes.cast_bytes::<usize>(8);

                    let c_bytes = c_ptr.read_bytes(8);
                    let c_value = c_bytes.cast_bytes::<usize>(0);

                    a_value + b_value + c_value
                }
            })
        },
        |graph| {
            graph.parse(stringify! {
                pub fn main(a_ptr: Ptr, b_ptr: Ptr, c_ptr: Ptr) {
                    let bytes = read_bytes(a_ptr, 8, b_ptr, 8, c_ptr, 8);
                    let a_value = bytes.cast_bytes::<usize>(0);
                    let b_value = bytes.cast_bytes::<usize>(8);
                    let c_value = bytes.cast_bytes::<usize>(16);

                    a_value + b_value + c_value
                }
            })
        },
    );
}

#[test]
fn merge_sequential_steps() {
    check_before_expected(
        |graph| {
            graph.parse(stringify! {
                pub fn main(a: Ptr, b: Ptr) {
                    let a = a.read_bytes(8).cast_bytes::<Ptr>(0);
                    let a = a + 16;
                    let a = a.read_bytes(8).cast_bytes::<usize>(0);

                    let b = b.read_bytes(8).cast_bytes::<Ptr>(0);
                    let b = b + 16;
                    let b = b.read_bytes(8).cast_bytes::<usize>(0);

                    a + b
                }
            })
        },
        |graph| {
            graph.parse(stringify! {
                pub fn main(a: Ptr, b: Ptr) {
                    let bytes = read_bytes(a, 8, b, 8);
                    let a = bytes.cast_bytes::<Ptr>(0);
                    let b = bytes.cast_bytes::<Ptr>(8);

                    let a = a + 16;
                    let b = b + 16;

                    let bytes = read_bytes(a, 8, b, 8);
                    let a = bytes.cast_bytes::<usize>(0);
                    let b = bytes.cast_bytes::<usize>(8);

                    a + b
                }
            })
        },
    );
}
