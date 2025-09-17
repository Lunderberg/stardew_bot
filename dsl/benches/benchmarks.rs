use std::cell::Cell;

use criterion::{criterion_group, criterion_main, Bencher, Criterion};

use dsl::{SymbolicGraph, SymbolicGraphCompile as _};

#[path = "../tests/common/mod.rs"]
mod common;
use common::generate_tests;
use dsl_runtime::{Runtime, RuntimeFunc, RuntimeOutput};

fn bench_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("compilation");
    group
        .noise_threshold(0.1)
        // .sample_size(1000)
        .sampling_mode(criterion::SamplingMode::Flat)
        .warm_up_time(std::time::Duration::from_millis(500))
        .measurement_time(std::time::Duration::from_millis(1000));

    macro_rules! generator {
        ($test_name:ident $(, ignore = $reason:literal)? $(,)?) => {
            group.bench_function(
                stringify! {$test_name},
                |bench: &mut Bencher| {
                    let builder = |graph: &SymbolicGraph| {
                        let build = || {
                            graph.compiler().disable_optimizations().compile()
                        };
                        bench.iter(build);
                        Ok(build()?)
                    };
                    let _ = common::$test_name(builder);
                },
            );
        };
    }

    generate_tests! {generator}

    group.finish();
}

struct RuntimeBenchmark<'a, 'b, Inner> {
    inner: Inner,
    bench: Cell<Option<&'a mut Bencher<'b>>>,
}
struct FuncBenchmark<'a, 'b, 'c, Inner> {
    runtime: &'c RuntimeBenchmark<'a, 'b, Inner>,
    name: String,
}

impl<'a, 'b, Inner> Runtime for RuntimeBenchmark<'a, 'b, Inner>
where
    Inner: Runtime,
{
    type Error = Inner::Error;

    type Func<'c>
        = FuncBenchmark<'a, 'b, 'c, Inner>
    where
        Self: 'c;

    fn get_function<'c>(
        &'c self,
        name: &str,
    ) -> Result<Self::Func<'c>, Self::Error> {
        Ok(FuncBenchmark {
            runtime: self,
            name: name.into(),
        })
    }
}

impl<'a, 'b, 'c, Inner> RuntimeFunc<'c> for FuncBenchmark<'a, 'b, 'c, Inner>
where
    Inner: Runtime,
{
    type Error = Inner::Error;

    fn with_reader(self, _: impl dsl_runtime::Reader + 'c) -> Self {
        unimplemented!()
    }

    fn evaluate(self) -> Result<RuntimeOutput, Self::Error> {
        let run_once = || {
            self.runtime
                .inner
                .get_function(&self.name)
                .and_then(|func| func.evaluate())
        };
        if let Some(bench) = self.runtime.bench.take() {
            bench.iter(run_once);
        }

        run_once()
    }
}

fn bench_execution_vm(c: &mut Criterion) {
    let mut group = c.benchmark_group("execution_vm");
    group
        .noise_threshold(0.1)
        // .sample_size(1000)
        .sampling_mode(criterion::SamplingMode::Flat)
        .warm_up_time(std::time::Duration::from_millis(500))
        .measurement_time(std::time::Duration::from_millis(1000));

    macro_rules! generator {
        ($test_name:ident $(, ignore = $reason:literal)? $(,)?) => {
            group.bench_function(
                stringify! {$test_name},
                |bench: &mut Bencher| {
                    let bench: Cell<Option<&mut Bencher>> =
                        Cell::new(Some(bench));
                    let builder = |graph: &SymbolicGraph| {
                        let inner = graph
                            .compiler()
                            .disable_optimizations()
                            .compile()?;
                        let bench = Cell::new(bench.take());
                        Ok(RuntimeBenchmark { inner, bench })
                    };
                    let _ = common::$test_name(builder);
                },
            );
        };
    }

    generate_tests! {generator}

    group.finish();
}

fn bench_execution_opt_vm(c: &mut Criterion) {
    let mut group = c.benchmark_group("execution_opt_vm");
    group
        .noise_threshold(0.1)
        // .sample_size(1000)
        .sampling_mode(criterion::SamplingMode::Flat)
        .warm_up_time(std::time::Duration::from_millis(500))
        .measurement_time(std::time::Duration::from_millis(1000));

    macro_rules! generator {
        ($test_name:ident $(, ignore = $reason:literal)? $(,)?) => {
            group.bench_function(
                stringify! {$test_name},
                |bench: &mut Bencher| {
                    let bench: Cell<Option<&mut Bencher>> =
                        Cell::new(Some(bench));
                    let builder = |graph: &SymbolicGraph| {
                        let inner = graph.compiler().compile()?;
                        let bench = Cell::new(bench.take());
                        Ok(RuntimeBenchmark { inner, bench })
                    };
                    let _ = common::$test_name(builder);
                },
            );
        };
    }

    generate_tests! {generator}

    group.finish();
}

fn bench_execution_interpreter(c: &mut Criterion) {
    let mut group = c.benchmark_group("execution_interpreter");
    group
        .noise_threshold(0.1)
        // .sample_size(1000)
        .sampling_mode(criterion::SamplingMode::Flat)
        .warm_up_time(std::time::Duration::from_millis(500))
        .measurement_time(std::time::Duration::from_millis(1000));

    macro_rules! generator {
        ($test_name:ident $(, ignore = $reason:literal)? $(,)?) => {
            group.bench_function(
                stringify! {$test_name},
                |bench: &mut Bencher| {
                    let bench: Cell<Option<&mut Bencher>> =
                        Cell::new(Some(bench));
                    let builder = |graph: &SymbolicGraph| {
                        let inner = graph
                            .compiler()
                            .disable_optimizations()
                            .interpreter()?;
                        let bench = Cell::new(bench.take());
                        Ok(RuntimeBenchmark { inner, bench })
                    };
                    let _ = common::$test_name(builder);
                },
            );
        };
    }

    generate_tests! {generator}

    group.finish();
}

fn bench_execution_opt_interpreter(c: &mut Criterion) {
    let mut group = c.benchmark_group("execution_opt_interpreter");
    group
        .noise_threshold(0.1)
        // .sample_size(1000)
        .sampling_mode(criterion::SamplingMode::Flat)
        .warm_up_time(std::time::Duration::from_millis(500))
        .measurement_time(std::time::Duration::from_millis(1000));

    macro_rules! generator {
        ($test_name:ident $(, ignore = $reason:literal)? $(,)?) => {
            group.bench_function(
                stringify! {$test_name},
                |bench: &mut Bencher| {
                    let bench: Cell<Option<&mut Bencher>> =
                        Cell::new(Some(bench));
                    let builder = |graph: &SymbolicGraph| {
                        let inner = graph.compiler().interpreter()?;
                        let bench = Cell::new(bench.take());
                        Ok(RuntimeBenchmark { inner, bench })
                    };
                    let _ = common::$test_name(builder);
                },
            );
        };
    }

    generate_tests! {generator}

    group.finish();
}

criterion_group!(
    benches,
    bench_compilation,
    bench_execution_vm,
    bench_execution_opt_vm,
    bench_execution_interpreter,
    bench_execution_opt_interpreter,
);
criterion_main!(benches);
