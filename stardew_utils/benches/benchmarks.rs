use stardew_utils::stardew_valley_pid;

use criterion::{criterion_group, criterion_main, Bencher, Criterion};

fn bench_find_process_id(b: &mut Bencher) {
    b.iter(stardew_valley_pid);
}

fn bench_flat_image(c: &mut Criterion) {
    let mut group = c.benchmark_group("initialization");
    group
        .noise_threshold(0.07)
        .sample_size(20)
        .sampling_mode(criterion::SamplingMode::Flat)
        .measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("find_process_id", bench_find_process_id);

    group.finish();
}

criterion_group!(benches, bench_flat_image);
criterion_main!(benches);
