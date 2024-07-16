use criterion::{criterion_group, criterion_main, Bencher, Criterion};
use stardew_utils::stardew_valley_pid;

use tui_explorer::{TerminalContext, TuiExplorer, TuiExplorerBuilder};

fn bench_construct_tui_explorer(b: &mut Bencher) {
    let pid = stardew_valley_pid().unwrap();
    b.iter(|| TuiExplorer::new(pid))
}

fn bench_initialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("initialization");
    // group
    //     .noise_threshold(0.07)
    //     .sample_size(20)
    //     .sampling_mode(criterion::SamplingMode::Flat)
    //     .measurement_time(std::time::Duration::from_secs(10));

    group
        .bench_function("construct_tui_explorer", bench_construct_tui_explorer);

    group.finish();
}

fn bench_full_tui(b: &mut Bencher) {
    let pid = stardew_valley_pid().unwrap();
    let mut tui = TuiExplorer::new(pid).unwrap();

    let mut context = TerminalContext::new().unwrap();
    b.iter(move || {
        context.draw(|frame| tui.draw(frame)).unwrap();
    });
}

fn bench_minimal_tui(b: &mut Bencher) {
    let pid = stardew_valley_pid().unwrap();
    let mut tui = TuiExplorerBuilder::new(pid)
        .unwrap()
        .initialize_view_to_stardew_dll()
        .unwrap()
        .build()
        .unwrap();

    let mut context = TerminalContext::new().unwrap();
    b.iter(move || {
        context.draw(|frame| tui.draw(frame)).unwrap();
    });
}

fn bench_minimal_frame(b: &mut Bencher) {
    let mut context = TerminalContext::new().unwrap();
    b.iter(move || {
        context
            .draw(|frame| {
                let block = ratatui::widgets::Block::default()
                    .borders(ratatui::widgets::Borders::ALL)
                    .title("frame");
                frame.render_widget(block, frame.size());
            })
            .unwrap();
    });
}

fn bench_per_frame(c: &mut Criterion) {
    let mut group = c.benchmark_group("per_frame");
    // group
    //     .noise_threshold(0.07)
    //     .sample_size(20)
    //     .sampling_mode(criterion::SamplingMode::Flat)
    //     .measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("full_tui", bench_full_tui);
    group.bench_function("minimal_tui", bench_minimal_tui);
    group.bench_function("minimal_frame", bench_minimal_frame);

    group.finish();
}

criterion_group!(benches, bench_initialization, bench_per_frame);
criterion_main!(benches);
