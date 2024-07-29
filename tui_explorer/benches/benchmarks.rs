use criterion::{criterion_group, criterion_main, Bencher, Criterion};

use tui_explorer::{Error, TerminalContext, TuiExplorer, TuiExplorerBuilder};

fn bench_construct_tui_explorer(b: &mut Bencher) {
    b.iter(|| TuiExplorer::new())
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

fn bench_full_tui(b: &mut Bencher) -> Result<(), Error> {
    let mut tui = TuiExplorer::new()?;

    let mut context = TerminalContext::new()?;
    b.iter(move || {
        context.draw(|frame| tui.draw(frame)).unwrap();
    });
    Ok(())
}

fn bench_tui_without_annotations(b: &mut Bencher) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_view_to_stardew_dll()?
        .build()?;

    let mut context = TerminalContext::new()?;
    b.iter(move || {
        context.draw(|frame| tui.draw(frame)).unwrap();
    });
    Ok(())
}

fn bench_minimal_tui(b: &mut Bencher) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .initialize_view_to_stardew_dll()?
        .build()?;

    let mut context = TerminalContext::new()?;
    b.iter(move || {
        context.draw(|frame| tui.draw(frame)).unwrap();
    });
    Ok(())
}

fn bench_minimal_frame(b: &mut Bencher) -> Result<(), Error> {
    let mut context = TerminalContext::new()?;
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
    Ok(())
}

fn bench_per_frame(c: &mut Criterion) {
    let mut group = c.benchmark_group("per_frame");
    // group
    //     .noise_threshold(0.07)
    //     .sample_size(20)
    //     .sampling_mode(criterion::SamplingMode::Flat)
    //     .measurement_time(std::time::Duration::from_secs(10));

    group
        .bench_function("full_tui", |bencher| bench_full_tui(bencher).unwrap());
    group.bench_function("tui_without_annotations", |bencher| {
        bench_tui_without_annotations(bencher).unwrap()
    });
    group.bench_function("minimal_tui", |bencher| {
        bench_minimal_tui(bencher).unwrap()
    });
    group.bench_function("minimal_frame", |bencher| {
        bench_minimal_frame(bencher).unwrap()
    });

    group.finish();
}

criterion_group!(benches, bench_initialization, bench_per_frame);
criterion_main!(benches);
