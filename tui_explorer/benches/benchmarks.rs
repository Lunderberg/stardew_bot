use criterion::{
    criterion_group, criterion_main, measurement::Measurement, Bencher,
    BenchmarkGroup, Criterion,
};

use crossterm::event::{Event, KeyCode};
use tui_explorer::{Error, TerminalContext, TuiExplorer, TuiExplorerBuilder};

fn bench_construct_tui_explorer(b: &mut Bencher) -> Result<(), Error> {
    b.iter(|| TuiExplorer::new().unwrap());
    Ok(())
}

fn bench_initialize_annotations(b: &mut Bencher) -> Result<(), Error> {
    b.iter(|| {
        TuiExplorerBuilder::new()
            .unwrap()
            .initialize_annotations()
            .unwrap()
    });

    Ok(())
}

fn bench_search_based_on_annotations(b: &mut Bencher) -> Result<(), Error> {
    b.iter(|| {
        TuiExplorerBuilder::new()
            .unwrap()
            .initialize_annotations()
            .unwrap()
            .search_based_on_annotations()
            .unwrap()
    });

    Ok(())
}

fn bench_initialize_view_to_game_obj(b: &mut Bencher) -> Result<(), Error> {
    b.iter(|| {
        TuiExplorerBuilder::new()
            .unwrap()
            .initialize_view_to_game_obj()
            .unwrap()
    });

    Ok(())
}

fn bench_initialization(c: &mut Criterion) {
    let mut group = c.benchmark_group("initialization");
    group
        .noise_threshold(0.07)
        .sample_size(10)
        .sampling_mode(criterion::SamplingMode::Flat)
        .measurement_time(std::time::Duration::from_secs(10));

    group.bench_function("construct_tui_explorer", |bencher| {
        bench_construct_tui_explorer(bencher).unwrap()
    });

    group.bench_function("initialize_annotations", |bencher| {
        bench_initialize_annotations(bencher).unwrap()
    });

    group.bench_function("search_based_on_annotations", |bencher| {
        bench_search_based_on_annotations(bencher).unwrap()
    });

    group.bench_function("initialize_view_to_game_obj", |bencher| {
        bench_initialize_view_to_game_obj(bencher).unwrap()
    });

    group.finish();
}

fn bench_render_full_tui_initialzied_to_stardew_dll<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorer::new()?;

    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context.draw(|frame| tui.draw(frame)).unwrap();
        })
    });
    Ok(())
}

fn bench_render_full_tui_initialized_to_game_obj<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_annotations()?
        .search_based_on_annotations()?
        .initialize_view_to_game_obj()?
        .build()?;

    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context.draw(|frame| tui.draw(frame)).unwrap();
        })
    });
    Ok(())
}

fn bench_scroll_in_stardew_dll<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_annotations()?
        .search_based_on_annotations()?
        // .initialize_view_to_game_obj()?
        .initialize_view_to_stardew_dll()?
        .build()?;

    group.bench_function(name, |bencher| {
        bencher.iter(|| {
            tui.handle_event(Event::Key(KeyCode::Up.into()));
            tui.handle_event(Event::Key(KeyCode::Down.into()));
        })
    });
    Ok(())
}

fn bench_scroll_in_region_with_game_obj<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_annotations()?
        .search_based_on_annotations()?
        .initialize_view_to_game_obj()?
        // .initialize_view_to_stardew_dll()?
        .build()?;

    group.bench_function(name, |bencher| {
        // let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            //context.draw(|frame| tui.draw(frame)).unwrap();
            tui.handle_event(Event::Key(KeyCode::Up.into()));
            tui.handle_event(Event::Key(KeyCode::Down.into()));
        })
    });
    Ok(())
}

fn bench_render_tui_initialized_to_stardew_dll<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_view_to_stardew_dll()?
        .build()?;
    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context.draw(|frame| tui.draw(frame)).unwrap();
        })
    });

    Ok(())
}

fn bench_render_tui_initialized_to_game_obj<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .init_symbols()
        .default_detail_formatters()
        .default_column_formatters()
        .initialize_view_to_game_obj()?
        .build()?;

    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context.draw(|frame| tui.draw(frame)).unwrap();
        })
    });

    Ok(())
}

fn bench_render_minimal_tui<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    let mut tui = TuiExplorerBuilder::new()?
        .initialize_view_to_stardew_dll()?
        .build()?;

    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context.draw(|frame| tui.draw(frame)).unwrap();
        })
    });

    Ok(())
}

fn bench_render_minimal_frame<M: Measurement>(
    group: &mut BenchmarkGroup<M>,
    name: &str,
) -> Result<(), Error> {
    group.bench_function(name, |bencher| {
        let mut context = TerminalContext::new().unwrap();
        bencher.iter(|| {
            context
                .draw(|frame| {
                    let block = ratatui::widgets::Block::default()
                        .borders(ratatui::widgets::Borders::ALL)
                        .title("frame");
                    frame.render_widget(block, frame.size());
                })
                .unwrap();
        });
    });

    Ok(())
}

fn bench_per_frame(c: &mut Criterion) {
    let mut group = c.benchmark_group("per_frame");
    // group
    //     .noise_threshold(0.07)
    //     .sample_size(10)
    //     .sampling_mode(criterion::SamplingMode::Flat)
    //     .measurement_time(std::time::Duration::from_secs(10));

    bench_render_full_tui_initialzied_to_stardew_dll(
        &mut group,
        "render_full_tui_at_stardew_dll",
    )
    .unwrap();

    bench_render_full_tui_initialized_to_game_obj(
        &mut group,
        "render_full_tui_at_game_obj",
    )
    .unwrap();

    bench_render_tui_initialized_to_stardew_dll(
        &mut group,
        "render_tui_at_stardew_dll",
    )
    .unwrap();

    bench_render_tui_initialized_to_game_obj(
        &mut group,
        "render_tui_at_game_obj",
    )
    .unwrap();

    bench_scroll_in_stardew_dll(&mut group, "scroll_in_stardew_dll").unwrap();

    bench_scroll_in_region_with_game_obj(
        &mut group,
        "scroll_in_region_with_game_obj",
    )
    .unwrap();

    bench_render_minimal_tui(&mut group, "render_minimal_tui").unwrap();
    bench_render_minimal_frame(&mut group, "render_minimal_frame").unwrap();

    group.finish();
}

criterion_group!(benches, bench_initialization, bench_per_frame);
criterion_main!(benches);
