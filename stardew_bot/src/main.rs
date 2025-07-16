use stardew_bot::{Error, StardewBot};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Arguments {
    #[structopt(long = "--num-frames")]
    num_frames: Option<usize>,

    #[structopt(long = "--show-startup-times")]
    show_startup_times: bool,

    #[structopt(long = "--show-bundle-status")]
    show_bundle_status: bool,

    #[structopt(long = "--predict-geodes")]
    predict_geodes: Option<usize>,

    #[structopt(long = "--predict-mine-levels")]
    predict_mine_levels: bool,
}

fn main() -> Result<(), Error> {
    let args = Arguments::from_args();

    let mut bot = StardewBot::new(args.show_startup_times)?
        .max_render_frames(args.num_frames);

    if args.show_bundle_status {
        bot.show_bundle_status()?;
    } else if args.predict_mine_levels {
        bot.show_mine_level_prediction()?;
    } else if let Some(num_geodes) = args.predict_geodes {
        bot.show_geode_prediction(num_geodes)?;
    } else {
        bot.run()?;
    }

    Ok(())
}
