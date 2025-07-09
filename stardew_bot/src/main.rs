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
}

fn main() -> Result<(), Error> {
    let args = Arguments::from_args();

    let mut bot = StardewBot::new(args.show_startup_times)?
        .max_render_frames(args.num_frames);

    if args.show_bundle_status {
        bot.show_bundle_status();
    } else {
        bot.run()?;
    }

    Ok(())
}
