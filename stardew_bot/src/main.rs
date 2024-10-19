use stardew_bot::{Error, StardewBot};

fn main() -> Result<(), Error> {
    let mut tui = StardewBot::new()?;
    tui.run()?;

    Ok(())
}
