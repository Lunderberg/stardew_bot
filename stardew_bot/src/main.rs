mod error;
use error::Error;

use tui_explorer::TuiExplorer;

//use sysinfo::{PidExt, ProcessExt, SystemExt};

use stardew_utils::stardew_valley_pid;

fn main() -> Result<(), Error> {
    let pid = stardew_valley_pid()?;

    println!("Stardew PID: {}", pid);

    let mut tui = TuiExplorer::new(pid)?;
    tui.run()?;

    Ok(())
}
