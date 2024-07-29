mod error;
use error::Error;

use tui_explorer::TuiExplorer;

//use sysinfo::{PidExt, ProcessExt, SystemExt};

fn main() -> Result<(), Error> {
    let mut tui = TuiExplorer::new()?;
    tui.run()?;

    Ok(())
}
