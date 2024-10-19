use tui_explorer::{Error, TuiExplorer};

fn main() -> Result<(), Error> {
    let mut tui = TuiExplorer::new()?;
    tui.run()?;

    Ok(())
}
