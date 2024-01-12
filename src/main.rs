use stardew_bot::{Error, Result, TuiExplorer};

use sysinfo::{PidExt, ProcessExt, SystemExt};

fn stardew_valley_pid() -> Result<u32> {
    let mut sys = sysinfo::System::new_all();
    sys.refresh_processes();
    sys.processes()
        .iter()
        .find_map(|(pid, proc)| {
            (proc.name() == "Stardew Valley" || proc.name() == "StardewValley")
                .then(|| pid.as_u32())
        })
        .ok_or(Error::StardewNotRunning)
}

fn main() -> Result<()> {
    let pid = stardew_valley_pid()?;

    println!("Stardew PID: {}", pid);

    let mut tui = TuiExplorer::new(pid)?;
    tui.run()?;

    Ok(())
}
