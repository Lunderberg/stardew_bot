use itertools::Itertools as _;
use stardew_bot::{Error, Result, TuiExplorer};

use sysinfo::{PidExt, ProcessExt, SystemExt};

fn stardew_valley_pid() -> Result<u32> {
    let mut sys = sysinfo::System::new_all();
    sys.refresh_processes();
    sys.processes()
        .iter()
        .map(|(_pid, proc)| proc)
        .filter(|proc| {
            proc.name() == "Stardew Valley" || proc.name() == "StardewValley"
        })
        .flat_map(|proc| {
            let threads = proc
                .tasks
                .iter()
                .map(|(_, thread)| thread)
                .sorted_by_key(|thread| thread.pid());
            std::iter::once(proc).chain(threads)
        })
        .map(|proc| proc.pid().as_u32())
        .skip(7)
        .next()
        .ok_or(Error::StardewNotRunning)
}

fn main() -> Result<()> {
    let pid = stardew_valley_pid()?;

    println!("Stardew PID: {}", pid);

    let mut tui = TuiExplorer::new(pid)?;
    tui.run()?;

    Ok(())
}
