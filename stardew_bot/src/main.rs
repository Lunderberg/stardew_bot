use stardew_bot::{Error, Result, TuiExplorer};

use sysinfo::{PidExt, ProcessExt, SystemExt};

fn stardew_valley_pid() -> Result<u32> {
    let mut sys = sysinfo::System::new_all();
    sys.refresh_processes();
    sys.processes()
        .iter()
        .map(|(_pid, proc)| proc)
        .find(|proc| {
            let correct_name = proc.name() == "Stardew Valley"
                || proc.name() == "StardewValley";
            let is_bash_script = matches!(
                proc.exe().file_name().and_then(|s| s.to_str()),
                Some("bash")
            );
            correct_name && !is_bash_script
        })
        .map(|proc| proc.pid().as_u32())
        .ok_or(Error::StardewNotRunning)
}

fn main() -> Result<()> {
    let pid = stardew_valley_pid()?;

    println!("Stardew PID: {}", pid);

    let mut tui = TuiExplorer::new(pid)?;
    tui.run()?;

    Ok(())
}
