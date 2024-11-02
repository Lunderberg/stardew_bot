mod error;
pub use error::Error;

pub fn stardew_valley_pid() -> Result<u32, Error> {
    let mut sys = sysinfo::System::new_all();
    sys.refresh_all();
    sys.processes()
        .iter()
        .map(|(_pid, proc)| proc)
        .find(|proc| {
            let correct_name = proc.name() == "Stardew Valley"
                || proc.name() == "StardewValley";
            let is_bash_script = matches!(
                proc.exe()
                    .and_then(|path| path.file_name())
                    .and_then(|name| name.to_str()),
                Some("bash")
            );
            correct_name && !is_bash_script
        })
        .map(|proc| proc.pid().as_u32())
        .ok_or(Error::StardewNotRunning)
}
