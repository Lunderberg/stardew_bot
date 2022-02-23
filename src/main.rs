mod mem_read;
use mem_read::MemoryReader;

mod error;
use error::{Error, Result};

use sysinfo::{PidExt, ProcessExt, SystemExt};

fn stardew_valley_pid() -> Result<u32> {
    let mut sys = sysinfo::System::new_all();
    sys.refresh_processes();
    sys.processes()
        .iter()
        .find_map(|(pid, proc)| {
            (proc.name() == "Stardew Valley").then(|| pid.as_u32())
        })
        .ok_or(Error::StardewNotRunning)
}

fn main() -> Result<()> {
    let pid = stardew_valley_pid()?;

    println!("Stardew PID: {}", pid);

    let reader = MemoryReader::new(pid)?;

    println!(
        "Total memory: {} MB",
        (reader.total_memory() as f32) / (1024.0 * 1024.0)
    );

    println!(
        "Total readable memory: {} MB",
        (reader.total_readable_memory() as f32) / (1024.0 * 1024.0)
    );

    println!(
        "Stack size: {} MB",
        (reader.stack_map()?.size() as f32) / (1024.0 * 1024.0)
    );
    println!(
        "Heap size: {} MB",
        (reader.heap_map()?.size() as f32) / (1024.0 * 1024.0)
    );

    reader.pointers_in_stack()?.for_each(|(map, ptr)| {
        println!(
            "0x{:016x} is in {}",
            ptr,
            map.filename()
                .map(|p| p.to_str())
                .flatten()
                .unwrap_or_else(|| "???")
        )
    });

    Ok(())
}
