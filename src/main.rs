//use itertools::Itertools;

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

    // let reader = MemoryReader::new(pid)?;

    // println!(
    //     "Total memory: {} MB",
    //     (reader.total_memory() as f32) / (1024.0 * 1024.0)
    // );

    // println!(
    //     "Total readable memory: {} MB",
    //     (reader.total_readable_memory() as f32) / (1024.0 * 1024.0)
    // );

    // println!(
    //     "Total writable memory: {} MB",
    //     (reader.total_writable_memory() as f32) / (1024.0 * 1024.0)
    // );

    // println!(
    //     "Stack size: {} MB",
    //     (reader.stack()?.size_bytes() as f32) / (1024.0 * 1024.0)
    // );

    // reader
    //     .potential_frame_pointers()?
    //     .into_iter()
    //     .sorted_by_key(|ptr_ptr| ptr_ptr.location)
    //     .for_each(|ptr_ptr| {
    //         let pointer_from = ptr_ptr.location;
    //         let pointer_to = ptr_ptr.value;
    //         let direction = if pointer_to > pointer_from { "+" } else { "-" };
    //         let abs_delta = if pointer_to > pointer_from {
    //             pointer_to - pointer_from
    //         } else {
    //             pointer_from - pointer_to
    //         };
    //         println!(
    //             "Pointer at {} points to {}, delta = {}{}",
    //             pointer_from, pointer_to, direction, abs_delta
    //         );
    //     });

    // let num_ptr_executable =
    //     reader.potential_return_instruction_pointers()?.count();
    // println!(
    //     "There exist {} pointers to executable space in the [stack]",
    //     num_ptr_executable
    // );

    // reader.potential_return_instruction_pointers()?.for_each(
    //     |(ptr_ptr, region)| {
    //         println!(
    //             "Pointer at {} points to {}, located in {:?}",
    //             ptr_ptr.location, ptr_ptr.value, region.name
    //         );
    //     },
    // );

    // reader
    //     .pointers_in_stack_with_region()?
    //     .for_each(|(ptr_ptr, region)| {
    //         let name =
    //             region.name.as_ref().map(|p| p.as_str()).unwrap_or("???");
    //         println!(
    //             "Pointer at {} points to {}, located in {}",
    //             ptr_ptr.location, ptr_ptr.value, name
    //         );
    //     });

    // reader.print_stack()?;

    Ok(())
}
