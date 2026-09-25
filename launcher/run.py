#!/usr/bin/env python3

import argparse
import contextlib
import ctypes
import dataclasses
import enum
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
import tempfile

from collections.abc import Iterator

libc = ctypes.CDLL(None, use_errno=True)
CLONE_NEWNS = 0x00020000
CLONE_NEWUSER = 0x10000000
MS_BIND = 0x00001000
MS_REC = 0x00004000
MS_PRIVATE = 0x00040000
MNT_DETACH = 0x00000002


def build_image(container_file: pathlib.Path) -> str:
    with tempfile.NamedTemporaryFile(prefix="podman_iidfile_") as iidfile:
        cmd = [
            "podman",
            "build",
            "--file",
            str(container_file),
            "--iidfile",
            iidfile.name,
        ]
        subprocess.run(cmd, check=True)

        image_id = iidfile.file.read().decode("utf-8")

    prefix = "sha256:"
    assert image_id.startswith(prefix)
    return image_id[len(prefix) :]


@contextlib.contextmanager
def running_container(image_id: str) -> Iterator[str]:
    container_name = subprocess.check_output(
        ["podman", "create", image_id],
        encoding="utf-8",
    )
    container_name = container_name.strip()
    try:
        yield container_name
    finally:
        subprocess.run(
            ["podman", "rm", container_name],
            check=True,
            # podman-rm prints out the name of each container that it
            # deletes.  This might make sense when deleting based on a
            # filter, but it's pointless noise when deleting an
            # explicitly-named container, so I need to trash the
            # stdout.
            stdout=subprocess.DEVNULL,
        )


def export_image_to_bundle(image_id: str, bundle: pathlib.Path | str):
    with (
        running_container(image_id) as container_name,
        bundle.open("wb") as zipped_output,
        subprocess.Popen(
            ["podman", "export", container_name], stdout=subprocess.PIPE
        ) as export_proc,
    ):
        gzip_proc = subprocess.run(
            ["gzip"], stdin=export_proc.stdout, stdout=zipped_output, check=True
        )

    if export_proc.returncode:
        raise subprocess.CalledProcessError(export_proc.returncode, export_proc.args)


class MountPoint:
    def __init__(self, src, dst=None, readonly: bool = False):
        if dst is None:
            if isinstance(src, tuple):
                src, dst = src
            else:
                dst = src

        self.src = pathlib.Path(src)
        self.dst = pathlib.Path(dst)
        self.readonly = readonly


def unshare(flags):
    retcode = libc.unshare(flags)
    if retcode != 0:
        err = ctypes.get_errno()
        raise OSError(err, os.strerror(err))


def enter_user_namespace():
    uid = os.getuid()
    gid = os.getgid()

    unshare(CLONE_NEWUSER)

    setgroups = pathlib.Path("/proc/self/setgroups")
    setgroups.write_text("deny\n")
    pathlib.Path("/proc/self/uid_map").write_text(f"{uid} {uid} 1\n")
    pathlib.Path("/proc/self/gid_map").write_text(f"{gid} {gid} 1\n")


def do_mount(
    source: str | pathlib.Path | None,
    target: str | pathlib.Path,
    filesystem: str | None = None,
    flags: int = 0,
    data: str | None = None,
):
    if source is not None:
        source = os.fsencode(source)
    target = os.fsencode(target)
    if filesystem is not None:
        filesystem = os.fsencode(filesystem)
    if data is not None:
        data = os.fsencode(data)

    retval = libc.mount(source, target, filesystem, ctypes.c_ulong(flags), data)

    if retval != 0:
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), os.fspath(target))


@contextlib.contextmanager
def with_mount(
    source: str | pathlib.Path,
    target: str | pathlib.Path,
    filesystem: str | None = None,
    flags: int = 0,
    data: str | None = None,
):
    source = pathlib.Path(source)
    target = pathlib.Path(target)

    is_dir = source.is_dir()
    if is_dir:
        target.mkdir(parents=True, exist_ok=True)
    else:
        target.parent.mkdir(parents=True, exist_ok=True)
        target.touch()

    do_mount(source, target, filesystem, flags, data)

    try:
        yield
    finally:
        libc.umount2(os.fsencode(target), MNT_DETACH)


def enter_private_mount_namespace():
    unshare(CLONE_NEWNS)

    do_mount(
        source=None,
        target="/",
        flags=MS_REC | MS_PRIVATE,
    )


def cp_reflink(src, dst):
    # The `--reflink=always` would be nice, except that `/tmp` is a
    # different file system, and so it needs to be an actual copy.  I
    # should look into the tradeoffs between using `/tmp` and using a
    # directory in the same file system as the bundle.
    subprocess.run(
        [
            "cp",
            # "--reflink=always",
            "--reflink=auto",
            "-a",
            src,
            dst,
        ],
        check=True,
    )


def run_container(
    bundle_path: pathlib.Path,
    command: list[str | pathlib.Path] | str | pathlib.Path | None = None,
    mounts: list[str | (str, str)] | None = None,
    env: dict[str, str] | None = None,
    cwd: str | pathlib.Path | None = None,
):
    if command is None:
        command = ["/bin/bash"]
    elif isinstance(command, (str, pathlib.Path)):
        command = [command]

    if mounts is None:
        mounts = []

    with contextlib.ExitStack() as stack:
        temp_dir = stack.enter_context(
            tempfile.TemporaryDirectory(prefix="stardew_chroot_")
        )
        temp_dir = pathlib.Path(temp_dir)

        enter_user_namespace()
        enter_private_mount_namespace()

        subprocess.run(["tar", "-C", temp_dir, "-xf", bundle_path], check=True)

        for mount in mounts:
            if not isinstance(mount, MountPoint):
                mount = MountPoint(mount)

            mounted_path = temp_dir.joinpath(mount.dst.relative_to("/"))
            if mount.readonly:
                mounted_path.parent.mkdir(parents=True, exist_ok=True)
                cp_reflink(mount.src, mounted_path)
            else:
                stack.enter_context(
                    with_mount(
                        mount.src,
                        mounted_path,
                        flags=MS_BIND | MS_REC,
                    )
                )

        # Since calling `os.chroot` alters the paths seen by the
        # process, the process that cleans up `temp_dir` cannot be the
        # same process that executes `os.chroot`.  Therefore, we need
        # to execute `os.chroot` in the child process.  This cannot be
        # done through `subprocess.run`, so we need to make our own
        # subprocess.
        pid = os.fork()
        if pid == 0:
            os.chroot(temp_dir)
            if cwd is None:
                cwd = "/"
            os.chdir(cwd)

            env_updates = {
                key: value for key, value in env.items() if value is not None
            }
            os.execve(
                command[0],
                command,
                {
                    **os.environ,
                    **env_updates,
                },
            )
            print("This should be unreachable in the child process.", file=sys.stderr)
            sys.exit(1)

        _, status = os.waitpid(pid, 0)
        exit_code = os.waitstatus_to_exitcode(status)

        is_success = exit_code == 0

        if exit_code == -9:
            # Not sure why, but Stardew closes with SIGKILL as
            # part of a normal exit.  Possibly due to some failed
            # attempts to open Steam libraries, since those print
            # statements occur prior to the SIGKILL, but not sure
            # exactly why.
            is_success = True

        if not is_success:
            raise subprocess.CalledProcessError(exit_code, command)


def main(args):
    script_dir = pathlib.Path(__file__).parent

    container_file = script_dir.joinpath("Containerfile")
    stardew_path = pathlib.Path.home().joinpath(
        ".steam",
        "steam",
        "steamapps",
        "common",
        "Stardew Valley",
        "Stardew Valley",
    )
    xauthority = os.environ["XAUTHORITY"]
    save_path = script_dir.joinpath("saves")
    startup_preferences_path = script_dir.joinpath("startup_preferences")

    save_path.mkdir(exist_ok=True, parents=True)

    bundle_path = script_dir.joinpath("container_bundle.tar.gz")
    if (
        not bundle_path.exists()
        or bundle_path.stat().st_mtime < container_file.stat().st_mtime
    ):
        image_id = build_image(container_file)
        export_image_to_bundle(image_id, bundle_path)

    run_container(
        bundle_path,
        command=stardew_path,
        cwd=stardew_path.parent,
        mounts=[
            stardew_path.parent,
            # Exposes the GPU device
            "/sys",
            # Exposes the OpenGL driver
            "/dev/dri",
            # Exposes the X11 server socket, along with the
            # .Xauthority file required to authenticate into it.
            xauthority,
            "/tmp/.X11-unix",
            # Exposes the /proc/self space, which is used by the .NET
            # runtime to determine its own process name, early in the
            # CLR bootstrap.
            "/proc",
            # Used by .NET GUID generation.  Without mounting these,
            # will crash when starting a new game or loading a save
            # game.
            "/dev/urandom",
            "/dev/random",
            # The save games and startup preferences
            (save_path, "/saves/StardewValley/Saves"),
            MountPoint(
                startup_preferences_path,
                "/saves/StardewValley/startup_preferences",
                readonly=True,
            ),
        ],
        env={
            "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT": "1",
            "XAUTHORITY": None,
            "DISPLAY": None,
            "XDG_CONFIG_HOME": "/saves",
        },
    )


@contextlib.contextmanager
def debug_on_except():
    try:
        yield
    finally:
        if isinstance(sys.exc_info()[1], Exception):
            import traceback

            try:
                import ipdb as pdb
            except ImportError:
                import pdb

            traceback.print_exc()
            pdb.post_mortem()


def arg_main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--pdb",
        action="store_true",
        help="Start a pdb post mortem on uncaught exception",
    )

    args = parser.parse_args()

    with contextlib.ExitStack() as stack:
        if args.pdb:
            stack.enter_context(debug_on_except())

        main(args)


if __name__ == "__main__":
    arg_main()
