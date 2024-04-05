#!/usr/bin/env python3
"""
# Wrapper to Handbrake cli to make it compatible with cp and mv.

# Can then be used as used similar to they are
"""

import sys
import os
from pathlib import Path, PosixPath
import subprocess
from typing import Optional
import threading
import datetime


def rename_suffix(file_path: PosixPath, new_suffix):
    p = Path(os.path.realpath(file_path))
    return str(p.parent.joinpath(f"{p.stem}.{new_suffix}"))


def output_reader(proc: subprocess.Popen) -> None:
    # Read byte by byte to capture live line, else readline could have been used:
    # `iter(proc.stdout.readline, b"")`
    while True:
        b = proc.stdout.read(1)
        if not b:
            break
        print("{}".format(b.decode("utf-8")), end="")


INPUT_FILE_PATH = sys.argv[1]
OUPUT_FILE_PATH: Optional[PosixPath] = None

if len(sys.argv) > 2:
    OUPUT_FILE_PATH = sys.argv[2]

if OUPUT_FILE_PATH is None:
    p = Path(os.path.realpath(INPUT_FILE_PATH))
    dir_path = p.parent
    ext = p.suffix
    preset = "HQ 720p30 Surround"
    OUPUT_FILE_PATH = dir_path.joinpath(f"{p.stem}-{preset}.m4v")

OUPUT_FILE_PATH_INFO = rename_suffix(OUPUT_FILE_PATH, "info")

preset = "HQ 720p30 Surround"
command = [
    "HandBrakeCLI",
    "-Z",
    preset,
    "-i",
    INPUT_FILE_PATH,
    "-o",
    str(OUPUT_FILE_PATH),
]

with open(OUPUT_FILE_PATH_INFO, "a") as f:
    d = datetime.datetime.now()
    date_str = d.strftime("%A, %d. %B %Y %I:%M%p")
    f.write(f"[{date_str}] Created using {sys.argv}\n")
    f.write(f"Created with {command}.\n")

print("[info]Executing command", command)
proc = subprocess.Popen(
    command,
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    bufsize=1,
)
t = threading.Thread(target=output_reader, args=(proc,))
t.start()
proc.wait()
t.join()
