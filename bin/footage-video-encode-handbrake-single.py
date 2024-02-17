#!/usr/bin/env python3
"""
# Wrapper to Handbrake cli to make it compatible with cp and mv.

# Can then be used as used similar to they are
"""

import sys
import os
from pathlib import Path
import subprocess
from typing import Optional
import threading


def output_reader(proc: subprocess.Popen) -> None:
    # Read byte by byte to capture live line, else readline could have been used:
    # `iter(proc.stdout.readline, b"")`
    while True:
        b = proc.stdout.read(1)
        if not b:
            break
        print("{}".format(b.decode("utf-8")), end="")


INPUT_FILE_PATH = sys.argv[1]
OUPUT_FILE_PATH: Optional[str] = None
if len(sys.argv) > 2:
    OUPUT_FILE_PATH = sys.argv[2]

if OUPUT_FILE_PATH is None:
    p = Path(os.path.realpath(INPUT_FILE_PATH))
    dir_path = p.parent
    ext = p.suffix
    preset = "HQ 720p30 Surround"
    OUPUT_FILE_PATH = dir_path.joinpath(f"{p.stem}-{preset}.m4v")

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
