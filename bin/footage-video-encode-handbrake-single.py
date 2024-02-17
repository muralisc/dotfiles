#!/usr/bin/env python3

# Wrapper to Handbrake cli to make it compatible with cp and mv
# Can then be used as used similar to they are

import sys
import subprocess
import threading


def output_reader(proc):
    # for line in iter(proc.stdout.readline, b""):
    #     print("got line: {0}".format(line.decode("utf-8")), end="")
    while True:
        b = proc.stdout.read(1)
        if not b:
            break
        print("{0}".format(b.decode("utf-8")), end="")


print(sys.argv)
INPUT_FILE_PATH = sys.argv[1]
OUPUT_FILE_PATH = sys.argv[2]
preset = "HQ 720p30 Surround"
proc = subprocess.Popen(
    ["HandBrakeCLI", "-Z", preset, "-i", INPUT_FILE_PATH, "-o", OUPUT_FILE_PATH],
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    bufsize=1,
)
t = threading.Thread(target=output_reader, args=(proc,))
t.start()
proc.wait()
t.join()
