#!/usr/bin/env python3

import argparse
import datetime
import json
import os
import psutil
import re
import socket
import sys
import subprocess
import time

def get_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--list-files", "-l", action='store_true')
    parser.add_argument("--keep-days", "-k", type=int)
    parser.add_argument("--wipeout", "-w", action='store_true')
    return vars(parser.parse_args())

def get_live_command_lines_matching(matching):
    """Get the command lines for current processes running a given program."""
    return [cmdline
            for cmdline in (proc.cmdline()
                            for proc in (psutil.Process(pid) for pid in psutil.pids())
                            if proc.status() != 'zombie')
            if cmdline and cmdline[0].endswith(matching)]

def get_option_value(cmdline, option):
    """Get the value for a selected command-line option."""
    pattern = "--%s=(.+)" % option
    for i, v in enumerate(cmdline):
        if v == option:
            return (cmdline[i+1] if i < len(cmdline) else None)
        if (m := re.match(pattern, v)):
            return m.group(1)
    return None

def get_motion_config_filename():
    """Get the filename in current use as a motion config."""
    motions = get_live_command_lines_matching("motion")
    if len(motions) > 1:
        raise RuntimeError("More than one motion process")
    if len(motions) == 0:
        raise RuntimeError("No motion process")
    return get_option_value(motions[0], "-c")

def get_config_value(filename, key):
    """Get a value from a motion config filename."""
    with open(filename) as instream:
        for line in instream:
            parts = line.split()
            if parts:
                if parts[0] == key:
                    return parts[1]
    return None

def run_on_host(hostname, command):
    """Run a command on a specified host."""
    if socket.gethosthame() == hostname:
        return subprocess.run(command).stdout
    else:
        return subprocess.run(["ssh", hostname, command]).stdout

def file_details(filename):
    """Return some details of a file as a dictionary."""
    stat = os.stat(filename)
    return {'filename': filename,
            'size': stat.st_size,
            'created': datetime.datetime.fromtimestamp(stat.st_ctime).isoformat()}

def full_filenames(directory):
    """Return the full names of all the regular files in a directory."""
    return [s for s in (os.path.join(directory, r)
                        for r in os.listdir(directory))
            if os.path.isfile(s)]

def get_files_details(clip_dir):
    """Return the details of all the regular files in a directory."""
    return [file_details(s) for s in full_filenames(clip_dir)]

def motion_main(list_files, keep_days, wipeout):
    clips_dir = get_config_value(get_motion_config_filename(),
                                 "target_dir")
    if list:
        json.dump(get_files_details(clips_dir),
                  sys.stdout)
    if wipeout:
        deleted = 0
        for filename in full_filenames(clips_dir):
            os.delete(filename)
            deleted += 1
        json.dump({'deleted': deleted}, sys.stdout)
    if keep_days:
        cutoff = time.time() - keep_days*24*60*60
        deleted = 0
        kept = 0
        for name in full_filenames(clips_dir):
            if os.stat(name).st_ctime) < cutoff:
                os.delete(name)
                deleted += 1
            else:
                kept += 1
        json.dump({'deleted': deleted,
                   'kept': kept},
                  sys.stdout)

if __name__ == "__main__":
    motion_main(**get_args())
