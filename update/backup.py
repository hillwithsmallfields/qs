import datetime
import subprocess
import os

def backup(filenames, archive_dir, messager=None):
    backup_filename = os.path.join(archive_dir, f"files-{datetime.datetime.now().isoformat(timespec='seconds').replace(':', '-').replace('T', '-')}.gz")
    if messager:
        messager.print("backing up to " + backup_filename)
    else:
        print("backing up to" ,backup_filename)
    result = subprocess.run(["tar", "cvzf", backup_filename] + filenames,
                            encoding='utf8',
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            text=True)
    if result.returncode == 0:
        for line in result.stdout.split("\n"):
            messager.print("    " + line)
    else:
        messager.print("Failed to back files up")
