import datetime
import os

def backup(filenames, archive_dir, messager=None):
    backup_filename = os.path.join(archive_dir, f"files-{datetime.datetime.now().isoformat(timespec='seconds').replace(':', '-').replace('T', '-')}.gz")
    if messager:
        messager.print("backing up to " + backup_filename)
    else:
        print("backing up to" ,backup_filename)
    os.system("tar czf %s %s" % (backup_filename, " ".join(filenames)))
