import datetime
import os

def backup(filename, archive_dir, template):
    if os.path.isfile(filename):
        print("backup", filename, archive_dir, template)
        os.system("gzip --to-stdout %s > %s" % (
            filename,
            os.path.join(archive_dir,
                         (template % datetime.datetime.now().isoformat()) + ".gz")))
