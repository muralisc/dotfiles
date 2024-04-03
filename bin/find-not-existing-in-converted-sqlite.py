# find-oldest-files.py -s [source-dir] -d sqlitedb -n count

# files
# AbsPath, index_timestamp, copycount

# copy_events
# AbsPath_key , copy_timestamp

from peewee import *
from datetime import datetime
from pathlib import Path

database_proxy = DatabaseProxy()

class BaseModel(Model):
    class Meta:
        database = database_proxy

class Files(BaseModel):
    abs_path = CharField(unique=True)
    index_timestamp = DateTimeField()
    copy_count = IntegerField()


def main():
    database = SqliteDatabase('files.db')
    database_proxy.initialize(database)
    database.create_tables([Files])

    source_dir = "~/data/footage_converted/2023/2023_01_01"
    with database.atomic():
        for path in Path(source_dir).expanduser().rglob("*"):
            if path.is_file():
                try:
                    Files.create(abs_path=str(path), index_timestamp=datetime.now(), copy_count=1)
                except:
                    pass
    with database:
        for file in Files.select():
            print(file.abs_path)
    # import shutil
    # shutil.copy(src, dst)

main()
