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

class Events(BaseModel):
    event_type = CharField()
    timestamp = DateTimeField()
    abs_path = CharField()


def main():
    database = SqliteDatabase('files.sqlite')
    database_proxy.initialize(database)
    database.create_tables([Files])

    source_dir = "~/data/footage_converted/2023"
    with database.atomic():
        for path in Path(source_dir).expanduser().rglob("*"):
            if path.is_file():
                try:
                    Files.create(abs_path=str(path), index_timestamp=datetime.now())
                except:
                    pass
    with database:
        print("Number of rows:", Files.select().count())
        # for file in Files.select():
        #     print(f"db row: {file.abs_path}, {file.index_timestamp}")

main()
