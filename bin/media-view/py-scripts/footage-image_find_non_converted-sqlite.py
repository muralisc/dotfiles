#!/usr/bin/env python3

# TODO distinguish from footage-image_find_non_converted other than sqlite !!

import peewee
from datetime import datetime
from pathlib import Path
import click

database_proxy = peewee.DatabaseProxy()


class BaseModel(peewee.Model):
    class Meta:
        database = database_proxy


class Files(BaseModel):
    abs_path = peewee.CharField(unique=True)
    index_timestamp = peewee.DateTimeField()


class Events(BaseModel):
    event_type = peewee.CharField()
    timestamp = peewee.DateTimeField()
    abs_path = peewee.CharField()


@click.group()
@click.pass_context
def main(ctx):
    pass


@main.command()
@click.pass_context
@click.option("--sqlite", required=True, help="Path to sqlite db")
@click.option("--source-dir", required=True, help="Path to directory to add to index")
def index(ctx, sqlite, source_dir):
    database = peewee.SqliteDatabase(sqlite)
    database_proxy.initialize(database)
    database.create_tables([Files])

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


@main.command()
@click.pass_context
@click.option("--sqlite", required=True, help="Path to sqlite db")
@click.option(
    "--prefix", required=True, help="Prefix of filepath to remove from index path"
)
@click.option(
    "--dest-dir", required=True, help="Destination folder containing file source"
)
def find_deleted(ctx, sqlite, prefix, dest_dir):
    database = peewee.SqliteDatabase(sqlite)
    database_proxy.initialize(database)
    with database:
        for copy_file_row in Files.select():
            copy_filepath = str(copy_file_row.abs_path)
            source_path = copy_filepath.replace(prefix, dest_dir)
            if (
                Path(copy_filepath).exists() is False
                and Path(source_path).exists() is True
            ):
                print(f"{source_path}")


if __name__ == "__main__":
    main()
