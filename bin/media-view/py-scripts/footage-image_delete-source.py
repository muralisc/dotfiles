#!/usr/bin/env python3

# TODO distinguish from footage-image_find_non_converted other than sqlite !!

# Index :
# python \
#   footage-image_delete-source.py \
#   --sqlite ~/shared_folders/transfer_london_home/footage.sqlite
#   --source-dir ~/data/footage index

# Check source files whic are deleted
# python \
#   footage-image_delete-source.py \
#   --sqlite ~/shared_folders/transfer_london_home/footage.sqlite
#   --source-dir ~/data/footage find_delted_no_hint --dest-dir ~/data/footage_converted

import peewee
from datetime import datetime
from pathlib import Path
import os
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
@click.option("--sqlite", required=True, help="Path to sqlite db")
@click.option("--source-dir", required=True, help="Path to directory to add to index")
@click.pass_context
def cli(ctx, sqlite, source_dir):
    # ensure that ctx.obj exists and is a dict (in case `cli()` is called
    # by means other than the `if` block below)
    ctx.ensure_object(dict)
    ctx.obj["SQLITE"] = sqlite
    ctx.obj["SOURCE_DIR"] = source_dir


@cli.command()
@click.pass_context
def index(ctx):
    sqlite = ctx.obj["SQLITE"]
    source_dir = ctx.obj["SOURCE_DIR"]
    database = peewee.SqliteDatabase(sqlite)
    database_proxy.initialize(database)
    database.create_tables([Files])

    with database.atomic():
        new_files_created = 0
        existing_files = 0
        for path in Path(source_dir).expanduser().rglob("*"):
            if path.is_file():
                try:
                    Files.create(abs_path=str(path), index_timestamp=datetime.now())
                    new_files_created += 1
                except:
                    existing_files += 1
        print(
            f"New files added to index {new_files_created} , exisitng files {existing_files}"
        )
    with database:
        print("Number of rows in sqlite :", Files.select().count())
        # for file in Files.select():
        #     print(f"db row: {file.abs_path}, {file.index_timestamp}")


@cli.command()
@click.pass_context
@click.option(
    "--converted-dir", required=True, help="Destination folder containing file source"
)
def find_delted_no_hint(ctx, converted_dir):
    sqlite = ctx.obj["SQLITE"]
    source_dir = ctx.obj["SOURCE_DIR"]

    database = peewee.SqliteDatabase(sqlite)
    database_proxy.initialize(database)
    with database:
        for source_file_row in Files.select():
            source_filepath = str(source_file_row.abs_path)
            copy_path = source_filepath.replace(source_dir, converted_dir)
            copy_path_no_extension = os.path.splitext(copy_path)[0]
            copy_path_jpg = copy_path_no_extension + ".jpg"
            extensions_to_exclude = [
                ".MOV",
                ".txt",
                ".tomb",
                ".mov",
                ".mp4",
                ".m4v",
                ".mkv",
                ".MP4",
                ".kdenlive",
                ".webm",
                ".log",
                ".arp",
                ".xmp",
                ".pp3",
                ".ARW",
                ".XML",
                "marriage/",
                "2000",
            ]
            if any(x in source_filepath for x in extensions_to_exclude):
                continue
            if (
                Path(copy_path_jpg).exists() is False
                and Path(source_filepath).exists() is True
            ):
                print(f"{source_filepath}")


@cli.command()
@click.pass_context
@click.option(
    "--converted-dir", required=True, help="Destination folder containing file source"
)
def find_delted_with_hint(ctx, dest_dir):
    """
    Find files delted in destination which exist in source
    Can make use of deleted hint file not-recently-played-cleanup-delete-log.log
    """
    sqlite = ctx.obj["SQLITE"]
    source_dir = ctx.obj["SOURCE_DIR"]

    HINT_FILE = "not-recently-played-cleanup-delete-log.log"

    database = peewee.SqliteDatabase(sqlite)
    database_proxy.initialize(database)
    with database:
        for copy_file_row in Files.select():
            copy_filepath = str(copy_file_row.abs_path)
            source_path = copy_filepath.replace(source_dir, dest_dir)
            if (
                Path(copy_filepath).exists() is False
                and Path(source_path).exists() is True
            ):
                print(f"{source_path}")


if __name__ == "__main__":
    cli(obj={})
