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
#   --sqlite ~/shared_folders/transfer_london_home/footage.sqlite \
#   --source-dir ~/data/footage find-delted-no-hint --converted-dir ~/data/footage_converted

import peewee
from datetime import datetime
from pathlib import Path
from rich import print as rprint
import os
import click

database_proxy = peewee.DatabaseProxy()


class BaseModel(peewee.Model):
    class Meta:
        database = database_proxy


class Files(BaseModel):
    abs_path = peewee.CharField(unique=True)
    added_timestamp = peewee.DateTimeField()
    lastViewed_timestamp = peewee.DateTimeField()
    # status = peewee.IntField() 1) VIEWING 2) VIEWED 3) NOT_VIEWED 4) DELETED


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
    """
    Decide if we need this ?
    """
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
                    Files.create(abs_path=str(path), added_timestamp=datetime.now())
                    new_files_created += 1
                except:
                    existing_files += 1
        print(
            f"New files added to index {new_files_created} , exisitng files {existing_files}"
        )
    with database:
        print("Number of rows in sqlite :", Files.select().count())
        # for file in Files.select():
        #     print(f"db row: {file.abs_path}, {file.added_timestamp}")


@cli.command()
@click.pass_context
@click.option(
    "--converted-dir", required=True, help="Destination folder containing file source"
)
def find_delted_no_hint(ctx, converted_dir):
    """
    This is the old way of doing things
    In the new approach, never delete converted items, instead just keep track in a db 
    and use it to directly delete the source !!
    """
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
            if source_dir not in source_filepath:
                continue
            if (
                Path(copy_path_jpg).exists() is False
                and Path(source_filepath).exists() is True
            ):
                rprint(f"INFO [blue] source {source_filepath} exist...[red] while {copy_path_jpg} do not.")
                print(f"{source_filepath}")


if __name__ == "__main__":
    cli(obj={})
