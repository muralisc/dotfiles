#!/usr/bin/env python3

from peewee import *
from datetime import datetime
from pathlib import Path
import click

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

@click.group()
@click.pass_context
def main(ctx):
    pass

@main.command()
@click.pass_context
@click.option('--sqlite', required=True, help='Path to sqlite db')
@click.option('--source-dir', required=True, help='Path to directory to add to index')
def index(ctx, sqlite, source_dir):
    database = SqliteDatabase(sqlite)
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
@click.option('--sqlite', required=True, help='Path to sqlite db')
@click.option('--prefix', required=True, help='Prefix of filepath to remove from index path')
@click.option('--dest-dir', required=True, help='Desitination folder containing file source')
def find_deleted(ctx, sqlite, prefix, dest_dir):
    database = SqliteDatabase(sqlite)
    with database:
        for copy_filepath in Files.select():
            source_path = copy_filepath.replace(prefix,dest_dir)
            print(f"copy_filepath: {copy_filepath}, source_path: {source_path}"
            if Path(copy_filepath).exist() is False and Path(source_path).exist() is True:
                print(f"To be delted: {source_path}")

if __name__ == '__main__':
    main()
