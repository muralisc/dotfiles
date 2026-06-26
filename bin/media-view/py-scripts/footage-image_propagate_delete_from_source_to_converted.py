#!/usr/bin/env python3

# TODO distinguish from footage-image_find_non_converted other than sqlite !!

# Index :

# When some source files are deleted and we want to delete the converted files

# python \
# footage-image_delete-source.py \
# --source-dir ~/data00/footage/2019 \
# find-delted-in-source-folder \
# --converted-dir ~/data00/footage_converted/2019

from datetime import datetime
from pathlib import Path
from rich import print as rprint
import os
import click


@click.group()
@click.option("--source-dir", required=True, help="Path to directory to add to index")
@click.pass_context
def cli(ctx, source_dir):
    # ensure that ctx.obj exists and is a dict (in case `cli()` is called
    # by means other than the `if` block below)
    ctx.ensure_object(dict)
    ctx.obj["SOURCE_DIR"] = source_dir


@cli.command()
@click.pass_context
@click.option(
    "--converted-dir", required=True, help="Destination folder containing file source"
)
def find_delted_in_source_folder(ctx, converted_dir):
    source_dir = ctx.obj["SOURCE_DIR"]
    for path in Path(converted_dir).expanduser().rglob("*"):
        if path.is_file():
            converted_filepath=str(path)
            source_filepath = converted_filepath.replace(converted_dir, source_dir)
            source_dirname = os.path.dirname(source_filepath)
            source_basename = os.path.basename(source_filepath)
            source_basename_no_extension = os.path.splitext(source_basename)[0]
            source_filepath_with_ext_list = list(Path(source_dirname).expanduser().glob(source_basename_no_extension + "*"))
            if len(source_filepath_with_ext_list) > 1:
                pass
                # print(f"ERROR : More than one file matched for path: {path}" )
                # print(f"{source_filepath_with_ext_list}" )
            elif len(source_filepath_with_ext_list) == 0:
                print("FOUND file in converted but not in source, converted_filepath:")
                print(f"{converted_filepath}" )
            else:
                pass
                # print(f"dirname: {source_dirname}")
                # print(f"bname: {source_basename}")
                # print(f"c: {converted_filepath}")
                # print(source_filepath_with_ext_list)
                # print(source_filepath_with_ext_list[0])


if __name__ == "__main__":
    cli(obj={})
