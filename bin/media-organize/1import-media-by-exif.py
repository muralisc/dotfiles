#!/usr/bin/env python3
"""
Organise a media dump into YYYY/YYYY_MM_DD/<Camera>/<file> using EXIF dates.
Works with photos (JPEG, HEIC, CR3, …) and video (MP4, MOV, …).

Usage:
    1import-media-by-exif.py --src ~/dump --dst ~/footage --op cp
    1import-media-by-exif.py --src ~/dump --dst ~/footage --op cp -n --default-camera WhatsApp
"""

import shutil
from datetime import datetime
from pathlib import Path
from typing import Optional

import click

from media_common import (
    Stats,
    console,
    date_from_filename,
    make_progress,
    parse_exif_date,
    print_summary,
    read_metadata,
)

# EXIF keys tried in priority order; covers photos, HEIC, and video containers
DATE_KEYS = [
    "EXIF:DateTimeOriginal",
    "EXIF:CreateDate",
    "QuickTime:CreateDate",
    "QuickTime:MediaCreateDate",
    "QuickTime:TrackCreateDate",
    "File:FileModifyDate",
]


def resolve_date(meta: dict, filename: str) -> Optional[datetime]:
    for key in DATE_KEYS:
        val = meta.get(key)
        if val:
            dt = parse_exif_date(str(val))
            if dt:
                return dt
    return date_from_filename(filename)


def resolve_camera(meta: dict, default: str) -> str:
    model = (meta.get("EXIF:Model") or "").strip().replace(" ", "_")
    return model or default


def unique_path(path: Path, used: set[str]) -> Path:
    """Append _1, _2, … until the path is free both on disk and within this run."""
    candidate = path
    n = 1
    while candidate.exists() or str(candidate) in used:
        candidate = path.parent / f"{path.stem}_{n}{path.suffix}"
        n += 1
    return candidate


def run(src: Path, dst: Path, op: str, dry_run: bool, default_camera: str, verbose: int) -> Stats:
    files = sorted(f for f in src.rglob("*") if f.is_file())
    if not files:
        console.print("[yellow]No files found.[/yellow]")
        return Stats()

    # -v: path relative to src; -vvv: full absolute path; else just the filename
    def show(f: Path) -> str:
        if verbose >= 3:
            return str(f)
        return str(f.relative_to(src)) if verbose >= 1 else f.name

    # Destination is shown relative to dst, or full absolute path at -vvv
    def show_dst(p: Path) -> str:
        return str(p) if verbose >= 3 else str(p.relative_to(dst))

    metadata = read_metadata(files)

    stats = Stats()
    used_dsts: set[str] = set()

    progress = make_progress()

    with progress:
        task = progress.add_task("Processing", total=len(files))

        for file_path, meta in zip(files, metadata):
            progress.update(task, advance=1, description=file_path.name[:40])

            dt = resolve_date(meta, file_path.name)
            if dt is None:
                console.print(f"[red]FAIL[/red]  {show(file_path)} — no date found, skipping")
                stats.failed.append((file_path, "no date"))
                continue

            camera = resolve_camera(meta, default_camera)
            initial = dst / dt.strftime("%Y/%Y_%m_%d") / camera / file_path.name

            # Idempotency: already placed here by a previous run
            if initial.exists() and str(initial) not in used_dsts:
                if verbose >= 2:  # skip logs are noisy; only at -vv
                    console.print(f"[dim]SKIP[/dim]  {show(file_path)} → {show_dst(initial)}")
                stats.skipped.append(file_path)
                used_dsts.add(str(initial))
                continue

            dst_file = unique_path(initial, used_dsts)
            used_dsts.add(str(dst_file))

            label = f"[dim]{op.upper()}[/dim]" if dry_run else f"[green]{op.upper()}[/green]"
            console.print(f"{label:<6}  {show(file_path)} → {show_dst(dst_file)}")

            if not dry_run:
                dst_file.parent.mkdir(parents=True, exist_ok=True)
                if op == "cp":
                    shutil.copy2(file_path, dst_file)
                else:
                    shutil.move(str(file_path), dst_file)

            stats.ok.append(file_path)

    return stats


@click.command()
@click.option("--src", required=True, type=click.Path(exists=True, file_okay=False), help="Source dump folder")
@click.option("--dst", required=True, type=click.Path(file_okay=False), help="Destination root folder")
@click.option("--op", required=True, type=click.Choice(["cp", "mv"]), help="Operation to perform")
@click.option("-n", "--dry-run", is_flag=True, default=False, help="Preview without making any changes")
@click.option("--default-camera", default="NoModelName", show_default=True, help="Fallback name when EXIF model is absent")
@click.option("-v", "--verbose", count=True, help="-v: paths relative to src; -vv: also log skipped (already-placed) files; -vvv: full absolute paths")
def main(src, dst, op, dry_run, default_camera, verbose):
    if dry_run:
        console.print("[dim]Dry run — no files will be moved or copied.[/dim]")
    stats = run(Path(src), Path(dst), op, dry_run, default_camera, verbose)
    print_summary(stats, f"{'would ' if dry_run else ''}{op}d")


if __name__ == "__main__":
    main()
