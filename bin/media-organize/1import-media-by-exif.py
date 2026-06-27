#!/usr/bin/env python3
"""
Organise a media dump into YYYY/YYYY_MM_DD/<Camera>/<file> using EXIF dates.
Works with photos (JPEG, HEIC, CR3, …) and video (MP4, MOV, …).

Usage:
    1import-media-by-exif.py --src ~/dump --dst ~/footage --op cp
    1import-media-by-exif.py --src ~/dump --dst ~/footage --op cp -n --default-camera WhatsApp
"""

import re
import shutil
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Optional

import click
import exiftool
from rich.console import Console
from rich.progress import BarColumn, MofNCompleteColumn, Progress, TextColumn, TimeElapsedColumn
from rich.table import Table

console = Console()

# EXIF keys tried in priority order; covers photos, HEIC, and video containers
DATE_KEYS = [
    "EXIF:DateTimeOriginal",
    "EXIF:CreateDate",
    "QuickTime:CreateDate",
    "QuickTime:MediaCreateDate",
    "QuickTime:TrackCreateDate",
    "File:FileModifyDate",
]

EXIF_DATE_FMTS = ["%Y:%m:%d %H:%M:%S%z", "%Y:%m:%d %H:%M:%S"]

# WhatsApp: IMG-20210615-WA0001.jpg  Signal: signal-2021-06-15-14-30-22.jpg  Screenshots: Screenshot_20210615-143022.png
FILENAME_DATE_RE = re.compile(r'(\d{4})[_\-](\d{2})[_\-](\d{2})')


@dataclass
class Stats:
    ok: list[Path] = field(default_factory=list)
    skipped: list[Path] = field(default_factory=list)
    failed: list[tuple[Path, str]] = field(default_factory=list)


def parse_exif_date(value: str) -> Optional[datetime]:
    for fmt in EXIF_DATE_FMTS:
        try:
            return datetime.strptime(value, fmt)
        except (ValueError, TypeError):
            pass
    return None


def date_from_filename(name: str) -> Optional[datetime]:
    m = FILENAME_DATE_RE.search(name)
    if m:
        try:
            return datetime(int(m.group(1)), int(m.group(2)), int(m.group(3)))
        except ValueError:
            pass
    return None


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


def run(src: Path, dst: Path, op: str, dry_run: bool, default_camera: str) -> Stats:
    files = sorted(f for f in src.rglob("*") if f.is_file())
    if not files:
        console.print("[yellow]No files found.[/yellow]")
        return Stats()

    console.print(f"Reading EXIF for [bold]{len(files)}[/bold] files…")
    chunk_size = 200
    metadata: list[dict] = []
    with exiftool.ExifToolHelper() as et:
        with Progress(
            TextColumn("[progress.description]{task.description}"),
            BarColumn(),
            MofNCompleteColumn(),
            TimeElapsedColumn(),
            console=console,
        ) as exif_progress:
            task = exif_progress.add_task("Reading EXIF", total=len(files))
            for i in range(0, len(files), chunk_size):
                chunk = files[i : i + chunk_size]
                metadata.extend(et.get_metadata([str(f) for f in chunk]))
                exif_progress.update(task, advance=len(chunk))

    stats = Stats()
    used_dsts: set[str] = set()

    progress = Progress(
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
        console=console,
    )

    with progress:
        task = progress.add_task("Processing", total=len(files))

        for file_path, meta in zip(files, metadata):
            progress.update(task, advance=1, description=file_path.name[:40])

            dt = resolve_date(meta, file_path.name)
            if dt is None:
                console.print(f"[red]FAIL[/red]  {file_path.name} — no date found, skipping")
                stats.failed.append((file_path, "no date"))
                continue

            camera = resolve_camera(meta, default_camera)
            initial = dst / dt.strftime("%Y/%Y_%m_%d") / camera / file_path.name

            # Idempotency: already placed here by a previous run
            if initial.exists() and str(initial) not in used_dsts:
                console.print(f"[dim]SKIP[/dim]  {file_path.name} → {initial.relative_to(dst)}")
                stats.skipped.append(file_path)
                used_dsts.add(str(initial))
                continue

            dst_file = unique_path(initial, used_dsts)
            used_dsts.add(str(dst_file))

            label = f"[dim]{op.upper()}[/dim]" if dry_run else f"[green]{op.upper()}[/green]"
            console.print(f"{label:<6}  {file_path.name} → {dst_file.relative_to(dst)}")

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
def main(src, dst, op, dry_run, default_camera):
    if dry_run:
        console.print("[dim]Dry run — no files will be moved or copied.[/dim]")
    stats = run(Path(src), Path(dst), op, dry_run, default_camera)

    table = Table(show_header=False, box=None, padding=(0, 2))
    table.add_row(f"[green]{len(stats.ok)}[/green]", f"{'would ' if dry_run else ''}{op}d")
    table.add_row(f"[yellow]{len(stats.skipped)}[/yellow]", "skipped (already exist)")
    table.add_row(f"[red]{len(stats.failed)}[/red]", "failed")
    console.print("\n[bold]Summary[/bold]")
    console.print(table)

    if stats.failed:
        console.print("\n[bold red]Failed files:[/bold red]")
        for path, reason in stats.failed:
            console.print(f"  {path}  [dim]({reason})[/dim]")


if __name__ == "__main__":
    main()
