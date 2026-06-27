"""Shared helpers for the media-organize scripts (date parsing, EXIF reading,
rich progress/summary). Imported by the sibling scripts that live in this folder
(e.g. 1import-media-by-exif.py, 2encode-images-for-viewing.py).
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Optional

import exiftool
from rich.console import Console
from rich.progress import BarColumn, MofNCompleteColumn, Progress, TextColumn, TimeElapsedColumn
from rich.table import Table

console = Console()

EXIF_DATE_FMTS = ["%Y:%m:%d %H:%M:%S%z", "%Y:%m:%d %H:%M:%S"]

# Matches a YYYY_MM_DD (or YYYY-MM-DD) token, e.g. in WhatsApp/Signal/Screenshot
# filenames or in a YYYY_MM_DD folder name.
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


def date_from_path(path: Path) -> Optional[datetime]:
    """Scan path components for a YYYY_MM_DD token; return the first valid date."""
    for part in path.parts:
        d = date_from_filename(part)
        if d is not None:
            return d
    return None


def make_progress() -> Progress:
    """Standard rich progress bar used across the media scripts."""
    return Progress(
        TextColumn("[progress.description]{task.description}"),
        BarColumn(),
        MofNCompleteColumn(),
        TimeElapsedColumn(),
        console=console,
    )


def read_metadata(files: list[Path], chunk_size: int = 200, description: str = "Reading EXIF") -> list[dict]:
    """Batch-read EXIF for files in chunks (reusing one exiftool process) with a
    progress bar. Returns metadata dicts aligned with the input order.
    """
    console.print(f"Reading EXIF for [bold]{len(files)}[/bold] files…")
    metadata: list[dict] = []
    with exiftool.ExifToolHelper() as et:
        with make_progress() as prog:
            task = prog.add_task(description, total=len(files))
            for i in range(0, len(files), chunk_size):
                chunk = files[i : i + chunk_size]
                metadata.extend(et.get_metadata([str(f) for f in chunk]))
                prog.update(task, advance=len(chunk))
    return metadata


def print_summary(stats: Stats, ok_label: str, skipped_label: str = "skipped (already exist)") -> None:
    """Render the standard borderless summary table plus a failed-files list."""
    table = Table(show_header=False, box=None, padding=(0, 2))
    table.add_row(f"[green]{len(stats.ok)}[/green]", ok_label)
    table.add_row(f"[yellow]{len(stats.skipped)}[/yellow]", skipped_label)
    table.add_row(f"[red]{len(stats.failed)}[/red]", "failed")
    console.print("\n[bold]Summary[/bold]")
    console.print(table)

    if stats.failed:
        console.print("\n[bold red]Failed files:[/bold red]")
        for path, reason in stats.failed:
            console.print(f"  {path}  [dim]({reason})[/dim]")
