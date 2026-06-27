#!/usr/bin/env python3
"""
Downsize images (HEIC, CR3, JPG, PNG, …) into 1920x1080 JPEGs that mirror the
source directory tree. Copies EXIF tags onto the converted JPEG (ImageMagick
drops them) and, when the source has no date, infers one from a YYYY_MM_DD
folder in the path. Feeds lightweight viewers (Immich, Raspberry Pi).

Usage:
    2encode-images-for-viewing.py --src ~/data00/footage --dst ~/data00/footage_converted -n
    2encode-images-for-viewing.py -s ~/data00/footage -d ~/data00/footage_converted -j 8
    2encode-images-for-viewing.py -s ~/data00/footage -d ~/out --converter rawtherapee --regex '.*CR3'
"""

import os
import re
import subprocess
import tempfile
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Optional

import click

from media_common import (
    Stats,
    console,
    date_from_path,
    make_progress,
    parse_exif_date,
    print_summary,
    read_metadata,
)

# EXIF keys checked to decide whether the source already carries a date
DATE_KEYS = [
    "EXIF:DateTimeOriginal",
    "EXIF:CreateDate",
    "QuickTime:CreateDate",
]

# Extensions included by default (matched case-insensitively)
DEFAULT_EXTS = ("heic", "cr3", "jpg", "jpeg", "png", "tif", "tiff")

# Camera RAW formats routed through rawtherapee when --converter rawtherapee
RAW_EXTS = ("cr3", "cr2", "nef", "arw", "dng", "raf", "orf", "rw2")


def source_has_date(meta: dict) -> bool:
    return any(parse_exif_date(str(meta[k])) for k in DATE_KEYS if meta.get(k))


def discover_files(
    src: Path,
    exts: tuple[str, ...],
    regex: Optional[re.Pattern],
    ignore: Optional[re.Pattern],
) -> list[Path]:
    """rglob, filtered by lowercased extension AND optional include/ignore regexes; sorted."""
    wanted = {e.lower().lstrip(".") for e in exts}
    out = []
    for f in src.rglob("*"):
        if not f.is_file():
            continue
        if f.suffix.lower().lstrip(".") not in wanted:
            continue
        if regex is not None and not regex.search(str(f)):
            continue
        if ignore is not None and ignore.search(str(f)):
            continue
        out.append(f)
    return sorted(out)


def dest_for(src_root: Path, dst_root: Path, f: Path) -> Path:
    """Mirror the relative directory; output name is always <stem>.jpg."""
    rel = f.relative_to(src_root).parent
    return dst_root / rel / f"{f.stem}.jpg"


def _run(cmd: list[str]) -> None:
    """Run a subprocess, raising RuntimeError with a stderr tail on failure."""
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        tail = (proc.stderr or proc.stdout or "").strip().splitlines()
        reason = tail[-1] if tail else f"exit {proc.returncode}"
        raise RuntimeError(f"{Path(cmd[0]).name}: {reason}")


def convert_one(
    f: Path, dst: Path, has_date: bool, geometry: str, quality: int, converter: str
) -> tuple[Path, Path, bool]:
    """Worker (runs in a thread). Returns (src, dst, used_path_date).

    Raises RuntimeError on a magick/exiftool failure for this file only.
    """
    used_path_date = False
    tmp: Optional[str] = None
    try:
        dst.parent.mkdir(parents=True, exist_ok=True)
        is_raw = f.suffix.lower().lstrip(".") in RAW_EXTS

        if converter == "rawtherapee" and is_raw:
            # Demosaic to a full-res temp JPEG, then resize with magick. Avoids
            # needing a .pp3 resize profile and reuses the magick resize path.
            fd, tmp = tempfile.mkstemp(suffix=".jpg")
            os.close(fd)
            _run(["rawtherapee-cli", "-o", tmp, "-j92", "-js1", "-Y", "-c", str(f)])
            _run(["magick", tmp, "-resize", geometry, "-quality", str(quality), str(dst)])
        else:
            _run(["magick", str(f), "-resize", geometry, "-quality", str(quality), str(dst)])

        # magick drops metadata — copy it back from the source.
        # -m ignores minor errors (e.g. Pixel's incomplete extended XMP) that
        # would otherwise make exiftool refuse to write the file.
        _run(["exiftool", "-m", "-overwrite_original_in_place", "-tagsFromFile", str(f), str(dst)])

        # Source lacked a date: infer one from the folder path if possible
        if not has_date:
            d = date_from_path(f)
            if d is not None:
                _run([
                    "exiftool", "-m", "-overwrite_original",
                    f"-AllDates={d.strftime('%Y:%m:%d 00:00:00')}", str(dst),
                ])
                used_path_date = True

        return f, dst, used_path_date
    except Exception:
        # Don't leave a partial/zero-byte dest that a later run would skip
        try:
            if dst.exists():
                dst.unlink()
        except OSError:
            pass
        raise
    finally:
        if tmp and os.path.exists(tmp):
            try:
                os.unlink(tmp)
            except OSError:
                pass


def run(
    src: Path,
    dst: Path,
    dry_run: bool,
    exts: tuple[str, ...],
    regex: Optional[re.Pattern],
    ignore: Optional[re.Pattern],
    geometry: str,
    quality: int,
    jobs: int,
    converter: str,
    verbose: int,
) -> Stats:
    files = discover_files(src, exts, regex, ignore)
    if not files:
        console.print("[yellow]No matching files found.[/yellow]")
        return Stats()

    # -v: path relative to src; -vvv: full absolute path; else just the filename
    def show(f: Path) -> str:
        if verbose >= 3:
            return str(f)
        return str(f.relative_to(src)) if verbose >= 1 else f.name

    # Destination is shown relative to dst, or full absolute path at -vvv
    def show_dst(p: Path) -> str:
        return str(p) if verbose >= 3 else str(p.relative_to(dst))

    stats = Stats()

    # Idempotency + collision handling → list of (src, dest) still to do
    pending: list[tuple[Path, Path]] = []
    claimed: dict[Path, Path] = {}
    for f in files:
        target = dest_for(src, dst, f)
        if target.exists():
            if verbose >= 2:  # skip logs are noisy; only at -vv
                console.print(f"[dim]SKIP[/dim]  {show(f)} → {show_dst(target)}")
            stats.skipped.append(f)
            continue
        if target in claimed:
            console.print(f"[red]FAIL[/red]  {show(f)} — dest name collision with {claimed[target].name}")
            stats.failed.append((f, f"dest name collision with {claimed[target].name}"))
            continue
        claimed[target] = f
        pending.append((f, target))

    if not pending:
        print_summary(stats, "would encode" if dry_run else "encoded")
        return stats

    # Read source EXIF (single-threaded) to decide the date-from-path fallback
    srcs = [f for f, _ in pending]
    has_date = {f: source_has_date(meta) for f, meta in zip(srcs, read_metadata(srcs))}

    if dry_run:
        for f, target in pending:
            note = ""
            if not has_date[f] and date_from_path(f) is not None:
                note = " [dim](date-from-path)[/dim]"
            console.print(f"[dim]ENCODE[/dim]  {show(f)} → {show_dst(target)}{note}")
            stats.ok.append(f)
        print_summary(stats, "would encode")
        return stats

    # Parallel conversion. magick/exiftool/rawtherapee-cli are subprocesses that
    # release the GIL, so threads give real N-way parallelism without pickling.
    with make_progress() as prog:
        task = prog.add_task("Encoding", total=len(pending))
        with ThreadPoolExecutor(max_workers=jobs) as ex:
            futs = {
                ex.submit(convert_one, f, target, has_date[f], geometry, quality, converter): f
                for f, target in pending
            }
            for fut in as_completed(futs):
                f = futs[fut]
                prog.update(task, advance=1, description=f.name[:40])
                try:
                    src_f, dst_f, used = fut.result()
                    note = " [dim](date-from-path)[/dim]" if used else ""
                    console.print(f"[green]ENCODE[/green]  {show(src_f)} → {show_dst(dst_f)}{note}")
                    stats.ok.append(src_f)
                except Exception as e:
                    console.print(f"[red]FAIL[/red]  {show(f)} — {e}")
                    stats.failed.append((f, str(e)))

    print_summary(stats, "would encode" if dry_run else "encoded")
    return stats


@click.command()
@click.option("-s", "--src", required=True, type=click.Path(exists=True, file_okay=False), help="Source root folder")
@click.option("-d", "--dst", required=True, type=click.Path(file_okay=False), help="Destination root (mirrors source)")
@click.option("-n", "--dry-run", is_flag=True, default=False, help="Preview without converting anything")
@click.option("--ext", "exts", multiple=True, default=DEFAULT_EXTS, show_default=True, help="Extensions to include (case-insensitive); repeatable")
@click.option("--regex", default=None, help="Optional full-path regex to include (ANDed with --ext)")
@click.option("--ignore", default=None, help="Optional full-path regex to exclude (skips matching files)")
@click.option("--geometry", default="1920x1080>", show_default=True, help="ImageMagick -resize geometry")
@click.option("--quality", type=int, default=75, show_default=True, help="JPEG quality")
@click.option("-j", "--jobs", type=int, default=os.cpu_count(), show_default=True, help="Parallel conversions")
@click.option("--converter", type=click.Choice(["magick", "rawtherapee"]), default="magick", show_default=True, help="RAW conversion backend")
@click.option("-v", "--verbose", count=True, help="-v: paths relative to src; -vv: also log skipped (already-converted) files; -vvv: full absolute paths")
def main(src, dst, dry_run, exts, regex, ignore, geometry, quality, jobs, converter, verbose):
    if dry_run:
        console.print("[dim]Dry run — no files will be converted.[/dim]")
    pattern = re.compile(regex) if regex else None
    ignore_pattern = re.compile(ignore) if ignore else None
    run(Path(src), Path(dst), dry_run, exts, pattern, ignore_pattern, geometry, quality, jobs, converter, verbose)


if __name__ == "__main__":
    main()
