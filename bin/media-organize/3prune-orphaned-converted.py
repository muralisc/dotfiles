#!/usr/bin/env python3
"""
Prune orphaned converted files: when an original is deleted from the source tree,
its counterpart in the converted tree is left behind. This finds those orphans
and (with --delete) removes them.

Matching mirrors 2encode-images-for-viewing.py, which writes every source file to
`<converted_root>/<relative_dir>/<stem>.jpg`. A converted file is an orphan when
no source file with the same relative dir + stem (any extension) exists.

Safe by default: it only lists what would be removed. Pass --delete to actually
delete (you'll be asked to confirm unless -y/--yes).

Usage:
    3prune-orphaned-converted.py -s ~/data00/footage -c ~/data00/footage_converted
    3prune-orphaned-converted.py -s ~/data00/footage -c ~/data00/footage_converted --delete
"""

from pathlib import Path

import click

from media_common import Stats, console, make_progress, print_summary


def source_stems(src_dir: Path, cache: dict[Path, set[str]]) -> set[str]:
    """Set of file stems present in src_dir (cached per directory)."""
    if src_dir not in cache:
        cache[src_dir] = (
            {p.stem for p in src_dir.iterdir() if p.is_file()} if src_dir.is_dir() else set()
        )
    return cache[src_dir]


def find_orphans(src_root: Path, converted_root: Path) -> tuple[list[Path], list[Path]]:
    """Return (orphaned converted files, kept converted files whose source exists)."""
    converted = sorted(p for p in converted_root.rglob("*") if p.is_file())
    cache: dict[Path, set[str]] = {}
    orphans: list[Path] = []
    kept: list[Path] = []
    for f in converted:
        rel = f.relative_to(converted_root)
        if f.stem not in source_stems(src_root / rel.parent, cache):
            orphans.append(f)
        else:
            kept.append(f)
    return orphans, kept


def run(src: Path, converted: Path, delete: bool, yes: bool, verbose: int) -> Stats:
    console.print(f"Scanning [bold]{converted}[/bold] against [bold]{src}[/bold]…")
    orphans, kept = find_orphans(src, converted)

    stats = Stats()
    stats.skipped = list(kept)  # sources still present

    # -v and up: show paths relative to the converted root instead of just filenames
    def show(f: Path) -> str:
        return str(f.relative_to(converted)) if verbose >= 1 else f.name

    if verbose >= 2:  # kept logs are noisy; only at -vv
        for f in kept:
            console.print(f"[dim]KEEP[/dim]  {show(f)}")

    if not orphans:
        console.print("[green]No orphaned converted files.[/green]")
        print_summary(stats, "deleted" if delete else "would delete", "kept (source present)")
        return stats

    for f in orphans:
        verb = "DELETE" if delete else "[dim]ORPHAN[/dim]"
        console.print(f"{verb}  {show(f)}")

    if not delete:
        stats.ok = list(orphans)  # would-be deletions
        console.print(f"\n[yellow]{len(orphans)} orphan(s) found.[/yellow] Re-run with --delete to remove them.")
        print_summary(stats, "would delete", "kept (source present)")
        return stats

    if not yes and not click.confirm(f"\nDelete {len(orphans)} orphaned converted file(s)?", default=False):
        console.print("[yellow]Aborted — nothing deleted.[/yellow]")
        print_summary(stats, "deleted", "kept (source present)")
        return stats

    with make_progress() as prog:
        task = prog.add_task("Deleting", total=len(orphans))
        for f in orphans:
            prog.update(task, advance=1, description=f.name[:40])
            try:
                f.unlink()
                stats.ok.append(f)
            except OSError as e:
                console.print(f"[red]FAIL[/red]  {show(f)} — {e}")
                stats.failed.append((f, str(e)))

    print_summary(stats, "deleted", "kept (source present)")
    return stats


@click.command()
@click.option("-s", "--src", required=True, type=click.Path(exists=True, file_okay=False), help="Source root folder (the originals)")
@click.option("-c", "--converted", required=True, type=click.Path(exists=True, file_okay=False), help="Converted root folder to prune")
@click.option("--delete", is_flag=True, default=False, help="Actually delete orphans (default: preview only)")
@click.option("-y", "--yes", is_flag=True, default=False, help="Skip the confirmation prompt when deleting")
@click.option("-v", "--verbose", count=True, help="-v: log paths relative to the converted root; -vv: also log kept (source-present) files")
def main(src, converted, delete, yes, verbose):
    if not delete:
        console.print("[dim]Preview — no files will be deleted (pass --delete to remove).[/dim]")
    run(Path(src).expanduser(), Path(converted).expanduser(), delete, yes, verbose)


if __name__ == "__main__":
    main()
