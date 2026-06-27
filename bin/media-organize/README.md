# media-organize

Practical usage examples.

## Script 1 — `1import-media-by-exif.py`

```bash
~/src/dotfiles/bin/media-organize/venv/bin/python3 1import-media-by-exif.py \
    --dst ~/data00/footage \
    --op mv --default-camera iPhone_13_mini \
    --src ~/data00/footage/uncategorised/whatsapp-8kuzhi-sisters
```

## Script 2 — `2encode-images-for-viewing.py`

```
~/src/dotfiles/bin/media-organize/venv/bin/python3 \
    ~/src/dotfiles/bin/media-organize/2encode-images-for-viewing.py \
    -s ~/data00/footage/ \
    -d ~/data00/footage_converted/  \
    -vvvn \
    --regex '2023' \
    --ignore 'murali_kuru_marriage'
```

## Verbose semantics (`-v` / `-vv` / `-vvv`)

All three scripts share the same verbosity behaviour. Summary counts are always
shown regardless of verbosity.

| Level     | `1import`             | `2encode`             | `3prune`                   |
| --------- | --------------------- | --------------------- | -------------------------- |
| (default) | filenames, no SKIP    | filenames, no SKIP    | filenames, no KEEP         |
| `-v`      | src-relative paths    | src-relative paths    | converted-relative paths   |
| `-vv`     | + per-file SKIP lines | + per-file SKIP lines | + per-file KEEP lines      |
| `-vvv`    | full absolute paths   | full absolute paths   | full absolute paths        |

