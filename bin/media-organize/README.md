Practical use examples:

ex:
Script 1
```
~/src/dotfiles/bin/media-organize/venv/bin/python3 1import-media-by-exif.py \
    --dst ~/data00/footage \
    --op mv --default-camera iPhone_13_mini \
    --src ~/data00/footage/uncategorised/whatsapp-8kuzhi-sisters
```

ex:
Script 2
```
~/src/dotfiles/bin/media-organize/venv/bin/python3 ~/src/dotfiles/bin/media-organize/2encode-images-for-viewing.py -s ~/data00/footage/ -d ~/data00/footage_converted/ --regex '2023' -nvv
```
