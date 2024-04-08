#!/bin/bash

 find $PWD -type f -size +1M -printf "%s\t%p\n" | sort -n > /tmp/nas-files.log 
 tail -10 /tmp/nas-files.log | numfmt --to=iec-i
