#!/bin/bash

# this file is to be used as opener for ranger esp 
# for firefox


log=$HOME/.xlog
path=${1#file://}
echo $path >> ~/file
if [ -d $path ]
then
     urxvtc -e ranger $path
else
     urxvtc -e ranger --selectfile=$path
fi
