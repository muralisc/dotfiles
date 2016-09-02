#!/bin/bash
# fucntion to extract keys from Awesome Window Manager
grep -Po "(?<=awful.key\()[^}]*}\s*,[^,]*," ~/.config/awesome/rc.lua | \
tr -d {},\" | python -c "
import sys
k = {}
for line in sys.stdin:
    words= line.split()
    mod = '+'.join( words[0:-1] )
    if words[-1] not in k.keys():
        k[ words[-1 ] ] = [mod]
    else:
        k[ words[-1 ] ].append(mod)
print( len(k) , 'keys used' )
for w in 'abcdefghijklmnopqrstuvwxyz':
    if w in k.keys():
        print( w, ' '.join( sorted(k[w]) ) )
for w in k.keys():
    if w not in 'abcdefghijklmnopqrstuvwxyz':
        print( w , ' '.join( sorted(k[w]) ) )
"
