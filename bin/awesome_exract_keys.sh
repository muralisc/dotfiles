grep -Po "(?<=awful.key\().*},[^,]*," ~/.config/awesome/rc.lua | tr -d {},\" | python -c "
import sys
k = {}
for line in sys.stdin:
    words= line.split()
    mod = '+'.join( words[0:-1] )
    if words[-1] not in k.keys():
        k[ words[-1 ] ] = [mod]
    else:
        k[ words[-1 ] ].append(mod)
for w in k.keys():
    print( w , ' '.join( sorted(k[w]) ) )
" | sort 
