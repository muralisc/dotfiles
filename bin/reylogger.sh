tail -1 /var/log/logkeys.log | sed 's/<#+[0-9]\+>//g' | sed 's/.*\([a-zA-Z<>#+]\{20\}\)/\1/'
