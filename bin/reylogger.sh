tail -1 /var/log/logkeys.log | sed 's/.*\([a-zA-Z<>#+]\{10\}\)/\1/'
