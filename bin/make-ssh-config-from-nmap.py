import sys

# Usage: bash bin/nmap-parse-output | python bin/make-ssh-config-from-nmap.py

if not sys.stdin.isatty():
    f = sys.stdin
else:
    f = open(sys.argv[1])

list_of_configs = [
    {"mac_id": "60:F2:62:C1:26:A9", "pref_hostname": "bonjikka", "username": "murali"},
    {"mac_id": "DC:A6:32:23:E7:F8", "pref_hostname": "rapspi4_photo", "username": "pi"},
    {"mac_id": "2c:f0:5d:9b:42:66", "pref_hostname": "mirkwood", "username": "murali"},
    {"mac_id": "90:09:D0:2C:CC:72", "pref_hostname": "nassutti1_i1", "username": "murali"},
    {"mac_id": "90:09:D0:2C:CC:73", "pref_hostname": "nassutti1_i2", "username": "murali"},
]

for line in f:
    found = False
    for config in list_of_configs:
        if line.count(config["mac_id"]) > 0:
            columns = line.split(" ")
            ip_addr = columns[4]
            pref_hostname = config["pref_hostname"]
            username = config["username"]
            print(
                f""" 
Host {pref_hostname}
    HostName {ip_addr} 
    User {username}
"""
            )
            found = True
            break
    if found is False:
        pass
        # print("did not find mac id in config for", line)
