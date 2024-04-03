import sys

# Usage: bash bin/nmap-parse-output | python bin/make-ssh-config-from-nmap.py

if not sys.stdin.isatty():
    f = sys.stdin
else:
    f = open(sys.argv[1])

list_of_configs = [
    {"mac_id": "60:F2:62:C1:26:A9", "pref_hostname": "bonjikka"},
    {"mac_id": "DC:A6:32:23:E7:F8", "pref_hostname": "rapspi4_photo"},
    {"mac_id": "2c:f0:5d:9b:42:66", "pref_hostname": "mirkwood"},

]

for line in f:
    for config in list_of_configs:
        if line.count(config["mac_id"]) > 0:
            columns = line.split(" ")
            ip_addr = columns[4]
            pref_hostname = config["pref_hostname"]
            print(
                f""" 
Host {pref_hostname}
    HostName {ip_addr} 
    User murali
"""
            )
        else:
            print(line)
