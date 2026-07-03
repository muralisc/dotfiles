#!/usr/bin/env python3
"""Scan the local network with nmap and emit SSH config entries for known hosts.

Usage:
    sudo python bin/make-ssh-config-from-nmap.py
"""

import re
import subprocess

SUBNETS = ["192.168.8.1/24", "192.168.1.1/24"]

LIST_OF_CONFIGS = [
    {"mac_id": "60:F2:62:C1:26:A9", "pref_hostname": "nuc", "username": "murali"},
    {"mac_id": "DC:A6:32:23:E7:F8", "pref_hostname": "rapspi4_photo", "username": "pi"},
    {"mac_id": "2c:f0:5d:9b:42:66", "pref_hostname": "mirkwood", "username": "murali"},
    {"mac_id": "90:09:D0:2C:CC:72", "pref_hostname": "nassutti1_i1", "username": "murali"},
    {"mac_id": "90:09:D0:2C:CC:73", "pref_hostname": "nassutti1_i2", "username": "murali"},
]


def scan(subnet):
    """Run `nmap -sn` and yield (ip_addr, mac_id) pairs for each host found."""
    output = subprocess.run(
        ["nmap", "-sn", subnet], capture_output=True, text=True, check=True
    ).stdout
    ip_addr = None
    for line in output.splitlines():
        report = re.search(r"Nmap scan report for .*?([\d.]+)\)?$", line)
        if report:
            ip_addr = report.group(1)
        mac = re.search(r"MAC Address: (\S+)", line)
        if mac and ip_addr:
            yield ip_addr, mac.group(1)


def main():
    by_mac = {config["mac_id"].lower(): config for config in LIST_OF_CONFIGS}
    for subnet in SUBNETS:
        for ip_addr, mac_id in scan(subnet):
            config = by_mac.get(mac_id.lower())
            if config is None:
                continue
            print(
                f"""
Host {config["pref_hostname"]}
    HostName {ip_addr}
    User {config["username"]}
"""
            )


if __name__ == "__main__":
    main()
