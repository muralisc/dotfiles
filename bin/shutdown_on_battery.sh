#!/bin/bash
for i in /sys/bus/*/devices/*/power/control; do sudo sh -c "echo on > $i"; done
sudo rmmod ehci_pci; sudo rmmod ehci_hcd;
# sudo shutdown -h now
sudo halt -p
# poweroff
