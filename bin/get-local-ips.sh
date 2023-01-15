echo "-> Local Gateway address (Router):"
ip route | grep default

echo "-> Get active ips in local network"
sudo nmap -sn 192.168.1.1/24 > /tmp/nmap-ping-scan.txt

echo ""
echo "-> Pi hole:"
grep -B2 B8:27:EB:E0:F9:05 /tmp/nmap-ping-scan.txt

echo ""
echo "-> mirkwood.lan:"
grep -B2 28:EE:52:1B:0A:52  /tmp/nmap-ping-scan.txt
