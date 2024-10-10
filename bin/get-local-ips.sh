echo "-> Local Gateway address (Router):"
ip route | grep default

python3 make-ssh-config-from-nmap.py
