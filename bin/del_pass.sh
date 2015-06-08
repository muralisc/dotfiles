#!/bin/bash
# ssh server 'sed  "/'$password_string'/p;d" .zsh_history'     # to test
for comp in "server" "vacha" "gen" "check"
do
    ssh $comp 'sed -i "/larum/d;p" .zsh_history'     # to delete
    ssh $comp 'sed -i "/harder/d;p" .zsh_history'     # to delete
done
