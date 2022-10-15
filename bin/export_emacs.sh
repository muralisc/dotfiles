#!/bin/bash
mkdir -p ~/public_html/emacs/
emacs -batch -l ~/.emacs -eval '(org-batch-store-agenda-views)'
emacs -l ~/.emacs -batch -eval '(progn
        (org-batch-agenda-csv "a"
       org-agenda-span (quote year)))' | awk -F"," '{print $1","$6","$2}' | column -t -s, > ~/public_html/emacs/agenda-csv-export.txt
emacs -batch -l ~/.emacs -eval '(org-batch-agenda "a" org-agenda-span 30)' >> ~/public_html/emacs/agenda-csv-export.txt
