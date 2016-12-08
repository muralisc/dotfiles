#!/bin/bash 

#check if git repos are changed/behind
for path in `find $HOME -iname .git`
do
    REPOROOT=${path/%.git/}
    cd "$REPOROOT"
    NO_OF_MODFILES=`git status -sb | grep M | wc -l`
    NO_OF_COMMITS=`git status -sb | grep ahead | wc -l`
    if [ $NO_OF_MODFILES -gt 0 ] || [ $NO_OF_COMMITS -gt 0 ] ; then
        echo $REPOROOT $NO_OF_MODFILES $NO_OF_COMMITS
        git diff --stat
        # echo -e "\n\n"
    fi
done
