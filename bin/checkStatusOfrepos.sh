
for i in ~/dotfiles \
    ~/.vim/plugged/vim-snippets/ \
    ~/.vim/plugged/vim-colorschemes/
do
    cd $i
    echo `git status -sb | grep M | wc -l` `git status -sb | grep ahead | wc -l` $i 
    echo "stat===================="
    git diff --stat
    echo "numstat================="
    git diff --numstat
    echo "shortstat==============="
    git diff --shortstat
    echo "dirstat================="
    git diff --dirstat
done
