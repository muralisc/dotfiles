
for i in ~/dotfiles \
    ~/.vim/plugged/vim-snippets/ \
    ~/.vim/plugged/vim-colorschemes/
do
    cd $i
    echo `git status -sb | grep M | wc -l` `git status -sb | grep ahead | wc -l` $i 
done
