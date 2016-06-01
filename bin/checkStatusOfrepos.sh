
for i in ~/dotfiles \
    ~/.vim/bundle/vim-snippets/ \
    ~/.vim/bundle/vim-colorschemes/
do
    cd $i
    echo `git status -sb | grep M | wc -l` $i 
done
