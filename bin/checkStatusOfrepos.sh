
for i in ~/dotfiles \
    ~/.vim/bundle/vim-snippets/ \
    ~/.vim/bundle/vim-colorschemes/
do
    echo ========================= $i
    cd $i
    git status -sb
done
