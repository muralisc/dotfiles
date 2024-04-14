FILE_INDEX=0
loop_count=0
for file in $(\ls ~/shared_folders/transfer_work/obsidian-images/); do
    if [[ $((loop_count%20)) = 0 ]]; then
        FILE_INDEX=$((FILE_INDEX+1))
        > ~/src/obsidian-vault2/All-Images-${FILE_INDEX}.md
    fi
echo "
$loop_count \`$file\`
![$file|120](obsidian-images/$file)
" >> ~/src/obsidian-vault2/All-Images-${FILE_INDEX}.md
loop_count=$((loop_count+1))
done
