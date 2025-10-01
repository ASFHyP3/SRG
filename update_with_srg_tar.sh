#! /bin/bash

echo "Checking for modified common files..."
common_files=$(comm -12 <(find . -type f | sed 's|^\./||' | sort) <(find my_proc -type f | sed 's|^my_proc/||' | sort))
for file in $common_files; do
    if ! git diff --quiet "$file" "my_proc/$file"; then
        echo "Replacing: $file"
        cp "my_proc/$file" "$file"
    else
        echo "Identical: $file"
    fi
done
echo Done.
