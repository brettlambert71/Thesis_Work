#!/bin/bash

Hypo=$1         # The prefix to search for
Hypo_Sequences=$2  # The destination directory to move files to

# Use `find` to locate all files with the specified prefix in subdirectories
for file in $(find . -type f -name "${Hypo}*"); do
  mv "$file" "$2"
done