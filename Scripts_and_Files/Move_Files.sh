#!/bin/bash

AC=$1         # The prefix to search for
Asarum_Sequences=$2  # The destination directory to move files to

# Use `find` to locate all files with the specified prefix in subdirectories
for file in $(find . -type f -name "${AC}*"); do
  mv "$file" "$2"
done