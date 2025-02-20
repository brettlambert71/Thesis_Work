#!/bin/bash

MD5=$1         # The prefix to search for


# Use `find` to locate all files with the specified prefix in subdirectories
for file in $(find . -type f -name "${MD5}*"); do
  rm "$file"
done