#!/bin/bash
fastqc=$1
Fast_QC_Reports=$2
# Use `find` to locate all files with the specified prefix in subdirectories

for file in $(find . -type f -name "${AC}*"); do
  fastqc "$file"

find . -type f -name "*${1}*" -exec mv {} "$2" \;
done

