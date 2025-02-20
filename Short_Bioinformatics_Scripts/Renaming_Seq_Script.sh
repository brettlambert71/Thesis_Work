#!/bin/bash

# Counter for sample number
sample_number=1
count=0 # Initialize count

# Use find to safely list files and avoid issues with spaces or special characters
for file in $(find . -maxdepth 1 -type f | sort); do
    # Remove leading ./ from file name
    file=$(basename "$file")

    # Determine if it's R1 or R2 based on the count
    if [[ $((count % 2)) -eq 0 ]]; then
        suffix="R1"
    else
        suffix="R2"
    fi

    # Zero-pad the sample number for the first nine samples
    if [[ $sample_number -lt 10 ]]; then
        padded_sample_number="0$sample_number"
    else
        padded_sample_number="$sample_number"
    fi

    # Construct the new filename
    new_name="sample${padded_sample_number}_${suffix}.fastq"

    # Rename the file
    mv "$file" "$new_name"

    # Increment sample number only after processing R2
    if [[ "$suffix" == "R2" ]]; then
        ((sample_number++))
    fi

    # Increment the count
    ((count++))
done
