#!/bin/bash
#SBATCH --account=PAS0331
#SBATCH --time=4:00:00
#SBATCH --nodes=2
#SBATCH --ntasks-per-node=2
#SBATCH --mem=10gb
#SBATCH --job-name=Trim_1
trimmed=$1
Trimmed_Seqs_1=$2
zip=$3
# Use `find` to locate all files with the specified prefix in subdirectories

for file in $(find . -type f -name "*${3}*"); do
  trim_galore "$file"
done
find . -type f -name "*${1}*" -exec mv {} "$2" \;

#Run with this: ./Trim_and_Move.sh trim ./Trimmed_Seqs_1