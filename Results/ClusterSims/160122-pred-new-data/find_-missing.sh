#!/bin/bash

# Check if an argument is provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 MAX_NUMBER"
    exit 1
fi

# Read the maximum number from the first argument
MAX_NUMBER=$1

# Check if MAX_NUMBER is a valid number
if ! [[ "$MAX_NUMBER" =~ ^[0-9]+$ ]]; then
    echo "Error: MAX_NUMBER must be a non-negative integer."
    exit 1
fi

# Use the current working directory
DIRECTORY=$(pwd)

# Create an array to hold the existing numbers
existing_numbers=()

# Loop over files and extract numbers
for file in "$DIRECTORY"/results????.RDS; do
    if [[ -f $file ]]; then
        number=$(basename "$file" .RDS | sed 's/results//')
        existing_numbers+=("$number")
    fi
done

# Sort and remove duplicates
existing_numbers=($(printf "%s\n" "${existing_numbers[@]}" | sort -u))

# Check for missing numbers
echo "Missing numbers:"
for ((i=0; i<=$MAX_NUMBER; i++)); do
    printf -v num "%04d" $i
    if [[ ! " ${existing_numbers[*]} " =~ " ${num} " ]]; then
        echo "$num"
    fi
done
