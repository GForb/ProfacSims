#!/bin/bash -l

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

# Path to the find_missing.sh script (modify as necessary)
FIND_MISSING_SCRIPT="./find-missing.sh"

# Run find_missing.sh and capture the output
MISSING_NUMBERS=$($FIND_MISSING_SCRIPT $MAX_NUMBER | grep -E '^[0-9]{4}$')


echo "Missing Numbers: $MISSING_NUMBERS"

SLURM_ARRAY=$(echo $MISSING_NUMBERS | sed 's/ /,/g' | tr '\n' ',')
echo "SLURM Array: $SLURM_ARRAY"


# Create a new SLURM script with a static array directive
cat << EOF > temp_slurm_script.sh
#!/bin/bash -l
#SBATCH --job-name=array_test
#SBATCH --output=output.array.%A.%a
#SBATCH --array=$SLURM_ARRAY
#SBATCH --time=0-12:00
#SBATCH --chdir=/scratch/users/k1811974/ProfacSims/160122-pred-new-data
module load r/4.2.2-gcc-10.3.0-withx-rmath-standalone-python3+-chk-version
Rscript --vanilla run_simulations.R \$SLURM_ARRAY_TASK_ID
EOF

# Submit the new job script
sbatch temp_slurm_script.sh

# Optionally, remove the temporary script after submission
rm temp_slurm_script.sh
