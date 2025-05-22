#!/bin/bash -l


#SBATCH --job-name=array_test
#SBATCH --output=output.array.%A.%a
#SBATCH --array=1-1000
#SBATCH --time=0-6:00
#SBATCH --chdir=/scratch/users/k1811974/ProfacSims/240211-cluster-x

module load r/4.2.2-gcc-10.3.0-withx-rmath-standalone-python3+-chk-version

Rscript --vanilla run_simulations_1rep.R $SLURM_ARRAY_TASK_ID
