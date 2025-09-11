
# ProfacSims

This reps contains the code for simualtions reported in chapter 5 of Gordon Forbes' PhD thesis.

## Installation

You can install the development version of ProfacSims from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("GForb/ProfacSims")
```

## To run the simualtions
use the function `ipdma_simulation` to run the simulations. This takes a list of simulation parameters as arguments and runs the simulations.

See `example_simulation.R` for an example.

## Organisation of the repo
R contains scripts which define the fucntions required to run the simulations. These can be accessed if the Repo is installed as an R package.
R scripts contains scripts for processing results, exploratory data analysis, and investigating simulations.
Latex for thesis and Qarto for thesis contain the scripts requried for plots and tables included in the thesis. The supplementary material was generated with latex for thesis.
Results contains the files sent to the cluster to run each simulation rep.

## How were the simulations run

The simulations were run using a high performance cluster. See Results/instructions.txt for how to pass the files to the cluster to run a particular set of simulations. 
For each batch of simulations run, the run_simulations.R script contains the parameters used to run the simulations.
Once results have been returned from the cluster, they are saved to a database `Results/Database/sim_results.db`.  This file is not comited to github due to size. 
The files which save the results to the database are in `R scripts/Saving Results`.
Extracts of results for each simulation batch are extracted using the scripts in `Database Extracts and scripts` and saved to a folder called `Results/Database-extracts` (not saved to repo).
These extracts are then used in the quarto files and r scripts that create the material included in the PhD thesis.


## Where were the plots included in the thesis created
The plots used in the main content of the thesis are created in the quarto files in the folder "Quarto for thesis". The individual pngs can be found in the files folders for the different png files.

The supplementary material is generated in the Latex for thesis folder. The plots are saved as pdf then read in by a latex file. The plots and latex file are created by files in this folder.
