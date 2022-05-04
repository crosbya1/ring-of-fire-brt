#!/bin/bash 
#SBATCH --account=def-bayne     # replace this with your own account
#SBATCH --nodes=1               # number of whole nodes
#SBATCH --ntasks-per-node=2     # 3 cores on each node
#SBATCH --mem-per-cpu=4        # use --mem=0 for all ~3.9G mem per core
#SBATCH --time=0-02:00:00       # time (D-HH:MM:SS)
#SBATCH --job-name=brt-pred
#SBATCH --output=%x-%j.out
#SBATCH --mail-user=crosby@ualberta.ca
#SBATCH --mail-type=ALL
#SBATCH --array="ALFL","AMCR"
# Load modules
module nixpkgs/16.09
module load gcc/9.3.0
module load gdal/3.2.3
module load r/4.1.0
module load jags/4.3.0

# Export the nodes names.
# If all processes are allocated on the same node,
# NODESLIST contains : node1 node1 node1 node1
export NODESLIST=$(echo $(srun hostname))

# Run R script
cd ../../2_pipeline/store/brt-boot-1/$SLURM_ARRAY_JOB_ID  && Rscript --vanilla brt-pred.R

  
