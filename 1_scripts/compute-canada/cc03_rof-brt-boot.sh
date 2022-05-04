#!/bin/bash
#SBATCH --account=def-bayne     # replace this with your own account
#SBATCH --nodes=1               # number of whole nodes
#SBATCH --ntasks-per-node=32    # 32 cores on each node
#SBATCH --mem-per-cpu=4G       # use --mem=0 for all ~3.9G mem per core
#SBATCH --time=04:00:00         # time (HH:MM:SS)
#SBATCH --job-name=rof-brt-boot
#SBATCH --output=%x-%j.out
#SBATCH --mail-user=crosby@ualberta.ca
#SBATCH --mail-type=ALL

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
Rscript --vanilla cc03_rof-brt-boot.R