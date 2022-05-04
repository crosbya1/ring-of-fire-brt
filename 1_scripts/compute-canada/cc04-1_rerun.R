# Ro code to find which predictions didn't finish and then re-run them
# Can be done in interactive mode
# Once a batch of re-runs has been done, clear the rerun folder and do the next batch


setwd("../../")

dl <- list.dirs("2_pipeline/store/brt-boot-pred", recursive = F)
sl <- list.dirs("2_pipeline/store/brt-boot-pred", recursive = F, full.names = F)

t <- do.call(rbind, lapply(1:length(dl), function(i){
  fl <- list.dirs(dl[i], recursive = F, full.names = FALSE)
  pl <- sapply(1:length(fl), function(x) length(list.files(file.path(dl[i], fl[x]), pattern = "RData")))
  pl <- ifelse(pl < 10, 1, 0)
  return(pl)
  }))

re <- which(rowSums(t) > 0)
sp <- sl[re]


dir <- "1_scripts/compute-canada/prediction/rerun"
if(!dir.exists(dir)){dir.create(dir)}

# Only do 31 at a time to avoid exceeding job limit of 1000
# After one batch is done running, delete those sh files and then create and run the next batch
do <- 1:length(sp)

for (spp in sp[do]){
i <- which(sl == spp)
fl <- list.dirs(dl[i], recursive = F, full.names = FALSE)
d <- rep(1, 32)
sink(paste0(dir, "/cc04_brt-pred_", spp, ".sh"))
cat(paste0("#!/bin/bash 
	#SBATCH --account=def-bayne     # replace this with your own account
	#SBATCH --nodes=1               # number of whole nodes
#SBATCH --ntasks-per-node=", 1, "     # 3 cores on each node
#SBATCH --mem-per-cpu=4G        # use --mem=0 for all ~3.9G mem per core
#SBATCH --time=0-01:00:00       # time (D-HH:MM:SS)
#SBATCH --job-name=brt-pred-", spp, "
#SBATCH --output=%x-%j.out
#SBATCH --mail-user=crosby@ualberta.ca
#SBATCH --mail-type=ALL
#SBATCH --array=", paste(fl[which(d > 0)], collapse = ","), "

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
cd ../../../../2_pipeline/store/brt-boot-pred/", spp, "/$SLURM_ARRAY_TASK_ID  && Rscript --vanilla brt-pred.R

	  "), fill = TRUE)  
  sink()
}


