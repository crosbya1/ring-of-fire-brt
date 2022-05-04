

setwd("../../")

dm <- list.files("2_pipeline/store/brt-boot-pred-mosaic", pattern = ".rds", full.names = T)

sm <- list.files("2_pipeline/store/brt-boot-pred-mosaic", pattern = ".rds", full.names = F)
sm <- sapply(1:length(sm), function(x) strsplit(sm[x], "\\.")[[1]][1])


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


sn <- sl[-which(sl %in% sm)]
saveRDS(sn, file = "1_scripts/compute-canada/mapping/sn.rds")
sn <- readRDS("1_scripts/compute-canada/mapping/sn.rds")

dir <- "1_scripts/compute-canada/mapping/rerun"
if(!dir.exists(dir)){dir.create(dir)}


for (spp in sn){
sink(paste0(dir, "/cc04_brt-map_", spp, ".sh"))
cat(paste0("#!/bin/bash 
#SBATCH --account=def-bayne     # replace this with your own account
#SBATCH --nodes=1               # number of whole nodes
#SBATCH --ntasks-per-node=", 1, "     # 3 cores on each node
#SBATCH --mem-per-cpu=16G        # use --mem=0 for all ~3.9G mem per core
#SBATCH --time=0-00:06:00       # time (D-HH:MM:SS)
#SBATCH --job-name=brt-map-", spp, "
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
cd ../../../../2_pipeline/store/brt-boot-pred/", spp,   "  && Rscript --vanilla brt-map.R
"), fill = TRUE)  
  sink()
}



