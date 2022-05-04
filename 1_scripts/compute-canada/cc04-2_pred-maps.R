
library(raster)
setwd("../../")
sl <- list.dirs("2_pipeline/store/brt-boot-pred", recursive = F, full.names=FALSE)
dl <- list.dirs("2_pipeline/store/brt-boot-pred", recursive = F)


t <- do.call(rbind, lapply(1:length(dl), function(i){
  fl <- list.dirs(dl[i], recursive = F, full.names = FALSE)
  pl <- sapply(1:length(fl), function(x) length(list.files(file.path(dl[i], fl[x]), pattern = "RData")))
  pl <- ifelse(pl < 10, 1, 0)
  return(pl)
}))

re <- which(rowSums(t) == 0)
sp.list <- sl[re]



dir <- file.path("2_pipeline/store/brt-boot-pred")
if(!dir.exists(dir)){dir.create(dir)}


for(spp in sp.list){
  dir1 <- file.path(dir, spp)
  sink(file.path(dir1, "brt-map.R"))
  cat("
    rpath <- '../../../../'
    library(raster)
    library(matrixStats)
    
    spp <- basename(getwd())
    
    load(file.path(rpath, '0_data/processed/predictor-layers/chunks2/chunks.RData'))
    r <- raster(paste0(
      rpath, '/0_data/processed/prediction-rasters/',
      'elev.tif'))
    s <- !is.na(values(r))
    
    dl <- list.dirs(recursive = F)
    sppred <- lapply(1:10, function(i){
      load(paste0(rpath, '/0_data/processed/predictor-layers/chunks2/variables-', i, '.RData'))
      f <- paste0(dl, '/', spp, '-chunk-', i, '.RData') 
      fl <- do.call(cbind, lapply(1:length(f), function(x){
        load(f[x])
        return(p)
      }))
    })
    v <- rowMeans(do.call(rbind, sppred))
    ri <- r
    values(ri)[s] <- v[s]
    dir1 <- '../../brt-boot-pred-mosaic'
    if(!dir.exists(dir1)){dir.create(dir1)}
    writeRaster(ri,
                paste0(dir1, '/', spp, '.tif'),
                overwrite=TRUE)
    saveRDS(v, file = paste0(dir1, '/', spp, '.rds'))

    rs <- r
    sd <- rowSds(do.call(rbind, sppred))
    values(rs)[s] <- sd[s]
    writeRaster(rs,
                paste0(dir1, '/', spp, '_uncertainty.tif'),
                overwrite=TRUE)
    saveRDS(sd, file = paste0(dir1, '/', spp, '_uncertainty.rds'))
  ")
  sink()
}

dir2 <- "1_scripts/compute-canada/mapping"
if(!dir.exists(dir2)){dir.create(dir2)}

for (spp in sp.list){
sink(paste0(dir2, "/cc04_brt-map_", spp, ".sh"))
cat(paste0("#!/bin/bash 
#SBATCH --account=def-bayne     # replace this with your own account
#SBATCH --nodes=1               # number of whole nodes
#SBATCH --ntasks-per-node=", 1, "     # 3 cores on each node
#SBATCH --mem-per-cpu=16G        # use --mem=0 for all ~3.9G mem per core
#SBATCH --time=0-00:08:00       # time (D-HH:MM:SS)
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
cd ../../../2_pipeline/store/brt-boot-pred/", spp,   "  && Rscript --vanilla brt-map.R
"), fill = TRUE)  
  sink()
}





