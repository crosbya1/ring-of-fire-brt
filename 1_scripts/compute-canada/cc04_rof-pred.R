# R code to create R files and BASH files for predicting bird density from 
# bootstrapped BRT models for birds in Far North Ontario 
# Can be run in interactive mode



library(dismo)
library(gbm)
library(raster)

setwd("../../")

r <- raster(paste0(
  "0_data/processed/prediction-rasters/",
  "elev.tif"))
s <- !is.na(values(r))

load("0_data/processed/BAMv6_RoFpackage_2022-04.RData")

sp.pred <- list.dirs("2_pipeline/store/brt-boot-1", recursive = FALSE, full.names = FALSE)


dir <- file.path("2_pipeline/store/brt-boot-pred")
if(!dir.exists(dir)){dir.create(dir)}

# Create the files to run jobs for all species simultaneously, with 32 cores so that each model is running siumultaneously 

for (spp in sp.pred){
  dir1 <- file.path(dir, spp)
  if(!dir.exists(dir1)){dir.create(dir1)}
  mods <- list.files(file.path("2_pipeline/store/brt-boot-1", spp), pattern = "RData")
  npred <- length(mods)
  for(i in 1:npred){
    dir.create(file.path(dir1, i))
    sink(file.path(dir1, i, "brt-pred.R"))
    cat(paste0("
    library(parallel)
    library(dismo)
    library(gbm)
    library(raster)
    
    CHUNKS <- 1:10
    wd <- '../../../..'
    x <- basename(getwd())
    setwd('../')
    spp <- basename(getwd())

    for(j in CHUNKS){
      gc()
	  f <- file.path(wd, paste0('2_pipeline/store/brt-boot-pred/', spp, '/', x, '/', spp, '-chunk-', j, '.RData'))
      load(file.path(wd, paste0('0_data/processed/predictor-layers/chunks2/variables-', j, '.RData')))
      load(file.path(wd, '2_pipeline/store/brt-boot-1', spp, paste0(x, '.RData')))
      p <- predict.gbm(r2, M, r2$n.trees, type='response')
      
      save(p, file=f)
      }
      "))
    sink()
    }
  }


dir <- "1_scripts/compute-canada/prediction"
if(!dir.exists(dir)){dir.create(dir)}

for (spp in sp.pred){
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
#SBATCH --array=", paste(1:32, collapse = ","), "

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
cd ../../../2_pipeline/store/brt-boot-pred/", spp, "/$SLURM_ARRAY_TASK_ID  && Rscript --vanilla brt-pred.R

	  "), fill = TRUE)  
  sink()
}
  


