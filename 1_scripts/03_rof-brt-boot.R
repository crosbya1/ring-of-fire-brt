# R code for fitting bootstrapped BRT SDM models for birds in Far North Ontario



library(mefa4)
library(gbm)
library(dismo)
library(ggplot2)
library(segmented)

load("0_data/processed/BAMv6_RoFpackage_2022-01.Rdata")


# Function to run bootstrapped BRT models
run_brt1 <- function(i, spp) {
  RATE=0.001
  #  u <- rel_inf(res)
  #  vars <- c("count", "offset", as.character(u$var[u$rel.inf > 0]))
  #  ntree <- res$n.trees
  ntree <- 10000
  si <- BB[,i]
  if (sum(y[si, spp]) < 1)
    return(structure(sprintf("0 detections for %s", spp), class="try-error"))
  xi <- data.frame(
    count=as.numeric(y[si, spp]),
    offset=off[si, spp],
    ecozone=ifelse(xx1$ecozone=="hudson_plain", 1, 0)[si],
    xx2[si, cn2])# [,vars]
  out <- try(gbm::gbm(xi$count ~ . + offset(xi$offset),
                      data=xi[,-(1:2)],
                      n.trees = ntree,
                      interaction.depth = 3,
                      shrinkage = RATE,
                      bag.fraction = 0.5,
                      distribution = "poisson",
                      var.monotone = NULL,
                      keep.data = FALSE,
                      verbose = FALSE,
                      n.cores = 1))
  if (!inherits(out, "try-error"))
    out$rof_settings <- list(RATE=RATE, spp=spp, i=i)
  out
}


# Test on 2 spp with 2 bootstrapped runs, using the 
dir <- "2_pipeline/store/brt-boot-1"
if(!dir.exists(dir)){dir.create(dir)}
system.time({
  for (spp in SPP[1:2]) {
    cat("\n\n------------------------------", spp, "------------------------------\n\n")
    d1 <- paste0("2_pipeline/store/brt-boot-1/", spp)
    if(!dir.exists(d1)){dir.create(d1)}
    for(i in 1:2){
      res <- run_brt1(i, spp)
      save(res, file=paste0("2_pipeline/store/brt-boot-1/", spp, "/", i, ".RData"))
    }
  }
})






















