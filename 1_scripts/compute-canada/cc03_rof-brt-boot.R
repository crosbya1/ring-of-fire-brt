

library(mefa4)
library(gbm)
library(dismo)
library(ggplot2)
library(segmented)
library(parallel)

load("../../0_data/processed/BAMv6_RoFpackage_2022-04.RData")


# Function to run bootstrapped BRT models
run_brt1 <- function(i, spp) {
  RATE=0.001
  u <- rel_inf(res)
  vars <- c("count", "offset", as.character(u$var[u$rel.inf > 0]))
  ntree <- res$n.trees
  # ntree <- 10000
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


rel_inf <- function(res) {
  rel.inf <- relative.influence(res, res$n.trees)
  rel.inf[rel.inf < 0] <- 0
  i <- order(-rel.inf)
  rel.inf <- 100 * rel.inf/sum(rel.inf)
  out <- data.frame(var = res$var.names[i], rel.inf = rel.inf[i])
  attr(out, "n.trees") <- res$n.trees
  out
}


dir <- "../../2_pipeline/store/brt-boot-1"
if(!dir.exists(dir)){dir.create(dir)}

cl <- makeCluster(5, timeout = 548000)
clusterExport(cl, list("run_brt1", "BB", "SPP", "y", "off", "xx1", "xx2"))

nfold <- 32
system.time({
for(i in 1:length(SPP)){
    spp <- SPP[i]
    cat("\n\n------------------------------", spp, "------------------------------\n\n")
    load(paste0("../../2_pipeline/store/brt2-xv/", spp, ".RData"))
    if(inherits(res, "gbm")){    
      d1 <- paste0("../../2_pipeline/store/brt-boot-1/", spp)
      if(!dir.exists(d1)){dir.create(d1)}
      cl <- makeCluster(nfold, timeout = 548000)
      clusterExport(cl, list("run_brt1", "rel_inf", "BB", "spp", "y", "off", "xx1", "xx2", "res", "cn2"))
      parLapply(cl, 1:nfold, function(x){
        library(mefa4)
        library(gbm)
        library(dismo)
        library(ggplot2)
        library(segmented)

        r2 <- run_brt1(x, spp)
        save(r2, file=paste0("../../2_pipeline/store/brt-boot-1/", spp, "/", x, ".RData"))
      })
    stopCluster(cl)
    rm(cl)
    }
  }
})













