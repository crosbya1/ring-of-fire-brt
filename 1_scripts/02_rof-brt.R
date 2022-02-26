# R code for fitting BRT models and calculating variable importance for birds in Far North Ontario 

library(mefa4)
library(gbm)
library(dismo)


load("0_data/processed/BAMv6_RoFpackage_2022-01.RData")


## run BRT with xv

run_brt_xv <- function(spp, RATE=0.001) {
  i <- 1
  si <- BB[,i]
  if (sum(y[si, spp]) < 1)
    return(structure(sprintf("0 detections for %s", spp), class="try-error"))
  xi <- data.frame(
    count=as.numeric(y[si, spp]),
    offset=off[si, spp],
    ecozone=ifelse(xx1$ecozone=="hudson_plain", 1, 0)[si],
    xx2[si, cn2])
  out <- try(gbm.step(xi,
                      gbm.y = 1,
                      gbm.x = 3:ncol(xi),
                      offset = xi$offset,
                      family = "poisson",
                      tree.complexity = 3,
                      learning.rate = RATE,
                      bag.fraction = 0.5))
  if (!inherits(out, "try-error"))
    out$rof_settings <- list(RATE=RATE, spp=spp, i=i)
  out
}


if(!dir.exists("2_pipeline/store/brt2-xv"))
  (dir.create("2_pipeline/store/brt2-xv"))
system.time({
  for (spp in SPP) {
    cat("\n\n------------------------------", spp, "------------------------------\n\n")
    res <- run_brt_xv(spp)
    save(res, file=paste0("2_pipeline/store/brt2-xv/", spp, ".RData"))
  }
})


## check variable importance

inherits(res, "try-error")

library(mefa4)
library(gbm)
library(dismo)
library(ggplot2)

rel_inf <- function(res) {
  rel.inf <- relative.influence(res, res$n.trees)
  rel.inf[rel.inf < 0] <- 0
  i <- order(-rel.inf)
  rel.inf <- 100 * rel.inf/sum(rel.inf)
  out <- data.frame(var = res$var.names[i], rel.inf = rel.inf[i])
  attr(out, "n.trees") <- res$n.trees
  out
}

.plot_fun <- function(i, res, u) {
  j <- as.character(u$var[i])
  x <- plot.gbm(res, j,
                n.trees = res$n.trees,
                return.grid=TRUE,
                type="response",
                ylab=paste(res$rof_settings$spp, "density (males/ha)"),
                xlab=paste0(j, " (", round(u$rel.inf[i], 2), "%)"))
  colnames(x) <- c("x", "y")
  x$var <- paste0(j, " (", round(u$rel.inf[i], 2), "%)")
  attr(x, "out.attrs") <- NULL
  x
}

plot_fun <- function(res) {
  u <- rel_inf(res)
  xx <- do.call(rbind, lapply(1:12, .plot_fun, res, u))
  p <- ggplot(xx, aes(x=x, y=y)) +
    geom_line() +
    facet_wrap(vars(var), scales="free_x") +
    ylab(paste(res$rof_settings$spp, "density (males/ha)")) +
    xlab("Predictor values") +
    theme_minimal()
}

for (spp in SPP) {
  cat(spp, "\n")
  load(paste0("2_pipeline/store/brt2-xv/", spp, ".RData"))
  if (inherits(res, "gbm")) {
    p <- plot_fun(res)
    ggsave(sprintf("2_pipeline/store/brt2-xv-pred-mosaic/%s-effects12.png", spp), p)
  }
}


# See best variables
RIall <-NULL
for (spp in SPP) {
  cat(spp, "\n")
  load(paste0("2_pipeline/store/brt2-xv/", spp, ".RData"))
  if (inherits(res, "gbm")) {
    u <- rel_inf(res)
    u$spp <- spp
    RIall <- rbind(RIall, u)
    p <- plot_fun(res)
    ggsave(sprintf("2_pipeline/store/brt2-xv-pred-mosaic/%s-effects12.png", spp), p)
  }
}
write.csv(RIall, row.names=FALSE, file="3_outputs/tables/SppBRTVarImp_v2.csv")

RIall$n0 <- ifelse(RIall$rel.inf > 0, 1, 0)
z <- xtabs(~var+n0,RIall)
z <- z[,"1"]/rowSums(z)
sort(z)
z["eskerpoint"]
which(names(sort(z)) == "eskerpoint")



