library(redist)
library(sf)
library(maptools)
library(coda)
library(tidyverse)
library(plyr)
library(Rcpp)
library(ggthemes)
library(RColorBrewer)

sourceCpp('cpp/count_seats.cpp', rebuild=TRUE)
sourceCpp('cpp/pBias.cpp', rebuild=TRUE)

# function to calculate bias from district map
# (code adapted from the paper replication code paper/fig5_fig6/fig5_fig6.R)
calc_sim_bias <- function(algout, dem_vote, rep_vote, range=0.1) {
    statebase <- sum(dem_vote) / (sum(dem_vote) + sum(rep_vote))
    equal <- .5 - statebase
    inc <- seq(0, range, by=.01)
    bias <- unique(c(rev(equal - inc), equal + inc))
    repseats <- matrix(NA, nrow=ncol(algout), ncol=length(bias))
    for(j in 1:length(bias)){
        repseats[,j] <- pBias(dem_vote, rep_vote, algout, bias[j])
        # print(j)
    }
    # make bias correspond to deviation from 50-50; convert to seat share
    bias <- seq(-1 * range, range, length=length(bias))
    dists <- length(unique(algout[,1]))
    repseats <- repseats / dists
    repseats <- 1 - repseats
    # plot step function
    xmin <- -1 * range
    xmax <- range
    # calculate bias - get change points
    storebias <- rep(NA, nrow(repseats))
    for(j in 1:nrow(repseats)){
        swing <- repseats[j,]
        # get the bias
        mod <- lm(swing ~ bias)
        null <- predict(mod, data.frame(bias=bias))
        # calculate the area
        gt0_area <- geiger:::.area.between.curves(bias[which(bias > 0)],
                                                  null[which(bias > 0)],
                                                  swing[which(bias > 0)],
                                                  xrange=c(-1,1))
        lt0_area <- geiger:::.area.between.curves(bias[which(bias <= 0)],
                                                  null[which(bias <= 0)],
                                                  swing[which(bias <= 0)],
                                                  xrange=c(-1,1))
        bias_area <- gt0_area + lt0_area
        storebias[j] <- bias_area
    }
    return (storebias)
}


# function to plot the simulation result with minimum bias
# (code adapted from the paper replication code paper/fig5_fig6/fig5_fig6.R)
plot_min_bias <- function(algout, storebias, map, xax, mindev=0.02875, maxdev=0.03125) {
    # ind <- which(storebias == min(storebias[xax < maxdev & xax > mindev]))[1]
    ind <- which(storebias == min(abs(storebias)))[1]
    min_map <- algout[,ind]
    map$new_districts <- as.factor(min_map)
    ggplot(map, aes(fill=new_districts)) +
        geom_sf(size=0) +
        scale_fill_brewer(palette='Set2')
}


# function to plot bias of each simulation instance (fig. 5)
# (code adapted from the paper replication code paper/fig5_fig6/fig5_fig6.R)
plot_sim_bias <- function(xax, storebias, range=0.1) {
    bias_min <- - (range - -1 * range) * 1 / 2
    bias_max <- (range - -1 * range) * 1 / 2
    # plot xax
    test <- (1 - -1) * (tapply(storebias,  round(xax, 4), mean) - bias_min) / 
        (bias_max - bias_min) + -1
    x <- as.numeric(names(test)) 
    n <- table(round(xax, 4))
    par(mfrow=c(1,1), mar=c(4.1, 4.3, 2.1, 1.0))
    # colrep <- rep('black', length(test[x<.05]))
    colrep <- rep('black', length(test))
    plot(
        # x[x<.05], test[x<.05], cex=n^(1/100), pch=16,
        x, test, cex=n^(1/100), pch=16,
        ylim=c(0, max(test)),
        main='Partisan Bias of Simulated Plans',
        xlab='% of Precincts Switched From Original District',
        ylab='Partisan Bias towards Democrats',
        xaxt='n',
        cex.lab=1.6,
        cex.axis=1.7,
        cex.main=1.6,
        col=colrep
    )
    axis(
        1,
        seq(0, max(x), by=0.05),
        as.character(seq(0, max(x), by=0.05)),
        cex.axis=1.7
    )
    abline(h=(1 - -1) * (storebias[1] - bias_min) / (bias_max - bias_min) + -1)
    abline(h=0, col='red', lty='dashed')
}