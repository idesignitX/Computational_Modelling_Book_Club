rm(list = ls())
library(data.table); library(ggplot2)

## discrepancy for power forgetting function
fx <- function(x, parameter) {
    a <- parameter[1]
    b <- parameter[2]
    c <- parameter[3]
    a * (b * x + 1)^(-c)
}

powdiscrep <- function(parms, rec, ri) {
    if (any(parms < 0) || any(parms > 1)) return(1e6)
    
    pow_pred <- parms["a"] * (parms["b"]*ri + 1)^(-parms["c"])
    msd <- sum((pow_pred - rec)^2) / length(ri)
    return(sqrt(msd))
}

## Carpenter et al's data (2008) Experiment 1
## Carpenter, S. K., Pashler, H., Wixter, J. T., and Vul, E. (2008). The effects
## of tests on learning and forgetting. Memory & Cognition, 36, 438-448.
rec <- c(.93, .88, .86, .66, .47, .34)
ri <- c(.005, 1, 2, 7, 14, 42)



## Find the best fitting parameter ---------------------------------------------
## initialize starting values
sparms <- c(1, .05, .7)
names(sparms) <- c("a", "b", "c")

## obtain best-fitting estimates
pout <- optim(sparms, powdiscrep, rec = rec, ri = ri)
pout$par
##         a         b         c 
## 0.9481106 0.1322038 0.5815314


x <- seq(0, 45, length.out = 100)
y <- fx(x, pout$par)
y_pred <- pout$par["a"] * (pout$par["b"]*ri + 1)^(-pout$par["c"])
## Figure 3.1
plot(ri, rec, cex = 2.5, pch = 21, bg = "dark grey",
     xlab = "Retention Interval (Days)", ylab = "Proportion Items Retained",
     xaxt = "n", xlim = c(0, 43), ylim = c(.3, 1))
lines(x, y, lwd = 2)
segments(x0 = ri, y0 = rec, x1 = ri, y1 = y_pred, lwd = 1)
axis(1, at = c(0:43))


pow_pred <- pout$par["a"] * (pout$par["b"]*c(0:max(ri)) + 1)^(-pout$par["c"])

## plot data and best-fitting predictions
x11()
par(cex.axis = 1.2, cex.lab = 1.4)
par(mar =(c(5, 5, 3, 2) + .1), las = 1)
plot(ri, rec,
     xlab = "Retention Interval (Days)",
     ylab = "Proportion Items Retained",
     ylim = c(.3, 1), xlim = c(0, 43), xaxt = "n", type = "n")
lines(c(0:max(ri)), pow_pred, lwd = 2)
points(ri, rec, pch = 21, bg = "dark grey", cex = 2)


dev <- pow_pred[ri+1]; dev
for(x in c(1:length(ri))) {
    lines(c(ri[x], ri[x]), c(dev[x], rec[x]), lwd = 1)
}
axis(1, at = c(0:43))

## listing 3.4 -------------------------------------------
## perform bootstrapping analysis
## Carpenter et al's Experiment 1 recruited 55 participants.
ns <- 55
## number of bootstrapping resampling 
nbs <- 1e3

## Each row stores one bootstrapping outcome
bsparms <- matrix(NA, nbs, length(sparms))
bspow_pred <- pout$par["a"] * (pout$par["b"]*ri + 1)^(-pout$par["c"])

i <- 1
for(i in seq_len(nbs)) {
    ## At each retention interval, vapply conducted one Monte Carlo simulation
    ## (ie resampling). The randomness is governed by rbinom
    recsynth <- vapply(bspow_pred, FUN = function(x) mean(rbinom(ns, 1, x)), numeric(1))

    ## tmp <- vapply(bspow_pred, FUN = function(x) print(x), numeric(1))

    ## bspow_pred
    ## rbinom(ns, 1, bspow_pred[1])
    ## rbinom(ns, 1, bspow_pred[2])
    ## recsynth
    ## rbinom(n, size, prob)
    ## optim used the simulated data to conduct one parameter estimation
    
    bsparms[i, ] <- unlist(optim(pout$par, powdiscrep, rec = recsynth, ri = ri)$par)
}


## recsynth:    resampled prediction
## bspow_pred:  point estimated prediction
## rec:         empirical data


## function to plot a histogram
histoplot <- function(x, l4x) {
    hist(x, xlab = l4x, main = "", xlim = c(0, 1), cex.lab = 1.5, cex.axis = 1.5)
    lq <- quantile(x, .025)
    uq <- quantile(x, .975)
    abline(v = lq, lty = "dashed", lwd = 2)
    abline(v = uq, lty = "dashed", lwd = 2)
    return(c(lq, uq))
}

x11(5,2)
par(mfcol = c(1,3), las = 1)
for(i in 1:dim(bsparms)[2]) {
    print(histoplot(bsparms[,i], names(sparms)[i]))
}
dim(bsparms)


d <- data.table(x = c(bsparms[,1], bsparms[,2], bsparms[,3]), 
                y = rep(c('a', 'b', 'c'), each = nbs))
qtl <- apply(bsparms, 2, quantile, c(.025, .975))

dv0 <- data.table(x = c(qtl[,1], qtl[,2], qtl[,3]),
                        y = c('a', 'a', 'b', 'b', 'c', 'c'))

dv1 <- data.table(x = c(pout$par[1],
                        pout$par[2],
                        pout$par[3]),
                 y = c('a', 'b', 'c'))
dv0

p0 <- ggplot() +
    geom_histogram(data = d, aes(x = x)) +
    geom_vline(data = dv0, aes(xintercept = x), linetype = 'dotted') +
    geom_vline(data = dv1, aes(xintercept = x), linetype = 'dashed', colour = "blue") +
    facet_grid(.~y, scales = 'free_x') +
    theme_minimal(base_size = 18) 
p0
