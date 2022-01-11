pkg <- c("tibble", "ggplot2", "data.table", "gridExtra")
sapply(pkg, require, character.only = TRUE)
wk <- ifelse(.Platform$OS.type =="windows",
             shortPathName("F:/PPT/ComputationalModelling/ch7/"),
             "/media/yslin/Tui/PPT/ComputationalModelling/ch7/")
setwd(wk)
rm(list = ls())
## "Suppose we administer an intelligence test to a chiuld who is one of a 
## group of children considered to be particularly gifted. The child scores 
## y = 144 on a test that is known to be normally distributed with sigma = 15
## for the population at large
##
## What is the likely value of mu for the population of gifted children, 
## assuming a uniform prior distribution for mu?"
##
##
run <- function(nmc = 5000, data = 144, propsd = 2, starting = 150, jump = 2) {
  chain <- rep(0, nmc)
  fixed_sd <- 15
  chain[1] <- starting
  
  ## starting from the 2nd
  for (i in 2:nmc) {
    current <- chain[i - 1]  
    proposal <- current + rnorm(1, 0, jump)
    
    ## Assume a Gaussian PDF / likelihood function, given a data set (ie obs)
    if (dnorm(data, proposal, fixed_sd) > dnorm(data, current, fixed_sd)) {
      chain[i] <- proposal
    } else {
      chain[i] <- ifelse(runif(1) < dnorm(data, proposal, fixed_sd)/dnorm(data,current,fixed_sd),
                         proposal, current)
      
    }
  }

  return(chain)
}

plot_chain <- function(obj, data = 144, starting = 150, burnlen = NULL) {
  
  nmc <- length(obj)
  if (is.null(burnlen)) {
    burnlen <- ceiling(nmc/2)    
  }

  
  x <- seq(100, 200, 1)
  y <- dnorm(x, data, 15)
  den <- density(obj)
  den2 <- density(obj[(burnlen + 1):nmc])
  ymax <- max(den$y, y, den2$y)
  
  par(mfrow = c(2, 1))
  plot(x = den$x, y = den$y, xlim = c(100, 200), ylim = c(0, ymax), type = "l",
       lty = "dashed", lwd = 2, main = "", xlab = "", col = "grey49")
  lines(x = x, y = y, col = "grey69", lwd = 3)      
  lines(x = den2$x, y = den2$y, lwd = 3)      
  
  
  plot(x = 1:burnlen, y = obj[1:burnlen], type = "l", col = "grey69", 
       xlim = c(1, nmc), ylim = range(obj), lwd = 2, xlab = "", ylab = "")
  lines(x = (burnlen + 1):nmc, y = obj[(burnlen + 1):nmc], lwd = 2)
  points(x = 1, y = starting, pch = 1, cex = 2)
  par(mfrow = c(1, 1))
  
  cat("Mean and SD:", round(mean(obj), 2), ",", round(sd(obj), 2), "\n")
  
  cat("Remove burnin:", round(mean(obj[(burnlen + 1):nmc]), 2), ",", 
      round(sd(obj[(burnlen + 1):nmc]), 2), "\n")
      return(NULL)
}

obs <- 144
chain0 <- run(5000, data = obs)
chain1 <- run(500, data = obs, starting = 10)
chain2 <- run(5000, data = obs, starting = 500, jump = 20)

res <- plot_chain(chain0, data = obs)
res <- plot_chain(chain1, data = obs, starting = 10)
res <- plot_chain(chain2, data = obs, starting = 500)
