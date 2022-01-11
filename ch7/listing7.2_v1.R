pkg <- c("ggplot2", "data.table", "gridExtra")
sapply(pkg, require, character.only = TRUE)
wk <- ifelse(.Platform$OS.type == "windows",
             shortPathName("E:/PPT/ComputationalModelling/ch7/"),
             "/media/yslin/Tui/PPT/ComputationalModelling/ch7/")
setwd(wk)
rm(list = ls())

run <- function(nmc, data, fixed_sd, jump = 2, starting = 500, priormu = 326, 
                priorsd = 88) {
  # nmc <- 5e3
  chain <- rep(0, nmc)

  ## Only estimate 1 parameter and assume the standard deviation is known as 15
  chain[1] <- starting
  
  for (i in 2:nmc) {
    current <- chain[i - 1]  
    proposal <- current + rnorm(1, 0, jump)

    ## Assume a Gaussian PDF / likelihood function, given a data set (ie obs)    
    newlik <- dnorm(data, proposal, fixed_sd) * dnorm(proposal, priormu, priorsd)        
    oldlik <- dnorm(data, current, fixed_sd) * dnorm(current, priormu, priorsd)
    llratio <- newlik / oldlik
    
    if (llratio > 1) { chain[i] <- proposal }
    else {
      chain[i] <- ifelse(runif(1) < llratio, proposal, current)
    }
  }
  
  return(chain)
}

plot_chain <- function(obj, data, fixed_sd = 20, starting = 150, 
                       burnlen = NULL, priormu = 326, priorsd = 88, 
                       xlim = NULL) {
  # obj <- chain0
  # burnlen <- NULL
  # starting <- 500
  # data <- 415
  # fixed_sd <- 20
  # 
  # priormu <- 326 
  # priorsd <- 88
  # xlim <- c(200, 600)
  nmc <- length(obj)
  if (is.null(burnlen)) {
    burnlen <- ceiling(nmc/2)    
  }

  den <- density(obj)
  den2 <- density(obj[(burnlen+1):nmc])
  
  if (is.null(xlim)) {
    xlim <- range(den$x)
  }
  
  x <- seq(xlim[1], xlim[2], 1)
  y <- dnorm(x, data, fixed_sd)
  ymax <- max(den$y, den2$y, y)

  yprior <- dnorm(x, priormu, priorsd)
  
  dden <- data.table(x = c(den$x, x, den2$x, x),
                  y = c(den$y, y, den2$y, yprior),
                  gp = c(rep("MCMC", length(den$x)),
                         rep("Normal PDF", length(x)),
                         rep("Excluding burnin", length(den2$x)),
                         rep("Prior", length(x))))

  x1 <- 1:burnlen
  x2 <- (burnlen+1):nmc
  y1 <- obj[1:burnlen]
  y2 <- obj[(burnlen+1):nmc]
  dtrace <- data.table(x = c(x1, x2),
                       y = c(y1, y2),
                       gp = c(rep("Burnin", length(x1)),
                              rep("Sampled", length(x2))))
  dp <- data.table(x = 0, y = starting)

  p0 <- ggplot() +
    geom_line(data = dden, aes(x = x, y = y, colour = gp), size = 2) +
    coord_cartesian(xlim = xlim) +
    theme(legend.position = "top",
          legend.title = element_blank())
  
  p1 <- ggplot() +
    geom_line(data = dtrace, aes(x = x, y = y, colour = gp)) +
    geom_point(data = dp, aes(x = x, y = y), size = 2) +
    theme(legend.position = "top",
          legend.title = element_blank())

  gridExtra::grid.arrange(p0, p1)
  
  cat("Mean and SD:", round(mean(obj), 2), ",", round(sd(obj), 2), "\n")
  cat("Remove burnin:", round(mean(obj[(burnlen+1):nmc]), 2), ",", 
      round(sd(obj[(burnlen+1):nmc]), 2), "\n")
  return(NULL)
}


obs <- 415   ## data: 1 data point
obssd <- 20
priormu <- 326
priorsd <- 88

chain0 <- run(5000, data = obs, fixed_sd = obssd, jump = 50, starting = 500,
              priormu = priormu, priorsd = priorsd)
res <- plot_chain(chain0, data = obs, starting = 500, xlim = c(200, 500))

chain1 <- run(5000, data = obs, fixed_sd = obssd, jump = 50, starting = 500,
              priormu = priormu, priorsd = 20)
res <- plot_chain(chain1, data = obs, starting = 500, xlim = c(200, 500),
                  priorsd = 20)

