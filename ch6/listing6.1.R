curve(dbeta(x, 2, 4), ylim = c(0, 6), ylab = "Probability Density", las = 1)
curve(dbeta(x, 8, 16), add = TRUE, lty ="dashed")
curve(dbeta(x, 6, 12), add = TRUE, lty = "dotted")
legend("topright", c("Johnnie", "Jane", "third"), inset= .05, 
       lty = c("solid", "dashed", "dotted"))

max( dbeta(seq(0, 1, length.out = 1e2), 2, 4)  )

qbeta(.50, 2, 4)
pbeta(.314, 2, 4)


## 6.2.5 Estimating the Bias of a Coin
h <- c(14, 113, 1130)
n <- c(26, 213, 2130)
round(h/n, 3)
# 0.538 0.531 0.531

curve(dbeta(x, 12, 12), ylim = c(0, 40), 
      ylab = "Probability Density", las = 1, lwd = 2)
means <- rep(NA, 3)
term1 <- term2 <- NA

cb <- c("#1b9e77", "#d95f02", "#7570b3")
for(i in 1:length(h)) {
  term1 <- 12 + h[i]
  term2 <- 12 + n[i] - h[i]
  print(c(term1, term2))
  means[i] <- term1 / (term1+term2)
  curve(dbeta(x, term1, term2), add = TRUE, 
        col = cb[i], lwd = 2)
}



legend("topright", c("{14, 26}", "{113, 213}", "{1130, 2130"), inset= .05, 
       lty = "solid", col = Manu::get_pal("Tui")[1:3])
abline(v = .5, lty = "dotted", col = "grey56")
round( means, 3)
# 0.520 0.527 0.530

interval <- qbeta(c(.025, .975), term1, term2)
round(interval, 3)

## Three different priors ---------------------------------------------------
5.5/11
10.5/11

curve(dbeta(x, 0, 0), ylab = "Probability Density", las = 1)
curve(dbeta(x, 1, 1), ylab = "Probability Density", las = 1)
curve(dbeta(x, .5, .5), ylab = "Probability Density", las = 1)
