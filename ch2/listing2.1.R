## Version 1 ---------------------
## Use many R functions and conventional plot functions
library(ggplot2); library(data.table)

nreps <- 1e4
nsamples <- 2e3

drift <- 0  # 0 drift rate the up and down step are equally likely
sdrw  <- .3
criterion <- 3

responses <- latencies <- rep(0, nreps)
evidence <- matrix(0, nreps, nsamples+1)

for (i in seq_len(nreps)) {
  evidence[i, ] <- cumsum(c(0, rnorm(nsamples, drift, sdrw)))
  p <- which(abs(evidence[i, ]) > criterion)[1]
  responses[i] <- sign(evidence[i, p])
  latencies[i] <- p
}

## Listing 2.2 ---------------------------
# plot up to 5 random-walk paths
tbpn <- min(nreps, 5)
plot(1:max(latencies[1:tbpn]) + 10, type = "n", las = 1, 
     ylim = c(-criterion-.5, criterion+.5),
     xlab = "Time", ylab = "Evidence")
for(i in seq_len(tbpn)) {
  lines(evidence[i, 1:(latencies[i]-1)])
}
abline(h = c(criterion, -criterion), lty = "dashed")

## Listing 2.3 --------------------------
top_rt <- latencies[responses > 0]
bot_rt <- latencies[responses < 0]
top_prop <- length(top_rt) / nreps
bot_prop <- length(bot_rt) / nreps

top_title <- paste("Top responses (", as.numeric(top_prop),
             ") m=", as.character(signif(mean(top_rt), 4)), sep="")
bot_title <- paste("Bottom responses (", as.numeric(bot_prop),
             ") m=", as.character(signif(mean(bot_rt), 4)), sep="")

par(mfrow = c(2,1))
hist(top_rt, col = "gray", xlab = "Decision time", xlim = c(0, max(latencies)),
     main = top_title, las = 1)
hist(bot_rt, col = "gray", xlab = "Decision time", xlim = c(0, max(latencies)),
     main = bot_title, las = 1)
par(mfrow = c(1,1))


## Version 2 ---------------------
## Write Euler scheme explicitly and use ggplot2 plot functions
farrell_rw <- function(v, sv, a, ntrial, nt) {
  ## v = mean drift rate
  ## sv = standard deviatoin of the drift rate distribution
  ## a = symmetric boundary 
  
  evidence <- matrix(NA, ntrial, nt + 1)   
  evidence[, 1] <- 0   ## assuming no bias, starting from 0 evidence
  response <- latency <- rep(NA, ntrial)
  
  for (j in seq_len(ntrial)) {
    acc_evid <- evidence[j, 1]
    for (i in 2:(nt+1)) {
      evidence[j, i] <- rnorm(1, v, sv)  ## assuming Gaussian noise
      acc_evid <- acc_evid + evidence[j, i]
      if( abs(acc_evid) >= a ) {
        latency[j] <- i
        response[j] <- sign(acc_evid)
        break
      }
    }
  }
  return(list(DT = latency, DV = evidence, R = response))
}
res0 <- farrell_rw(v = 0, sv = .3, a = 3, ntrial = 1e4, nt = 2e3)
res1 <- farrell_rw(v = .2, sv = .3, a = 3, ntrial = 1e4, nt = 2e3)
res2 <- farrell_rw(v = .03, sv = .3, a = 3, ntrial = 1e4, nt = 2e3)

x <- y <- trial <- NULL
x1 <- y1 <- trial1 <- NULL
for(i in 1:5) {
  x <- c(x, 1:res0$DT[i])
  y <- c(y, cumsum( res0$DV[i, 1:res0$DT[i]] ))
  trial <- c(trial, rep(i, res0$DT[i]))
  
  x1 <- c(x1, 1:res1$DT[i])
  y1 <- c(y1, cumsum( res1$DV[i, 1:res1$DT[i]] ))
  trial1 <- c(trial1, rep(i, res1$DT[i]))
  
}

d0 <- data.table(x = x, y = y, trial = factor(trial))
d1 <- data.table(x = x1, y = y1, trial = factor(trial1))
d0$dr <- 0
d1$dr <- .2
d <- rbind(d0, d1)
dh <- data.frame(y = c(3, -3))

p0 <- ggplot() +
  geom_line(data = d, aes(x = x, y = y, colour= trial)) +
  geom_hline(data = dh, aes(yintercept = y), linetype = "dashed") +
  facet_grid(dr~., switch = "y") +
  xlab("Time") + ylab("Evidence") +
  theme_bw(base_size = 14)

p0


d <- data.table(RT = res0$DT, R = res0$R) 
d$gp <- ifelse(d$R > 0, "Top responses", "Bottom responses")
nreps <- 1e4
# d[, .N, .(gp)]$N/nreps

top_rt <- res0$DT[res0$R > 0]
bot_rt <- res0$DT[res0$R < 0]
top_prop <- length(top_rt) / nreps
bot_prop <- length(bot_rt) / nreps


top_lab <- paste0("Proportion = ", signif(top_prop, 3), "; Mean = ", signif(mean(top_rt), 4))
bot_lab <- paste0("Proportion = ", signif(bot_prop, 3), "; Mean = ", signif(mean(bot_rt), 4))
danno <- data.frame(
  x = c(600, 600), 
  y = c(600, 600),
  ff = c("plain","plain"),
  gp = c("Top responses", "Bottom responses"),
  lab = c(top_lab, bot_lab))

p1 <- ggplot() +
  geom_histogram(data = d, aes(RT)) +
  facet_grid(gp~., switch = "y") +
  geom_text(data = danno, aes(x = x, y = y, label = lab, fontface =ff),
            size = 7) + 
  xlab("Decision time") + ylab("Frequency") +
  theme_bw()
p1


## Figure 2.3
## Increase the drift rate from 0 to .03, resulting in the increase in the 
## positive (ie top) responses. Not difference in the mean RT.
d <- data.table(RT = res2$DT, R = res2$R) 
d$gp <- ifelse(d$R > 0, "Top responses", "Bottom responses")
nreps <- 1e4
# d[, .N, .(gp)]$N/nreps

top_rt <- res2$DT[res2$R > 0]
bot_rt <- res2$DT[res2$R < 0]
top_prop <- length(top_rt) / nreps
bot_prop <- length(bot_rt) / nreps


top_lab <- paste0("Proportion = ", signif(top_prop, 3), "; Mean = ", signif(mean(top_rt), 4))
bot_lab <- paste0("Proportion = ", signif(bot_prop, 3), "; Mean = ", signif(mean(bot_rt), 4))
danno <- data.frame(
  x = c(400, 400), 
  y = c(1000, 1200),
  ff = c("plain","plain"),
  gp = c("Top responses", "Bottom responses"),
  lab = c(top_lab, bot_lab))

p2 <- ggplot() +
  geom_histogram(data = d[gp == "Top responses"], aes(RT, colour = gp), fill = "white") +
  geom_histogram(data = d[gp == "Bottom responses"], aes(RT, colour = gp), fill = "white") +
  geom_text(data = danno, aes(x = x, y = y, label = lab, fontface =ff, colour = gp),
            size = 7) + 
  xlab("Decision time") + ylab("Frequency") +
  theme_bw() +
  theme(legend.position = c(.8,.8),
        legend.title = element_blank())
p2

## Listing 2.4---------------
farrell_rw2 <- function(v, sv, a, sz, s, ntrial, nt) {
  ## v = mean drift rate
  ## s = within-trial drift rate standard deviation (sqrt(diffusion coefficient))
  ## sv = standard deviation of the between-trial drift rate distribution
  ## a = symmetric boundary 
  ## sz = the between-trial standard deviation of the starting point (assuming Gaussian dist.) 
  
  evidence <- matrix(NA, ntrial, nt + 1)   
  
  # evidence[, 1] <- 0   ## assuming no bias, starting from 0 evidence
  response <- latency <- rep(NA, ntrial)
  
  for (j in seq_len(ntrial)) {
    
    start_evidence <- rnorm(1, 0, sz)  ## assuming mean 0
    evidence[j, 1] <- start_evidence
    acc_evid <- evidence[j, 1]
    drift_rate <- rnorm(1, v, sv)  
    
    for (i in 2:(nt+1)) {
      evidence[j, i] <- rnorm(1, drift_rate, s)  ## assuming Gaussian noise
      acc_evid <- acc_evid + evidence[j, i]
      if( abs(acc_evid) >= a ) {
        latency[j] <- i
        response[j] <- sign(acc_evid)
        break
      }
    }
  }
  return(list(DT = latency, DV = evidence, R = response))
}

res3 <- farrell_rw2(v = .035, sv = 0, a = 3, sz = 0.8, s = .3, ntrial = 1e3, nt = 2e3)
res4 <- farrell_rw2(v = .035, sv = 0.025, a = 3, sz = 0, s = .3, ntrial = 1e3, nt = 2e3)

d <- data.table(RT = res3$DT, R = res3$R) 
d$gp <- ifelse(d$R > 0, "Top responses", "Bottom responses")
nreps <- 1e3
d[, .(N = .N, 
     M = mean(RT)), .(gp)]


top_rt <- res3$DT[res3$R > 0]
bot_rt <- res3$DT[res3$R < 0]
top_prop <- length(top_rt) / nreps
bot_prop <- length(bot_rt) / nreps


top_lab <- paste0("Proportion = ", signif(top_prop, 3), "; Mean = ", signif(mean(top_rt), 4))
bot_lab <- paste0("Proportion = ", signif(bot_prop, 3), "; Mean = ", signif(mean(bot_rt), 4))
danno <- data.frame(
  x = c(200, 200), 
  y = c(150, 100),
  ff = c("plain","plain"),
  gp = c("Top responses", "Bottom responses"),
  lab = c(top_lab, bot_lab))

## Figure 2.4
p3 <- ggplot() +
  geom_histogram(data = d[gp == "Top responses"], aes(RT, colour = gp), fill = "white") +
  geom_histogram(data = d[gp == "Bottom responses"], aes(RT, colour = gp), fill = "white") +
  geom_text(data = danno, aes(x = x, y = y, label = lab, fontface =ff, colour = gp),
             size = 7) + 
  # facet_grid(gp~.)+
  xlab("Decision time") + ylab("Frequency") +
  theme_bw() +
  theme(legend.position = c(.8,.8),
        legend.title = element_blank())
p3


## Figure 2.5
d <- data.table(RT = res4$DT, R = res4$R) 
d$gp <- ifelse(d$R > 0, "Top responses", "Bottom responses")
nreps <- 1e3
d[, .(N = .N, 
      M = mean(RT)), .(gp)]


top_rt <- res4$DT[res4$R > 0]
bot_rt <- res4$DT[res4$R < 0]
top_prop <- length(top_rt) / nreps
bot_prop <- length(bot_rt) / nreps


top_lab <- paste0("Proportion = ", signif(top_prop, 3), "; Mean = ", signif(mean(top_rt), 4))
bot_lab <- paste0("Proportion = ", signif(bot_prop, 3), "; Mean = ", signif(mean(bot_rt), 4))
danno <- data.frame(
  x = c(200, 200), 
  y = c(150, 100),
  ff = c("plain","plain"),
  gp = c("Top responses", "Bottom responses"),
  lab = c(top_lab, bot_lab))

## Figure 2.4
p4 <- ggplot() +
  geom_histogram(data = d[gp == "Top responses"], aes(RT, colour = gp), fill = "white") +
  geom_histogram(data = d[gp == "Bottom responses"], aes(RT, colour = gp), fill = "white") +
  geom_text(data = danno, aes(x = x, y = y, label = lab, fontface =ff, colour = gp),
            size = 7) + 
  # facet_grid(gp~.)+
  xlab("Decision time") + ylab("Frequency") +
  theme_bw() +
  theme(legend.position = c(.8,.8),
        legend.title = element_blank())
p4
