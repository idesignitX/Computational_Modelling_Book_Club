GCMprednoisy <- function(probe, exemplars, c, w, sigma, b) {
    ## calculate likelihood of N_A, `A` responses out of N given parameter c
    ## 'probe' is a vector representing the stimulus to be categorised
    ## 'exemplars' is a two-element list, representing two exemplars: 
    ##             - the first list is items in the 'A' exemplar in the memory, 
    ##             - the second list is items in the 'B' exemplar in the memory
    ##             - Each list is a matrix in which the rows correspond to 
    ##               individual exemplar
    ## 'c' is the scaling parameter, and 'w' is a vector giving weighting for
    ## each stimulus dimension (the colunms in 'stim') and 'exemplars'
    ##
    ## p <- gcm0(stim[1, ], exemplars, c, w, sigma, b)
    ## Example
    ## probe <- c(-1.025, 0.493, 0.048, -0.666)
    ## stim <- as.matrix(read.csv("faceStim.csv", header = FALSE))
    ## colnames(stim) <- c("eh", "es", "nl", "mh")
    ## exemplars <- list(a = stim[1:5, ], b = stim[6:10, ])
    ## 
    ## theta <- runif(6)
    ## c <- theta[1]
    ## w <- theta[2]  ## weight 1
    ## w[2] <- (1 - w[1])*theta[3]
    ## w[3] <- (1 - sum(w[1:2]))*theta[4]
    ## w[4] <- (1 - sum(w[1:3]))
    ## sigma <- theta[5]
    ## b <- theta[6]
    ## cres <- GCMprednoisy(probe, exemplars, c, w, sigma, b)
    ## res

    dist <- list()
    for(ex in exemplars) {
        dist[[length(dist) + 1]] <- apply(as.array(ex), 1, function(x) {
            sqrt(sum(w*(x - probe)^2))
        })
    }

    sumsim <- unlist(lapply(dist, function(a) {  sum(exp(-c*a))  }))
    ## This only works for 2 categories
    ## we also simplify Nosofsky model in only applying noise at the end

    r_prob <- c(0, 0)
    r_prob[1] <- pnorm(sumsim[1] - sumsim[2] - b, sd = sigma)
    r_prob[2] <- 1 - r_prob[1]
    return(r_prob)
}

GCMutil <- function(theta, stim, exemplars, data, N, retpreds) {
    nDat <- length(data)
    dev <- pred <- rep(NA, nDat)

    c <- theta[1]
    w <- theta[2]  ## weight 1
    w[2] <- (1 - w[1])*theta[3]
    w[3] <- (1 - sum(w[1:2]))*theta[4]
    w[4] <- (1 - sum(w[1:3]))
    sigma <- theta[5]
    b <- theta[6]

    ## For each i test stimulus, the probability of an A response
    ## is calculated
    for(i in seq_len(nDat)) {
        p <- GCMprednoisy(stim[i, ], exemplars, c, w, sigma, b)
        ## 1 = category A; 2 = category B
        pred[i] <- p[1]
        dev[i] <- -2*log(dbinom(data[i], size = N, prob = p[1]))
    }

    if (retpreds) {
        return(pred)
    } else {
        ## return joint probability across all 34 faces
        return(sum(dev))
    }
}
