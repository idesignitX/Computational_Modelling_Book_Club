GCMpred <- function(probe, exemplars, c, w) {

    Euclidean <- function(x ,y, w) {
        if (length(x) != length(y)) stop("x and y must have the same number of elements")
        if (length(x) != length(w)) stop("So does w")
        ## take power first
        sqrt(sum(w*(x - y)^2))
    }


    # probe <- stim[1,]
    # exemplars, c, w
    
    dist <- list()
    nexemplar <- length(exemplars); nexemplar
    
    for(i in 1:nexemplar) {
        ex <- exemplars[[i]]; 
        tmp <- NULL
        for(j in 1:nrow(ex)) {
            tmp <- c(tmp, Euclidean(ex[j, ], probe, w))
        }
        dist[[length(dist) + 1]] <- tmp
    }


    ## Similarity scores summing across 5 exemplars
    sumsim <- unlist(lapply(dist, function(a) {  sum(exp(-c*a))  })); 
    return( unlist(sumsim) / sum(unlist(sumsim)) )
}

