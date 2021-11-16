pkg <- c("dfoptim", "ggplot2", "data.table")
sapply(pkg, require, character.only = TRUE)
wk <- ifelse(.Platform$OS.type =="windows",
             shortPathName("E:/PPT/ComputationalModelling/ch4/"),
             "/media/yslin/Tui/ComputationalModelling/ch4/")
setwd(wk)
rm(list = ls())
## Maximum likelihood estimation
source("GCMprednoisy.R")


## Data ------------------------------------------------------------------
## Nosofsky, R. M., (1991). Tests of an exemplar model for relating perceptual
## classification and recognition memory. Journal of Experimental Psychology:
## Huaman Perception and Performance, 17, 3-27
## 
## discrepancy for power forgetting function
## 
## Obtain predicted probabilitie, Pa and Pb
## There were 2 responses per face from 40 participants
## Assumption 1: the matches between exemplars and the categorisation stimlus
## are noisy.
## Assumption 2: four weights for each dimension. w1 + w2 + w3 + w4 = 1
## Summed similarity means there are a similarity score for each feature of a
## face stimlus. The subject worked out a similarity score feature-after-feature.
## So in the process of matching a stimulus to an exemplar, the subject
## will computer a summed similarity score.

## The data were processed across responses and participants
## There were 80 subjects in the classification experiment and 58 subjects
## in the similarity-ratings study. 10 subjects in the similarity study
## were not included in the analyses. left 48 for analyses
N <- 2*40  ## assuming two responses per face from 40 learners

## Table A1:
## Physical specifications and multidimensional scaling (MDS) coordinates for 
## the 34 faces. 
## Eye heights, eye separation, nose length and mouth height
stim <- as.matrix(read.csv("faceStim.csv", header = FALSE))
## stim <- as.matrix(read.table("faceStim.csv", sep = ","))
colnames(stim) <- c("eh", "es", "nl", "mh")
head(stim)
str(stim)

exemplars <- list(a = stim[1:5, ], b = stim[6:10, ])
## exemplars

## Table 1. Only experiment 1A, learners only, observed
data <- scan(file = "facesDataLearners.txt")
data <- ceiling(data*N)
data
## w1 <- .25
## w2 <- .25
## w3 <- .25
bestfit <- 1e4

for(w1 in c(.25, .5, .75)) {
    for(w2 in c(.25, .5, .75)) {
        for(w3 in c(.25, .5, .75)) {
            
            fitres <- nmkb(par = c(1, w1, w2, w3, 1, .2),
                           fn = function(theta) GCMutil(theta, stim, exemplars, data, N, FALSE),
                           lower = c(0,  0, 0, 0,   0, -5),
                           upper = c(10, 1, 1, 1,  10,  5),
                           control = list(trace = 0))
            ## fitres$value is the deviance

            ## retain the smallest deviance
            if (fitres$value < bestfit) {
                bestres <- fitres
                bestfit <- fitres$value
            }
        }
    }
}

round(bestres$par, 2)
## c,   w1,  w2,  w3,   sigma, b
## 2.55 0.37 0.01 0.98  1.15   0.08

theta <- bestres$par
w <- theta[2]
w[2] <- (1 - w[1])*theta[3]
w[3] <- (1 - sum(w[1:2]))*theta[4]
w[4] <- (1 - sum(w[1:3]))
round(w, 2)
## eye height and nose length got more attention than
## eye separation and mouth height
##      eh   es   nl   mh
## c    w1   w2   w3   w4   sigma b
## 2.55 0.37 0.01 0.61 0.01 1.15  0.08
stim
## return predicted probability
preds <- GCMutil(bestres$par, stim, exemplars, data, N, TRUE)
round(preds, 2) - data

pdf(file = "GCMfits.pdf", width = 5, height = 5)
plot(preds, data/N, xlab = "Data", ylab = "Predictions")
dev.off()

bestres$par
