pkg <- c("dfoptim", "ggplot2", "data.table")
sapply(pkg, require, character.only = TRUE)
wk <- ifelse(.Platform$OS.type =="windows",
             shortPathName("E:/PPT/ComputationalModelling/ch4/"),
             "/media/yslin/Tui/PPT/ComputationalModelling/ch4/")
setwd(wk)
rm(list = ls())
source("GCMpred.R")

## The data were processed across responses and participants
## There were 80 subjects in the classification experiment ... (see listing4.4)
N <- 2*80  ## two responses per face from 80 ppl
N_A <- round(N*.968)  ## First face observed response probability is .968
N_A

c <- 4
w <- c(.19, .12, .25, .45)

## Table A1:
## Physical specifications and multidimensional scaling (MDS) coordinates for 
## the 34 faces. 
## Eye heights, eye separation, nose length and mouth height
stim <- as.matrix(read.table("faceStim.csv", sep = ","))
exemplars <- list(a = stim[1:5, ], b = stim[6:10, ])
str(stim)
##  num [1:34, 1:4] -1.025 -0.172 -0.98 -0.951 -0.96 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : chr [1:4] "V1" "V2" "V3" "V4"

## GCMpred returns the probability of categorising the 1st face as category A
## and that of category B
preds <- GCMpred(stim[1,], exemplars, c, w)

round(preds, 2)
## [1] 0.93 0.07
likelihood <- dbinom(N_A, size = N, prob = preds[1]); likelihood
likelihood
