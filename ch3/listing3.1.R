## Listing 3.1
rm(list = ls())
getregpred <- function(parms, data) {
    pred <- parms["b0"] + parms["b1"] * data[,2]
    par(ask = TRUE)
    plot(data[,2], type = "n", las = 1, ylim = c(-2, 2), xlim = c(-2, 2),
         xlab = "X", ylab = "Y")
    par(ask = TRUE)
    points(data[,2], data[,1], pch = 21, bg = "gray")
    lines(data[,2], pred, lty = "solid")
    return(pred)
}

## Obtain current predictions and compute discrepancy
rmsd <- function(parms, data1) {
    n <- nrow(data1)
    pred <- getregpred(parms, data1)
    ## mean squared deviation
    msd <- sum((pred - data1)^2) / n
    return(sqrt(msd))
}
    

rho <- .8      ## True b1
intercept <- 0 ## True b0
nDataPts <- 20

## generate synthetic data
data <- matrix(0, nDataPts, 2)

data[, 2] <- rnorm(nDataPts)
data[, 1] <- rnorm(nDataPts) * sqrt(1.0 - rho^2) + data[,2] * rho + intercept


x <- c(-0.89982415,  1.56116441, -1.78037455,  1.45501677,  1.27647533,
       -1.07390318, -0.41044245,  0.72778349, -0.52919587,  1.60584716,
        0.19985433,  1.83350908, -0.96984766, -0.54173508, -0.80143901,
       0.06808271, -1.17132121, -0.34542501,  0.37408046, -1.82817883)
y <- c(-0.89242375,  1.10527017, -1.53634022,  1.07810283,  0.43829784,
       -0.45811606, -0.26481776,  0.09022096,  0.01988166,  1.52272055,
       -0.38993976,  1.02379374, -1.17145366, -0.10908183, -1.26349586,
       1.15732887, -0.853354  , -0.95531825, -0.52389054, -1.54687098)
m0 <- lm(y~x)
summary(m0)
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.66668 -0.33968 -0.09139  0.22904  1.23831 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.13077    0.10751  -1.216     0.24    
## x            0.73129    0.09592   7.624 4.83e-07 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
## 
## Residual standard error: 0.4801 on 18 degrees of freedom
## Multiple R-squared:  0.7636,	Adjusted R-squared:  0.7504 
## F-statistic: 58.13 on 1 and 18 DF,  p-value: 4.829e-07

## do conventional regression analysis
lm(data[,1] ~ data[,2])
## Call:
## lm(formula = data[, 1] ~ data[, 2])
## 
## Coefficients:
## (Intercept)    data[, 2]  
## -0.1523       0.7819

## assign starting values
startParms <- c(-1., .2)
names(startParms) <- c("b1", "b0")

## obtain parameter estimates
xout <- optim(startParms, rmsd, data1 = data)
xout$par
xout$counts
xout$value
## $par
##          b1          b0 
##  0.89106336 -0.07604052 
## 
## $value
## [1] 0.523369
## 
## $counts
## function gradient 
##      119       NA 
## 
## $convergence
## [1] 0

