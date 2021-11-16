## Yi-Shin Lin, 26-10-2021
import numpy as np
import scipy.optimize

## Obtain current predictions and compute discrepancy
def rmsd(parameter, x, true_y):
    ## parameter must be a two-element vector of b0 and b1
    ## x is a n-element input variable
    pred_y = parameter[0] + parameter[1] * x
    msd = np.sum(np.power(pred_y - true_y, 2)) / len(x)
    out = np.sqrt(msd)
    return out

rho = .8      ## True b1
intercept = 0 ## True b0
nDataPts = 20

noise = np.random.normal(0, 1, nDataPts)
true_x = np.random.normal(0, 1, nDataPts)
true_y = intercept + rho * true_x + noise * np.sqrt(1.0 - np.power(rho, 2))
len(true_x)

## do conventional linear regression analysis, using 1-degree of polynominal fit
np.polyfit(true_x, true_y, 1)
## b1, b0
## Out[41]: array([ 0.67643854, -0.05588051])

## assign starting values
## b0, b1
start_parameter = np.empty(2)
start_parameter[0] = .2
start_parameter[1] = -1
xout = scipy.optimize.fmin(rmsd, x0 = start_parameter, args = (true_x, true_y))
## Optimization terminated successfully.
##          Current function value: 0.651065
##          Iterations: 43
##          Function evaluations: 84
xout
## b0, b1
## array([-0.05590365,  0.67643164])

