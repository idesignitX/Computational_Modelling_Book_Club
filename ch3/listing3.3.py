## Yi-Shin Lin, 27-10-2021
## listing 3.4 for bootstrapping, 28-10-2021
import numpy as np
import scipy.optimize
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *
## Obtain current predictions and compute discrepancy
def rmsd(parameter, re, ri):
    ## parameter must be a three-element vector of a, b, and c
    ## re = proportion items retained
    ## ri = retention interval (days)
    if np.any(parameter < 0) or np.any(parameter > 1):
        return 1e6

    pred_re = parameter[0] * np.power((parameter[1] * ri + 1), -parameter[2])
    msd = np.sum(np.power(pred_re - re, 2)) / len(re)
    out = np.sqrt(msd)
    return out

def fx(x, parameter):
    a = parameter[0]
    b = parameter[1]
    c = parameter[2]
    out = a * np.power(b*x+1, -c)
    return out

rec = np.array([.93, .88, .86, .66, .47, .34])
rti = np.array([.005, 1, 2, 7, 14, 42])
## Find the best fitting parameter ---------------------------------------------
## initialize starting values
start_parameter = np.array([1, .05, .7])
xout = scipy.optimize.fmin(rmsd, x0 = start_parameter, args = (rec, rti))
xout
## Optimization terminated successfully.
##          Current function value: 0.026192
##          Iterations: 101
##          Function evaluations: 177
## Out[46]: array([0.94810343, 0.13216776, 0.58159765])


x = np.linspace(0, 45, 100)
y = fx(x, xout)

plt.figure(figsize = (4, 3))
ax = plt.axes()
ax.scatter(rti, rec)
ax.plot(x, y)
plt.show()


## listing 3.4 -------------------------------------------
## perform bootstrapping analysis
## Carpenter et al's Experiment 1 recruited 55 participants.
n = len(rec)
ns = 55
## number of bootstrapping resampling 
nbs = 1000
nparameter = len(xout)
## Each row stores one bootstrapping outcome
bsparms = np.empty((nbs, nparameter,))
bsparms[:] = np.nan
bspow_pred = xout[0] * np.power(xout[1]*rti + 1, -xout[2])

for i in range(0, nbs):
    ## At each retention interval, vapply conducted one Monte Carlo simulation
    ## (ie resampling). The randomness is governed by rbinom
    recsynth = np.empty(n)
    recsynth[:] = np.nan
    recsynth
    for j in range(0, len(bspow_pred)):
        recsynth[j] = np.mean(np.random.binomial(1, bspow_pred[j], ns))

    start_parameter = xout
    bsparms[i,:] = scipy.optimize.fmin(rmsd, x0 = xout, args = (recsynth, rti))



vline0 = np.quantile(bsparms, .025, axis = 0)
vline1 = np.quantile(bsparms, .975, axis = 0)

da = pd.DataFrame({'x': bsparms[:,0]})
db = pd.DataFrame({'x': bsparms[:,1]})
dc = pd.DataFrame({'x': bsparms[:,2]})

(ggplot(da, aes(x="x")) +
    geom_histogram() +
 geom_vline(xintercept = vline0[0], linetype = "dotted") +
 geom_vline(xintercept = vline1[0], linetype = "dotted") +
 geom_vline(xintercept = xout[0], linetype = "dashed", colour = "purple")
)

(ggplot(db, aes(x="x")) +
    geom_histogram() +
 geom_vline(xintercept = vline0[1], linetype = "dotted") +
 geom_vline(xintercept = vline1[1], linetype = "dotted") +
 geom_vline(xintercept = xout[1], linetype = "dashed", colour = "purple")
)

(ggplot(dc, aes(x="x")) +
    geom_histogram() +
 geom_vline(xintercept = vline0[2], linetype = "dotted") +
 geom_vline(xintercept = vline1[2], linetype = "dotted") +
 geom_vline(xintercept = xout[2], linetype = "dashed", colour = "purple")
)
