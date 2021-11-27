## Yi-Shin Lin, 26-11-2021
import numpy as np
from scipy.stats import dweibull
import scipy.optimize

ns = 30
nobs = 20
## To include .9, set the stop = 1, because Python does not include the last
## value
qp = np.arange(.1, 1, .2)

## The variability introduced in randomly generating the parameters for
## individuals; rnorm
shift = np.random.normal(250, 50, ns)
scale = np.random.normal(200, 50, ns)
shape = np.random.normal(2, .25, ns)

dat = np.empty((nobs, ns))
kk = np.empty((len(qp), ns))
dat[:] = np.nan
kk[:] = np.nan

## Another sampling variability arising from the response times
## rweibull
for i in range(0, ns):
    ## Each column represents one participant. Each of them made 20 responses
    dat[:,i] = dweibull.rvs(c = shape[i], loc = shift[i], scale = scale[i], size = nobs)
     ## dat[:,i] = scale[i] * np.random.weibull(shape[i], nobs) + shift[i]
    ## calculate sample quantiles for each participant
    kk[:,i] = np.quantile(dat[:,i], qp)


## FITTING VIA QUANTILE AVERAGING -------------------------
## averaging across participants
vinq = np.mean(kk, 1)

## https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.dweibull.html
## the dweibull in scipy divide its shape parameter outside the exponent
## by a factor 2.
## fit the shifted Weibull to averaged quantiles
def weib_qdev(x, q_emp, qp_):
    if np.any(x <= 0):
        return 1e7
    
    q_pred = dweibull.ppf(qp_, x[2], x[0], x[1])
    dev = np.sqrt(np.mean(np.power(q_pred - q_emp, 2)))
    return dev

start_parameter = np.array((225, 225, 1))
res = scipy.optimize.fmin(weib_qdev, x0 = start_parameter, args = (vinq, qp))
res.round(2)
## array([258.99, 192.49,   2.  ])
## np.mean(shift)
## np.mean(scale)
## np.mean(shape)
## Out[136]: 249.21859980971297
## Out[137]: 200.7213185946087
## Out[138]: 1.9828631795064895

## FITTING INDIVIDUAL PARTICIPANTS -----------------------
def weib_dev(x, rts, nob):
    if np.any(x <= 0):
        return 1e7

    like = dweibull.pdf(x = rts - x[0], c = x[2], loc = 0, scale = x[1])
    dev = np.sum(-2 * np.log(like))
    return dev

nparameter = 3
bsparms = np.empty((ns, nparameter,))
bsparms[:] = np.nan

for i in range(0, ns):
    dat_i = dat[:,i]
    start_parameter = np.array((100, 225, 1))
    bsparms[i,:] = scipy.optimize.fmin(weib_dev, x0 = start_parameter, args = (dat_i, nobs))

np.mean(bsparms, 0)
np.std(bsparms, 0)
(np.mean(shift), np.mean(scale), np.mean(shape))
## True                    (260.465, 210.111, 1.881)
## Vincentised average     (264.15,  200.12,  1.92)
## Individual estimate     (203.594, 207.754, 1.755)
## Sd                      (68.66,   63.56,   0.71)
