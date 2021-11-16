## Yi-Shin Lin, 16-11-2021
import numpy as np
from scipy.stats import binom
from scipy.stats import norm
from scipy.optimize import minimize
import pandas as pd
import math

## Obtain current predictions and compute discrepancy
def Euclidean(x, y, w):
    ## Euclidean(stim[:1].to_numpy(), stim[:2].to_numpy(), w)
    term0 = w*np.power(x - y, 2)
    out = np.sqrt( np.sum(term0) )
    return out

def GCMprednoisy(probe, exemplars, c, w, sigma, b):
    '''
    Calculate GCM likelihood of categorising a face stimulus, `probe` into category `A` out of N total responses, given parameter c, w, and b.

    Arguments:
	    probe: a numpy array 
	    exemplars: a two-element dictionary
	    c: a number
	    w: a numpy array
            sigma: a number
	    b: a number
    Return:
            a probability of categorising stimulus, `probe` into category A
    Example:
        res = GCMpred(stim[:1].to_numpy(), exemplars, c, w, sigma, b)
    '''
    nexem = len(exemplars)
    sim_score = np.empty(nexem)
    sim_score[:] = np.nan
    
    for i, key in enumerate(exemplars.keys()):
        ex = exemplars[key]
        nobs = len(ex)    ## number of observations in an exemplar
        
        tmp = np.empty(nobs)
        tmp[:] = np.nan
        for j in range(0, nobs):
            tmp[j] = Euclidean(ex.iloc[j].to_numpy(), probe, w)

        sim_score[i] = np.sum(np.exp(-c*tmp))

    ## Whether the difference of similarity score exceeds a threshold b
    out = norm.cdf(sim_score[0] - sim_score[1] - b, loc = 0, scale = sigma)
    return out

def GCMutil(theta):
    ## , stim, exemplars, data, N):
    ## theta = np.array([1, .25, .25, .25, 1, .2])
    ## nDat = len(data)
    ## theta = init_guess
    dev = np.empty(nDat)
    pre = np.empty(nDat)
    dev[:] = np.nan
    pre[:] = np.nan
    w = np.empty(4)
    w[:] = np.nan

    c = theta[0]
    w[0] = theta[1]
    w[1] = (1 - w[0]) * theta[2]
    w[2] = (1 - np.sum(w[:2])) * theta[3]
    w[3] = (1 - np.sum(w[:3]))
    sigma = theta[4]
    b = theta[5]

    for i in range(0, nDat):
        p = GCMprednoisy(stim.iloc[i].to_numpy(), exemplars, c, w, sigma, b)
        dev[i] = -2*np.log(binom.pmf(k = data[i], n = N, p = p))

    out = np.sum(dev)
    return out
    

N = 2*40 ## assuming two responses per face from 40 learners
stim = pd.read_csv('faceStim.csv', header = None)
exemplars = {
    "A": stim[:5],
    "B": stim[5:10]
}


tmp = pd.read_table("facesDataLearners.txt", header = None)
data = np.ceil(tmp.to_numpy()*N)
nDat = len(data)
bestfit = 1e4

w1 = np.array([.25, .5, .75])
w2 = np.array([.25, .5, .75])
w3 = np.array([.25, .5, .75])
bnds = ((0, 10), (0, 1), (0, 1), (0, 1), (0, 10), (-5, 5))

for i in w1:
     for j in w2:
         for k in w3:
             init_guess = np.array([1, i, j, k, 1, .2])
             res = minimize(fun = GCMutil, x0 = init_guess, method='L-BFGS-B', bounds = bnds)
             # res = minimize(fun = GCMutil, x0 = init_guess, method='Nelder-Mead', options={'xatol': 1e-8, 'disp': True})
             if(res.fun < bestfit):
                     bestres = res
                     bestfit = res.fun

bestres.x
bestres.fun
## 153.10190314638706
## array([2.54562316, 0.37134223, 0.00864732, 0.97985185, 1.15405855,
##        0.07934023])
