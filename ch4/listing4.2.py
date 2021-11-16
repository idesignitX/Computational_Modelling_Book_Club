## Yi-Shin Lin, 16-11-2021
import numpy as np
from scipy.stats import binom
import scipy.optimize
import pandas as pd

## Obtain current predictions and compute discrepancy
def Euclidean(x, y, w):
    ## Euclidean(stim[:1].to_numpy(), stim[:2].to_numpy(), w)
    term0 = w*np.power(x - y, 2)
    out = np.sqrt( np.sum(term0) )
    return out

def GCMpred(probe, exemplars, c, w):
    ## tmp = GCMpred(stim[:1].to_numpy(), exemplars, c, w)
    ## {0: array([0.        , 0.77503833, 0.41018329, 0.54557113, 0.82417462]),
    ##  1: array([0.87030496, 1.01944575, 0.80819033, 1.24746426, 1.33244921])}
    nexem = len(exemplars)
    out = np.empty(nexem)
    for i, key in enumerate(exemplars.keys()):
        ex = exemplars[key]
        nobs = len(ex)
        
        tmp = np.empty(nobs)
        tmp[:] = np.nan
        for j in range(0, nobs):
            tmp[j] = Euclidean(ex.iloc[j].to_numpy(), probe, w)

        out[i] = np.sum(np.exp(-c*tmp))

    return out / np.sum(out)

N = 2*80 ## two responses per face from 80 ppl
N_A = np.round(N*.968) ## First face observed response probability is .968
c = 4
w = np.array([.19, .12, .25, .45])

stim = pd.read_csv('faceStim.csv', header = None)
    
exemplars = {
    "A": stim[:5],
    "B": stim[5:10]
}
## len(exemplars)

preds = GCMpred(stim.iloc[0].to_numpy(), exemplars, c, w)
binom.pmf(k = N_A, n = N, p = preds[0])
## Out[163]: 0.02503622303456052

