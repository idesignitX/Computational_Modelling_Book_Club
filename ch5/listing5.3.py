## Yi-Shin Lin, 27-11-2021
import numpy as np
from scipy.stats import norm
import pandas as pd
from plotnine import *

def weighted_sd(x, w, mu):
    term0 = w * np.power(x - mu, 2)
    wvar = np.sum(term0) / np.sum(w)
    return np.sqrt(wvar)

def weighted_mean(x, w):
    return np.sum(x * w) / np.sum(w)


## The Gaussian distribution for the express saccade and the regular saccade
express_saccade_dist = np.array((100, 10))
regular_saccade_dist = np.array((150, 20))

N = 1000
pShort = .3  ## the probability of an express saccade
random.seed(4)
whichD = random.choices([0, 1], weights = [pShort, 1 - pShort], k = N)

dat = np.empty(N)
dat[:] = np.nan


for i in range(0, len(whichD)):
    if whichD[i] == 0:  ## if False (0), express saccade
        dat[i] = norm.rvs(express_saccade_dist[0], express_saccade_dist[1], 1)
    else:  ## else True (1), regular saccade
        dat[i] = norm.rvs(regular_saccade_dist[0], regular_saccade_dist[1], 1)


## guess parameters
mu1 = np.median(dat) * .8
mu2 = np.median(dat) * 1.2
sd1 = np.std(dat)
sd2 = np.std(dat)
ppi = .5  ## the proportion of data thought to belong to the first distribution
oldppi = 0
((mu1, mu2))
((sd1, sd2))

while (abs(ppi - oldppi) > 1e-5):
    oldppi = ppi
    ## Expectation step - the expected likelihood of each trial generating by 
    ## the process of the (2nd) regular saccade distribution
    total_density = (1-ppi) * norm.pdf(dat, mu1, sd1) + ppi * norm.pdf(dat, mu2, sd2) 
    l = (ppi * norm.pdf(dat, mu2, sd2)) / total_density

    ## Maximization step - calculating the maximum likelihoods
    mu1 = weighted_mean(dat, 1-l)
    mu2 = weighted_mean(dat, l)
    sd1 = weighted_sd(dat, 1-l, mu1)
    sd2 = weighted_sd(dat, l, mu2)
    ## ((mu1, mu2))
    ## ((sd1, sd2))
    ppi = np.mean(l)
    print(ppi)


y1 = (1-ppi) * norm.pdf(dat, mu1, sd1)
y2 = ppi * norm.pdf(dat, mu2, sd2)
df = pd.DataFrame({'RT': dat, 'y1': y1, 'y2':y2})
myPlot = (ggplot(df, aes(x = 'RT', y = after_stat('density'))) +
    geom_histogram(colour = "black", fill = "white", binwidth = 3) +
    geom_line(aes(x = 'RT', y = 'y1')) +
    geom_line(aes(x = 'RT', y = 'y2')) +
    xlab("RT (ms)") + ylab("Density"))

myPlot.save("GMMexample.png", dpi = 600)
