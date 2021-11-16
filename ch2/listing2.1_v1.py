## Yi-Shin Lin, 23-10-2021
import numpy as np
import pandas as pd
from plotnine import *

def farrell_rw(v, sv, a, ntrial, nt):
  if ntrial <= 0:
    print("ntrial must > 0.")
  if nt <= 0:
    print("nt must > 0.")
  evidence = np.empty((ntrial, nt + 1,))
  evidence[:] = np.nan
  evidence[:,0] = 0
  
  response = np.empty((ntrial))
  latency = np.empty((ntrial))
  response[:] = np.nan
  latency[:] = np.nan
  
  for i in range(0, ntrial):
      acc_evid = evidence[i, 0]
      for j in range(1, nt+1):
        evidence[i, j] = np.random.normal(v, sv, 1)
        acc_evid = acc_evid + evidence[i, j]
        if abs(acc_evid) >= a:
          latency[i] = j
          response[i] = np.sign(acc_evid)
          break

  return latency, evidence, response


def farrell_rw2(v, sv, a, sz, s, ntrial, nt ):
  ## add the between-trial variability of the drift rate and the starting point
  ##
  ## v = between-trial mean drift rate
  ## sv = between-trial standard deviation drift rate
  ## s = within-trial standard deviation drift rate; s^2 is offen dubbed diffusion coefficient
  ## sz = between-trial standard deviation starting point.
  if ntrial <= 0:
    print("ntrial must > 0.")
  if nt <= 0:
    print("nt must > 0.")
  evidence = np.empty((ntrial, nt + 1,))
  evidence[:] = np.nan
  
  # evidence[:,0] = 0
  
  response = np.empty((ntrial))
  latency = np.empty((ntrial))
  response[:] = np.nan
  latency[:] = np.nan
  
  for i in range(0, ntrial):
    start_evidence = np.random.normal(0, sz, 1)
    evidence[i, 0] = start_evidence
    acc_evid = evidence[i, 0]
    drift_rate = np.random.normal(v, sv, 1)  ## within-trial mean drift rate
    
    for j in range(1, nt+1):
      evidence[i, j] = np.random.normal(drift_rate, s, 1)
      acc_evid = acc_evid + evidence[i, j]
      if abs(acc_evid) >= a:
        latency[i] = j
        response[i] = np.sign(acc_evid)
        break
  return latency, evidence, response

def make_df(latency, evidence, n) :
  first_fewDT = latency[0:n]
  first_fewDV = evidence[0:n,:]
  a = np.arange((int(first_fewDT[0])+1))
  b = np.arange((int(first_fewDT[1])+1))
  c = np.arange((int(first_fewDT[2])+1))
  d = np.arange((int(first_fewDT[3])+1))
  e = np.arange((int(first_fewDT[4])+1))
  at = np.full(shape = len(a), fill_value = 1, dtype = int)
  bt = np.full(shape = len(b), fill_value = 2, dtype = int)
  ct = np.full(shape = len(c), fill_value = 3, dtype = int)
  dt = np.full(shape = len(d), fill_value = 4, dtype = int)
  et = np.full(shape = len(e), fill_value = 5, dtype = int)

  time = np.concatenate((a,b,c,d,e), axis = 0)
  tr = np.concatenate((at,bt,ct,dt,et), axis = 0)

  a = np.cumsum(first_fewDV[0, 0:int(first_fewDT[0] + 1)])
  b = np.cumsum(first_fewDV[1, 0:int(first_fewDT[1] + 1)])
  c = np.cumsum(first_fewDV[2, 0:int(first_fewDT[2] + 1)])
  d = np.cumsum(first_fewDV[3, 0:int(first_fewDT[3] + 1)])
  e = np.cumsum(first_fewDV[4, 0:int(first_fewDT[4] + 1)])
  dv = np.concatenate((a,b,c,d,e), axis = 0)

  df = pd.DataFrame({'x': time, 'y': dv, 'trial':tr})
  df['trial'] = df['trial'].astype(object)
  return df

# def farrell_rw2(v, sv, a, sz, s, ntrial, nt ):
my_latency0, my_evidence0, my_response0 = farrell_rw(0, .3, 3, 10000, 2000)
my_latency1, my_evidence1, my_response1 = farrell_rw(.2, .3, 3, 10000, 2000)
my_latency2, my_evidence2, my_response2 = farrell_rw(.03, .3, 3, 10000, 2000) 

df0 = make_df(my_latency0, my_evidence0, 5)
df1 = make_df(my_latency1, my_evidence1, 5)

df0['dr'] = 0
df1['dr'] = 0.2
d = pd.concat([df0,df1], ignore_index = True)

(
    ggplot(d, aes(x="x", y="y", colour="trial")) +
    geom_line() + xlab('Time') + ylab('Evidence') +
    facet_grid('dr~.') 
)


df2 = pd.DataFrame({'RT': my_latency0, 'R': my_response0})
df2['Rtype'] = np.where(df2['R'] == 1, 'Top responses', 'Bottom responses')

df3 = pd.DataFrame({'RT': my_latency2, 'R': my_response2})
df3['Rtype'] = np.where(df3['R'] == 1, 'Top responses', 'Bottom responses')

df2['dr'] = 0
df3['dr'] = 0.03


df2.R.value_counts() / df2.shape[0]
df3.R.value_counts() / df2.shape[0]

df2.groupby(['R']).mean()
df3.groupby(['R']).mean()

(
  ggplot(df2, aes(x="RT")) +
  geom_histogram() +
  facet_grid('Rtype~dr')
)

(
  ggplot(df3, aes(x="RT")) +
  geom_histogram() +
  facet_grid('Rtype~dr')
)

## def farrell_rw2(v, sv, a, sz, s, ntrial, nt ):
my_latency3, my_evidence3, my_response3 = farrell_rw2(.035, 0, 3, 0.8, 0.3, 1000, 2000)
my_latency4, my_evidence4, my_response4 = farrell_rw2(.035, 0.025, 3, 0, 0.3, 1000, 2000)

df4 = pd.DataFrame({'RT': my_latency3, 'R': my_response3})
df4['Rtype'] = np.where(df4['R'] == 1, 'Top responses', 'Bottom responses')

df5 = pd.DataFrame({'RT': my_latency4, 'R': my_response4})
df5['Rtype'] = np.where(df5['R'] == 1, 'Top responses', 'Bottom responses')

df4.R.value_counts() / df4.shape[0]
df5.R.value_counts() / df5.shape[0]

df4.groupby(['R']).mean()
df5.groupby(['R']).mean()


(
  ggplot(df4, aes(x="RT")) +
  geom_histogram() +
  facet_grid('Rtype~.')
)

(
  ggplot(df5, aes(x="RT")) +
  geom_histogram() +
  facet_grid('Rtype~.')
)

