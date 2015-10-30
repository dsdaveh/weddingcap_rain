"""
@author Dipanjan Paul/Jules Malin
"""

## For each day, develop weather information pattern on the past 3 days
## leading to the current day and the next 3 days starting from the current day.
## This weather pattern will include 7 days of weather information.

import csv
import sys
import pandas as pd
import logging
import argparse
import numpy as np
from sklearn import datasets, linear_model
from numpy import *

# configure logging
logger = logging.getLogger("example")

handler = logging.StreamHandler(sys.stderr)
handler.setFormatter(logging.Formatter(
    '%(asctime)s %(levelname)s %(name)s: %(message)s'))

logger.addHandler(handler)
logger.setLevel(logging.DEBUG)

regr = linear_model.LinearRegression()


def data_cleaner(parm):
    # wrap the inputs and outputs in csv interpreters

    #keys = pd.read_csv("key.csv",header=0)

    data = pd.read_csv("/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/train.csv",header=0)
    Id = data.Id.unique().tolist()
    
    data.loc[:,"Ref_c_to"] = 0
    data.loc[:,"Ref_c_from"] = 0

    data.loc[:,"Ref_5x5_10th_c_to"] = 0
    data.loc[:,"Ref_5x5_10th_c_from"] = 0

    data.loc[:,"Ref_5x5_50th_c_to"] = 0
    data.loc[:,"Ref_5x5_50th_c_from"] = 0

    data.loc[:,"Ref_5x5_90th_c_to"] = 0
    data.loc[:,"Ref_5x5_90th_c_from"] = 0

    data.loc[:,"RefComposite_c_to"] = 0
    data.loc[:,"RefComposite_c_from"] = 0
    
    data.loc[:,"RefComposite_5x5_10th_c_to"] = 0
    data.loc[:,"RefComposite_5x5_10th_c_from"] = 0
    
    data.loc[:,"RefComposite_5x5_50th_c_to"] = 0
    data.loc[:,"RefComposite_5x5_50th_c_from"] = 0
    
    data.loc[:,"RefComposite_5x5_90th_c_to"] = 0
    data.loc[:,"RefComposite_5x5_90th_c_from"] = 0
    
    data.loc[:,"RhoHV_c_to"] = 0
    data.loc[:,"RhoHV_c_from"] = 0
    
    data.loc[:,"RhoHV_5x5_10th_c_to"] = 0
    data.loc[:,"RhoHV_5x5_10th_c_from"] = 0
    
    data.loc[:,"RhoHV_5x5_50th_c_to"] = 0
    data.loc[:,"RhoHV_5x5_50th_c_from"] = 0
    
    data.loc[:,"RhoHV_5x5_90th_c_to"] = 0
    data.loc[:,"RhoHV_5x5_90th_c_from"] = 0
    
    data.loc[:,"Zdr_c_to"] = 0
    data.loc[:,"Zdr_c_from"] = 0
    
    data.loc[:,"Zdr_5x5_10th_c_to"] = 0
    data.loc[:,"Zdr_5x5_10th_c_from"] = 0
    
    data.loc[:,"Zdr_5x5_50th_c_to"] = 0
    data.loc[:,"Zdr_5x5_50th_c_from"] = 0
    
    data.loc[:,"Zdr_5x5_90th_c_to"] = 0
    data.loc[:,"Zdr_5x5_90th_c_from"] = 0
    
    data.loc[:,"Kdp_c_to"] = 0
    data.loc[:,"Kdp_c_from"] = 0
    
    data.loc[:,"Kdp_5x5_10th_c_to"] = 0
    data.loc[:,"Kdp_5x5_10th_c_from"] = 0
    
    data.loc[:,"Kdp_5x5_50th_c_to"] = 0
    data.loc[:,"Kdp_5x5_50th_c_from"] = 0
    
    data.loc[:,"Kdp_5x5_90th_c_to"] = 0
    data.loc[:,"Kdp_5x5_90th_c_from"] = 0
    
    

    lm_to_date_coef = (lambda m: regr.fit(lst[m - 3:m + 1, 1:], lst[m - 3:m + 1, 0:1]).coef_)
    lm_from_date_coef = (lambda m: regr.fit(lst[m:m + 3, 1:], lst[m:m + 3, 0:1]).coef_)

    for s in Id:

        loc_i = data.loc[data.Id==s,:].index[4:]

        lst = np.array(data.loc[data.Id == s,'Id'])
        lst = transpose(vstack((lst, range(len(lst)))))

        arr = array(lst[::,1],dtype=int)[4:]
        
        lst = np.array(data.loc[data.Id == s,'Ref'])
        lst = transpose(vstack((lst, range(len(lst)))))
        ref_lm_c_to = map(lm_to_date_coef, arr[:])
        ref_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Ref_c_to"] = ref_lm_c_to
        data.loc[loc_i,"Ref_c_from"] = ref_lm_c_from

        lst = np.array(data.loc[data.Id == s,'Ref_5x5_10th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Ref_5x5_10th_lm_c_to = map(lm_to_date_coef, arr[:])
        Ref_5x5_10th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Ref_5x5_10th_c_to"] = Ref_5x5_10th_lm_c_to
        data.loc[loc_i,"Ref_5x5_10th_c_from"] = Ref_5x5_10th_lm_c_from

        lst = np.array(data.loc[data.Id == s,'Ref_5x5_50th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Ref_5x5_50th_lm_c_to = map(lm_to_date_coef, arr[:])
        Ref_5x5_50th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Ref_5x5_50th_c_to"] = Ref_5x5_50th_lm_c_to
        data.loc[loc_i,"Ref_5x5_50th_c_from"] = Ref_5x5_50th_lm_c_from

        lst = np.array(data.loc[data.Id == s,'Ref_5x5_90th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Ref_5x5_90th_lm_c_to = map(lm_to_date_coef, arr[:])
        Ref_5x5_90th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Ref_5x5_90th_c_to"] = Ref_5x5_90th_lm_c_to
        data.loc[loc_i,"Ref_5x5_90th_c_from"] = Ref_5x5_90th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RefComposite'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RefComposite_lm_c_to = map(lm_to_date_coef, arr[:])
        RefComposite_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RefComposite_c_to"] = RefComposite_lm_c_to
        data.loc[loc_i,"RefComposite_c_from"] = RefComposite_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RefComposite_5x5_10th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RefComposite_5x5_10th_lm_c_to = map(lm_to_date_coef, arr[:])
        RefComposite_5x5_10th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RefComposite_5x5_10th_c_to"] = RefComposite_5x5_10th_lm_c_to
        data.loc[loc_i,"RefComposite_5x5_10th_c_from"] = RefComposite_5x5_10th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RefComposite_5x5_50th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RefComposite_5x5_50th_lm_c_to = map(lm_to_date_coef, arr[:])
        RefComposite_5x5_50th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RefComposite_5x5_50th_c_to"] = RefComposite_5x5_50th_lm_c_to
        data.loc[loc_i,"RefComposite_5x5_50th_c_from"] = RefComposite_5x5_50th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RefComposite_5x5_90th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RefComposite_5x5_90th_lm_c_to = map(lm_to_date_coef, arr[:])
        RefComposite_5x5_90th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RefComposite_5x5_90th_c_to"] = RefComposite_5x5_90th_lm_c_to
        data.loc[loc_i,"RefComposite_5x5_90th_c_from"] = RefComposite_5x5_90th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RhoHV'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RhoHV_lm_c_to = map(lm_to_date_coef, arr[:])
        RhoHV_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RhoHV_c_to"] = RhoHV_lm_c_to
        data.loc[loc_i,"RhoHV_c_from"] = RhoHV_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RhoHV_5x5_10th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RhoHV_5x5_10th_lm_c_to = map(lm_to_date_coef, arr[:])
        RhoHV_5x5_10th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RhoHV_5x5_10th_c_to"] = RhoHV_5x5_10th_lm_c_to
        data.loc[loc_i,"RhoHV_5x5_10th_c_from"] = RhoHV_5x5_10th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RhoHV_5x5_50th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RhoHV_5x5_50th_lm_c_to = map(lm_to_date_coef, arr[:])
        RhoHV_5x5_50th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RhoHV_5x5_50th_c_to"] = RhoHV_5x5_50th_lm_c_to
        data.loc[loc_i,"RhoHV_5x5_50th_c_from"] = RhoHV_5x5_50th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'RhoHV_5x5_90th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        RhoHV_5x5_90th_lm_c_to = map(lm_to_date_coef, arr[:])
        RhoHV_5x5_90th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"RhoHV_5x5_90th_c_to"] = RhoHV_5x5_90th_lm_c_to
        data.loc[loc_i,"RhoHV_5x5_90th_c_from"] = RhoHV_5x5_90th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Zdr'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Zdr_lm_c_to = map(lm_to_date_coef, arr[:])
        Zdr_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Zdr_c_to"] = Zdr_lm_c_to
        data.loc[loc_i,"Zdr_c_from"] = Zdr_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Zdr_5x5_10th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Zdr_5x5_10th_lm_c_to = map(lm_to_date_coef, arr[:])
        Zdr_5x5_10th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Zdr_5x5_10th_c_to"] = Zdr_5x5_10th_lm_c_to
        data.loc[loc_i,"Zdr_5x5_10th_c_from"] = Zdr_5x5_10th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Zdr_5x5_50th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Zdr_5x5_50th_lm_c_to = map(lm_to_date_coef, arr[:])
        Zdr_5x5_50th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Zdr_5x5_50th_c_to"] = Zdr_5x5_50th_lm_c_to
        data.loc[loc_i,"Zdr_5x5_50th_c_from"] = Zdr_5x5_50th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Zdr_5x5_90th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Zdr_5x5_90th_lm_c_to = map(lm_to_date_coef, arr[:])
        Zdr_5x5_90th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Zdr_5x5_90th_c_to"] = Zdr_5x5_90th_lm_c_to
        data.loc[loc_i,"Zdr_5x5_90th_c_from"] = Zdr_5x5_90th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Kdp'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Kdp_lm_c_to = map(lm_to_date_coef, arr[:])
        Kdp_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Kdp_c_to"] = Kdp_lm_c_to
        data.loc[loc_i,"Kdp_c_from"] = Kdp_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Kdp_5x5_10th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Kdp_5x5_10th_lm_c_to = map(lm_to_date_coef, arr[:])
        Kdp_5x5_10th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Kdp_5x5_10th_c_to"] = Kdp_5x5_10th_lm_c_to
        data.loc[loc_i,"Kdp_5x5_10th_c_from"] = Kdp_5x5_10th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Kdp_5x5_50th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Kdp_5x5_50th_lm_c_to = map(lm_to_date_coef, arr[:])
        Kdp_5x5_50th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Kdp_5x5_50th_c_to"] = Kdp_5x5_50th_lm_c_to
        data.loc[loc_i,"Kdp_5x5_50th_c_from"] = Kdp_5x5_50th_lm_c_from
        
        lst = np.array(data.loc[data.Id == s,'Kdp_5x5_90th'])
        lst = transpose(vstack((lst, range(len(lst)))))
        Kdp_5x5_90th_lm_c_to = map(lm_to_date_coef, arr[:])
        Kdp_5x5_90th_lm_c_from = map(lm_from_date_coef, arr[:])

        data.loc[loc_i,"Zdr_5x5_90th_c_to"] = Kdp_5x5_90th_lm_c_to
        data.loc[loc_i,"Zdr_5x5_90th_c_from"] = Kdp_5x5_90th_lm_c_from

        # Every 1000 rows send an update to the user for progress tracking.
        #if i % 1000 == 0:
        #    logger.info("Completed row %d" % i)

    data.to_csv("rain_clean_py.csv",index=False)

if __name__ == "__main__":
    # set up logger
    parser = argparse.ArgumentParser(description=__doc__)
    data_cleaner('null')
