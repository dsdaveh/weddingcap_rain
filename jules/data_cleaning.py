# -*- coding: utf-8 -*-
"""
Created on Sun Oct 25 14:03:57 2015

@author: jmalin
"""

# change the location of the downloaded test file as necessary.
#infile="/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/test.csv"
#infile="kaggle/sample.csv"
#outfile="/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/sample_solution4.csv"

import csv
import sys
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
    if parm == 'train':
        writer = csv.writer(open("/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/jules/Python/train_cln.csv", "wb"), delimiter=',')
        reader = csv.reader(open("/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/train.csv", "rb"), delimiter=',')
        ##reader = csv.reader(open("testsml.csv","rb"), delimiter=',')
    elif parm == 'test':
        writer = csv.writer(open("/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/jules/Python/test_cln.csv", "wb"), delimiter=',')
        reader = csv.reader(open("/Users/jmalin/Documents/Personal/School/Capstone/weddingcap_rain/test.csv", "rb"), delimiter=',')

    # read in the header
    header = reader.next()

    # determine the columns that hold the various fields in the training data
    id_ind = header.index('Id')
    mp_ind = header.index('minutes_past')
    rd_ind = header.index('radardist_km')
    #    ref_ind = header.index('Ref')
    ref_ind = header.index('Ref')
    ref1_ind = header.index('Ref_5x5_10th')
    ref2_ind = header.index('Ref_5x5_50th')
    ref3_ind = header.index('Ref_5x5_90th')
    #    rfc_ind = header.index('RefComposite')
    rfc_ind = header.index('RefComposite')
    rfc1_ind = header.index('RefComposite_5x5_10th')
    rfc2_ind = header.index('RefComposite_5x5_50th')
    rfc3_ind = header.index('RefComposite_5x5_90th')
    #    rho_ind = header.index('RhoHV')
    rho_ind = header.index('RhoHV')
    rho1_qc_ind = header.index('RhoHV_5x5_10th')
    rho2_ind = header.index('RhoHV_5x5_50th')
    rho3_ind = header.index('RhoHV_5x5_90th')
    #    zdr_ind = header.index('Zdr')
    zdr_ind = header.index('Zdr')
    zdr1_ind = header.index('Zdr_5x5_10th')
    zdr2_ind = header.index('Zdr_5x5_50th')
    zdr3_ind = header.index('Zdr_5x5_90th')
    #    kdp_ind = header.index('Kdp')
    kdp_ind = header.index('Kdp')
    kdp1_ind = header.index('Kdp_5x5_10th')
    kdp2_ind = header.index('Kdp_5x5_50th')
    kdp3_ind = header.index('Kdp_5x5_90th')

    if parm == 'train':
        xpt_ind = header.index('Expected')

    ##header.remove('Kdp')

    if parm == 'train':
        header=['Id', 'minutes_past', 'radardist_km', 'Ref_5x5_10th', 'Ref_5x5_50th', 'Ref_5x5_90th', 
        'RefComposite', 'RefComposite_5x5_10th', 'RefComposite_5x5_50th', 'RefComposite_5x5_90th',
        'RhoHV', 'RhoHV_5x5_10th', 'RhoHV_5x5_50th', 'RhoHV_5x5_90th', 'Zdr', 'Zdr_5x5_10th', 
        'Zdr_5x5_50th', 'Zdr_5x5_90th', 'Kdp', 'Kdp_5x5_10th', 'Kdp_5x5_50th', 'Kdp_5x5_90th' ]
    elif parm == 'test':
        header=['Id', 'minutes_past', 'radardist_km', 'Ref_5x5_10th', 'Ref_5x5_50th', 'Ref_5x5_90th', 
        'RefComposite', 'RefComposite_5x5_10th', 'RefComposite_5x5_50th', 'RefComposite_5x5_90th',
        'RhoHV', 'RhoHV_5x5_10th', 'RhoHV_5x5_50th', 'RhoHV_5x5_90th', 'Zdr', 'Zdr_5x5_10th', 
        'Zdr_5x5_50th', 'Zdr_5x5_90th', 'Kdp', 'Kdp_5x5_10th', 'Kdp_5x5_50th', 'Kdp_5x5_90th']

    writer.writerow(header)

    # Define a function to calculate the mean for the list of radar readings within a record. The array has to be
    # moved to the list var lst before the calcmean function is called
    #calcmean = (lambda m, n: np.mean(lst[m + 1:n + 1]))

    calcdiff = (lambda m, n: (lst[m + 1] - lst[n]))

    calc_indx = (lambda pos: list(range(l[pos] - l[pos - 1])))

    # Impute missing values
    impute99900lm = (lambda m, n: [regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99900.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99900.0]).coef_[0],
                                    regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99900.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99900.0]).intercept_]
                        if (sum(lst[m + 1:n + 1, 0] == -99900.0) < len(lst[m + 1:n + 1, 0])) else [0, 0])

    impute99901lm = (lambda m, n: [regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99901.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99901.0]).coef_[0],
                                    regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99901.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99901.0]).intercept_]
                        if (sum(lst[m + 1:n + 1, 0] == -99901.0) < len(lst[m + 1:n + 1, 0])) else [0, 0])

    impute99903lm = (lambda m, n: [regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99903.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99903.0]).coef_[0],
                                    regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != -99903.0]),
                                           lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != -99903.0]).intercept_]
                        if (sum(lst[m + 1:n + 1, 0] == -99903.0) < len(lst[m + 1:n + 1, 0])) else [0, 0])

    #imputenanlm = (lambda m, n: [regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != 'nan']),
    #                                       lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != 'nan']).coef_[0],
    #                                regr.fit((lst[m + 1:n + 1, 1:][lst[m + 1:n + 1, 0] != 'nan']),
    #                                       lst[m + 1:n + 1, 0][lst[m + 1:n + 1, 0] != 'nan']).intercept_]
    #                    if (sum(lst[m + 1:n + 1, 0] == 'nan') < len(lst[m + 1:n + 1, 0])) else [0, 0])

    impute99900mean = (lambda i, j: np.mean(lst[i + 1:j + 1][lst[i + 1:j + 1] != -99900.0])
                        if (sum(lst[i + 1:j + 1] == -99900.0) < len(lst[i + 1:j + 1])) else 0)

    impute99903mean = (lambda i, j: np.mean(lst[i + 1:j + 1][lst[i + 1:j + 1] != -99903])
                        if (sum(lst[i + 1:j + 1] == -99903) < len(lst[i + 1:j + 1])) else 0)

    impute999mean = (lambda i, j: np.mean(lst[i + 1:j + 1][lst[i + 1:j + 1] != 999.0])
                        if (sum(lst[i + 1:j + 1] == 999.0) < len(lst[i + 1:j + 1])) else 0)

    #imputenanmean = (lambda i, j: np.mean(np.array(lst[i + 1:j + 1][lst[i + 1:j + 1] != 'nan'], dtype='float'))
    #                    if (sum(lst[i + 1:j + 1] == 'nan') < len(lst[i + 1:j + 1])) else 0)

    # The idea is to consolidate the information based on radar read time. If one record has reading from multiple
    # radars,.. a separate output record will be written for each set of radar reading.

    for i, row in enumerate(reader):

        times = np.array(row[tte_ind].split(' '), dtype='float')

        l = list()
        l.append(-1)
        for k in range(len(times) - 1):
            if times[k + 1] > times[k]:
                l.append(k)
        l.append(len(times) - 1)

        indx = map(calc_indx,range(len(l))[1:])

        #r_cnt = len(l) - 1

        lst = times
        tme_diff = map(calcdiff, l[0:-1], l[1:])

        # Calculate the mean of each set of radar reading within a record.

        # The "Composite" feature has erroneous reading with -99900. This value has to be imputed
        # Also the array has to be moved to lst so that the mean calculation functions can be called

        dtr = np.array(row[dtr_ind].split(' '), dtype='float')
        lst = np.array(dtr)
        dtr_mean = map(impute99900mean, l[0:-1], l[1:])

        comp = np.array(row[comp_ind].split(' '), dtype='float')
        lst = np.array(comp)
        lst = transpose(vstack((lst, hstack(indx))))
        comp_lm = map(impute99900lm, l[0:-1], l[1:])

        hbrdscn = np.array(row[hbrdscn_ind].split(' '), dtype='float')
        lst = np.array(hbrdscn)
        lst[lst == -99901.0] = -99900.0
        lst[lst == -99902.0] = -99900.0
        lst[lst == -99903.0] = -99900.0
        lst[lst == 999.0] = -99900.0
        lst = transpose(vstack((lst, hstack(indx))))
        hbrdscn_lm = map(impute99900lm, l[0:-1], l[1:])

        hydmtr = np.array(row[hydmtr_ind].split(' '), dtype='float')
        lst = np.array(hydmtr)
        hydmtr_mean = map(impute99900mean, l[0:-1], l[1:])

        ##        kdp = np.array(row[kdp_ind].split(' '), dtype='float')

        rr1 = np.array(row[rr1_ind].split(' '), dtype='float')
        lst = np.array(rr1)
        lst[lst == 999.0] = -99900.0
        lst = transpose(vstack((lst, hstack(indx))))
        rr1_lm = map(impute99900lm, l[0:-1], l[1:])

        rr2 = np.array(row[rr2_ind].split(' '), dtype='float')
        lst = np.array(rr2)
        lst[lst == -99903.0] = -99900.0
        lst = transpose(vstack((lst, hstack(indx))))
        rr2_lm = map(impute99900lm, l[0:-1], l[1:])

        rr3 = np.array(row[rr3_ind].split(' '), dtype='float')
        lst = np.array(rr3)
        lst[lst == -99903.0] = -99900.0
        lst = transpose(vstack((lst, hstack(indx))))
        rr3_lm = map(impute99900lm, l[0:-1], l[1:])

        rqi = np.array(row[rqi_ind].split(' '), dtype='float')
        lst = np.array(rqi)
        lst[lst == -99903.0] = 999.0
        rqi_mean = map(impute999mean, l[0:-1], l[1:])

        rfl = np.array(row[rfl_ind].split(' '), dtype='float')
        lst = np.array(rfl)
        rfl_mean = map(impute99903mean, l[0:-1], l[1:])

        rfl_qc = np.array(row[rfl_qc_ind].split(' '), dtype='float')
        lst = np.array(rfl_qc)
        lst[lst == -99900.0] = 999.0
        rfl_qc_mean = map(impute999mean, l[0:-1], l[1:])

        rhohv = np.array(row[rhohv_ind].split(' '), dtype='float')
        lst = np.array(rhohv)
        lst[lst == -99901.0] = -99903.0
        lst[lst == -99900.0] = -99903.0
        rhohv_mean = map(impute99903mean, l[0:-1], l[1:])

        vel = np.array(row[vel_ind].split(' '), dtype='float')
        vel[vel == -99900.0] = -99901.0
        vel[vel == -99902.0] = -99901.0
        vel[vel == -99903.0] = -99901.0
        lst = np.array(vel)
        lst = transpose(vstack((lst, hstack(indx))))
        vel_lm = map(impute99901lm, l[0:-1], l[1:])

        zdr = np.array(row[zdr_ind].split(' '), dtype='float')
        lst = np.array(zdr)
        lst[lst == -99900.0] = -99903.0
        lst = transpose(vstack((lst, hstack(indx))))
        zdr_lm = map(impute99903lm, l[0:-1], l[1:])

        lwv = np.array(row[lwv_ind].split(' '), dtype='str')
        lwv[lwv == 'nan'] = -99903.0
        lst = np.array(lwv,dtype='float')
        lst = transpose(vstack((lst, hstack(indx))))
        lwv_lm = map(impute99903lm, l[0:-1], l[1:])

        mwm = np.array(row[mwm_ind].split(' '), dtype='str')
        mwm[mwm == 'nan'] = -99903.0
        lst = np.array(mwm,dtype='float')
        lst = transpose(vstack((lst, hstack(indx))))
        mwm_lm = map(impute99903lm, l[0:-1], l[1:])

        mwsd = np.array(row[mwsd_ind].split(' '), dtype='str')
        mwsd[mwsd == 'nan'] = -99903.0
        lst = np.array(mwsd,dtype='float')
        lst = transpose(vstack((lst, hstack(indx))))
        mwsd_lm = map(impute99903lm, l[0:-1], l[1:])

        ##approx_rr1 = sigmoid(avg_rr1 * time_period, 70)

        # Write the clean record
        for j in range(len(l) - 1):
            id_num = row[id_ind]
            out_row = [id_num]
            if parm == 'train':
                out_row.extend([tme_diff[j], dtr_mean[j], comp_lm[j][0], comp_lm[j][1],
                            hbrdscn_lm[j][0], hbrdscn_lm[j][1], hydmtr_mean[j],
                            rr1_lm[j][0], rr1_lm[j][1], rr2_lm[j][0], rr2_lm[j][1],
                            rr3_lm[j][0], rr3_lm[j][1], rqi_mean[j], rfl_mean[j], rfl_qc_mean[j],
                            rhohv_mean[j], vel_lm[j][0], vel_lm[j][1], zdr_lm[j][0],
                            zdr_lm[j][1], lwv_lm[j][0], lwv_lm[j][1], mwm_lm[j][0], mwm_lm[j][1],
                            mwsd_lm[j][0], mwsd_lm[j][1], row[xpt_ind]])
            elif parm == 'test':
                out_row.extend([tme_diff[j], dtr_mean[j], comp_lm[j][0], comp_lm[j][1],
                            hbrdscn_lm[j][0], hbrdscn_lm[j][1], hydmtr_mean[j],
                            rr1_lm[j][0], rr1_lm[j][1], rr2_lm[j][0], rr2_lm[j][1],
                            rr3_lm[j][0], rr3_lm[j][1], rqi_mean[j], rfl_mean[j], rfl_qc_mean[j],
                            rhohv_mean[j], vel_lm[j][0], vel_lm[j][1], zdr_lm[j][0],
                            zdr_lm[j][1], lwv_lm[j][0], lwv_lm[j][1], mwm_lm[j][0], mwm_lm[j][1],
                            mwsd_lm[j][0], mwsd_lm[j][1]])

            writer.writerow(out_row)
            del out_row

        # Every 1000 rows send an update to the user for progress tracking.
        if i % 1000 == 0:
            logger.info("Completed row %d" % i)


if __name__ == "__main__":
    # set up logger
    parser = argparse.ArgumentParser(description=__doc__)
    data_cleaner('train')
    data_cleaner('test')