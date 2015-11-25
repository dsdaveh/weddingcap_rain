import numpy as np
import pandas as pd
import xgboost as xgb
import graphviz

#import train and summary stats - only using samples before final run to make processing more manageable
train = pd.read_csv("/Users/jamesramadan/Documents/Kaggle/Rain2/train.csv")
train10000= train[0:10000]
train = train10000
train.describe()
print("train dataset: raw")
print(train[0:10])
train.mean()
train.count()

#import test and summary stats
test = pd.read_csv("/Users/jamesramadan/Documents/Kaggle/Rain2/test.csv")
test5000= test[0:5000]
test = test5000
test.describe()
print("test dataset: raw")
print(test[0:10])
test.count()

#Drop NA Refs - Train
train_noRefNA = train.dropna(subset = ['Ref', 'Expected'] )
print("train dataset: dropRefNAs")
print(train_noRefNA[0:10])

#Drop NA Refs - Test
test_noRefNA = test.dropna(subset = ['Ref'] )
print("test dataset: dropRefNAs")
print(test_noRefNA[0:10])
test_noRefNA.count()

#Fill remaining NAs with mean - Train
train_imputed = train.fillna(train.mean())
print("train dataset: MeanImputed")
print(train_imputed[0:10])
train_imputed.count()
#train_imputed.to_csv("/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james/train_meanImputed.csv")

#Fill remaining NAs with mean - Test
test_imputed = test.fillna(test.mean())
print("test dataset: MeanImputed")
print(test_imputed[0:10])
test_imputed.dtypes
#train_imputed.to_csv("/Users/jamesramadan/Documents/Kaggle/Rain2/weddingcap_rain:/james/train_meanImputed.csv")


#Aggregate functions - Train
train_grouped = train_imputed.groupby('Id', as_index = 'False' )
train_agg = train_grouped.agg({'Id': np.mean, 'radardist_km': np.mean, 'Ref': np.mean, 'Ref_5x5_10th': np.mean, 'Ref_5x5_50th': np.mean,
'Ref_5x5_90th': np.mean, 'RefComposite' : np.mean, 'RefComposite_5x5_10th': np.mean, 'RefComposite_5x5_50th': np.mean, 'RefComposite_5x5_90th': np.mean,
'RhoHV': np.mean,'RhoHV_5x5_10th': np.mean,'RhoHV_5x5_50th': np.mean, 'RhoHV_5x5_90th': np.mean, 'Zdr': np.mean,'Zdr_5x5_10th': np.mean,
'Zdr_5x5_50th': np.mean, 'Zdr_5x5_90th': np.mean, 'Kdp': np.mean,'Kdp_5x5_10th': np.mean,'Kdp_5x5_50th': np.mean, 'Kdp_5x5_90th': np.mean, 'Expected': np.mean})
train_agg
print("train dataset: Aggregated")
print(train_agg[0:1])
train_agg.count()


#Aggregate functions - Test
test_grouped = test_imputed.groupby('Id', as_index = 'False' )
test_agg = test_grouped.agg({'Id': np.mean, 'radardist_km': np.mean, 'Ref': np.mean, 'Ref_5x5_10th': np.mean, 'Ref_5x5_50th': np.mean,
'Ref_5x5_90th': np.mean, 'RefComposite' : np.mean, 'RefComposite_5x5_10th': np.mean, 'RefComposite_5x5_50th': np.mean, 'RefComposite_5x5_90th': np.mean,
'RhoHV': np.mean,'RhoHV_5x5_10th': np.mean,'RhoHV_5x5_50th': np.mean, 'RhoHV_5x5_90th': np.mean, 'Zdr': np.mean,'Zdr_5x5_10th': np.mean,
'Zdr_5x5_50th': np.mean, 'Zdr_5x5_90th': np.mean, 'Kdp': np.mean,'Kdp_5x5_10th': np.mean,'Kdp_5x5_50th': np.mean, 'Kdp_5x5_90th': np.mean})
print("test dataset: Aggregated")
print(test_agg[0:1])
test_agg.dtypes

#Create Validation Set
train_agg_train = train_agg[0:700]
print(train_agg_train)
train_agg_valid = train_agg[700:948]
print(train_agg_valid)

#dMatrix creation
train_label = train_agg_train['Expected']
train_data = train_agg_train
train_data = train_data.drop('Expected', axis =1)
print(train_data)

valid_label = train_agg_valid['Expected']
valid_data = train_agg_valid
valid_data = valid_data.drop('Expected', axis =1)

dtrain = xgb.DMatrix(train_data, train_label)
print(dtrain)
dvalid = xgb.DMatrix(valid_data, valid_label)
dtest = xgb.DMatrix(test_agg)

# Making model - validation
param = {'bst:max_depth':2, 'bst:eta': 0.00001, 'gamma': 1, 'objective': 'reg:linear', 'eval_metric': 'rmse'}
num_round = 100
bst = xgb.train(param, dtrain, num_round)
#bst.save_model('0001.model')
#bst.dump_model('/Users/jamesramadan/Documents/Kaggle/Rain2/dump.raw.txt')
preds = bst.predict(dvalid)
labels = dvalid.get_label()

#MAE
sum(abs(preds - labels)) / len(preds)

xgb.plot_importance(bst)
xgb.plot_tree(bst, num_trees =2)


# Making model - test

param = {'bst:max_depth':20, 'bst:eta': 0.00001, 'gamma': 10, 'objective': 'reg:linear', 'eval_metric': 'rmse'}
num_round = 10
watchlist = [(dvalid,'eval'), (dtrain,'train')]
bst = xgb.train(param, dtrain, num_round, watchlist)
bst.save_model('0001.model')
bst.dump_model('/Users/jamesramadan/Documents/Kaggle/Rain2/dump.raw.txt')

preds = bst.predict(dvalid)
preds = bst.predict(dtest)


xgb.plot_importance(bst)
xgb.plot_tree(bst, num_trees=2)