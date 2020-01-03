#!/usr/bin/env python3

import xgboost as xgb
import pandas as pd
import numpy as np
from xgboost.sklearn import XGBRegressor
from sklearn.model_selection import KFold
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from sklearn.metrics import mean_absolute_error
import joblib

# dtrain = xgb.DMatrix('../../../../data/freefall/xgb-gpa-train.dmatrix')
# dtest = xgb.DMatrix('../../../../data/freefall/xgb-gpa-dtest.dmatrix')
test = pd.read_csv('../data/gpa-w-compass-testing.csv')
train = pd.read_csv('../data/gpa-w-compass-training.csv')

target = 'Y'
x_vars = [x for x in train.columns if x not in [target]]

classf = XGBRegressor(n_estimators = 2000, objective = 'reg:squarederror', verbosity = 3, n_jobs = 3)

param_grid = {
    'eta': [.001, .01, .05, .3],
    'max_depth': [2, 4, 6, 8],
    'min_child_weight': [1, 2, 4],
    'gamma': [.5, 1, 2],
    'subsample': [.5, .8, .9],
    'colsample_bytree': [.5, .8, .9]}
print('\n\n***')
for keys, vals in param_grid.items():
    print(keys, ': ', vals)
print('\nGrid size:', np.prod([len(x) for x in param_grid.values()]))
print('***')

rs_cv = RandomizedSearchCV(estimator = classf, 
                                      param_distributions = param_grid, 
                                      cv = 5,
                                      n_iter = 100,
                                      n_jobs = -1,
                                      pre_dispatch = '2 * n_jobs',
                                      verbose = 4)

rs_cv.fit(X = train[x_vars], y = train[target])

print('Best params:', rs_cv.best_params_)

joblib.dump(rs_cv.best_params_, 'models/best-params-xgb-gpa.pkl')
joblib.dump(rs_cv.best_estimator_, 'models/best-params-xgb-gpa.pkl')
