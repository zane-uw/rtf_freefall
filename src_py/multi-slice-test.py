import xgboost as xgb
import pandas as pd
import numpy as np
from xgboost.sklearn import XGBRegressor
from sklearn.model_selection import KFold
from sklearn.model_selection import RandomizedSearchCV, GridSearchCV
from joblib import dump, load

train = pd.read_csv("../../../data/freefall/pytrain.csv")
test = pd.read_csv("../../../data/freefall/pytest.csv")

train = train.dropna(subset = ['Y'])
train_y = train['Y']
train_id = train['system_key']
dtrain = train.drop(columns = ['Y', 'system_key'])

test = test.dropna(subset = ['Y'])
test_y = test['Y']
test_id = test['system_key']
dtest = test.drop(columns = ['Y', 'system_key', 'Unnamed: 0'])

dtrain = xgb.DMatrix(dtrain, label = train_y, missing = np.NaN)
dtest = xgb.DMatrix(dtest, label = test_y, missing = np.NaN)

# setup grid search
param_grid = {
    'min_child_weight': [1, 3, 5, 10],
    'gamma': [.5, 1, 1.5, 2, 5],
    'subsample': [.4, .6, .8],
    'colsample_bytree': [.4, .6, .8],
    'max_depth': [3, 5, 7, 9]
}
print("There are", np.prod([len(x) for x in param_grid.values()]), "elements to search over")

# Model setup
xgb_mod = XGBRegressor(learning_rate = .2, 
                        n_estimators = 600, 
                        objective = 'reg:squarederror', 
                        silent = True, 
                        nthread = 1)						# <---- Number of threads set here

Y = train['Y'].values
X = train.drop(columns = ['Y', 'system_key'])

# init small values for testing, even small numbers will take a long time here
# number of fits = folds * param_combos
nfolds = 2
param_comb = 1
# nfolds = 7
# param_comb = 21 

kf = KFold(n_splits = nfolds, shuffle = True, random_state = 1001)

random_search = RandomizedSearchCV(xgb_mod, 
                                   param_distributions = param_grid, 
                                   n_iter = param_comb,
                                   scoring = 'neg_mean_absolute_error',
                                   n_jobs = -1,  						# <---- Number of processors set here default (-1) = all
                                   cv = kf.split(X, Y),
                                   verbose = 3,
                                   random_state = 1001)

random_search.fit(X, Y)
print('Best params:', random_search.best_params_)