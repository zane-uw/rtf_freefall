# Comments ----------------------------------------------------------------

# Predicting any course <= 2.5

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(xgboost)
library(xgboostExplainer)
library(tidyverse)
library(pROC)
library(doParallel)
library(mice)
library(parsnip)
library(tidymodels)
library(caret)

setwd(rstudioapi::getActiveProject())
set.seed(24601)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 250)

# parallel setup
# stopCluster(cl)
# registerDoSEQ()
cl <- makeCluster((detectCores() / 2), type = "PSOCK"); cl
registerDoParallel(cl)

#' Add quotes to an unquoted list of characters
#'
#' @inheritDotParams comma separated characters
#' @example Cs(add, quotes, to, these)
Cs <- function(...){as.character(sys.call())[-1]}

#' return the numeric index of a single data frame column
#'
#' @param df A data frame
#' @param x A (quoted) column name
#' @example
#' varwhich(mtcars, 'cyl')
varwhich <- function(df, x){ return( which(names(df) == x) ) }

type_names <- function(data, func = is.character){
  x = unlist(lapply(data, func))
  return(names(x[x == T]))
}


load("data/courses-taken.RData")

# calc new Y --------------------------------------------------------------
# the 'y_any_grade' was/is calculated a little differently than I want here

xf.trs.courses$numeric.grade <- recode(xf.trs.courses$grade,
                                       "A"  = "40",
                                       "A-" = "38",
                                       "B+" = "34",
                                       "B"  = "31",
                                       "B-" = "28",
                                       "C+" = "24",
                                       "C"  = "21",
                                       "C-" = "18",
                                       "D+" = "14",
                                       "D"  = "11",
                                       "D-" = "08",
                                       "E"  = "00",
                                       "F"  = "00")
xf.trs.courses$numeric.grade <- as.numeric(xf.trs.courses$numeric.grade) / 10
xf.trs.courses$grade2.5 <- if_else(xf.trs.courses$numeric.grade <= 2.5, 1, 0)
xf.trs.courses$other <- if_else(xf.trs.courses$grade %in% Cs(HW, I, NC, NS, W, W3, W4, W5, W6, W7), 1, 0)
xf.trs.courses$Y <- if_else(xf.trs.courses$grade2.5 == 1 | xf.trs.courses$other == 1, 1, 0)

new.y <- xf.trs.courses %>%
  group_by(system_key, yrq) %>%
  summarize(Y = sum(Y, na.rm = T)) %>%       # any doesn't exaclty play well with group_by, extra step to recode T/F or sum as 0/1
  mutate(Y = if_else(Y >= 1, 1, 0)) %>%
  ungroup()
rm(courses.taken, xf.trs.courses)

load("src_r/risk_gpa25/data/updated-gpa25-data.RData")
# CORRECTION - don't use the scaled data, apply that later
# rm(dat, y_any_grade)


# dept-wise, keep most populated
i <- apply(stu.deptwise.wide, 2, function(x) sum(is.na(x)) / nrow(stu.deptwise.wide)) <= .9
stu.deptwise.wide <- stu.deptwise.wide[,i]

i <- apply(stu.deptwise.wide, 2, function(x) sum(x > 0) / nrow(stu.deptwise.wide)) >= .1
i[is.na(i)] <- T
stu.deptwise.wide <- stu.deptwise.wide[,i]

dat <- dat %>% inner_join( select(dat.scaled, c(system_key, yrq, qtr.seq)) ) %>%
  select(-starts_with("Ethnic"),
         -HispanicInd,
         -yearly_honor_type,
         -ResidentDesc,
         -tran_yr,
         -tran_qtr) %>%
  inner_join(new.y) %>%
  inner_join(stu.deptwise.wide)

rm(dat.scaled, y_any_grade, stu.deptwise.wide, i, new.y)

# remove near-zero var vars
# dat.scaled <- dat.scaled[,-nearZeroVar(dat.scaled)]
# dat.scaled <- dat.scaled %>% filter(!is.na(qgpa))

# encode dummies/factors
# using dat.scaled
dat <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(scholarship_type = lag(scholarship_type),
         ext_premajor = if_else(tran_major_abbr == 'EPRMJ', 1, 0)) %>%
  ungroup()

# let's keep only the most popular majors and call the rest 'other'
(i <- tapply(dat$system_key, dat$tran_major_abbr, function(x) length(x)))
i <- i[-grep('PRE|EPRMJ|TPRMAJ', names(i))]
round((i / sum(i)) * 100, 2)
i <- i[i >= 250]

dat <- dat %>%
  mutate(major_abbr = if_else(tran_major_abbr %in% names(i), tran_major_abbr, 'other_major')) %>%
  select(-tran_major_abbr)


# tricky part -------------------------------------------------------------

# Encoding depends on the model here. For trees we'd like to be able to split along every possible value of a categorical variable
# This generates a complete set (no reference category)
# this ought to be equivalent to `model.matrix(~x-1)`

# some of these we will remove ahead of time b/c at some point I did this:
# x <- subset(dat, select = cat.vars)
# apply(x, 2, table)

dat <- subset(dat, select = -c(InternationalStudentInd, aa_degree, direct_transfer))

# ordinal variables -> OHE
ord.vars <- Cs(class,
               appl_class,
               hs_math_level)

cat.vars <- Cs(child_of_alum,
               # class,
               running_start,
               s1_gender,
               conditional,
               with_distinction,
               low_family_income,
               # appl_class,
               last_school_type,
               major.change,
               ft,
               premajor,
               scholarship_type,
               ext_premajor,
               major_abbr,
               probe,
               qgpa15,
               qgpa20,
               provisional,
               hs_for_lang_type,
               # hs_math_level,
               reg.late.binary,
               tran_branch,
               nonmatric,
               ext_premajor,
               honors_program,
               res_in_question)
cat.vars <- unique(c(cat.vars, type_names(dat, func = is.character), type_names(dat, func = is.logical)))

# <<-- DECISION TIME -->>
# Full rank or not full rank?
f <- paste('~', paste(cat.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat, fullRank = F)
cat.var.mat <- dat %>%
  select(cat.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)# predict(encoder, dat.scaled)
dim(cat.var.mat)
colnames(cat.var.mat)

# <<-- DECISION TIME -->>
# Full rank or not full rank?
f <- paste('~', paste(ord.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat, fullRank = F)
ord.var.mat <- dat %>%
  select(ord.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)
dim(ord.var.mat)
colnames(ord.var.mat)


# The alternative I'll create for now is to recode them w/ reference cats, ie as factors. This won't really be ideal for majors so let's do those separately
dat <- dat %>% mutate_at(cat.vars, as.factor) %>% mutate_at(ord.vars, as.factor)
mm <- data.frame(model.matrix(~dat$major_abbr - 1))
names(mm) <- str_sub(names(mm), start = 5)

dat <- subset(dat, select = -major_abbr)
dat <- bind_cols(dat, mm)


# mod.vars <- Cs(system_key, yrq, Y, qtr.seq, class, tenth_day_credits, num_courses,
#                attmp, nongrd, tot_creds, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act, trans_gpa,
#                conditional, with_distinction, low_family_income,
#                appl_class, high_sch_gpa, hs_for_lang_type, hs_for_lang_yrs, hs_yrs_for_lang, hs_math_level, hs_yrs_math,
#                hs_yrs_arts, hs_yrs_science, hs_yrs_soc_sci, hs_yrs_english, last_school_type, reg.late.days,
#                major.change, major.change.count, n.unmet, ft, ft.creds.over, n.major.courses, csum.major.courses,
#                premajor, n.alt.grading, avg.class.size, n.writing, n.diversity, n.engl_comp, n.qsr,
#                n.vlpa, n.indiv_soc, n.nat_world, sum.fees, stem.courses, stem.credits, avg.stem.grade, csum.stem.courses, )

# mod.vars <- setdiff(names(dat), cat.vars)

lag.vars <- Cs(qgpa, cum.gpa, pts, cum.pts, cum.attmp, tot_creds, attmp, qgpa20, n.w, csum.w, avg.stem.grade)


# Decision point for model building ---------------------------------------------

# do you want to drop the factor variables and replace them with `cat.var.mat`?
mod.dat <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  # gen lags
  mutate_at(lag.vars,
            lag, n = 1) %>%
  mutate(d1 = qgpa - lag(qgpa),
         d1 = if_else(is.na(d1), 0, d1),
         rundiff = d1 + lag(d1)) %>%
  select(-d1) %>%
  ungroup() %>%
# <<-- DECISION TIME -->>
  select(Y, everything())
# mod.dat <- bind_cols(mod.dat, data.frame(cat.var.mat))


# Also, combine the IC variables
ic <- mod.dat %>% select(starts_with('IC', ignore.case = F))
mod.dat$ic_tot <- rowSums(ic)
rm(ic)
mod.dat <- mod.dat %>% select(-starts_with('IC', ignore.case = F))

mod.dat <- mod.dat %>%
  mutate_at(vars(starts_with('major_abbr')), as.factor)


# push? -------------------------------------------------------------------
  # write_csv(mod.dat, path = 'src_r/risk_gpa25/data/data_any-adverse-qtrly-outcome-no-preproc.csv')
  # sesh <- ssh::ssh_connect(config::get('ssh', 'config.yml'))
  # ssh::scp_upload(sesh, files = 'src_r/risk_gpa25/data/data_any-adverse-qtrly-outcome-no-preproc.csv',
  #                 to = 'data/freefall/', verbose = T)
  # ssh::ssh_disconnect(sesh)

# finish tidy-up for R-stuff -----------------------------------------------------

key.vars <- c('system_key', 'yrq')
mod.keys <- mod.dat %>% select(key.vars)
mod.dat <- mod.dat %>% select(-key.vars)
mod.dat$Y <- factor(mod.dat$Y, levels = c(0, 1), labels = c('N', 'Y'))

# one more bit of cleanup
names(mod.dat) <- str_replace_all(names(mod.dat), " ", "")

rm(f, encoder, mod.vars, stu.deptwise.wide, new.y, dat.scaled, i, mm) # cat.var.mat, ord.var.mat, lag.vars, ord.vars, cat.vars


# nzv <- nearZeroVar(mod.dat, saveMetrics = T, names = T, foreach = T)

# train/test split --------------------------------------------------------

# ix <- createDataPartition(mod.dat$Y, p = .8, list = F)

# In keeping with the goal, we'll predict the most current term
ix <- cumsum(tapply(mod.keys$yrq, mod.keys$system_key, function(x) length(x)))

training <- mod.dat[-ix,]
testing <- mod.dat[ix,]
table(training$Y); table(testing$Y)


# recipe ------------------------------------------------------------------
#
# init.rec <- recipe(Y ~., data = training) %>%
#   step_knnimpute(all_numeric(), options = list(nthread = length(cl))) %>%
#   step_center(all_numeric()) %>%
#   step_scale(all_numeric()) %>%
#   prep(training = training, retain = T, verbose = T)
#
# # processed versions:
# train.data <- juice(init.rec)
# test_data  <- bake(init.rec, testing)

# This isn't really working - recipe spec stalls out on stage 1 regardless of imputation strat
# the old preProcess command appears to work but the predict fun refuses to run for frustratingly opaque reasons

# This is _all_ kinds of annoying since Python will do the imputation w/o complaint


# Pre process setup -------------------------------------------------------
prep <- preProcess(training[,-1],
                   # na.remove = F,
                   method = c('nzv', 'scale', 'center'),
                   outcome = training$Y,
                   verbose = T)           # This runs in a minute or so where `step_x-impute()` just runs forever

training.pp <- predict(prep, newdata = training[,-1])
testing.pp <- predict(prep, testing[,-1])


# impute ------------------------------------------------------------------

training.imp <- mice(training.pp, method = 'cart', m = 1, maxit = 1)
testing.imp <- mice(testing.pp, method = 'cart', m = 1, maxit = 1)

x_train <- complete(training.imp)
x_test <- complete(testing.imp)
y_train <- training$Y
y_test <- testing$Y

# trainControl ------------------------------------------------------------

tcon <- trainControl(verboseIter = T,
                     allowParallel = T,
                     method = 'none')

# Model(s) -------------------------------------------------------------------
# registerDoParallel(cl)


# --> Mod 1: baseline glm -----------------------------------------------------

# fit.baseline <- train(y_train ~.,
#                       x_train,
#                       method = 'glm',
#                       trControl = tcon)

# you're _really_ trying my patience this week caret/parsnip
f <- glm(y_train~., data = x_train, family = binomial(link = 'logit'))
f.pred.class <- if_else(predict(f, newdata = x_test, type = 'response') >= .5, 1, 0)
confusionMatrix(factor(f.pred.class, levels = c(0, 1), labels = c('N', 'Y')), reference = y_test, positive = 'Y', mode = 'everything')


# --> Mod 2: regularization -----------------------------------------------

###
###
finicky.caret.glmnet.data <- bind_cols(x_train, 'Y' = y_train)
###
###

glmnet.grid <- expand.grid(lambda = seq(.01, .5, length.out = 10),
                           alpha = seq(.01, .99, length.out = 30))

tcon <- trainControl(verboseIter = T,
                     allowParallel = T,
                     method = 'cv',
                     number = 5,
                     search = 'random',
                     classProbs = T,
                     summaryFunction = twoClassSummary)

cv.glmnet <- train(Y ~.,
                    finicky.caret.glmnet.data,
                    method = 'glmnet',
                    metric = 'ROC',
                    tuneGrid = glmnet.grid,
                    tuneLength = 30,
                    trControl = tcon)       # sooooooooooooooo why is it not permuting?
                                            # Does 'random' not work? It's keeping lambda fixed and iterating over alpha in sequence
                                            # or the feedback is just wrong...

# haha, omg -_- cataloging the various failures after lengthy run times here:

# Aggregating results
# Selecting tuning parameters
# Fitting alpha = 0.01, lambda = 0.3 on full training set
# Error in h2o.getConnection() :
#   No active connection to an H2O cluster. Did you run `h2o.init()` ?
#   In addition: Warning message:
#   In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,

# Aggregating results
# Selecting tuning parameters
# Fitting alpha = 0.01, lambda = 0.5 on full training set
# Error in (function (x, y, family = c("gaussian", "binomial", "poisson",  :
#                                        unused argument (verbose = TRUE)
#     In addition: Warning message:
#     In nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,  :
#
#     Error in (function (x, y, family = c("gaussian", "binomial", "poisson",  :
#     unused argument (verbose = TRUE)
#     Timing stopped at: 0.395 0.043 0.456

# Aggregating results
# Selecting tuning parameters
# Fitting alpha = 0.01, lambda = 0.01 on full training set
# Error in `[.data.frame`(data, , all.vars(Terms), drop = FALSE) :
#   undefined columns selected

# ^^^^ odd that we should return such a radically different value for lambda that time

# fit.glmnet <- glmnet::glmnet(x = as.matrix(x_train), y = y_train, family = 'binomial', alpha = .01, lambda = .3, standardize = F, type.logistic = 'modified.Newton')
confusionMatrix(predict(cv.glmnet, newdata = x_test), reference = y_test, positive = "Y", mode = 'prec_recall')

# Notes
# That's not really much improvement over the baseline fit or my xgb modeling earlier. Perhaps an additional penalty/weight to the incorrect cases?


# XGB w/ new data ---------------------------------------------------------

# less than full rank for trees

tree.dat <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  # gen lags
  mutate_at(lag.vars,
            lag, n = 1) %>%
  mutate(d1 = qgpa - lag(qgpa),
         d1 = if_else(is.na(d1), 0, d1),
         rundiff = d1 + lag(d1)) %>%
  select(-d1) %>%
  ungroup() %>%
  # <<-- DECISION TIME -->>
  select(Y, everything()) %>%
  select(-one_of(c(cat.vars, ord.vars, key.vars))) %>%
  bind_cols(., data.frame(cat.var.mat), data.frame(ord.var.mat))

# dm.train <- bind_cols(data.frame(cat.var.mat, ord.var.mat))[-ix,]
# dm.test <- bind_cols(data.frame(cat.var.mat, ord.var.mat))[ix,]

training <- tree.dat[-ix,]
testing <- tree.dat[ix,]
table(training$Y); table(testing$Y)

  # # Pre-process again
  # prep <- preProcess(training[,-1],
  #                    # na.remove = F,
  #                    method = c('nzv', 'scale', 'center'),
  #                    outcome = training$Y,
  #                    verbose = T)
  #
  # training.pp <- predict(prep, newdata = training[,-1])
  # testing.pp <- predict(prep, testing[,-1])
  #
  # training.pp <- bind_cols(training.pp, dm.train)
  # testing.pp <- bind_cols(testing.pp, dm.test)

dtrain <- xgb.DMatrix(as.matrix(training[,-1]), label = training$Y)
dtest <- xgb.DMatrix(as.matrix(testing[,-1]), label = testing$Y)



# xgb tuning --------------------------------------------------------------

# set small eta, large nrounds
# tune row, column sampling
# then tune eta

hpgrid <- expand.grid(subsample = c(.5, .75, 1),             # row sampling
                      colsample_bytree = c(.4, .6, .8, 1))   # column sampling

# # for quick testing
# hpgrid <- expand.grid(subsample = .5,
#                       colsample_bytree = .4)

# other params, currently fixed
xgb.params <- list(
  objective = 'binary:logistic',
  nthread = length(cl),
  # eta = .3,   # seq(.01, .4, length.out = 20), will tune this
  gamma = 0,
  round = 1,
  # num_parallel_tree = 6,
  eval_metric = 'aucpr',
  metrics = list('aucpr', 'error'))

ntrees <- 50

# function to do CV, return scores
aucpr.hp <- apply(hpgrid, 1, function(param.list){
  # extract params to test
  iSubsampleRate <- param.list[["subsample"]]
  iColsampleRate <- param.list[["colsample_bytree"]]

  mod <- xgb.cv(data =  dtrain,
                params = xgb.params,
                nrounds = ntrees,
                nfold = 5,
                # showsd = TRUE,
                verbose = T,
                eta = 2 / ntrees,
                'subsample' = iSubsampleRate,
                'colsample_bytree' = iColsampleRate,
                print_every_n = 3,
                early_stopping_rounds = 10)

  # keep aucpr of the last iteration
  aucpr <- tail(mod$evaluation_log$test_aucpr_mean, 1)
  return(c(aucpr, iSubsampleRate, iColsampleRate))
  })

# transpose result to get a data frame
aucpr.hp
aucpr.hp[,which.max(aucpr.hp)]

xgb.params$subsample <- aucpr.hp[,which.max(aucpr.hp)][2]
xgb.params$colsample_bytree <- aucpr.hp[,which.max(aucpr.hp)][3]

# now tune eta
(eta.grid <- data.frame(eta = seq(.001, .21, length.out = 20)))

# [TODO] generalize this function
#        add statement(s) to update progress
aucpr.eta.tune <- apply(eta.grid, 1, function(param.list){
  # extract params to test
  i.eta <- param.list[["eta"]]
  # i.ntrees <- param.list[["ntrees"]]

  mod <- xgb.cv(data =  dtrain,
                params = xgb.params,
                nrounds = ntrees,
                nfold = 5,
                # showsd = TRUE,
                verbose = T,
                'eta' = i.eta,
                print_every_n = 4,
                early_stopping_rounds = 10)

  # keep aucpr of the last iteration
  aucpr <- tail(mod$evaluation_log$test_aucpr_mean, 1)
  return(c(aucpr, i.eta))
})

aucpr.eta.tune
(xgb.params$eta <- aucpr.eta.tune[2,][which.max(aucpr.eta.tune[1,])])


# tune max depth ----------------------------------------------------------

ntrees = 50
hpgrid <- expand.grid(max_depth = seq(2, 10))

aucpr.max_depth.tune <- apply(hpgrid, 1, function(param.list){
  # extract params to test
  i.max_depth <- param.list[["max_depth"]]

  mod <- xgb.cv(data =  dtrain,
                params = xgb.params,
                nrounds = ntrees,
                nfold = 5,
                # showsd = TRUE,
                verbose = T,
                'max_depth' = i.max_depth,
                print_every_n = 4,
                early_stopping_rounds = 10)

  # keep aucpr of the last iteration
  aucpr <- tail(mod$evaluation_log$test_aucpr_mean, 1)
  return(c(aucpr, i.max_depth))
})

aucpr.max_depth.tune



# xgbcv w/ best params ----------------------------------------------------

xgbcv <- xgb.cv(params = xgb.params,
                data = dtrain,
                tree_method = 'hist',
                nrounds = 100,
                nfold = 5,
                prediction = T,
                # save_models = T,
                showsd = T,
                stratified = F,
                verbose = 2,
                print_every_n = 3,
                early_stopping_rounds = 10,
                metrics = list('aucpr', 'error'),
                watchlist = list(train = dtrain, test = dtest))

print(xgbcv, verbose = T)
plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_aucpr_mean, col = 'blue', type = 'l'); title(main = 'aucpr')
lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_aucpr_mean, col = 'red')
plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_error_mean, col = 'blue', type = 'l'); title(main = 'error')
lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_error_mean, col = 'red')
hist(xgbcv$pred)
table(xgbcv$pred >= .5, training$Y)






  # Old
  #
  # xgb.params <- list(
  #   objective = 'binary:logistic',
  #   nthread = length(cl),
  #   eta = .3,   # seq(.01, .4, length.out = 20), will tune this next
  #   # max_depth = 6,
  #   # min_child_weight = 1,
  #   gamma = 0,
  #   subsample = .8,
  #   # colsample_bylevel = .8,
  #   colsample_bytree = .8,
  #   round = 1,
  #   num_parallel_tree = 6,
  #   metrics = list('aucpr', 'error'))
  #   # watchlist = list(train = dtrain, test = dtest))
  #
  # # cross-validation
  # xgbcv <- xgb.cv(params = xgb.params,
  #                 data = dtrain,
  #                 tree_method = 'hist',
  #                 nrounds = 500,
  #                 nfold = 5,
  #                 prediction = T,
  #                 # save_models = T,
  #                 showsd = T,
  #                 stratified = F,
  #                 verbose = 2,
  #                 print_every_n = 3,
  #                 early_stopping_rounds = 10)

  # print(xgbcv, verbose = T)
  # plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_aucpr_mean, col = 'blue', type = 'l'); title(main = 'aucpr')
  # lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_aucpr_mean, col = 'red')
  # plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_error_mean, col = 'blue', type = 'l'); title(main = 'error')
  # lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_error_mean, col = 'red')
  # hist(xgbcv$pred)
  # table(xgbcv$pred >= .5, training$Y)