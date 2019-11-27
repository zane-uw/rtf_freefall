# Comments ----------------------------------------------------------------

# For this file, focus on predicting quarterly GPA <= 2.5 (binary)

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(caret)
library(xgboostExplainer)
library(tidyverse)
library(pROC)
library(doParallel)
library(xgboost)

set.seed(24601)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 200)

# parallel setup
# stopCluster(cl)
# registerDoSEQ()
cl <- makeCluster((detectCores() - 1), type = "PSOCK"); cl
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

setwd(rstudioapi::getActiveProject())

load("src_r/risk_gpa25/data/updated-gpa25-data.RData")

# Initial processing ----------------------------------------------------------

# remove near-zero var vars
dat.scaled <- dat.scaled[,-nearZeroVar(dat.scaled)]
dat.scaled <- dat.scaled %>% filter(!is.na(qgpa))

# encode dummies/factors
# using dat.scaled
# There's one var that needs both a lag and re-encoding
dat.scaled <- dat.scaled %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(scholarship_type = lag(scholarship_type))

cat.vars <- Cs(class, child_of_alum, running_start, s1_gender, conditional, with_distinction,
               low_family_income, appl_class, last_school_type, major.change, ft, premajor, scholarship_type)
f <- paste('~', paste(cat.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat.scaled)
cat.var.mat <- dat.scaled %>%
  select(cat.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)# predict(encoder, dat.scaled)
dim(cat.var.mat)
colnames(cat.var.mat)

mod.vars <- Cs(system_key, yrq, Y, qtr.seq, class, tenth_day_credits, num_courses,
               attmp, nongrd, tot_creds, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act, trans_gpa,
               conditional, with_distinction, low_family_income,
               appl_class, high_sch_gpa, hs_for_lang_type, hs_for_lang_yrs, hs_yrs_for_lang, hs_math_level, hs_yrs_math,
               hs_yrs_arts, hs_yrs_science, hs_yrs_soc_sci, hs_yrs_english, last_school_type, reg.late.days,
               major.change, major.change.count, n.unmet, ft, ft.creds.over, n.major.courses, csum.major.courses,
               premajor, n.alt.grading, avg.class.size, n.writing, n.diversity, n.engl_comp, n.qsr,
               n.vlpa, n.indiv_soc, n.nat_world, sum.fees, stem.courses)

mod.vars <- setdiff(mod.vars, cat.vars)

lag.vars <- Cs(qgpa, cum.gpa, pts, cum.pts, cum.attmp, tot_creds, attmp, qgpa20, n.w, csum.w, avg.stem.grade)

mod.dat <- dat.scaled %>%
  # filter(!is.na(qgpa)) %>%
  # target should be numeric for XGBoost
  mutate(Y = if_else(qgpa <= 2.5, 1, 0)) %>%
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
  select(one_of(mod.vars), one_of(lag.vars))


mod.dat <- bind_cols(mod.dat, data.frame(cat.var.mat))
mod.key.vars <- mod.dat %>% select(system_key, yrq)
mod.dat <- mod.dat %>%
  select(-system_key, -yrq)

rm(f, encoder, cat.vars, mod.vars, lag.vars, cat.var.mat)

# XGB model setup ---------------------------------------------------------

# split
i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]

dtrain <- xgb.DMatrix(as.matrix(training[,-1]), label = training$Y)
dtest <- xgb.DMatrix(as.matrix(testing[,-1]), label = testing$Y)

# wt <- sum(training$Y == 0) / sum(training$Y == 1)
xgb.params<- list(
  objective = "binary:logistic",
  eta = 0.05,
  max_depth = 8,
  subsample = .5,
  num_parallel_tree = 6,
  max_delta_step = 1,
  # scale_pos_weight = wt,
  eval_metric = "aucpr")

# cross-validate xgboost to get the accurate measure of error
xgbcv <- xgb.cv(params = xgb.params,
                data = dtrain,
                nrounds = 500,
                nfold = 5,
                prediction = TRUE,
                showsd = TRUE,
                stratified = TRUE,
                verbose = 2,
                print_every_n = 1,
                early_stopping_rounds = 10)

# best iteration:
hist(xgbcv$pred)
mean(as.numeric(xgbcv$pred > .5) != training$Y)
print(xgbcv, verbose = T)

# plot the AUC for the training and testing samples
plot(x = xgbcv$evaluation_log$iter, y = xgbcv$evaluation_log$train_aucpr_mean, type = 'l', col = 'blue')
lines(x = xgbcv$evaluation_log$iter, y = xgbcv$evaluation_log$test_aucpr_mean, col = 'red')

# fit:
xgb.fit <- xgboost(data = dtrain,
                   params = xgb.params,
                   nrounds = xgbcv$best_iteration,
                   verbose = TRUE,
                   print_every_n = 1,
                   early_stopping_rounds = 10)

# predictions:
xgb.pred <- predict(xgb.fit, dtest)
# error
# mean(as.numeric(xgb.pred > 0.5) != testing$Y)
for(i in seq(.25, .6, by = .05)){
  print(paste0('i: ', i, ' err: ', round(mean(as.numeric(xgb.pred > i) != testing$Y), 2)))
}

# confusion matrix (caret)
p <- ifelse(xgb.pred >= .5, 1, 0)
confusionMatrix(factor(p), factor(testing$Y), positive = '1', mode = 'everything')  # aucpr is an improvement
p <- ifelse(xgb.pred >= .3, 1, 0)
confusionMatrix(factor(p), factor(testing$Y), positive = '1', mode = 'everything')  # aucpr is an improvement
# large increase in false+ there

# importance
imp.mat <- xgb.importance(model = xgb.fit)
# print(imp.mat)
xgb.plot.importance(importance_matrix = imp.mat[1:15])

# Compare cuts instead of .5 cutoff
pqt <- cut(xgb.pred, 5)
table(pqt)
table(pqt, testing$Y)

ggplot(data = data.frame('p' = xgb.pred, 'Y' = testing$Y), aes(x = xgb.pred)) +
  geom_histogram() +
  facet_grid(rows = vars(Y))

# What's going on in the bottom-left?
# x <- testing[xgb.pred >= .797 & testing$Y == 0,]
# skim(x)


# save off xgb objects ----------------------------------------------------

xgb.save(xgb.fit, fname = 'models/xgb-gpa25-fit.model')
xgb.DMatrix.save(dtest, fname = 'data/xgb-gpa25-test.dmatrix')
xgb.DMatrix.save(dtrain, fname = 'data/xgb-gpa25-train.dmatrix')

# caret-ized
# # setup control
# xgb.ctrl <- trainControl(method = "cv",
#                          number = 5,
#                          verboseIter = T,
#                          returnResamp = "all",
#                          classProbs = TRUE,
#                          summaryFunction = twoClassSummary,
#                          allowParallel = T)
#
# nrounds <- 1e3
# grid_init <- expand.grid(nrounds = nrounds, # nrounds = seq(500, nrounds, 500),
#                          eta = c(.001, .01, .025, .05, .1),
#                          max_depth = c(2, 4, 6, 8, 10),
#                          gamma = c(0, 1),
#                          colsample_bytree = 1,
#                          min_child_weight = 1,
#                          subsample = 1)
#
# xgb.train <- train(Y ~.,
#                    data = training,
#                    method = "xgbTree",
#                    trControl= xgb.ctrl,
#                    tuneGrid = grid_init,
#                    verbose = T,
#                    early.stop.round = 20, na.action = na.pass)
#
# # plot of the AUC against max_depth and eta
# ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) +
#   geom_point() +
#   theme_bw() +
#   scale_size_continuous(guide = "none")

