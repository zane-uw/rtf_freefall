# Comments ----------------------------------------------------------------

# For this file, focus on predicting quarterly GPA <= 2.5 (binary)

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(caret)
library(package)
library(xgboostExplainer)
library(tidyverse)
library(pROC)
library(doParallel)
library(xgboost)

set.seed(24601)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 500)

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

mod.vars <- Cs(system_key, yrq, Y, qtr.seq, class, honors_program, tenth_day_credits, scholarship_type, num_ind_study, num_courses,
               attmp, nongrd, tot_creds, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act, trans_gpa,
               aa_degree, direct_transfer, conditional, provisional, with_distinction, res_in_question, low_family_income,
               appl_class, high_sch_gpa, hs_for_lang_type, hs_for_lang_yrs, hs_yrs_for_lang, hs_math_level, hs_yrs_math,
               hs_yrs_arts, hs_yrs_science, hs_yrs_soc_sci, hs_yrs_english, last_school_type, reg.late.days, reg.late.binary,
               major.change, major.change.count, n.unmet, ft, ft.creds.over, n.major.courses, csum.major.courses, n.holds,
               premajor, nonmatric, rep.courses, n.alt.grading, avg.class.size, n.writing, n.diversity, n.engl_comp, n.qsr,
               n.vlpa, n.indiv_soc, n.nat_world, n.gen_elective, sum.fees, stem.courses)

lag.vars <- Cs(qgpa, cum.gpa, pts, cum.pts, cum.attmp, tot_creds, attmp, qgpa15, qgpa20, probe, n.w, csum.w, avg.stem.grade)

mod.dat <- dat.scaled %>%
  filter(!is.na(qgpa)) %>%
  # target should be numeric for XGBoost
  mutate(Y = if_else(qgpa <= 2.5, 1, 0)) %>%
  # mutate(Y = factor(if_else(qgpa <= 2.5, "y", "n"), levels = c('n', 'y'))) %>%
       # scholarship_type = factor(scholarship_type),
       # honors_program = factor(honors_program),
       # yearly_honor_type = factor(yearly_honor_type),
       # trans_gpa = if_else(is.na(trans_gpa), -99, trans_gpa),       # let's try explicitly modeling the missing vals (later try creating dummies)
       # high_sch_gpa = if_else(is.na(high_sch_gpa), -99, high_sch_gpa)) %>%
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


# XGB model setup ---------------------------------------------------------
xgb.dat <- mod.dat %>%
  select(-system_key, -yrq)

# split
i.train <- createDataPartition(y = xgb.dat$Y, p = .75, list = F)
training <- xgb.dat[i.train,]
testing  <- xgb.dat[-i.train,]

dtrain <- xgb.DMatrix(as.matrix(training[,-1]), label = training$Y)
dtest <- xgb.DMatrix(as.matrix(testing[,-1]), label = testing$Y)

xgb.params<- list(
  objective = "binary:logistic",
  eta = 0.01,
  max.depth = 8,
  eval_metric = "auc",
  nthread = 6)

# cross-validate xgboost to get the accurate measure of error
xgbcv <- xgb.cv(params = xgb.params,
                data = dtrain,
                nrounds = 200,
                nfold = 5,
                prediction = TRUE,
                showsd = TRUE,
                stratified = TRUE,
                verbose = 2,
                print_every_n = 5,
                early_stopping_rounds = 10)

# best iteration:
xgbcv$best_iteration    # probably need to control over-fitting better

hist(xgbcv$pred)

# plot the AUC for the training and testing samples
plot(x = xgbcv$evaluation_log$iter, y = xgbcv$evaluation_log$train_auc_mean, type = 'l', col = 'blue')
lines(x = xgbcv$evaluation_log$iter, y = xgbcv$evaluation_log$test_auc_mean, col = 'red')

# fit
xgb.fit <- xgboost(data = dtrain,
                   params = xgb.params,
                   nrounds = 200,
                   verbose = TRUE,
                   print_every_n = 2,
                   early_stopping_rounds = 20)

# predictions:
xgb.pred <- predict(xgb.fit, dtest)

# confusion matrix (caret)
p <- ifelse(xgb.pred >= .5, 1, 0)
confusionMatrix(factor(p), factor(testing$Y), positive = '1', mode = 'everything')

# importance
imp.mat <- xgb.importance(model = xgb.fit)
print(imp.mat)
xgb.plot.importance(importance_matrix = imp.mat[1:15])

# Compare cuts instead of .5 cutoff
pqt <- cut(xgb.pred, 5)
table(pqt)
table(pqt, testing$Y)

# What's going on in the bottom-left?
x <- testing[xgb.pred >= .758 & testing$Y == 0,]
skim(x)

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

