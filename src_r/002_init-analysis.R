# Comments ----------------------------------------------------------------

# For this file, let's focus on predicting quarterly GPA <= 2.5 (binary)

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(caret)
library(package)
library(xgboostExplainer)
library(tidyverse)
library(pROC)
library(doParallel)

set.seed(4567)
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

#' convert SQL Y/N to numeric 0/1
#' Should result in NA for non-Y/N values
#'
#' @param x a vector of Y/N values
#' @example
#' binarize.yn(c('Y', 'Y', 'N', 'Y', 'Q'))
#' [TODO] - make general function with in/out text opts
binarize.yn <- function(x){
  y <- rep(NA, length.out = length(x)) # vector(mode = 'numeric', length = length(x))
  # I usually prefer the first syntax for this task iot simplify NA's; vector() will create 0's if mode = 'numeric'
  # although one might get away with unlist(...(mode = 'list')) :P
  y[x == "Y"] <- 1
  y[x == "N"] <- 0
  return(y)
}
binarize.logical <- function(x){
  y <- rep(NA, length.out = length(x))
  y[x == T] <- 1
  y[x == F] <- 0
  return(y)
}

# let's generalize those
binarize.vals <- function(x, val0, val1){
  y <- rep(NA, length.out = length(x))
  y[x == val0] <- 0
  y[x == val1] <- 1
  return(y)
}
# test <- c(T, F, T, F, F, F)
# binarize.vals(test, T, F)
# test <- c("N", "Y", "Y", "N")
# binarize.vals(test, "Y", "N")
# rm(test)

setwd(rstudioapi::getActiveProject())

load("data/merged-dataset.RData")

# Initial cleanup/variable selection/reduction ----------------------------------------------------------
#
# dat <- mrg.dat %>%
#   filter(!is.na(qgpa)) %>%
#   group_by(system_key) %>%
#   arrange(system_key, yrq) %>%
#   mutate(l1.qgpa = lag(qgpa, n = 1),
#          l2.qgpa = lag(qgpa, n = 2),
#          l1.cum.gpa = lag(cum.gpa, n = 1),
#          l1.probe = lag(probe),
#          l1.qgpa15 = lag(qgpa15),
#          l1.qgpa20 = lag(qgpa20),
#          l1.cum.pts = lag(cum.pts),
#          l1.cum.attmp = lag(cum.attmp)) %>%
#    ungroup() %>%

# [TODO] Rather than use lag funs, I'll create some quarterly features

# pre-processing ----------------------------------------------------------

dat <- mrg.dat %>%
  filter(!is.na(qgpa)) %>%
  rename(class.standing = class) %>%
  # group_by(system_key) %>%
  # arrange(system_key, yrq) %>%
  # mutate(l1.qgpa = lag(qgpa, n = 1),
  #        l2.qgpa = lag(qgpa, n = 2),
  #        l1.cum.gpa = lag(cum.gpa, n = 1),
  #        l1.probe = lag(probe),
  #        l1.qgpa15 = lag(qgpa15),
  #        l1.qgpa20 = lag(qgpa20),
  #        l1.cum.pts = lag(cum.pts),
  #        l1.cum.attmp = lag(cum.attmp)) %>%
  # ungroup() %>%
  mutate(Y = factor(if_else(qgpa <= 2.5, "y", "n"), levels = c('n', 'y')),
         # Y = factor(if_else(qgpa <= 2.5, 1, 0), levels = c(0, 1)),
         # qgpa15 = factor(qgpa15, levels = c(0, 1)),
         probe = factor(probe, levels = c(0,1)),
         scholarship_type = factor(scholarship_type),
         honors_program = factor(honors_program),
         yearly_honor_type = factor(yearly_honor_type),
         trans_gpa = if_else(is.na(trans_gpa), -99, trans_gpa),       # let's try explicitly modeling the missing vals (later try creating dummies)
         high_sch_gpa = if_else(is.na(high_sch_gpa), -99, high_sch_gpa)) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  # add a quarter increment, lagged vars
  mutate(qtr = row_number(),
         l1.qgpa = lag(qgpa, n = 1),
         l1.cgpa = lag(cum.gpa, n = 1),
         l1.cum.pts = lag(cum.pts, 1),
         l1.cum.attmp = lag(cum.attmp, 1),
         l1.tot_creds = lag(tot_creds, 1),
         # running gpa change
         d1 = qgpa - lag(qgpa),
         d1 = if_else(is.na(d1), 0, d1),
         rundiff = d1 + lag(d1)) %>%
  select(-d1) %>%
  ungroup()

# select test score
sat <- scale(dat$s1_high_satm + dat$s1_high_satv)
act <- scale(dat$s1_high_act)
dat$std_test_high <- pmax(act[,1], sat[,1], na.rm = T)
rm(sat, act)


# mod.dat <- dat %>%
#   select(Y, class.standing, tenth_day_credits, num_ind_study, num_courses, attmp, nongrd, deduct, tot_creds, s1_gender, running_start,
#          std_test_high, starts_with("Ethnic"), HispanicInd, InternationalStudentInd, age, conditional, provisional, res_in_question,
#          low_family_income, appl_class, high_sch_gpa, starts_with("hs"), ft, ft.creds.over, n.unmet, reg.late.days, reg.late.binary)

mod.dat <- dat %>%
  select(system_key, Y, class.standing, tenth_day_credits, num_ind_study, num_courses, attmp, nongrd, deduct, tot_creds, honors_program,
         std_test_high, conditional, provisional, res_in_question, low_family_income, appl_class, high_sch_gpa, starts_with("hs"),
         ft, ft.creds.over, n.unmet, reg.late.days, reg.late.binary, qtr, starts_with('l1.'), rundiff) %>%
  mutate_if(is.logical, binarize.logical) %>%
  # local xform
  group_by(system_key) %>%
  mutate(reg.late.days = scale(reg.late.days),
         tenth_day_credits = scale(tenth_day_credits),
         attmp = scale(attmp)) %>%
  ungroup() %>%
  select(-system_key)

# I would also like to keep the missing entries - mainly in quarter #1 where we have a cold start problem
# of course, we won't know any 'truth' to impute/fill with for someone with no data. Technically that's something
# we could also model -> impute later; for now let's not.
mod.dat <- mod.dat %>%
  mutate_at(vars(l1.qgpa, l1.cgpa, l1.cum.pts, l1.cum.attmp, l1.tot_creds), replace_na, replace = -99) %>%
  mutate(rundiff = replace_na(rundiff, -99))


# Validation checks -------------------------------------------------------

(near0 <- nearZeroVar(mod.dat, saveMetrics = T, allowParallel = T))
mod.dat <- mod.dat[,near0$zeroVar == F]

nrow(mod.dat[complete.cases(mod.dat),]) / nrow(mod.dat)       # The change to HS and trans GPA coding is a large improvement here

  # missing.threshold <- 0.3
  # (i <- which(apply(mod.dat, 2, function(x) sum(is.na(x)) / nrow(mod.dat)) > missing.threshold))
  # # for(mt in seq(0, .3, by = .05)){
  # #   print(mt)
  # #   print(which(apply(mod.dat, 2, function(x) sum(is.na(x)) / nrow(mod.dat) > mt)))
  # # }
  #
  # mod.dat <- subset(mod.dat, select = -i)
  # nrow(mod.dat[complete.cases(mod.dat),]) / nrow(mod.dat)

# FOR TIME BEING:
mod.dat <- mod.dat[complete.cases(mod.dat),]

# Split train/test -------------------------------------------------------------

i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
# i <- varwhich(dat, "Y_i")
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]

# Control opts -------------------------------------------------------------------

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 10,
#                      repeats = 3)
# ctrl <- trainControl(method = "cv", number = 5, sampling = "smote")
# ctrl <- trainControl(method = "cv", number = 5, sampling = "down")

# Fit via GLMNET; elastic lasso ---------------------------------------------------------------
# cv to tune hyperparams
# trying ROC for training metric rather than Acc.
ctrl <- trainControl(method = "cv",
                     number = 5,
                     # sampling = 'down',
                     classProbs = T,
                     summaryFunction = twoClassSummary,
                     allowParallel = T,
                     verboseIter = T)

cvfit <- train(Y ~.,
             data = training,
             method = "glmnet",
             trControl = ctrl,
             metric = "ROC",
             # glmnet options
             family = "binomial",
             tuneGrid = expand.grid(alpha = seq(.1, .9, by = .1), lambda = seq(0.01, 0.3, by = 0.01)))
getTrainPerf(cvfit)
confusionMatrix(cvfit)
cvfit.pr <- predict(cvfit, newdata = testing)
confusionMatrix(cvfit.pr, reference = testing$Y, positive = 'y')
# fit$results
cvfit$bestTune
plot(cvfit)

fitcontrol <- trainControl(method = "none", classProbs = T)
# rerun w/ bestTune
glm.mod <- train(Y~.,
                 data = training,
                 method = "glmnet",
                 family = "binomial",
                 # alpha = ,
                 # lambda = ,
                 trControl = fitcontrol,
                 metric = "ROC",
                 tuneGrid = expand.grid(alpha = cvfit$bestTune$alpha,
                                        lambda = cvfit$bestTune$lambda))

# plot(glm.mod$finalModel, xvar = "dev", label = T)
# plot(glm.mod$finalModel, label = T)
glm.mod.pr <- predict(glm.mod, newdata = testing)
confusionMatrix(glm.mod.pr, reference = testing$Y, positive = 'y')

# varimp:
varImp(glm.mod, scale = FALSE)

glm.mod.probs <- predict(glm.mod, newdata = testing, type = 'prob')
(cut <- quantile(glm.mod.probs$y, probs = .75))
sum(glm.mod.probs$y >= cut)

p <- ifelse(glm.mod.probs$y >= cut, 1, 0)
table('.75 cut' = p, 'truth' = testing$Y)


# alt outcome: GPA drop ---------------------------------------------------

gpa.drop <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(delta = qgpa - lag(qgpa, n = 1),
         Y = factor(ifelse(abs(delta < 0), 'y', 'n'), levels = c('n', 'y'))) %>%
  ungroup() %>%
  select(Y, everything()) %>%
  mutate_if(is.logical, binarize.logical) %>%
  # local xform
  group_by(system_key) %>%
  mutate(reg.late.days = scale(reg.late.days),
         tenth_day_credits = scale(tenth_day_credits),
         attmp = scale(attmp)) %>%
  ungroup() %>%
  select(-system_key)

vars <- names(mod.dat)
gpa.drop <- gpa.drop[,vars]
gpa.drop <- subset(gpa.drop, select = -c(rundiff))

near0 <- nearZeroVar(gpa.drop, saveMetrics = T, allowParallel = T)
gpa.drop <- gpa.drop[,near0$zeroVar == F]
gpa.drop <- gpa.drop[complete.cases(gpa.drop),]

i.train <- createDataPartition(y = gpa.drop$Y, p = .75, list = F)
training <- gpa.drop[i.train,]
testing  <- gpa.drop[-i.train,]

drop.ctrl <- trainControl(method = "cv",
                          number = 5,
                          # sampling = 'down',        # not imbalanced like the above
                          # classProbs = T,
                          # summaryFunction = twoClassSummary,
                          allowParallel = T,
                          verboseIter = T)

drop.cv <- train(Y ~.,
             data = training,
             method = "glmnet",
             trControl = drop.ctrl,
             metric = "Accuracy",
             # glmnet options
             family = "binomial",
             tuneGrid = expand.grid(alpha = seq(.1, .9, by = .1), lambda = seq(0.01, 0.3, by = 0.01)))
getTrainPerf(drop.cv)
confusionMatrix(drop.cv)
drop.cv.pr <- predict(drop.cv, newdata = testing)
confusionMatrix(drop.cv.pr, reference = testing$Y, positive = 'y')
# fit$results
drop.cv$bestTune
plot(drop.cv)

fitcontrol <- trainControl(method = "none", classProbs = T)
# rerun w/ bestTune
drop.mod <- train(Y~.,
                 data = training,
                 method = "glmnet",
                 family = "binomial",
                 trControl = fitcontrol,
                 metric = "ROC",
                 tuneGrid = expand.grid(alpha = cvfit$bestTune$alpha,
                                        lambda = cvfit$bestTune$lambda))

drop.mod.pr <- predict(drop.mod, newdata = testing)
confusionMatrix(drop.mod.pr, reference = testing$Y, positive = 'y')

# varimp
varImp(glm.mod, scale = FALSE)


# Split train/test -------------------------------------------------------------

i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
# i <- varwhich(dat, "Y_i")
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]


#
# # Fit via GBM (caret) -----------------------------------------------------
#
# # need to redo some data for GBM testing
# gbmdat <- rbind(testing, training)
# gbmdat$Y <- as.numeric(levels(gbmdat$Y))[gbmdat$Y]
# gbmdat$Y <- factor(gbmdat$Y, levels = c(1, 0), labels = c('Y', 'N'))
# gbmdat$s1_gender <- ifelse(gbmdat$s1_gender == "M", 0, 1)
# i <- sapply(gbmdat, is.character)
# gbmdat[,i] <- apply(gbmdat[,i], 2, binarize.yn)
# # change logicals
# (i <- sapply(gbmdat, is.logical))
# gbmdat[,i] <- apply(gbmdat[,i], 2, function(x){
#   ifelse(x == T, 1, 0)
# })
# j <- sample(1:nrow(gbmdat), replace = F, size = .7*nrow(gbmdat))
# Y <- gbmdat$Y[j]
# X <- gbmdat[j, -1]
# test.y <- gbmdat$Y[-j]
# test.x <- gbmdat[-j, -1]
#
#     #
#     # # control setup
#     #
#     # # tgrid <- expand.grid(interaction.depth = 6,
#     # #                      n.trees = 500,
#     # #                      shrinkage = .01,
#     # #                      n.minobsinnode = 20)
#     #
#     # ctrl <- trainControl(method = 'cv',
#     #                      number = 5,
#     #                      # summaryFunction = twoClassSummary,   # for ROC
#     #                      classProbs = T,
#     #                      allowParallel = T,
#     #                      verboseIter = T)
#     #
#     # # fit
#     # gbmfit <- train(x = X,
#     #                 y = Y,
#     #                 method = 'gbm',
#     #                 trControl = ctrl,
#     #                 metric = 'Kappa',
#     #                 preProc = c("center", "scale")) # ,
#     #                 # tuneGrid = tgrid)
#     #
#     # # eval
#     # summary(gbmfit)
#     # print(gbmfit)
#     # gbm.pred <- predict(gbmfit, test.x, 'raw')
#     # table(gbm.pred, test.y)
#     # postResample(gbm.pred, test.y)
#     #
#     # gbm.pp <- predict(gbmfit, test.x, 'prob')
#     # head(gbm.pp)
#     # auc <- roc(ifelse(test.y == 'Y', 1, 0), gbm.pp[,1])
#     # print(auc$auc)
#     # plot(auc)
#     #
#     # table(cut(gbm.pp[,1], breaks = 5))
#     # table(cut(gbm.pp[,1], breaks = 5), test.y)    # lot of low-probability Y's
#     #
#     #
#     # ctrl$summaryFunction <- twoClassSummary
#     # gbmroc <- train(x = X,
#     #                 y = Y,
#     #                 method = 'gbm',
#     #                 trControl = ctrl,
#     #                 metric = 'ROC',
#     #                 preProc = c("center", "scale"))
#     # # Kappa v ROC
#     # print(gbmfit)
#     # print(gbmroc)
#     # table(gbm.pred, test.y)
#     # table(predict(gbmroc, test.x, 'raw'), test.y)
#     # # both have high false negative rates
#     #
#     # # GBM adjustments ---------------------------------------------------------
#     #
#     # # adjust for class imbalance; sampling inside training proc
#     # tgrid <- expand.grid(interaction.depth = c(5, 7, 9),
#     #                      n.trees = 500,
#     #                      shrinkage = .01,
#     #                      n.minobsinnode = 20)
#     #
#     # ctrl <- trainControl(method = 'cv',
#     #                      number = 5,
#     #                      classProbs = T,
#     #                      allowParallel = T,
#     #                      verboseIter = T)
#     #
#     # # init weights to penalize bad predictions
#     # weights <- ifelse(Y == "Y",
#     #                   (1/table(Y)[1]) * 0.5,
#     #                   (1/table(Y)[2]) * 0.5)
#     #
#     # # weighted model
#     # gbm.w <- train(x = X,
#     #                y = Y,
#     #                method = 'gbm',
#     #                weights = weights,
#     #                metric = 'Kappa',
#     #                trControl = ctrl,
#     #                tuneGrid = tgrid)
#     #
#     # # down-sampled model
#     # ctrl$sampling <- 'down'
#     # gbm.down <- train(x = X,
#     #                   y = Y,
#     #                   method = 'gbm',
#     #                   verbose = T,
#     #                   metric = 'Kappa',
#     #                   trControl = ctrl,
#     #                   tuneGrid = tgrid)
#     #
#     # # up-sampled
#     # ctrl$sampling <- 'up'
#     # gbm.up <- train(x = X,
#     #                 y = Y,
#     #                 method = 'gbm',
#     #                 verbose = T,
#     #                 metric = 'Kappa',
#     #                 trControl = ctrl,
#     #                 tuneGrid = tgrid)
#     #
#     # # w/ SMOTE
#     #   # ctrl$sampling <- 'smote'
#     #   # gbm.smote <- train(x = X,                # be sure you want to do this, time-wise
#     #   #                    y = Y,
#     #   #                    method = 'gbm',
#     #   #                    verbose = T,
#     #   #                    metric = 'ROC',
#     #   #                    trControl = ctrl)
#     #
#     # gbmlist <- list(vanilla = gbmfit,
#     #                 vanilla.roc = gbmroc,
#     #                 weighted = gbm.w,
#     #                 downsamp = gbm.down,
#     #                 upsamp = gbm.up)
#     #                 # smote = gbm.smote)
#
#
# # adjusting the weights/sampling results in many more false+, some improvement in false-
#
# # save off GBM results for time savings -----------------------------------
#
# # save(gbmlist, file = 'data/gbm-fitted-models-metric-kappa-.RData')
# load('data/gbm-fitted-models-metric-kappa-.RData')
#
# # GBM model comparisons ---------------------------------------------------
#
# # lapply(gbmlist, print)
#
# conf.list <- function(mod, x = test.x, y = test.y){
#   p <- predict(mod, x, 'raw')
#   confusionMatrix(p, reference = y, mode = "prec_recall", positive = "Y")
# }
#
# lapply(gbmlist, conf.list)
#
#
#
# # Fit via XGB -------------------------------------------------------------
#
# # xgb can handle missing data but caret is weird about it
# # preprocessing steps
# # dv <- dummyVars(Y ~., data = mod.dat)
# # xgb.dat <- data.frame(predict(dv, newdata = mod.dat))
# # xgb.dat$Y <- as.numeric(levels(mod.dat$Y))[mod.dat$Y]
#
# xgb.dat <- mod.dat[,-nearZeroVar(mod.dat)]
# # split
# i.train <- createDataPartition(y = xgb.dat$Y, p = .80, list = F)
# # i <- varwhich(dat, "Y_i")
# training <- xgb.dat[i.train,]
# # train_x <- subset(training, select = -c(Y))
# # train_y <- as.factor(training$Y)
# testing  <- xgb.dat[-i.train,]
# # test_x <- subset(testing, select = -c(Y))
# # test_y <- as.factor(testing$Y)
#
# # setup control
# xgb.ctrl <- trainControl(method = "cv",
#                      number = 3,
#                      verboseIter = F,
#                      allowParallel = T)
#
# nrounds <- 1e3
# grid_init <- expand.grid(nrounds = seq(200, nrounds, 50),
#                          eta = c(.025, .05, .1, .3),
#                          max_depth = c(2, 3, 4, 5, 6),
#                          gamma = 0,
#                          colsample_bytree = 1,
#                          min_child_weight = 1,
#                          subsample = 1)
#
# xgb.fit <- train(Y ~.,
#              data = training,
#              method = "xgbTree",
#              trControl= xgb.ctrl,
#              tuneGrid = grid_init,
#              verbose = T)
# ###
# # to avoid re-running this while I update a slew of stuff:
# # save.image()
# # load(".RData")
# ###
#
# # predictions for test set
# xgb.pr <- predict(xgb.fit, newdata = testing)
# confusionMatrix(xgb.pr, testing$Y, positive = "1", mode = "everything")
# densityplot(xgb.fit, pch = "|")
#
# ep <- predict(xgb.fit, newdata = testing, type = "prob")
# par(mfrow = c(1,2))
# hist(ep[,1]); hist(ep[,2])
# par(mfrow = c(1,1))
#
# # var imp plot
# col.names = names(training[,-1])
# imp = xgb.importance(col.names, xgb.fit$finalModel)
# xgb.plot.importance(imp)
#
#
# # xgb explainer -----------------------------------------------------------
#
# dv <- dummyVars(Y ~., data = xgb.dat)
# X <- data.frame(predict(dv, newdata = mod.dat))
# Y <- as.numeric(levels(mod.dat$Y))[mod.dat$Y]
# # split
# i.train <- sample(1:nrow(mod.dat), size = nrow(mod.dat)*.8, replace = F)
#
# train_x <- X[i.train,]
# train_y <- Y[i.train]
#
# test_x <- X[-i.train,]
# test_y <- Y[-i.train]
#
# # cv setup
# cv <- createFolds(train_y, k = 7)
# # control
# ctrl <- trainControl(method = "cv", index = cv)
# xgb.train.data = xgb.DMatrix(data.matrix(train_x), label = train_y, missing = NA)
# param <- list(objective = "binary:logistic", base_score = 0.5)
#
# # fit
# xgboost.cv = xgb.cv(param = param, data = xgb.train.data, folds = cv, nrounds = 1500, early_stopping_rounds = 100, metrics='auc')
# # pick best, run
# best_iteration = xgboost.cv$best_iteration
# xgb.model <- xgboost(param = param,  data = xgb.train.data, nrounds=best_iteration)
# # predictions
# xgb.test.data = xgb.DMatrix(data.matrix(test_x), missing = NA)
# xgb.preds = predict(xgb.model, xgb.test.data)
# xgb.roc_obj <- roc(test_y, xgb.preds)
# cat("XGB AUC ", auc(xgb.roc_obj))
#
# # importance
# col_names = attr(xgb.train.data, ".Dimnames")[[2]]
# imp = xgb.importance(col_names, xgb.model)
# xgb.plot.importance(imp)
#
# explr = buildExplainer(xgb.model, trainingData = xgb.train.data, type = "binary", base_score = 0.5, trees_idx = NULL)
# pred.breakdown = explainPredictions(xgb.model, explr, xgb.test.data)
#
# # cat('Breakdown Complete','\n')
# weights = rowSums(pred.breakdown)
# pred.xgb = 1/(1+exp(-weights))
# cat(max(xgb.preds-pred.xgb),'\n')
#
# idx_to_get = as.integer(802)
# test_x[idx_to_get,]
# showWaterfall(xgb.model, explr, xgb.test.data, data.matrix(test_x) ,idx_to_get, type = "binary")
#
# # how about someone w/ a high prob?
# pr <- predict(xgb.model, xgb.test.data)
# showWaterfall(xgb.model, explr, xgb.test.data, data.matrix(test_x), 47, type = "binary")
#
# pr.cut <- cut(pr, 5)
# table(pr.cut, test_y)
#
#
# # XGB Explainer: Y = Quarterly GPA ----------------------------------------
#
# reg.dat <- dat %>%
#   select(Y = qgpa, class.standing, tenth_day_credits, num_ind_study,
#          num_courses, attmp, nongrd, deduct, tot_creds,
#          s1_gender, running_start, std_test_high, starts_with("Ethnic"), HispanicInd,
#          InternationalStudentInd, age, conditional, provisional, res_in_question,
#          low_family_income, appl_class, high_sch_gpa, starts_with("hs"),
#          ft, ft.creds.over, n.unmet, reg.late.days, reg.late.binary)
#
# dv <- dummyVars(Y ~., data = reg.dat)
# regX <- data.frame(predict(dv, newdata = reg.dat))
# regY <- reg.dat$Y
# # split
# i.train <- sample(1:nrow(mod.dat), size = nrow(mod.dat)*.8, replace = F)
# train_x <- regX[i.train,]
# train_y <- regY[i.train]
# test_x <- regX[-i.train,]
# test_y <- regY[-i.train]
#
# # cv setup
# cv <- createFolds(train_y, k = 7)
# # control
# ctrl <- trainControl(method = "cv", index = cv)
# xgb.train.data = xgb.DMatrix(data.matrix(train_x), label = train_y, missing = NA)
# param <- list(objective = "reg:linear", base_score = 0.5)
#
# # fit -> select
# xgboost.cv <- xgb.cv(param = param, data = xgb.train.data, folds = cv, nrounds = 1500, early_stopping_rounds = 100, metrics = "rmse")
# best_iteration <- xgboost.cv$best_iteration
# xgb.model <- xgboost(param = param,  data = xgb.train.data, nrounds = best_iteration)
# # predictions
# xgb.test.data <- xgb.DMatrix(data.matrix(test_x), missing = NA)
# xgb.preds <- predict(xgb.model, xgb.test.data)
#
# # importance
# col_names <- attr(xgb.train.data, ".Dimnames")[[2]]
# imp <- xgb.importance(col_names, xgb.model)
# xgb.plot.importance(imp)
#
# explr <- buildExplainer(xgb.model, trainingData = xgb.train.data, type = "regression", base_score = 0.5, trees_idx = NULL)
# pred.breakdown <- explainPredictions(xgb.model, explr, xgb.test.data)
#
# gpa.pred <- predict(xgb.model, xgb.test.data)
# par(mfrow = c(1,2))
# hist(gpa.pred, main = "test-predictions")
# hist(test_y, main = "test-truth")
# par(mfrow = c(1,1))
# plot(test_y, gpa.pred)
# test.resid <- test_y - gpa.pred
# hist(test.resid, main = "residuals")
# plot(test.resid, test_y)
# table(cut(test.resid, breaks = 7))
#
#
# showWaterfall(xgb.model, explr, xgb.test.data, data.matrix(test_x), 47, type = "regression")
# showWaterfall(xgb.model, explr, xgb.test.data, data.matrix(test_x), which(gpa.pred == min(gpa.pred)), type = "regression")
#
# # var breakdown plot(s)
# plot(test_x$std_test_high, pred.breakdown$std_test_high)



# build data for python ---------------------------------------------------
# and send it to axdd-assessment

library(ssh)
pydat <- mrg.dat[,-nearZeroVar(mrg.dat)]
pydat <- pydat %>%
  select(-tran_yr, -tran_qtr) %>%
  group_by(system_key) %>%
  arrange(yrq, .by_group = T) %>%
  mutate(qtr.num = row_number(),    # xgboost package is fine w/ leaving missing data, don't recode now
         Y = lead(cum.gpa, n = 1),
         n.unmet.cum = cumsum(n.unmet),
         reg.late.mean = mean(reg.late.days)) %>%
  ungroup() %>%
  select(system_key, Y, qtr.num, class, tenth_day_credits, scholarship_type,
         num_courses, pts, attmp, nongrd, deduct, qgpa, tot_creds, cum.pts, cum.attmp,
         s1_gender, child_of_alum, running_start, s1_high_satm, s1_high_satv, s1_high_act,
         starts_with("EthnicGrp"), HispanicInd, ResidentDesc, age,
         trans_gpa, conditional, with_distinction, low_family_income, appl_class,
         high_sch_gpa, starts_with("hs_"), last_school_type, reg.late.days,
         reg.late.mean, tran_major_abbr, major.change, major.change.count, n.unmet,
         n.unmet.cum, ft, ft.creds.over) %>%
  mutate(class = factor(class))

# construct dummy var matrix
dv <- dummyVars(Y ~., data = pydat)
X <- data.frame(predict(dv, newdata = pydat))
Y <- pydat$Y
XY <- cbind(Y, X)
# split
i.train <- sample(1:nrow(pydat), size = nrow(pydat)*.8, replace = F)
pytrain <- XY[i.train,]
pytest <- XY[-i.train,]

sesh <- ssh_connect(config::get('ssh', 'config.yml'))
# remote.path <- "data/rtf_freefall"
write_csv(pytrain, "data/pytrain.csv")
write_csv(pytest, "data/pytest.csv")
write_csv(pydat, "data/pydat.csv")
scp_upload(sesh, files = "data/pytrain.csv", to = "data/freefall", verbose = T)
scp_upload(sesh, files = "data/pytest.csv", to = "data/freefall", verbose = T)
scp_upload(sesh, files = "data/pydat.csv", to = "data/freefall", verbose = T)
ssh_disconnect(sesh)
