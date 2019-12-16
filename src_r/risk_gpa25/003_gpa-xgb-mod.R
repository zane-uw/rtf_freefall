# Comments ----------------------------------------------------------------

# For this file, predict GPA

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
# don't need these, just tidying up
rm(dat, stu.deptwise.wide)

# Initial processing ----------------------------------------------------------

dat.scaled <- dat.scaled %>%
  select(-starts_with("Ethnic"),
         -HispanicInd,
         -yearly_honor_type,
         -ResidentDesc)

# remove near-zero var vars
# dat.scaled <- dat.scaled[,-nearZeroVar(dat.scaled)]
# dat.scaled <- dat.scaled %>% filter(!is.na(qgpa))

# encode dummies/factors
# using dat.scaled
dat.scaled <- dat.scaled %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(scholarship_type = lag(scholarship_type)) %>%
  ungroup()

cat.vars <- Cs(class, child_of_alum, running_start, s1_gender, conditional, with_distinction,
               low_family_income, appl_class, last_school_type, major.change, ft, premajor, scholarship_type,
               tran_major_abbr)

f <- paste('~', paste(cat.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat.scaled)
cat.var.mat <- dat.scaled %>%
  select(cat.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)# predict(encoder, dat.scaled)
dim(cat.var.mat)
colnames(cat.var.mat)

# mod.vars <- Cs(system_key, yrq, Y, qtr.seq, class, tenth_day_credits, num_courses,
#                attmp, nongrd, tot_creds, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act, trans_gpa,
#                conditional, with_distinction, low_family_income,
#                appl_class, high_sch_gpa, hs_for_lang_type, hs_for_lang_yrs, hs_yrs_for_lang, hs_math_level, hs_yrs_math,
#                hs_yrs_arts, hs_yrs_science, hs_yrs_soc_sci, hs_yrs_english, last_school_type, reg.late.days,
#                major.change, major.change.count, n.unmet, ft, ft.creds.over, n.major.courses, csum.major.courses,
#                premajor, n.alt.grading, avg.class.size, n.writing, n.diversity, n.engl_comp, n.qsr,
#                n.vlpa, n.indiv_soc, n.nat_world, sum.fees, stem.courses, stem.credits, avg.stem.grade, csum.stem.courses, )

mod.vars <- setdiff(names(dat.scaled), cat.vars)

lag.vars <- Cs(qgpa, cum.gpa, pts, cum.pts, cum.attmp, tot_creds, attmp, qgpa20, n.w, csum.w, avg.stem.grade)

mod.dat <- dat.scaled %>%
  # filter(!is.na(qgpa)) %>%
  # target should be numeric for XGBoost
  mutate(Y = qgpa) %>%
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
  select(Y, one_of(mod.vars), one_of(lag.vars))

mod.dat <- bind_cols(mod.dat, data.frame(cat.var.mat))
mod.key.vars <- mod.dat %>% select(system_key, yrq)
mod.dat <- mod.dat %>%
  select(-system_key, -yrq, -tran_yr, -tran_qtr)

rm(f, encoder, cat.vars, mod.vars, lag.vars, cat.var.mat)


# examine Y ---------------------------------------------------------------

ggplot(mod.dat, aes(x = Y)) + geom_histogram(bins = 50) + geom_vline(xintercept = 2.5)
ggplot(mod.dat, aes(x = qgpa, y = Y)) + geom_point() + stat_smooth()
any(is.na(mod.dat$Y))

sum((mod.dat$Y <= 2.5), na.rm = T)/nrow(mod.dat)

# cormat <- round(cor(mod.dat, use = 'pairwise.complete.obs'), 2)
# cbind(tail(sort(cormat[,1]), n = 10))
# cbind(head(sort(cormat[,1]), n = 10))
#
# rm(cormat)

# XGB model setup ---------------------------------------------------------

# split
mod.dat <- mod.dat[!is.na(mod.dat$Y),]
i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]

dtrain <- xgb.DMatrix(as.matrix(training[,-1]), label = training$Y)
dtest <- xgb.DMatrix(as.matrix(testing[,-1]), label = testing$Y)

xgb.params<- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  gamma = 1,
  subsample = .8,
  colsample_bytree = .75,
  # num_parallel_tree = 2,
  eval_metric = 'rmse',
  eval_metric = 'mae')

# cross-validation
xgbcv <- xgb.cv(params = xgb.params,
                data = dtrain,
                nrounds = 500,
                nfold = 5,
                prediction = T,
                # save_models = T,
                showsd = T,
                stratified = T,
                verbose = 2,
                print_every_n = 1,
                early_stopping_rounds = 10)

print(xgbcv, verbose = T)
# some analysis:
plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_rmse_mean, col = 'blue', type = 'l'); title(main = 'RMSE')
lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_rmse_mean, col = 'red')
plot(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$train_mae_mean, col = 'blue', type = 'l'); title(main = 'MAE')
lines(xgbcv$evaluation_log$iter, xgbcv$evaluation_log$test_mae_mean, col = 'red')
hist(xgbcv$pred)
plot(xgbcv$pred, training$Y, main = 'actual / predicted')
abline(lm(training$Y ~ xgbcv$pred), col = 'red')
abline(h = 2.5, col = 'green')
abline(v = 2.5, col = 'green')
resid <- training$Y - xgbcv$pred
plot(density(resid))
abline(v = 0)
plot(resid, training$Y)
abline(h = 2.5, col = 'green')
abline(h = mean(training$Y), col = 'darkblue')
qqline(resid)

# re-fit w/ best_nrounds:
xgb.fit <- xgboost(data = dtrain,
                   params = xgb.params,
                   nrounds = xgbcv$best_iteration,
                   verbose = 2,
                   print_every_n = 20,
                   early_stopping_rounds = 10,
                   nthread = length(cl))

# summary:
print(xgb.fit, verbose = T)

xgb.fit[1:5]

# predictions:
xgb.pred <- predict(xgb.fit, dtest)
hist(xgb.pred)
plot(xgb.pred, testing$Y)
abline(v = 2.5, col = 'green')
abline(h = 2.5, col = 'green')

resid <- testing$Y - xgb.pred
hist(resid)
plot(resid, testing$Y)

imp.mat <- xgb.importance(model = xgb.fit, feature_names = names(training)[-1])
# print(importance_matrix)
xgb.plot.importance(importance_matrix = imp.mat, top_n = 20)

xgb.plot.deepness(xgb.fit)
xgb.plot.shap(data = as.matrix(testing[,-1]), model = xgb.fit, top_n = 5)

# save dmats --------------------------------------------------------------

xgb.DMatrix.save(dtrain, 'data/xgb-gpa-train.dmatrix')
xgb.DMatrix.save(dtest, 'data/xgb-gpa-dtest.dmatrix')
write_csv(training, 'data/gpa-w-compass-training.csv')
write_csv(testing, 'data/gpa-w-compass-testing.csv')

# save fitted model -------------------------------------------------------

xgb.save(xgb.fit, fname = 'models/xgb-gpa-w-compass-fit.model')


# push -> remote server ---------------------------------------------------

sesh <- ssh::ssh_connect(config::get('ssh', 'config.yml'))
# remote.path <- "data/rtf_freefall"
ssh::scp_upload(sesh, files = 'data/xgb-gpa-train.dmatrix', to = 'data/freefall', verbose = T)
ssh::scp_upload(sesh, files = 'data/xgb-gpa-dtest.dmatrix', to = 'data/freefall', verbose = T)
ssh::scp_upload(sesh, files = 'data/gpa-w-compass-testing.csv')
ssh::scp_upload(sesh, files = 'data/gpa-w-compass-training.csv')
ssh::ssh_disconnect(sesh)

# alt models --------------------------------------------------------------

# ols (way too much missing data)
ols.fit <- lm(Y ~., data = training, na.action = na.exclude, model = T, x = T, y = T)
summary(ols.fit)
plot(ols.fit)
ols.pr <- predict(ols.fit, newdata = testing[,-1])
plot(ols.pr, testing$Y)
mean((testing$Y - ols.pr), na.rm = T)
