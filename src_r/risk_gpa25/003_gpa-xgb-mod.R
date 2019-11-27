# Comments ----------------------------------------------------------------

# For this file, predict GPA

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

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

# remove near-zero var vars
dat.scaled <- dat.scaled[,-nearZeroVar(dat.scaled)]
dat.scaled <- dat.scaled %>% filter(!is.na(qgpa))

# encode dummies/factors
# using dat.scaled
cat.vars <- Cs(class, scholarship_type, child_of_alum, running_start, s1_gender, conditional, with_distinction,
               low_family_income, appl_class, last_school_type, major.change, ft, premajor)
f <- paste('~', paste(cat.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat.scaled)
cat.var.mat <- dat.scaled %>%
  select(cat.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)# predict(encoder, dat.scaled)
dim(cat.var.mat)
colnames(cat.var.mat)

mod.vars <- Cs(system_key, yrq, Y, qtr.seq, class, tenth_day_credits, scholarship_type, num_courses,
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
  select(one_of(mod.vars), one_of(lag.vars))

mod.dat <- bind_cols(mod.dat, data.frame(cat.var.mat))
mod.key.vars <- mod.dat %>% select(system_key, yrq)
mod.dat <- mod.dat %>%
  select(-system_key, -yrq)

rm(f, encoder, cat.vars, mod.vars, lag.vars, cat.var.mat)


# examine Y ---------------------------------------------------------------

ggplot(mod.dat, aes(x = Y)) + geom_histogram(bins = 50)
ggplot(mod.dat, aes(x = qgpa, y = Y)) + geom_point() + stat_smooth()
any(is.na(mod.dat$Y))
cormat <- round(cor(mod.dat, use = 'pairwise.complete.obs'), 2)
cbind(sort(cormat[,1]))

rm(cormat)

# XGB model setup ---------------------------------------------------------

# split
i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]

dtrain <- xgb.DMatrix(as.matrix(training[,-1]), label = training$Y)
dtest <- xgb.DMatrix(as.matrix(testing[,-1]), label = testing$Y)

xgb.params<- list(
  objective = "reg:squarederror",
  eta = 0.01,
  max_depth = 6,
  gamma = 1,
  subsample = .8,
  colsample_bytree = .75,
  num_parallel_tree = 2,
  eval_metric = 'rmse',
  eval_metric = 'mae')

# cross-validation
xgbcv <- xgb.cv(params = xgb.params,
                data = dtrain,
                nrounds = 300,
                nfold = 5,
                prediction = TRUE,
                showsd = TRUE,
                stratified = TRUE,
                verbose = 2,
                print_every_n = 1,
                early_stopping_rounds = 10)

print(xgbcv, verbose = T)

# some analysis:
hist(xgbcv$pred)
plot(xgbcv$pred, training$Y)
abline(lm(training$Y ~ xgbcv$pred), col = 'red')
abline(h = 2.5, col = 'green')
abline(v = 2.5, col = 'green')
resid <- training$Y - xgbcv$pred
plot(density(resid))
plot(resid, training$Y)
abline(h = 2.5, col = 'green')

# fit:
xgb.fit <- xgboost(data = dtrain,
                   params = xgb.params,
                   nrounds = xgbcv$best_iteration,
                   verbose = TRUE,
                   print_every_n = 1,
                   early_stopping_rounds = 10)

# summary:
print(xgb.fit, verbose = T)

# predictions:
xgb.pred <- predict(xgb.fit, dtest)