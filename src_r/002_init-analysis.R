# Comments ----------------------------------------------------------------

# For this file, let's focus on predicting quarterly GPA <= 2.5

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()


library(caret)
library(doMC)
library(tidyverse)

registerDoMC(cores = 4)
set.seed(4567)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 500)

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

# [TODO] Rather than use lag funs, I'll create some quarterly features - that probably means converting data to wide

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
  mutate(Y = factor(if_else(qgpa <= 2.5, 1, 0), levels = c(0, 1)),
         # qgpa15 = factor(qgpa15, levels = c(0, 1)),
         probe = factor(probe, levels = c(0,1)),
         scholarship_type = factor(scholarship_type),
         honors_program = factor(honors_program),
         yearly_honor_type = factor(yearly_honor_type),
         trans_gpa = if_else(is.na(trans_gpa), -99, trans_gpa),       # let's try explicitly modeling the missing vals (later try creating dummies)
         high_sch_gpa = if_else(is.na(high_sch_gpa), -99, high_sch_gpa)) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(qtr = row_number()) %>%
  ungroup()

# select test score
sat <- scale(dat$s1_high_satm + dat$s1_high_satv)
act <- scale(dat$s1_high_act)
dat$std_test_high <- pmax(act[,1], sat[,1], na.rm = T)
rm(sat, act)


mod.dat <- dat %>%
  select(Y, class.standing, tenth_day_credits, num_ind_study, num_courses, attmp, nongrd, deduct, tot_creds, s1_gender, running_start,
         std_test_high, starts_with("Ethnic"), HispanicInd, InternationalStudentInd, age, conditional, provisional, res_in_question,
         low_family_income, appl_class, high_sch_gpa, starts_with("hs"), ft, ft.creds.over, n.unmet, reg.late.days, reg.late.binary)

# Validation checks -------------------------------------------------------

(near0 <- nearZeroVar(mod.dat, saveMetrics = T, allowParallel = T))
mod.dat <- mod.dat[,near0$zeroVar == F]

nrow(mod.dat[complete.cases(mod.dat),]) / nrow(mod.dat)       # The change to HS and trans GPA coding is a large improvement here
# missing.threshold <- 0.3
# (i <- which(apply(dat, 2, function(x) sum(is.na(x)) / nrow(dat)) > missing.threshold))
for(mt in seq(0, .3, by = .05)){
  print(mt)
  print(which(apply(mod.dat, 2, function(x) sum(is.na(x)) / nrow(mod.dat) > mt)))
}

# dat <- subset(dat, select = -i)
# nrow(dat[complete.cases(dat),]) / nrow(dat)

# FOR TIME BEING:
mod.dat <- mod.dat[complete.cases(mod.dat),]

# Split train/test -------------------------------------------------------------

i.train <- createDataPartition(y = mod.dat$Y, p = .75, list = F)
# i <- varwhich(dat, "Y_i")
training <- mod.dat[i.train,]
testing  <- mod.dat[-i.train,]

# Preprocess --------------------------------------------------------------


# Control -------------------------------------------------------------------

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 10,
#                      repeats = 3)
ctrl <- trainControl(method = "cv", number = 10)

# Fit via GLMNET ---------------------------------------------------------------

fit <- train(Y ~.,
             data = training,
             method = "glmnet",
             trControl= ctrl,
             # glmnet options
             family = "binomial",
             tuneGrid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, by = 0.001)))

confusionMatrix(fit)
fit.pr <- predict(fit, newdata = testing)
confusionMatrix(fit.pr, reference = testing$Y, mode = "prec_recall", positive = "1")



# Fit via XGB -------------------------------------------------------------

# xgb can handle missing data
# preprocessing steps
dv <- dummyVars(Y ~., data = mod.dat)
xgb.dat <- data.frame(predict(dv, newdata = mod.dat))
xgb.dat$Y <- as.numeric(levels(mod.dat$Y))[mod.dat$Y]

xgb.dat <- xgb.dat[,-nearZeroVar(xgb.dat)]
# split
i.train <- createDataPartition(y = xgb.dat$Y, p = .80, list = F)
# i <- varwhich(dat, "Y_i")
training <- xgb.dat[i.train,]
train_x <- subset(training, select = -c(Y))
train_y <- as.factor(training$Y)
testing  <- xgb.dat[-i.train,]
test_x <- subset(testing, select = -c(Y))
test_y <- as.factor(testing$Y)

# setup control
xgb.ctrl <- trainControl(method = "cv",
                     number = 3)

nrounds <- 1e3
grid_init <- expand.grid(nrounds = seq(200, nrounds, 50),
                         eta = c(.025, .05, .1, .3),
                         max_depth = c(2, 3, 4, 5, 6),
                         gamma = 0,
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         subsample = 1)

xgb.fit <- train(Y ~.,
             x = train_x,
             y = train_y,
             method = "xgbTree",
             trControl= xgb.ctrl,
             tuneGrid = grid_init,
             verbose = T)