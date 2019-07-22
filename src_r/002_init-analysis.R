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

# pre-processing ----------------------------------------------------------

dat <- mrg.dat %>%
  filter(!is.na(qgpa)) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(l1.qgpa = lag(qgpa, n = 1),
         l2.qgpa = lag(qgpa, n = 2),
         l1.cum.gpa = lag(cum.gpa, n = 1),
         l1.probe = lag(probe),
         l1.qgpa15 = lag(qgpa15),
         l1.qgpa20 = lag(qgpa20),
         l1.cum.pts = lag(cum.pts),
         l1.cum.attmp = lag(cum.attmp)) %>%
  ungroup() %>%
  mutate(Y_i = factor(if_else(qgpa <= 2.5, 1, 0), levels = c(0, 1)),
         # qgpa15 = factor(qgpa15, levels = c(0, 1)),
         probe = factor(probe, levels = c(0,1)),
         scholarship_type = factor(scholarship_type),
         honors_program = factor(honors_program),
         yearly_honor_type = factor(yearly_honor_type))

# removing variables observed after grades
dat <- dat %>%
  select(Y_i, class, honors_program, tenth_day_credits, scholarship_type, num_ind_study, num_courses, attmp, nongrd, deduct,
         tot_creds, starts_with("l1."), starts_with("l2."), s1_gender, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act,
         s1_high_act, starts_with("Ethnic"), HispanicInd, InternationalStudentInd, ResidentDesc, age, trans_gpa, aa_degree, direct_transfer,
         conditional, provisional, with_distinction, res_in_question, low_family_income, appl_class, high_sch_gpa, starts_with("hs_"),
         last_school_type, reg.late.days, reg.late.binary, tran_branch, tran_major_abbr, major.change, major.change.count, n.unmet,
         ft, ft.creds.over)

# Validation checks -------------------------------------------------------

near0 <- nearZeroVar(dat, saveMetrics = T, allowParallel = T)
near0
dat <- dat[,near0$zeroVar == F]

# select test score
sat <- scale(dat$s1_high_satm + dat$s1_high_satv)
act <- scale(dat$s1_high_act)
dat$std.test <- pmax(act[,1], sat[,1], na.rm = T)
rm(sat, act)
dat <- subset(dat, select = -c(s1_high_satm, s1_high_satv, s1_high_act))

nrow(dat[complete.cases(dat),]) / nrow(dat)
missing.threshold <- 0.3
i <- which(apply(dat, 2, function(x) sum(is.na(x)) / nrow(dat)) > missing.threshold)
dat <- subset(dat, select = -i)
nrow(dat[complete.cases(dat),]) / nrow(dat)


# Split train/test -------------------------------------------------------------

i.train <- createDataPartition(y = dat$Y_i, p = .75, list = F)
i <- varwhich(dat, "Y_i")
training <- dat[i.train, -i]
testing  <- dat[-i.train, -i]

# Preprocess --------------------------------------------------------------

