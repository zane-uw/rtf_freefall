# test model w/ new quarterly outcome data

rm(list = ls())
gc()

library(odbc)
library(dbplyr)
library(caret)
library(tidyverse)
library(xgboost)


setwd(rstudioapi::getActiveProject())
con <- dbConnect(odbc(), 'sqlserver01')


grep('transcript', db_list_tables(con), value = T)
dbListFields(con, 'transcript')

new.outcomes <- tbl(con, in_schema('sec', 'transcript')) %>%
  filter(tran_yr == 2020,
         tran_qtr == 1,
         special_program %in% c(1, 2, 3, 13, 14, 16, 17, 31, 32, 33)) %>%
  select(system_key, resident, veteran, class, honors_program, tenth_day_credits,
         scholarship_type, yearly_honor_type, num_ind_study, num_courses, enroll_status,
         qtr_grade_points, qtr_graded_attmp, qtr_nongrd_earned, qtr_deductible) %>%
  collect()
  #
  # # keys
  # sk <- data.frame('system_key' = unique(new.outcomes$system_key))
  # # [TODO] add as temp table for queries; will require some re-factoring
  # copy_to(con, sk, '##syskeys', overwrite = T)
  # # and finally, make a reference:
  # syskeys <- tbl(con, '##syskeys')


# This is the point where we (I) need a proper function for building the data...
# ...
# ...



# 04-10-2020 trying... ---------------------------------------------------------------
# preds for OMAD to go with launch of RAD tool

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

# merged data set from scratch-reboot file
load('data/merged-dataset_2020-04-10.RData')
# Model object
mod <- xgb.load('models/xgb-fit-any-adverse-quarterly-outcome_2020-01-22.model')

dat <- subset(mrg.dat, select = -c(InternationalStudentInd, aa_degree, direct_transfer))

# describe vars for re-coding transformations
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

lag.vars <- Cs(qgpa, cum.gpa, pts, cum.pts, cum.attmp, tot_creds, attmp, qgpa20, n.w, csum.w, avg.stem.grade)
key.vars <- Cs(system_key)

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

dat <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  # gen lags                        # don't gen these for 20201 data, replace lag-qgpa w/ 20201 data
  # mutate_at(lag.vars,
  #           lag, n = 1) %>%
  mutate(d1 = qgpa - lag(qgpa),
         d1 = if_else(is.na(d1), 0, d1),
         rundiff = d1 + lag(d1)) %>%
  select(-d1) %>%
  ungroup() %>%
  # <<-- DECISION TIME -->>
  # select(Y, everything()) %>%
  select(-one_of(c(cat.vars, ord.vars))) %>%
  bind_cols(., data.frame(cat.var.mat), data.frame(ord.var.mat))

# handle major_abbr
# The alternative I'll create for now is to recode them w/ reference cats, ie as factors. This won't really be ideal for majors so let's do those separately
# dat <- dat %>% mutate_at(cat.vars, as.factor) %>% mutate_at(ord.vars, as.factor)
myrq <- max(dat$yrq)
syskey <- dat$system_key[dat$yrq == myrq]
cur.dat <- dat %>% filter(yrq == myrq) %>%
  select(-system_key, -yrq) %>%
  replace_na(list(n.holds = 0))
cur.dat <- xgb.DMatrix(as.matrix(cur.dat[,-1]))

pred <- predict(mod, cur.dat)
hist(pred)
