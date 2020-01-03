# Comments ----------------------------------------------------------------

# Predicting any course <= 2.5

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(caret)
library(xgboostExplainer)
library(tidyverse)
library(pROC)
library(doParallel)
library(xgboost)
library(mice)

setwd(rstudioapi::getActiveProject())
set.seed(24601)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 250)

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

cat.vars <- Cs(class,
               child_of_alum,
               running_start,
               s1_gender,
               conditional,
               with_distinction,
               low_family_income,
               appl_class,
               last_school_type,
               major.change,
               ft,
               premajor,
               scholarship_type,
               ext_premajor,
               major_abbr)
cat.vars <- unique(c(cat.vars, type_names(dat, func = is.character), type_names(dat, func = is.logical)))

f <- paste('~', paste(cat.vars, collapse = '+'))
encoder <- dummyVars(as.formula(f), dat)
cat.var.mat <- dat %>%
  select(cat.vars) %>%
  mutate_all(as.factor) %>%
  predict(encoder, .)# predict(encoder, dat.scaled)
dim(cat.var.mat)
colnames(cat.var.mat)

# The alternative I'll create for now is to recode them w/ reference cats, ie as factors. This won't really be ideal for majors so let's do those separately
dat <- dat %>% mutate_at(cat.vars, as.factor)
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

mod.vars <- setdiff(names(dat), cat.vars)

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




# push? -------------------------------------------------------------------
  # write_csv(mod.dat, path = 'src_r/risk_gpa25/data/data_any-adverse-qtrly-outcome.csv')
  # sesh <- ssh::ssh_connect(config::get('ssh', 'config.yml'))
  # ssh::scp_upload(sesh, files = 'src_r/risk_gpa25/data/data_any-adverse-qtrly-outcome.csv',
  #                 to = 'data/freefall/', verbose = T)
  # ssh::ssh_disconnect(sesh)

# finish tidy-up for R-stuff -----------------------------------------------------

mod.key.vars <- mod.dat %>% select(system_key, yrq)
mod.dat <- mod.dat %>% select(-system_key, -yrq)

rm(f, encoder, cat.vars, mod.vars, lag.vars, cat.var.mat, stu.deptwise.wide, new.y, dat.scaled, i)


# imputation --------------------------------------------------------------

imp <- mice(as.matrix(mod.dat[,2:20]))


# model setup -------------------------------------------------------------

# simple, listwise deletion
lgfit <- glm(Y ~., data = mod.dat, family = binomial(link = 'logit'))
