# Comments ----------------------------------------------------------------

# Predicting any course <= 2.5 and 'adverse outcomes' like W, I, NC

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(xgboost)
library(xgboostExplainer)
library(tidyverse)
# library(pROC)
library(doParallel)
# library(mice)
# library(parsnip)
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

get.most.recent.file <- function(path = ".", ...){
  fs <- list.files(path = path, full.names = T, pattern = ...)
  return(fs[which.max(file.mtime(fs))])
}

get.most.recent.file('OMAD_adverse_outcome_mod/data/', pattern = 'merged')
load(get.most.recent.file('OMAD_adverse_outcome_mod/data', pattern = 'merged'))



# Initial tidying up ------------------------------------------------------

# remove extraneous columns
dat <- mrg.dat %>%
  select(-(AddDropclass:NoShow),
         -major_abbr,
         -starts_with('csum.dept.creds_'),
         -starts_with('nclass.dept_'),
         -starts_with('cumavg.dept_'),
         -ends_with('week_1'),
         -ends_with('week_2'),
         -ends_with('week_3'),
         -ends_with('week_4'),
         -ends_with('week_5'),
         -ends_with('week_6'),
         -ends_with('week_7'),
         -ends_with('week_8'),
         -ends_with('week_9'),
         -ends_with('week_10'),
         -ends_with('week_11'),
         -ends_with('week_12')) %>%
# fill NAs
  replace_na(list('visit_advising' = 0,
                  'visit_ic' = 0,
                  'advising_week_0' = 0,
                  'ic_week_0' = 0,
                  'ic_tot' = 0)) %>%
  mutate(ft = as.numeric(ft))


# create Y ----------------------------------------------------------------

#
# >>> need to load courses.taken here <<<
#
load('OMAD_adverse_outcome_mod/data/Y-courses-taken.RData')

adv.Y <- courses.taken %>%
  mutate(grade25 = if_else(numeric.grade <= 2.5, 1, 0),
         other = if_else(grade %in% c('HW', 'I', 'NC', 'NS', 'W', 'W3', 'W4', 'W5', 'W6', 'W7'), 1, 0)) %>%
  group_by(system_key, yrq) %>%
  summarize(s = sum(grade25, other, na.rm = T),
            Y = if_else(s >= 1, 1, 0)) %>%
  select(-s)

# add to dat
dat <- dat %>% left_join(adv.Y)

# remove near zero variance (cleaning)
i <- caret::nearZeroVar(dat, foreach = T, allowParallel = T)
dat <- dat[,-i]
data.var.names <- names(dat)

# [TODO]
# calc the 1st diff in GPA - don't want to include this below b/c for 'new' data we want the diff
# but don't want to double-lag the gpa
# I could fix this in the code with some create/select
dat <- dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(d1 = qgpa - lag(qgpa),
         d1 = if_else(is.na(d1), 0, d1),
         rundiff = d1 + lag(d1)) %>%
  ungroup() %>%
  select(-d1)

# Recode categorical vars (not doing this here right now) ------------------------------------
# just run the lags and split the data
process.transformations <- function(df, OHE.enc = T){
  lag.vars <- Cs(qgpa,
                 cum.gpa,
                 pts,
                 cum.pts,
                 cum.attmp,
                 tot_creds,
                 attmp,
                 qgpa20,
                 n.w,
                 csum.w,
                 avg.stem.grade,
                 csum.stem.credits,
                 # ic_tot,
                 visit_advising,
                 visit_ic)

  # categorical/ordinal vars
  cat.vars <- Cs(class,
                 scholarship_type,
                 qgpa20,
                 s1_gender,
                 child_of_alum,
                 running_start,
                 resident,
                 conditional,
                 with_distinction,
                 low_family_income,
                 hs_for_lang_type,
                 hs_math_level,
                 last_school_type,
                 guardian.ed.max,
                 guardian.ed.min,
                 major.change,
                 dual_major,
                 premajor,
                 n.unmet,
                 visit_advising,
                 visit_ic,
                 n.writing,
                 n.diversity,
                 n.engl_comp,
                 n.qsr,
                 n.vlpa,
                 n.indiv_soc,
                 n.nat_world,
                 ft)

  dat <- df %>%
    group_by(system_key) %>%
    arrange(system_key, yrq) %>%
    # gen lags
    mutate_at(lag.vars,
              lag,
              n = 1) %>%
    ungroup()

  if(OHE.enc == T){
  # Less than full rank
  f <- paste('~', paste(cat.vars, collapse = '+'))
  encoder <- dummyVars(as.formula(f), dat, fullRank = F)
  cat.var.mat <- dat %>%
    select(all_of(cat.vars)) %>%
    mutate_all(factor, exclude = NULL) %>%
    predict(encoder, .)# predict(encoder, dat.scaled)
  # dim(cat.var.mat)
  # colnames(cat.var.mat)

  dat <- dat %>% select(-all_of(cat.vars)) %>%
    bind_cols(., data.frame(cat.var.mat))

  return(dat)
  }

  return(dat)
}



# split newest data -------------------------------------------------------

# new prediction dat (curr 20201->2)
split.yrq <- max(dat$yrq)
# create lags but not the OHE
dat <- process.transformations(dat, OHE.enc = F)
new.pred.data <- dat %>% filter(dat$yrq == split.yrq) # %>% select(-Y)
# don't want lags for 'new' data b/c the lag is built-in

dat <- dat %>% filter(dat$yrq < split.yrq)


# Save ------------------------------------------------

# transformations will be in the python pipeline
# as will train/test split
write_csv(dat, 'OMAD_adverse_outcome_mod/data/transformed-data-to-py.csv')
new.pred.data %>% select(-Y) %>% write_csv(., 'OMAD_adverse_outcome_mod/data/new-data-to-predict.csv')

cat.vars <- Cs(class,
               scholarship_type,
               qgpa20,
               s1_gender,
               child_of_alum,
               running_start,
               resident,
               conditional,
               with_distinction,
               low_family_income,
               hs_for_lang_type,
               hs_math_level,
               last_school_type,
               guardian.ed.max,
               guardian.ed.min,
               major.change,
               dual_major,
               premajor,
               n.unmet,
               visit_advising,
               visit_ic,
               n.writing,
               n.diversity,
               n.engl_comp,
               n.qsr,
               n.vlpa,
               n.indiv_soc,
               n.nat_world,
               ft)

write_lines(cat.vars, 'OMAD_adverse_outcome_mod/data/cat-var-list.txt')
write_lines(data.var.names, 'OMAD_adverse_outcome_mod/data/model-var-list.txt')