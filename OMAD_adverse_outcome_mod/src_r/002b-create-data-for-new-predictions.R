# Fetch a current quarter to predict new data

rm(list = ls())
gc()

library(tidyverse)
library(dbplyr)
library(odbc)

setwd(rstudioapi::getActiveProject())

YRQ_NOW <- 20202
YRQ_LAG <- 20201
EOP_CODES <- c(1, 2, 13, 14, 16, 17, 31, 32, 33)

get.most.recent.file <- function(path = ".", ...){
  fs <- list.files(path = path, full.names = T, pattern = ...)
  return(fs[which.max(file.mtime(fs))])
}

get.most.recent.file('OMAD_adverse_outcome_mod/data/', pattern = 'merged')
load(get.most.recent.file('OMAD_adverse_outcome_mod/data', pattern = 'merged'))

# remove extraneous columns
lag.dat <- mrg.dat %>%
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
  mutate(ft = as.numeric(ft)) %>%


keep.dat <- mrg.dat %>% # mrg.dat[mrg.dat$yrq == YRQ_LAG,]
  filter(yrq == YRQ_LAG) %>%
  select(system_key,
         yrq,
         pts,
         qgpa,
         qgpa20,
         tot_creds,
         qgpa15,
         starts_with('cum.'),
         s1_gender,
         child_of_alum,
         running_start,
         resident,
         conditional,
         with_distinction,
         low_family_income,
         starts_with('hs_'),
         last_school_type,
         high_sch_gpa,
         trans_gpa,
         best_satr_v,
         best_satr_m,
         best_satr_c,
         guardian.ed.max,
         guardian.ed.min,
         visit_advising,
         visit_ic,
         n.w,
         csum.w,
         csum.alt.grading,
         stem.credits,
         avg.stem.grade,
         csum.stem.credits,
         ic_tot)



# Data where recalc or new fetch required
# class, tenth_day_credits, num_ind_study, num_courses, attmp, nongrd, major.change, major.change.count,
# dual_major, n.majors, premajor, n.major.courses, csum.major.courses, age, n.unmet,
# avg_class_size, n.writing, n.diversity, n.engl_comp, n.qsr, n.vlpa, n.indiv_soc, n.nat_world,
# sum.fees, n.alt.grading, csum.alt.grading, stem.courses,

# get.current.qtr <- function(YRQ_NOW){}

con <- dbConnect(odbc(), 'sqlserver01')

curr.reg <- tbl(con, in_schema('sec', 'registration')) %>%
  mutate(yrq = regis_yr*10 + regis_qtr) %>%
  filter(yrq == YRQ_NOW)

reg.courses <-