rm(list = ls())
gc()


library(caret)
library(doMC)
library(tidyverse)

registerDoMC(cores = 4)
set.seed(4567)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 500)

Cs <- function(...){as.character(sys.call())[-1]}
# take a data.frame and colname -> return the index. Mostly intended for subsetting `y` var(s)
varwhich <- function(df, x){ return( which(names(df) == x) ) }

setwd(rstudioapi::getActiveProject())

load("data/merged-dataset.RData")

# pre-processing ----------------------------------------------------------


dat <- mrg.dat %>%
  filter(!is.na(qgpa)) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(l1.qgpa = lag(qgpa, n = 1),
         l2.qgpa = lag(wgpa, n = 2),
         l1.cum.gpa = lag(cum.gpa, n = 1),
         l1.probe = lag(probe),
         l1.qgpa15 = lag(qgpa15),
         l1.qgpa20 = lag(qgpa20),
         l1.cum.pts = lag(cum.pts),
         l1.cum.attmp = lag(cum.attmp),
         ) %>%
  ungroup() %>%
  mutate(qgpa15 = factor(qgpa15, levels = c(0, 1)),
         probe = factor(probe, levels = c(0,1)),
         scholarship_type = factor(scholarship_type),
         honors_program = factor(honors_program),
         yearly_honor_type = factor(yearly_honor_type))

# removing variables observed after grades
dat <- dat %>%
  select(system_key, yrq, qgpa15, class, honors_program, tenth_day_credits, scholarship_type, num_ind_study, num_courses, attmp, nongrd, deduct,
         tot_creds, starts_with("l1."), starts_with("l2."), s1_gender, child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act,
         s1_high_act, starts_with("Ethnic"), HispanicInd, InternationalStudentInd, ResidentDesc, age, trans_gpa, aa_degree, direct_transfer,
         conditional, provisional, with_distinction, res_in_question, low_family_income, appl_class, high_sch_gpa, starts_with("hs_"),
         last_school_type, reg.late.days, reg.late.binary, tran_branch, tran_major_abbr, major.change, major.change.count, n.unmet,
         ft, ft.creds.over)