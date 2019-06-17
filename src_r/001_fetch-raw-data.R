rm(list = ls())
gc()

library(tidyverse)
library(dbplyr)
library(odbc)

setwd(rstudioapi::getActiveProject())

source("src_r/config.R")
con <- dbConnect(odbc::odbc(), config$dns, Database = config$sdb, UID = config$uid, PWD = rstudioapi::askForPassword())
edw <- dbConnect(odbc::odbc(), config$dns, Database = config$edw, UID = config$uid, PWD = rstudioapi::askForPassword())

# students ----------------------------------------------------------------

# stu <- tbl(con, in_schema("sec", "student_1")) %>%
#   select(system_key, spcl_program, honors_program, class, enroll_status)

# stu.mm <- tbl(con, in_schema("sec", "sr_mini_master")) %>%
#   select(mm_system_key, mm_year, mm_qtr, mm_proc_ind, mm_birth_date, mm_sex, mm_resident, mm_spcl_program,
#          mm_honors_program, mm_class, mm_ncr_code, starts_with("mm_hs"), mm_hs_gpa = mm_high_sch_gpa,
#          mm_enroll_status, mm_schol_type, mm_tot_credits, mm_ethnic_code, mm_hispanic_code) %>%
#   filter(mm_class >= 1 & mm_class <= 4, mm_qtr != 3, mm_year >= 2008, mm_proc_ind == 2) %>%
#   collect()
#
# stu.mm <- stu.mm %>% mutate(yrq = mm_year * 10 + mm_qtr) %>% filter(yrq >= 20084)
#
# stu.grp <- tbl(con, in_schema("sec", "sr_student_grp")) %>%
#   # select() %>%´
#   collect()# %>%
#   mutate(fyrq = stu_grp_first_yr * 10 + stu_grp_first_qtr) %>%
#   filter(fyrq >= 20084)

# Fetch current year + qtr
currentq <- tbl(con, in_schema("sec", "sdbdb01")) %>%
  select(current_yr, current_qtr) %>%
  collect() %>%
  mutate(current_yrq = current_yr*10 + current_qtr)

dimstu <- tbl(edw, in_schema("sec", "dimStudent")) %>%
  filter(StudentClassCode %in% 1:4 | StudentClassGroupShortDesc == "UG") %>%
  select(StudentKeyId, SDBSrcSystemKey, BirthDate, EthnicGrpAfricanAmerInd, EthnicGrpAfricanAmerInd, EthnicGrpAsianInd,
         EthnicGrpCaucasianInd, EthnicGrpHawaiiPacIslanderInd, EthnicGrpMultipleInd, EthnicGrpNotIndicatedInd,
         GenderCode, HispanicInd, InternationalStudentInd, ResidentDesc, StudentClassCode) %>%
  collect()


# quarterly transcript ----------------------------------------------------

transcript <- tbl(con, in_schema("sec", "transcript")) %>% # top_n(n = 1, wt = tran_yr) %>% collect()
  filter(class %in% 1:4, tran_yr >= 2006, add_to_cum == 1) %>%
  select(system_key, tran_yr, tran_qtr, resident, veteran, class, special_program, honors_program,
         tenth_day_credits, scholarship_type, yearly_honor_type,num_ind_study, num_courses,
         enroll_status, qtr_grade_points, qtr_graded_attmp, over_qtr_grade_pt, over_qtr_grade_at,
         qtr_nongrd_earned, over_qtr_nongrd, qtr_deductible, over_qtr_deduct) %>%
  collect() %>%
  mutate(yrq = tran_yr*10 + tran_qtr) %>%
         # qgpa = qtr_grade_points / qtr_graded_attmp,
         # probe = if_else(scholarship_type == 3, 1, 0)) %>%
  filter(yrq >= 20064)


# Majors ------------------------------------------------------------

mjr <- tbl(con, in_schema("sec", "transcript_tran_col_major")) %>% filter(tran_yr >= 2006) %>% collect()

# grads/degrees granted ---------------------------------------------------
degrees <- tbl(con, in_schema("sec", "student_2_uw_degree_info")) %>%
  filter(deg_earned_yr >= 2006,
         deg_level == 1,
         deg_type %in% 1:8,
         deg_status == 9) %>%
  collect() %>%
  mutate(deg_yrq = deg_earned_yr*10 + deg_earned_qtr) %>%
  filter(deg_yrq >= 20062)


# xfer students -----------------------------------------------------------

# transfers <- tbl("con", in_schema("sec", "sr_transfer")) %>%
#   select(system_key) %>%


# first time first year students ------------------------------------------

# first freshman applications
appl.ftfy <- tbl(con, in_schema("sec", "sr_adm_appl")) %>%
  filter(appl_type == "1", # %in% c("1", "2", "4", "5", "6"),
         appl_yr >= 2006,
         ncr_code == 1) %>%
  select(system_key, appl_type, appl_no, appl_qtr, appl_yr, appl_status) %>%
  collect()
# appl_type:
  # 1	FRESHMAN
  # 2	2YR TRANSFER
  # 4	4YR TRANSFER
  # 5	POSTBAC
  # 6	UG RET ON LV
  # 7	GR RET ON LV

# student_2 is created when a student is admitted - contains first yr/qtr, which isn't in _1
stu2.first.reg <- tbl(con, in_schema("sec", "student_2")) %>%
  filter(first_yr_regis >= 2006) %>%
  select(system_key, first_yr_regis, first_qtr_regis) %>%
  # inner_join(appl_ftfy, copy = T) %>%
  collect()


# student_1 merges with appl_yr/qtr/num -----------------------------------

stu1 <- tbl(con, in_schema("sec", "student_1")) %>%
  filter(last_yr_enrolled >= 2004) %>%
  select(system_key, s1_gender, birth_dt, last_yr_enrolled, last_qtr_enrolled, admitted_for_yr, admitted_for_qtr,
         current_appl_yr, current_appl_qtr, current_appl_no, resident, high_sch_ceeb_cd, ethnic_code, hispanic_code,
         child_of_alum, running_start, college_in_hs, s1_high_satv, s1_high_satm, s1_high_act) %>%
  collect()

# application data --------------------------------------------------------

# create an in-db filtering file
fil <- tbl(con, in_schema("sec", "student_1")) %>%
  select(system_key, appl_yr = current_appl_yr, appl_qtr = current_appl_qtr, appl_no = current_appl_no) %>%
  distinct()

appl.income <- tbl(con, in_schema("sec", "sr_adm_appl_income_data")) %>% semi_join(fil) %>% collect()

appl <- tbl(con, in_schema("sec", "APPLHISTApplication")) %>%
  filter(appl_type %in% c(1, 2, 4, 6, "R"),
         appl_yr >= 2003,
         appl_status %in% c(11, 12, 15, 16, 26)) %>%
  select(system_key, appl_yr, appl_qtr, appl_no, appl_type, appl_status, ai_college, ai_high_sch, appl_branch, athlete_code,
         class, conditional, deg_lvl_goal, eop_group, high_sch_code, high_sch_gpa,
         # hs_def_arts, hs_def_english, hs_def_for_lang, hs_def_math, hs_def_science, hs_def_soc_sci,
         hs_for_lang_type, hs_for_lang_yrs, hs_math_level, hs_yrs_arts, hs_yrs_for_lang, hs_yrs_math, hs_yrs_science, hs_yrs_soc_sci,
         home_addr_code, last_school_type, ncr_code, special_program, trans_gpa, aa_degree, direct_transfer,
         best_satr_v, best_satr_m, best_satr_c) %>%
  semi_join(fil) %>%
  collect()

appl.guardian <- tbl(con, in_schema("sec", "sr_adm_appl_guardian_data")) %>% semi_join(fil) %>% collect()
appl.req.major <- tbl(con, in_schema("sec", "sr_adm_appl_req_col_major")) %>% semi_join(fil) %>% collect()
appl.init.major <- tbl(con, in_schema("sec", "sr_adm_appl_college_major")) %>% semi_join(fil) %>% collect()


# reduce raw data -> dataset -------------------------------------------------------------------
rm(con, fil, edw, config)

# save(transcript, dimstu, mjr, degrees, appl_ftfy, stu2.first.reg, stu1, currentq, file = "data/raw-data.RData")

# Processing:

