rm(list = ls())
gc()

library(tidyverse)
library(dbplyr)
library(odbc)

setwd(rstudioapi::getActiveProject())

source("src-r/config.R")
con <- dbConnect(odbc::odbc(), config$dns, Database = config$db, UID = config$uid, PWD = rstudioapi::askForPassword("pwd-"))
edw <- dbConnect(odbc::odbc(), config$dns, Database = "EDWPresentation", UID = config$uid, PWD = rstudioapi::askForPassword("pwd-"))

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


cal <- tbl(edw, in_schema("sec", "dimDate")) %>%
  filter(AcademicQtrKeyId >= 20064, AcademicQtrCensusDayInd == "Y") %>%
  select(CalendarDateKeyId, AcademicQtrKeyId)

currentq <- tbl(con, in_schema("sec", "sdbdb01")) %>%
  select(current_yr, current_qtr) %>%
  collect()

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


# Majors (edw) ------------------------------------------------------------

# mjr <- tbl(edw, in_schema("sec", "dimMajor")) %>% collect()
mjr <- tbl(con, in_schema("sec", "transcript_tran_col_major")) %>% filter(tran_yr >= 2006) %>% collect()

# grads/degrees granted ---------------------------------------------------
degrees <- tbl(con, in_schema("sec", "student_2_uw_degree_info")) %>%
  filter(deg_earned_yr >= 2006,
         deg_level == 1,
         deg_type %in% 1:8,
         deg_status == 9) %>%
  # select(system_key, index1, deg_branch, deg_college, deg_grad_honor,
  #        deg_earned_yr, deg_earned_qtr, deg_gpa, deg_uw_credits,
  #        deg_trans_credits, deg_exten_credits) %>%
  collect() %>%
  mutate(deg_yrq = deg_earned_yr*10 + deg_earned_qtr) %>%
  filter(deg_yrq >= 20062)


# xfer students -----------------------------------------------------------

# transfers <- tbl("con", in_schema("sec", "sr_transfer")) %>%
#   select(system_key) %>%


# first time first year students ------------------------------------------

# first freshman applications
appl_ftfy <- tbl(con, in_schema("sec", "sr_adm_appl")) %>%
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

# student_2 is created when a student is admitted
stu2.first.reg <- tbl(con, in_schema("sec", "student_2")) %>%
  filter(first_yr_regis >= 2006) %>%
  select(system_key, first_yr_regis, first_qtr_regis) %>%
  # inner_join(appl_ftfy, copy = T) %>%
  collect()


# student_1 merges with appl_yr/qtr/num -----------------------------------

stu1 <- tbl(con, in_schema("sec", "student_1")) %>%
  filter(class <= 4) %>%
  select(system_key, current_appl_yr, current_appl_qtr, current_appl_no, s1_gender, ethnic_code, hispanic_code, resident) %>%
  collect()


# write -------------------------------------------------------------------

save(transcript, dimstu, mjr, degrees, appl_ftfy, stu2.first.reg, stu1, cal, currentq, file = "data/raw-data.RData")
