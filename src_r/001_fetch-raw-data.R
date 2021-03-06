rm(list = ls())
gc()

library(tidyverse)
library(dbplyr)
library(odbc)

setwd(rstudioapi::getActiveProject())


# EXTRACT -----------------------------------------------------------------
# YAML + keyring
x <- config::get("sdb", file = "config.yml")
con <- dbConnect(odbc::odbc(), x$dns, Database = x$db, UID = x$uid, PWD = keyring::key_get("sdb"))
x <- config::get("edw", file = "config.yml")
edw <- dbConnect(odbc::odbc(), x$dns, Database = x$db, UID = x$uid, PWD = keyring::key_get("sdb"))
rm(x)

# get > current yr+qtr -----------------------------------------------------

currentq <- tbl(con, in_schema("sec", "sdbdb01")) %>%
  select(current_yr, current_qtr) %>%
  collect() %>%
  mutate(current_yrq = current_yr*10 + current_qtr)

# get > create filtering query of EOP student ids --------------------------

# student_2 is created when a student is admitted - contains first yr/qtr, which isn't in _1
yrq1 <- tbl(con, in_schema("sec", "student_2")) %>%
  select(system_key, first_yr_regis, first_qtr_regis) %>%
  # inner_join(appl_ftfy, copy = T) %>%
  # collect() %>%
  mutate(first.yrq = first_yr_regis*10 + first_qtr_regis) %>%
  filter(first.yrq >= 20064)

eop.codes <- c(1, 2, 13, 14, 16, 17, 31, 32, 33)
db.eop <- tbl(con, in_schema("sec", "transcript")) %>%
  filter(special_program %in% eop.codes) %>%
  select(system_key) %>%
  distinct() %>%
  semi_join(yrq1) # %>%
  # collect()

rm(yrq1)

dimstu <- tbl(edw, in_schema("sec", "dimStudent")) %>%
  filter(StudentClassCode %in% 1:4 | StudentClassGroupShortDesc == "UG") %>%
  select(SDBSrcSystemKey, EthnicGrpAfricanAmerInd, EthnicGrpAfricanAmerInd, EthnicGrpAsianInd,
         EthnicGrpCaucasianInd, EthnicGrpHawaiiPacIslanderInd, EthnicGrpMultipleInd, EthnicGrpNotIndicatedInd,
         GenderCode, HispanicInd, InternationalStudentInd, ResidentDesc, starts_with("Record")) %>%
  semi_join(db.eop, by = c("SDBSrcSystemKey" = "system_key"), copy = T) %>%
  collect() %>%
  group_by(SDBSrcSystemKey) %>%
  filter(RecordEffEndDttm == max(RecordEffEndDttm)) %>%
  select(-starts_with("Record"))


# get > quarterly transcript ----------------------------------------------------

transcript <- tbl(con, in_schema("sec", "transcript")) %>%
  filter(class %in% 1:4, tran_yr >= 2006, add_to_cum == 1) %>%
  select(system_key, tran_yr, tran_qtr, resident, veteran, class, special_program, honors_program,
         tenth_day_credits, scholarship_type, yearly_honor_type,num_ind_study, num_courses,
         enroll_status, qtr_grade_points, qtr_graded_attmp, over_qtr_grade_pt, over_qtr_grade_at,
         qtr_nongrd_earned, over_qtr_nongrd, qtr_deductible, over_qtr_deduct) %>%
  semi_join(db.eop) %>%
  collect() %>%
  mutate(yrq = tran_yr*10 + tran_qtr) %>%
         # qgpa = qtr_grade_points / qtr_graded_attmp,
         # probe = if_else(scholarship_type == 3, 1, 0)) %>%
  filter(yrq >= 20064)

courses.taken <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
  filter(tran_yr >= 2006) %>%
  select(system_key, tran_yr, tran_qtr, index1, dept_abbrev, course_number, course_credits, course_branch,
         summer_term, grade, duplicate_indic, repeat_course, honor_course) %>%
  semi_join(db.eop) %>%
  collect() %>%
  mutate(yrq = tran_yr*10 + tran_qtr) %>%
  filter(yrq >= 20064)


# get > student majors ------------------------------------------------------------

mjr <- tbl(con, in_schema("sec", "transcript_tran_col_major")) %>%
  filter(tran_yr >= 2006) %>%
  semi_join(db.eop) %>%
  collect()

# [unused] get > grads/degrees granted ---------------------------------------------------
  # degrees <- tbl(con, in_schema("sec", "student_2_uw_degree_info")) %>%
  #   filter(deg_earned_yr >= 2006,
  #          deg_level == 1,
  #          deg_type %in% 1:8,
  #          deg_status == 9) %>%
  #   semi_join(db.eop) %>%
  #   collect() %>%
  #   mutate(deg_yrq = deg_earned_yr*10 + deg_earned_qtr) %>%
  #   filter(deg_yrq >= 20062)

# [unused] get > first time first year students ------------------------------------------
#
# # first freshman applications
# appl.ftfy <- tbl(con, in_schema("sec", "sr_adm_appl")) %>%
#   filter(appl_type == "1", # %in% c("1", "2", "4", "5", "6"),
#          appl_yr >= 2006,
#          ncr_code == 1) %>%
#   select(system_key, appl_type, appl_no, appl_qtr, appl_yr, appl_status) %>%
#   semi_join(db.eop) %>%
#   collect()
# # appl_type:
#   # 1	FRESHMAN
#   # 2	2YR TRANSFER
#   # 4	4YR TRANSFER
#   # 5	POSTBAC
#   # 6	UG RET ON LV
#   # 7	GR RET ON LV

# get > student_1 merges with appl_yr/qtr/num -----------------------------------

stu1 <- tbl(con, in_schema("sec", "student_1")) %>%
  filter(last_yr_enrolled >= 2004) %>%
  select(system_key, s1_gender, birth_dt, last_yr_enrolled, last_qtr_enrolled, admitted_for_yr, admitted_for_qtr,
         current_appl_yr, current_appl_qtr, current_appl_no, high_sch_ceeb_cd, ethnic_code, hispanic_code,
         child_of_alum, running_start, s1_high_satv, s1_high_satm, s1_high_act) %>%
  semi_join(db.eop) %>%
  collect()

# get > application records (multiple tables) --------------------------------------------------------

# create an in-db filtering file for the applications
app.filter <- tbl(con, in_schema("sec", "student_1")) %>%
  inner_join(db.eop) %>%
  select(system_key, appl_yr = current_appl_yr, appl_qtr = current_appl_qtr, appl_no = current_appl_no) %>%
  distinct()

appl.income <- tbl(con, in_schema("sec", "sr_adm_appl_income_data")) %>% semi_join(app.filter) %>% collect()

sr.appl <- tbl(con, in_schema("sec", "sr_adm_appl")) %>%
  filter(appl_type %in% c(1, 2, 4, 6, "R"),
         appl_yr >= 2003,
         appl_status %in% c(11, 12, 15, 16, 26)) %>%
  select(system_key, appl_yr, appl_qtr, appl_no, appl_type, appl_status, appl_branch, athlete_code,
         class, conditional, deg_lvl_goal, eop_group, high_sch_gpa,
         hs_for_lang_type, hs_for_lang_yrs, hs_yrs_for_lang, hs_math_level, hs_yrs_math, hs_yrs_arts,
         hs_yrs_science, hs_yrs_soc_sci,hs_yrs_english, hs_esl_engl,
         home_addr_code, last_school_type, ncr_code, special_program,
         trans_gpa, aa_degree, direct_transfer,
         appl_cohort, provisional, with_distinction, res_in_question, low_family_income) %>%
  semi_join(app.filter) %>%
  collect()

# best test scores
appl.hist <- tbl(con, in_schema("sec", "APPLHistApplication")) %>%
  filter(appl_type %in% c(1, 2, 4, 6, "R"),
         appl_yr >= 2003,
         appl_status %in% c(11, 12, 15, 16, 26)) %>%
  select(system_key, appl_yr, appl_qtr, appl_no, best_satr_v, best_satr_m, best_satr_c) %>%
  semi_join(app.filter) %>%
  collect()



appl.guardian <- tbl(con, in_schema("sec", "sr_adm_appl_guardian_data")) %>% semi_join(app.filter) %>% collect()
appl.req.major <- tbl(con, in_schema("sec", "sr_adm_appl_req_col_major")) %>% semi_join(app.filter) %>% collect()
appl.init.major <- tbl(con, in_schema("sec", "sr_adm_appl_college_major")) %>% semi_join(app.filter) %>% collect()


# get > unmet course add requests -----------------------------------------------

unmet <- tbl(con, in_schema("sec", "sr_unmet_request")) %>%
  filter(unmet_yr >= 2006) %>%
  collect() %>%
  mutate(yrq = unmet_yr*10 + unmet_qtr) %>%
  filter(yrq >= 20064) %>%
  select(system_key = unmet_system_key, yrq) %>%
  group_by(system_key, yrq) %>%
  summarize(n.unmet = n())
# just curious
unmet %>% filter(yrq %% 10 != 3) %>% group_by(yrq) %>% summarize(y = sum(n.unmet)) %>% ggplot(., aes(x = yrq, y = y)) + geom_line()
unmet %>% filter(yrq %% 10 != 3) %>% group_by(yrq) %>% summarize(y = n_distinct(system_key)) %>% ggplot(., aes(x = yrq, y = y)) + geom_line()
# unmet %>% filter(yrq %% 10 != 3) %>% group_by(yrq) %>% summarize(y = median(n.unmet)) %>% ggplot(., aes(x = yrq, y = y)) + geom_line()          # haha
unmet %>% filter(yrq %% 10 != 3) %>% group_by(yrq) %>% summarize(y = mean(n.unmet)) %>% ggplot(., aes(x = yrq, y = y)) + geom_line()
# So the number of students and courses are going up but mean is going down



# get > calendar + registration_courses for early/late registration -------------

cal <- tbl(con, in_schema("sec", "sys_tbl_39_calendar")) %>%
  filter(first_day >= "2000-01-01") %>%                           # arbitrary, some kind of limit is helpful
  select(table_key, first_day, tenth_day, last_day_add) %>%
  #collect() %>%
  mutate(yrq = as.numeric(table_key),
         regis_yr = round(yrq %/% 10, 0),        # r floor div operator works incorrectly in sql tbl, presumably b/c of the % but I'm not looking it up right now
         regis_qtr = yrq %% 10)

# this table is potentiallly huge so I'm doing some transformations in place (it will still be huge)
# min add dt
#
# query <- "with dateconvert as(
#   select system_key,
#   regis_yr,
#   regis_qtr,
#   add_dt_tuit,
#   percentile_disc(0.5) within group (order by add_dt_tuit)
#   over(partition by system_key, regis_yr, regis_qtr) as median_add_dt
#   from UWSDBDataStore.sec.registration_courses
#   )
#   select distinct system_key,
#   regis_yr,
#   regis_qtr,
#   add_dt_tuit
#   into ##med_reg_dates
#   from dateconvert
#   where add_dt_tuit = median_add_dt"
#
# # med.dates <-
# DBI::dbGetQuery(con, query)

# regc <- tbl(con, in_schema("sec", "registration_courses")) %>%      # <<--- First add date can be derived here
#   semi_join(db.eop) %>%
#   mutate(yrq = regis_yr*10 + regis_qtr) %>%
#   filter(yrq >= 20064) %>%
#   select(system_key, yrq, add_dt_tuit) %>%
#   group_by(system_key, yrq) %>%
#   filter(add_dt_tuit == min(add_dt_tuit)) %>%
#   distinct() %>%
#   # summarize(med.date = median(add_dt_tuit)) %>%
#   # mutate(pct = percent_rank(add_dt_tuit))
#   # mutate(x = as.numeric(add_dt_tuit)) %>%
#   left_join(cal) %>%
#   collect() %>%
#   mutate(reg.lateness = as.numeric(difftime(add_dt_tuit, first_day, units = "days")))
# regc <- tbl(con, "##med_reg_dates") %>%
#   semi_join(db.eop) %>%
#   mutate(yrq = regis_yr*10 + regis_qtr) %>%
#   filter(yrq >= 20064) %>%
#   select(system_key, yrq, add_dt_tuit) %>%
#   left_join(cal) %>%
#   collect() %>%
#   mutate(reg.late.days = as.numeric(difftime(add_dt_tuit, first_day, units = "days")))

# "fixes" still don't eliminate the extremely off dates, so I'll stick with the min and correct the ones I see right now
regc <- tbl(con, in_schema("sec", "registration_courses")) %>%
  semi_join(db.eop) %>%
  mutate(yrq = regis_yr*10 + regis_qtr) %>%
  filter(yrq >= 20064) %>%
  select(system_key, yrq, add_dt_tuit) %>%
  group_by(system_key, yrq) %>%
  filter(add_dt_tuit == min(add_dt_tuit)) %>%
  distinct() %>%
  left_join(cal) %>%
  collect() %>%
  mutate(reg.late.days = as.numeric(difftime(add_dt_tuit, first_day, units = "days")))

regc$reg.late.days[regc$reg.late.days <= -365] <- median(regc$reg.late.days[regc$reg.late.days < 0])
regc$reg.late.binary <- if_else(regc$reg.late.days > 0, 1, 0)


# TRANSFORMATIONS -------------------------------------------------------------------


# xform > application tables ----------------------------------------------

xf.guardian.ed <- appl.guardian %>%
  group_by(system_key) %>%
  summarize(guardian.ed.max = max(guardian_ed_level),
            guardian.ed.min = min(guardian_ed_level)) %>%
  ungroup()

xf.income <- appl.income %>%
  mutate(yrq = appl_yr*10 + appl_qtr) %>%
  group_by(system_key, yrq) %>%
  summarize(income_dependent_true = if_else(all(income_dependent == T), T, F),
            income_gross_median = median(income_gross, na.rm = T),
            income_fam_size_median = median(income_fam_size, na.rm = T),
            income_fam_ratio = income_gross_median / income_fam_size_median) %>%
  ungroup()
# best.sat <- appl.hist %>%
#   group_by(system_key) %>%
#   summarize_at(vars(starts_with('best')), mean, na.rm = T)
# I'll use the student_1 version in this case but that means that the zeroes need to be converted to missing

# Only use the first appl init major. See the following
sum(appl.init.major$index1 == 1)
sum(appl.init.major$index1 >= 2)

xf.init.placement.major <- appl.init.major %>%
  group_by(system_key) %>%
  select(major_abbr) %>%
  slice(1)
# in this case, there are enough 2nd/3rd requests that we might want to reshape this and keep them
# I'm going to treat the "000000" as NA b/c almost all of them occur in the 2nd/3rd choices
# that leads to _not_ keeping the 2nd/3rd options
appl.req.major$req_major_abbr[appl.req.major$req_major_abbr == "000000"] <- NA
xf.init.req.major <- appl.req.major %>%
  filter(index1 == 1) %>%
  select(system_key, req_major_abbr)

# main application table
xf.sr.appl <- sr.appl %>%
  select(system_key, trans_gpa, aa_degree, direct_transfer, conditional, provisional, with_distinction,
         res_in_question, low_family_income, appl_class = class, high_sch_gpa, starts_with("hs_"), last_school_type,
         -hs_esl_engl) %>%
  mutate(trans_gpa = na_if(trans_gpa, 0),
         high_sch_gpa = na_if(high_sch_gpa, 0))


# xform > student_1, dimStu (EDW) -----------------------------------------

# dimstu + student_1
xf.student <- stu1 %>%
  inner_join(dimstu, by = c("system_key" = "SDBSrcSystemKey")) %>%
  mutate(age = as.numeric(difftime(Sys.Date(), birth_dt, units = "days")) / 364.25)


# xform > majors ----------------------------------------------------------

xf.n.majors <- mjr %>%
  group_by(system_key) %>%
  summarize(n_distinct(tran_major_abbr))

xf.stu.major <- mjr %>%
  mutate_if(is.character, trimws) %>%
  mutate(yrq = tran_yr*10 + tran_qtr) %>%
  group_by(system_key) %>%
  filter(index1 == 1) %>%
  select(system_key, yrq, tran_branch, tran_major_abbr) %>%
  mutate(major.change = if_else(tran_major_abbr == lag(tran_major_abbr, order_by = yrq), 0, 1, missing = 0),
         major.change.count = cumsum(major.change))

# xform > transcript ------------------------------------------------------

xf.trs <- transcript %>%
  # exclude summer qtr
  filter(tran_qtr != 3) %>%
  mutate(pts = pmax(qtr_grade_points, over_qtr_grade_pt),
         attmp = pmax(qtr_graded_attmp, over_qtr_grade_at),
         nongrd = pmax(qtr_nongrd_earned, over_qtr_nongrd),
         deduct = pmax(qtr_deductible, over_qtr_deduct),
         qgpa = pts / attmp,
         tot_creds = attmp + nongrd - deduct,
         qgpa15 = if_else(qgpa <= 1.5, 1, 0),
         qgpa20 = if_else(qgpa <= 2, 1, 0),
         probe = if_else(scholarship_type == 3, 1, 0)) %>%
  select(system_key, tran_yr, tran_qtr, yrq, class, honors_program, tenth_day_credits,
         scholarship_type, yearly_honor_type, num_ind_study, num_courses, pts, attmp,
         nongrd, deduct, qgpa, tot_creds, qgpa15, qgpa20, probe) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(cum.pts = cumsum(pts),
         cum.attmp = cumsum(attmp),
         cum.gpa = cum.pts / cum.attmp) %>%
  ungroup()

xf.trs.courses <- courses.taken %>%
  # filter(tran_qtr != 3) %>%
  mutate_if(is.character, trimws) %>%
  mutate(course = paste(dept_abbrev, course_number, sep = "_"),
         duplicate_indic = as.numeric(duplicate_indic)) %>%
  rename(course.index = index1) %>%
  select(-tran_yr, -tran_qtr)

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
xf.trs.courses$course.withdraw <- ifelse(grepl("W", xf.trs.courses$grade), 1, 0)
xf.trs.courses$course.nogpa <- ifelse(xf.trs.courses$grade %in% c("", "CR", "H", "HP", "HW", "I", "N", "NC", "NS", "P", "S", "W", "W3", "W4", "W5", "W6", "W7"), 1, 0)
# table(xf.trs.courses$grade, xf.trs.courses$course.nogpa)
# xform > late registrations ----------------------------------------------

xf.late.reg <- regc %>%
  select(system_key, yrq, reg.late.days, reg.late.binary)

# xform > unmet course requests -------------------------------------------

xf.unmet.requests <- unmet

# xform > misc stuff
# do we assume missing running start is a 'No'?
# to.new.val <- function(x, val, newval){
#   x[x == val] <- newval
#   x
# }
(i <- sapply(xf.student, is.character))
apply(xf.student[,i], 2, unique)

xf.student$running_start[xf.student$running_start == " "] <- "N"
xf.student$s1_gender[xf.student$s1_gender == " "] <- NA
xf.student <- subset(xf.student, select = -c(GenderCode, ethnic_code, hispanic_code, high_sch_ceeb_cd, current_appl_yr,
                                             current_appl_qtr, current_appl_no, admitted_for_yr, admitted_for_qtr,
                                             birth_dt, last_yr_enrolled, last_qtr_enrolled))

# [TODO] age isn't technically correctly coded for transcript records. Need to correct this.

# COMBINE -----------------------------------------------------------------

# [TODO] a functional way to merge all these dataframes for quick modifications to the included data tables

# combine > tidy up env --------------------------------------------------------------

rm(list = setdiff(objects(), objects(pattern = "xf.")))       # ls() works fine too. BONUS CHALLENGE: wrap this in a function XP

# combine > merge datasets beginning w/ the 'wide' application data -------------------------
# I'm doing in this in more steps that are strictly necessary in order to facilitate in-place/imperative
# validation as needed
# e.g. this potential problem of making extra work later using ML models that aren't robust to missing inputs
# which depends on one's comfort w/ complete.cases v. more records w/ fewer columns <img-hypercube>
n.in <- function(source, target){   # source typically ought to be the known/a priori more complete of the two
  su <- unique(source)
  tu <- unique(target)        # list input would be nice
  over <- sum(su %in% tu)

  print(paste("src:", length(su), sep = " "))
  print(paste("targ:", length(tu), sep = " "))
  print(paste("overlap:", over, sep = " "))
  print(paste("missing from target:", length(su) - over, sep = " "))
}
n.in(xf.sr.appl$system_key, xf.income$system_key)
n.in(xf.sr.appl$system_key, xf.guardian.ed$system_key)
n.in(xf.sr.appl$system_key, xf.init.placement.major$system_key)
n.in(xf.sr.appl$system_key, xf.init.req.major$system_key)

# mrg.appl <- xf.trs %>%
#   left_join(xf.guardian.ed) %>%
#   left_join(xf.income) %>%
#   left_join(xf.init.placement.major) %>%
#   left_join(xf.init.req.major)

# Let's focus on the transcript + main appl file
n.in(xf.sr.appl$system_key, xf.trs$system_key)

mrg.dat <- xf.trs %>%
  inner_join(xf.student) %>%
  left_join(xf.sr.appl) %>%
  left_join(xf.late.reg) %>%
  left_join(xf.stu.major) %>%
  left_join(xf.unmet.requests)

# combine > create some initial features ----------------------------------

# FT = 12 credits
mrg.dat$ft <- ifelse(mrg.dat$tenth_day_credits >= 12, T, F)
mrg.dat$ft.creds.over <- ifelse(mrg.dat$tenth_day_credits >= 12,
                                mrg.dat$tenth_day_credits - 12, 0)
mrg.dat$n.unmet[is.na(mrg.dat$n.unmet)] <- 0
mrg.dat$s1_high_act[mrg.dat$s1_high_act == 0] <- NA
mrg.dat$s1_high_satm[mrg.dat$s1_high_satm == 0] <- NA
mrg.dat$s1_high_satv[mrg.dat$s1_high_satv == 0] <- NA

# save --------------------------------------------------------------------

save(mrg.dat, file = "data/merged-dataset.RData")
# may use these later
save(courses.taken, xf.trs.courses, file = 'data/courses-taken.RData')
