rm(list = ls())
gc()

setwd(rstudioapi::getActiveProject())

library(tidyverse)
library(dbplyr)
library(odbc)
library(edwHelpers)

options(tibble.print_min = 500)

load("data/merged-dataset.RData")
load("data/courses-taken.RData")



# correction to credits earned v attempted in xf.trs.courses
xf.trs.courses$course_credits_earned <- ifelse(xf.trs.courses$grade %in% c("", "00", "F", "I", "NC", "HW", "W", "W3", "W4", "W5", "W6", "W7"), 0, xf.trs.courses$course_credits) # no W2

# fetch additional data --------------------------------------------

cf <- config::get("sdb", file = "config.yml")
con <- dbConnect(odbc::odbc(), cf$dns, Database = cf$db, UID = cf$uid, PWD = keyring::key_get("sdb"))
rm(cf)

# keys
sk <- data.frame('system_key' = unique(courses.taken$system_key))
# [TODO] add as temp table for queries; will require some re-factoring
copy_to(con, sk, '##syskeys', overwrite = T)
# and finally, make a reference:
syskeys <- tbl(con, '##syskeys')

# transcripts actually need the section to merge with other tables later
tran.courses.taken <- tbl(con, in_schema('sec', 'transcript_courses_taken')) %>%
  inner_join(syskeys) %>%
  select(system_key, tran_yr, tran_qtr, index1, dept_abbrev, course_number, section_id, course_credits, course_branch, summer_term, grade, duplicate_indic,
         repeat_course, honor_course, grade_system) %>%
  collect() %>%
  mutate(yrq = (tran_yr*10) + tran_qtr,
         course.alt.grading = ifelse(grade_system > 0, 1, 0),
         duplicate_indic = as.numeric(duplicate_indic)) %>%
  mutate_if(is.character, trimws) %>%
  select(-tran_yr, -tran_qtr, -grade_system) %>%
  rename(course.index = index1)

xf.trs.courses <- xf.trs.courses %>% inner_join(tran.courses.taken)


# course CIP/STEM indicator
# A course is counted as a "Science, Technology,
# Engineering, and Mathematics (STEM) Course" if it belongs to a Curriculum that
# is assigned a CIP Code that has been classified as STEM by the Washington
# State Office of Financial Management (OFM).

stem.courses <- tbl(con, in_schema("EDWPresentation.sec", "dmSCH_dimCurriculumCourse")) %>%
  filter(FederalSTEMInd == "Y") %>%
  select(dept_abbrev = CurriculumCode,
         course_number = CourseNbr,
         course_level = CourseLevelNbr,
         cip = CIPCode) %>%
  distinct() %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(course = paste(dept_abbrev, course_number, sep = "_"),
         is.stem = 1)

# # UW degrees (for retention)
# degress <- tbl(con, in_schema('sec', 'student_2_uw_degree_info')) %>%
#   filter(deg_level == 1,
#          deg_earned_yr >= 2008) %>%
#   select(system_key, index1, deg_level, deg_earned_yr, deg_earned_qtr) %>%
#   inner_join(sk, copy = T) %>%
#   collect() %>%
#   mutate(degree_yrq = (deg_earned_yr*10) + deg_earned_qtr)
#

# course tags (qsr, vlpa, etc)
sched <- tbl(con, in_schema("sec", "time_schedule")) %>% # filter(dept_abbrev == "SISLA", course_no == 355) %>% collect()
  filter(ts_year >= 2008) %>%
  select(ts_year, ts_quarter, course_branch, dept_abbrev, course_no, section_id, sln, current_enroll, parent_sln, current_enroll, writing_crs, diversity_crs, english_comp, qsr, vis_lit_perf_arts,
         indiv_society, natural_world, gen_elective, fee_amount) %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(yrq = (ts_year*10) + ts_quarter,
         course = paste(dept_abbrev, course_no, sep = "_"))


# student grades, credits w/in dept_abbrev -------------------------------------------
# Using tops of the ranges
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

# # choose a cutoff for n-students
# xf.trs.courses %>%
#   group_by(dept_abbrev) %>%
#   summarize(nd = n_distinct(system_key)) %>%
#   arrange(desc(nd)) %>%
#   summarize(quantile(nd, probs = .75),
#             quantile(nd, probs = .66),
#             median(nd))

# Using mean n-students as cutoff
# ct <- xf.trs.courses %>% group_by(dept_abbrev) %>% summarize(nd = n_distinct(system_key)) %>% summarize(mean(nd)) %>% deframe()
#
# # remove depts with fewer than `median` students
# stu.dept.data.wide <- xf.trs.courses %>%
#   # simplify
#   filter(course.nogpa == 0) %>%
#   select(system_key, dept_abbrev, course, numeric.grade, course_credits) %>%
#   group_by(dept_abbrev) %>%
#   # reduce total - may want to choose a different method above
#   filter(n_distinct(system_key) >= ct) %>%
#   ungroup() %>%
#   # choose highest course grade by student
#   group_by(system_key, course) %>%
#   filter(numeric.grade == max(numeric.grade)) %>%
#   ungroup() %>%
#   # fix dept_abbrev for col names make things easier later
#   mutate(dept_abbrev = str_replace(dept_abbrev, ' ', '-')) %>%
#   # calculate
#   group_by(system_key, dept_abbrev) %>%
#   summarize(grade = mean(numeric.grade, na.rm = T),
#             creds = sum(course_credits, na.rm = T)) %>%
#   # spread(dept_abbrev, g)                              # deprecated, use pivot_wider
#   pivot_wider(., id_cols = system_key,
#               names_from = dept_abbrev,
#               values_from = c(grade, creds))
#
# rm(ct)
# apply(stu.dept.data.wide, 2, function(x) sum(!is.na(x)))
# # cbind(table(round(apply(stu.dept.data.wide, 1, function(x) sum(is.na(x))) / ncol(stu.dept.data.wide), 2)))
#
# hist(stu.dept.data.wide$grade_MATH)
# hist(stu.dept.data.wide$grade_CSE)
# hist(stu.dept.data.wide$creds_MATH)
#
# j <- grepl("creds_", names(stu.dept.data.wide))
# c <- apply(stu.dept.data.wide[,j], 2, sum, na.rm = T)
# j <- names(c[order(c, decreasing = T)])[1:10]
#
# for(i in 1:length(j)){
#   d <- stu.dept.data.wide[,j[i]]
#   hist(d)
# }


# this...isn't really what we want since it isn't appropriately windowed over time per student
# might be useful some other time though



# stu dept grades and credits over time -----------------------------------------------

# instead:
# accum -> reduce to 1 row per student + dept_abbr + yrq
stu.deptwise.data <- xf.trs.courses %>%
  group_by(system_key, yrq, dept_abbrev) %>%
  summarize(sgrade = sum(numeric.grade, na.rm = T),
            n = n(),
            creds.dept = sum(course_credits, na.rm = T)) %>%
  ungroup() %>%
  arrange(system_key, dept_abbrev, yrq) %>%
  group_by(system_key, dept_abbrev) %>%
  mutate(csum.grade = cumsum(sgrade),
         nclass.dept = cumsum(n),
         cumavg.dept = csum.grade / nclass.dept,
         csum.dept.creds = cumsum(creds.dept)) %>%
  ungroup() %>%
  select(system_key, dept_abbrev, yrq, cumavg.dept, nclass.dept, csum.dept.creds) %>%
  arrange(system_key, yrq, dept_abbrev)

# pivot widely
# 196k (sparse) features is probably ~too~ many =D
# reduce to top_n departments

top.n.depts <- stu.deptwise.data %>%
  group_by(dept_abbrev) %>%
  summarize(nd = n_distinct(system_key)) %>%
  ungroup() %>%
  filter(cume_dist(nd) >= 0.90) %>%
  select(-nd)

stu.deptwise.wide <- stu.deptwise.data %>%
  inner_join(top.n.depts) %>%
  pivot_wider(.,
              id_cols = c(system_key, yrq),
              names_from = dept_abbrev,
              values_from = c(cumavg.dept, nclass.dept, csum.dept.creds),
              values_fill = list(cumavg.dept = NA,
                                 nclass.dept = 0,
                                 csum.dept.creds = 0))

## what are 'most difficult' departments for sub-200 level courses?
# xf.trs.courses %>% group_by(dept_abbrev) %>% filter(course_number < 200, !is.na(numeric.grade)) %>%
#   summarize(n = n(), mu = mean(numeric.grade), v = median(numeric.grade), s = e1071::skewness(numeric.grade), q = paste0(quantile(numeric.grade), collapse = "->")) %>%
#   filter(n >= 25) %>%
#   arrange(mu)



# repeat courses (quarterly), W's, courses w/ non-std grading ----------------------------------------------------------
# count repeats in yrq
# sum, cumsum of W's by quarter
rep.w.alt.grading <- xf.trs.courses %>%
  mutate(w = ifelse(grepl('W', xf.trs.courses$grade) == T, 1, 0)) %>%
  arrange(system_key, yrq) %>%
  group_by(system_key, yrq) %>%
  summarize(rep.courses = sum(repeat_course),
            n.w = sum(w),
            n.alt.grading = sum(course.alt.grading)) %>%
  group_by(system_key, add = F) %>%
  mutate(csum.rep.courses = cumsum(rep.courses),
         csum.w = cumsum(n.w),
         csum.alt.grading = cumsum(n.alt.grading))

# calculate quarterly STEM, GenEd by student ------------------------------

# Merge transcripts w/ stem courses then create credits, courses, and grades by student+quarter

stu.stem <- xf.trs.courses %>%
  select(system_key, yrq, course, numeric.grade, course_credits) %>%
  left_join(select(.data = stem.courses, course, is.stem)) %>%
  arrange(system_key, yrq) %>%
  group_by(system_key, yrq) %>%
  summarize(stem.courses = sum(is.stem, na.rm = T),
            stem.credits = sum(course_credits * is.stem, na.rm = T),
            avg.stem.grade = mean(numeric.grade * is.stem, na.rm = T)) %>%
  group_by(system_key, add = F) %>%
  mutate(csum.stem.courses = cumsum(stem.courses),
         csum.stem.credits = cumsum(stem.credits),
         avg.stem.grade = ifelse(is.nan(avg.stem.grade), NA, avg.stem.grade)) %>%
  ungroup()
# Should NA -> 0? recoded to something else?



# create avg course size and calc the writing/divers/qsr/etc --------------

stu.reqs <- xf.trs.courses %>%
  select(system_key, yrq, course, section_id, numeric.grade) %>%
  left_join(sched, by = c('course' = 'course', 'yrq' = 'yrq', 'section_id' = 'section_id'))


# pre-major and courses in major --------------------------------------------------------

maj <- tbl(con, in_schema('sec', 'transcript_tran_col_major')) %>%
  inner_join(syskeys) %>%
  select(system_key, tran_yr, tran_qtr, index1, tran_major_abbr) %>%
  collect() %>%
  mutate(yrq = (tran_yr*10) + tran_qtr) %>%
  mutate_if(is.character, trimws)

# it may be worthwhile to check for 'PRE' in other abbreviations
# cbind(grep('PRE', unique(maj$tran_major_abbr), value = T))

maj.w <- pivot_wider(maj, id_cols = c('system_key', 'yrq'), names_from = index1, names_prefix = 'maj_', values_from = tran_major_abbr)
# combine with courses - we're only using this to calculate courses w/ same abbrev as major right now
# part of me is wondering if collapsing the 3 majors into a single string might work better, something to look into later
# e.g.
# x <- maj.w[1:5,]
# x <- cbind(apply(x[,3:5], 1, paste0, collapse = " - "))
# grepl("PRE", xx)
#
# maj.w <- maj.w %>%
#   right_join(xf.trs.courses) %>%
#   mutate(in.major = ifelse(maj_1 == dept_abbrev | maj_2 == dept_abbrev))
# I like the other approach, w/o merging

# calc who is a pre-major
# and 'N MATR', 'T NM', 'B NM'
maj.w$majors <- apply(maj.w[,3:5], 1, paste0, collapse = " - ")
maj.w$premajor <- ifelse(grepl("PRE", maj.w$majors), 1, 0)
maj.w$nonmatric <- ifelse(grepl("N MATR | T NM | B NM", maj.w$majors), 1, 0)


# Then do the 'classes in major' by making a larger table from maj + xf.trs.courses, then reducing it
big.tb <- xf.trs.courses %>%
  select(system_key, yrq, dept_abbrev) %>%
  left_join(maj) %>%
  mutate(course.equals.major = ifelse(tran_major_abbr == dept_abbrev, 1, 0)) %>%
  group_by(system_key, yrq) %>%
  summarize(n.major.courses = sum(course.equals.major)) %>%
  group_by(system_key, add = F) %>%
  arrange(system_key, yrq) %>%
  mutate(csum.major.courses = cumsum(n.major.courses))



# Holds -------------------------------------------------------------------
# link hold to academic calendar
cal <- tbl(con, in_schema("EDWPresentation.sec", "dimDate")) %>%
  select(yrq = AcademicQtrKeyId, dt = CalendarDate)

holds <- tbl(con, in_schema("sec", "student_1_hold_information")) %>%
  inner_join(cal, by = c('hold_dt' = 'dt')) %>%
  collect() %>%
  inner_join(sk, by = c('system_key' = 'system_key')) %>%
  group_by(system_key, yrq) %>%
  summarize(n.holds = n()) %>%
  ungroup()

# FE: scaling -------------------------------------------------------------








