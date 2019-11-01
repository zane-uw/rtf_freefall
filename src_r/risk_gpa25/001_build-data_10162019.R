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

# keys
sk <- data.frame('system_key' = unique(courses.taken$system_key))

# fetch additional data --------------------------------------------

cf <- config::get("sdb", file = "config.yml")
con <- dbConnect(odbc::odbc(), cf$dns, Database = cf$db, UID = cf$uid, PWD = keyring::key_get("sdb"))
rm(cf)

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
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(course = paste(dept_abbrev, course_number, sep = "_"))

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
sched <- tbl(con, in_schema("sec", "time_schedule")) %>%
  filter(ts_year >= 2008,
         current_enroll > 0) %>%
  select(ts_year, ts_quarter, course_branch, dept_abbrev, course_no, current_enroll, writing_crs, diversity_crs, english_comp, qsr, vis_lit_perf_arts,
         indiv_society, natural_world) %>%
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
                                       "E"  = "00")
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

stu.top.depts <- stu.deptwise.data %>%
  group_by(dept_abbrev) %>%
  summarize(nd = n_distinct(system_key)) %>%
  ungroup() %>%
  filter(cume_dist(nd) >= 0.90) %>%
  select(-nd)

stu.deptwise.wide <- stu.deptwise.data %>%
  inner_join(stu.top.depts) %>%
  pivot_wider(.,
              id_cols = c(system_key, yrq),
              names_from = dept_abbrev,
              values_from = c(cumavg.dept, nclass.dept, csum.dept.creds),
              values_fill = list(cumavg.dept = NA,
                                 nclass.dept = 0,
                                 csum.dept.creds = 0))



# repeat courses (quarterly) ----------------------------------------------------------
# count repeats in yrq
repeat.courses <- xf.trs.courses %>%
  arrange(system_key, yrq) %>%
  group_by(system_key, yrq) %>%
  summarize(rep.courses = sum(repeat_course)) %>%
  group_by(system_key, add = F) %>%
  mutate(csum.rep.courses = cumsum(rep.courses))



# calculate quarterly STEM, GenEd by student ------------------------------

# Merge transcripts w/ stem courses then create credits, courses, and grades by student+quarter

stu.stem <- xf.trs.courses %>%
  select(system_key, yrq, course, numeric.grade, course_credits) %>%
  inner_join(select(.data = stem.courses, course)) %>%
  arrange(system_key, yrq) %>%
  group_by(system_key, yrq) %>%
  summarize(stem.courses = n(),
            stem.credits = sum(course_credits),
            avg.stem.grade = mean(numeric.grade, na.rm = T),
            sgrade = sum(numeric.grade, na.rm = T)) %>%
  group_by(system_key, add = F) %>%
  mutate(csum.stem.courses = cumsum(stem.courses),
         csum.stem.credits = cumsum(stem.credits),
         cumavg.stem = cumsum(sgrade) / csum.stem.courses) %>%
  select(-sgrade)


# FE: scaling -------------------------------------------------------------


