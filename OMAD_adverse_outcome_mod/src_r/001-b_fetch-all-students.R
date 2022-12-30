rm(list = ls())

library(tidyverse)
library(dbplyr)
library(odbc)

setwd(rstudioapi::getActiveProject())

YRQ_0 <- 20124
EOP_CODES <- c(1, 2, 13, 14, 16, 17, 31, 32, 33)

# for summer:  [NOTE]: making this generic, not just summer. The registration qtr isn't always what I want
filteryr <- 2023
filterqtr <- 1

# **COMPASS DATA** ------------------------------------------------------------

# actually since this is once per quarter we won't have any data for the quarter we're trying to predict :\

# con <- dbConnect(odbc::odbc(), "compass", timezone = Sys.timezone(),
#                         UID = config::get("sdb", file = "config.yml")$uid, PWD = keyring::key_get("sdb"))
#
# # Not sure why my laptop returns corrupted/garbled data from compass
# # ed. fixed after updating everything (?)
# # test <- tbl(con, 'appointment') %>% select(Date)
# # glimpse(test)
#
# appt <- tbl(con, 'appointment') %>%
#   # filter(year(Date) >= 2008) %>%
#   select(ID,
#          student_no,
#          staff_id,
#          Contact_Type,
#          Date,
#          Time_In,
#          Time_Out,
#          .data$AddDropclass : .data$Notes,
#          Event_Type,
#          Source,
#          TimeDateIn,
#          TimeDateOut,
#          AutoLogOut) %>%
#   collect()
#
# dbDisconnect(con); rm(con)
#
# con <- dbConnect(odbc(), 'sdb', UID = config::get('sdb')$uid, PWD = config::get('sdb')$pwd)
# cal <- tbl(con, in_schema(sql("EDWPresentation.sec"), 'dimDate')) %>%
#   filter(CalendarYr >= 2008) %>%
#   select(CalendarDate, AcademicQtrKeyId, AcademicQtrDayNum, AcademicQtrWeekNum, AcademicQtrName, AcademicQtrCensusDayInd, AcademicYrName) %>%
#   collect() %>%
#   mutate(yr = as.numeric(AcademicQtrKeyId) %/% 10)
#
# sid <- tbl(con, in_schema('sec', 'student_1')) %>%
#   select(system_key, student_no) %>%
#   collect()
#
# dbDisconnect(con); rm(con)
#
# dat <- appt %>%
#   # mutate(Date = as.POSIXct(Date, tz = 'UTC')) %>%
#   mutate(t_in = paste(str_sub(Date, end = 10), str_sub(Time_In, end = 5), sep = ' '),
#          Date = as.POSIXct(str_sub(Date, 1, 10), tz = 'UTC')) %>%
#   filter(str_length(t_in) == 16) %>%
#   inner_join(cal, by = c('Date' = 'CalendarDate')) %>%
#   mutate(AcademicQtrKeyId = as.numeric(AcademicQtrKeyId),
#          qtr.num = AcademicQtrKeyId %% 10,
#          qtr.label = factor(qtr.num, labels = c('Winter', 'Spring', 'Summer', 'Autumn')), # levels = c('Winter', 'Spring', 'Summer', 'Autumn')),
#          t_in = strftime(t_in, format = '%Y-%m-%d %H:%M', tz = 'UTC'))
#
# # dat$staff_id[is.na(dat$staff_id)] <- 'None'
# dat <- dat %>%
#   mutate_at(vars(.data$AddDropclass : .data$Other), as.numeric) %>%
#   mutate(NoShow = as.numeric(if_else(NoShow == 'Y', 1, 0)))
#
# hs <- dat[dat$Source == 'HS',]
# # ic <- dat %>%
# #   filter(dat$Source %in% c('Arlyn', 'basement', 'Biology', 'chem', 'dev3', 'EpMac', 'FrontDesk', 'Lecture Room', 'mac', 'Math', 'test10' 'Writing Center'))
# ic <- dat[grep('^IC', dat$Contact_Type),]
# adv <- dat %>% filter(dat$Contact_Type %in% c('Appointment', 'Quick Question', 'Notes', 'Email Contact', 'Email', 'Telephone',
#                                               'Email Message', 'Workshop', 'General Question', 'Classroom Presentation', 'ECC Meeting or Event',
#                                               'Admin Notes'))
# # create weekly data
# adv.agg <- adv %>%
#   group_by(student_no, yr, qtr.num) %>%
#   summarize_at(vars(AddDropclass:NoShow), sum, na.rm = T) %>%
#   ungroup() %>%
#   mutate(visit_advising = 1)
# ic.activity.agg <- ic %>%
#   group_by(student_no, yr, qtr.num, Contact_Type) %>%
#   summarize(n = n()) %>%
#   ungroup() %>%
#   pivot_wider(., names_from = 'Contact_Type', values_from = 'n')
# ic.activity.agg[,4:17] <- apply(ic.activity.agg[,4:17], 2, function(x) replace_na(x, 0))
#
# str_clean <- function(x){
#   x <- str_remove_all(x, '[[:punct:]]')
#   # x <- tolower(x)
#   x <- str_replace_all(x, " ", "_")
# }
#
# n <- str_clean(names(ic.activity.agg[4:ncol(ic.activity.agg)]))
# # n <- str_remove_all(n, '[[:punct:]]')
# # n <- c(names(ic.activity.agg[1:3]), n)
# names(ic.activity.agg) <- c(names(ic.activity.agg[1:3]), n)
# ic.activity.agg$visit_ic <- 1
#
# # by weeks:
# # NA weeks will be 99? Probably they should be the next year/qtr and get a 0
# next.yrq <- function(x){
#   x <- x + ifelse(x %% 10 == 4, 7, 1)
#   return(x)
# }
#
# # [TODO] fix the yrq sequencing for the above data as well
#
# adv.wk <- adv %>%
#   filter(NoShow == 0) %>%
#   rename(yrq = AcademicQtrKeyId, week = AcademicQtrWeekNum) %>%
#   # mutate(visit_advising = 1) %>%
#   group_by(student_no, yrq, week) %>%
#   # summarize(visit_advising = sum(visit_advising)) %>%
#   summarize(visit_advising = n()) %>%
#   ungroup() %>%
#   mutate(week = replace_na(week, 0),
#          yrq = if_else(week == 0, next.yrq(yrq), yrq)) %>%
#   pivot_wider(., names_from = 'week', values_from = 'visit_advising', names_prefix = 'advising_week_')
# adv.wk[,3:ncol(adv.wk)] <- apply(adv.wk[,3:ncol(adv.wk)], 2, function(x) replace_na(x, 0))
#
# ic.wk <- ic %>%
#   rename(yrq = AcademicQtrKeyId, week = AcademicQtrWeekNum) %>%
#   group_by(student_no, yrq, week) %>%
#   summarize(visit_ic = n()) %>%
#   ungroup() %>%
#   mutate(week = replace_na(week, 0),
#          yrq = if_else(week == 0, next.yrq(yrq), yrq)) %>%
#   pivot_wider(., names_from = 'week', values_from = 'visit_ic', names_prefix = 'ic_week_')
# ic.wk[,3:ncol(ic.wk)] <- apply(ic.wk[,3:ncol(ic.wk)], 2, function(x) replace_na(x, 0))
#
# # add system_key and combine
# compass.feats <- full_join(adv.agg, ic.activity.agg) %>%
#   filter(student_no > 1) %>%
#   mutate(yrq = (yr*10)+qtr.num) %>%
#   select(-yr, -qtr.num) %>%
#   inner_join(sid) %>%
#   select(-student_no)
# compass.feats <- data.frame(apply(compass.feats, 2, replace_na, 0))   # sometimes you just want to kick it old school
#
# compass.weekly <- full_join(adv.wk, ic.wk) %>%
#   filter(student_no > 1) %>%
#   inner_join(sid) %>%
#   select(-student_no) %>%
#   mutate_all(replace_na, 0)
#
# rm(con, adv.wk, ic.wk, n, adv.agg, ic.activity.agg, hs, ic, adv, dat, cal, sid, appt)


# **SDB DATA** ----------------------------------------------------------------

con <- dbConnect(odbc(), 'sqlserver01', UID = config::get('sdb')$uid, PWD = keyring::key_get('sdb'))

# Utility tables -------------------------------------------------------------------
# calendar
# current year, quarter
# filtering query for EOP students

# academic calendar
cal <- tbl(con, in_schema(sql("EDWPresentation.sec"), "dimDate")) %>%
  select(yrq = AcademicQtrKeyId, dt = CalendarDate)

currentq <- tbl(con, in_schema("sec", "sdbdb01")) %>%
  select(current_yr, current_qtr, gl_first_day, gl_regis_year, gl_regis_qtr) %>%
  # collect() %>%
  mutate(current_yrq = current_yr*10 + current_qtr,
         gl_regis_yrq = gl_regis_year*10 + gl_regis_qtr)

# athletes and eop queries ------------------------------------------------


# ath_tbl <- tbl(con, in_schema(sql('EDWPresentation.sec'), 'EnrolledStudent')) %>%
#   semi_join( tbl(con, in_schema('sec', 'student_2_sport_code') ),
#              by = c('SystemKey' = 'system_key'), copy = T) %>%
#   select(system_key = SystemKey)
#
# db.eop <- tbl(con, in_schema("sec", "transcript")) %>%
#   filter(special_program %in% EOP_CODES) %>%
#   select(system_key) %>%
#   full_join( tbl(con, in_schema('sec', 'registration')) %>%
#                filter(special_program %in% EOP_CODES) %>%
#                select(system_key)
#   ) %>%
#   full_join( tbl(con, in_schema('sec', 'student_1')) %>%
#                filter(spcl_program %in% EOP_CODES) %>%
#                select(system_key)
#   ) %>%
#   full_join( tbl(con, in_schema(sql("EDWPresentation.sec"), 'EnrolledStudent')) %>%
#                filter(as.numeric(SpecialProgramCode) %in% EOP_CODES) %>%
#                select(system_key = SystemKey) %>%
#                distinct()
#   ) %>%
#   # ADD ISS O students
#   full_join( tbl(con, in_schema('sec', 'student_1_college_major')) %>%
#                filter(major_abbr == "ISS O") %>%
#                select(system_key)
#   ) %>%
#   full_join(ath_tbl) %>%
#   distinct()

# to fetch all distinct students w/ transcripts since yrq0
# not strictly necessary since the transcript creation function will get all if we remove the semi_join
# distinct_students <- tbl(con, in_schema("sec", "transcript")) %>%
#   select(system_key) %>%
#   distinct()



# TRANSCRIPTS -------------------------------------------------------------

create.transcripts <- function(from_yrq = YRQ_0){
  transcript <- tbl(con, in_schema("sec", "transcript")) %>%
    # semi_join(db.eop) %>%
    mutate(yrq = tran_yr*10 + tran_qtr) %>%
    filter(yrq >= from_yrq) %>%       # tran_qtr != 3, add_to_cum == 1
    select(system_key,
           yrq,
           class,
           honors_program,
           tenth_day_credits,
           scholarship_type,
           yearly_honor_type,
           num_ind_study,
           num_courses,
           qtr_grade_points,
           qtr_graded_attmp,
           over_qtr_grade_pt,
           over_qtr_grade_at,
           qtr_nongrd_earned,
           over_qtr_nongrd,
           qtr_deductible,
           over_qtr_deduct,
           special_program) %>%
    collect() %>%
    mutate(pts = pmax(qtr_grade_points, over_qtr_grade_pt),                           # NB mssql doesn't support pmax
           attmp = pmax(qtr_graded_attmp, over_qtr_grade_at),
           nongrd = pmax(qtr_nongrd_earned, over_qtr_nongrd),
           deduct = pmax(qtr_deductible, over_qtr_deduct),
           qgpa = pts / attmp,
           tot_creds = attmp + nongrd - deduct,
           qgpa15 = if_else(qgpa <= 1.5, 1, 0),
           qgpa20 = if_else(qgpa <= 2, 1, 0),
           probe = if_else(scholarship_type == 3, 1, 0),
           eop = if_else(special_program %in% c(1, 2, 13, 14, 16, 17, 31, 32, 33), 1, 0),
           athletics = if_else(special_program %in% c(25, 26, 27, 30, 31, 32, 33, 34, 40, 41, 42), 1, 0)) %>%
    select(-starts_with('over_qtr'),
           -qtr_grade_points,
           -qtr_graded_attmp,
           -qtr_nongrd_earned,
           -qtr_deductible,
           -special_program)

  # system calendar for filtering join
  sys.cal <-tbl(con, in_schema('sec', 'sys_tbl_39_calendar')) %>%
    filter(first_day > '2019-01-01') %>%
    select(table_key, first_day) %>%
    mutate(yrq = as.integer(table_key),
           cal_year = round(yrq / 10, 0),
           cal_qtr = round(yrq %% 10, 0))

  # combine with current quarter from registration
  # need to calculate attempted from the regis_courses current
  reg.courses <- tbl(con, in_schema('sec', 'registration_courses')) %>%
    # semi_join(currentq, by = c('regis_yr' = 'gl_regis_year', 'regis_qtr' = 'gl_regis_qtr')) %>%
    # update for summer
    # [TODO] handle this better
    filter(regis_yr == filteryr, regis_qtr == filterqtr) %>%
    filter(!(request_status %in% c('E', 'L'))) %>%
    # semi_join(db.eop) %>%
    # filter drops prior to first day using the sys.call filter above
    left_join(sys.cal, by = c('regis_yr' = 'cal_year', 'regis_qtr' = 'cal_qtr')) %>%
    filter(!(request_status == 'D' & request_dt < first_day))

  calc.attmp <- reg.courses %>%
    # select(system_key, index1, request_status, starts_with('crs_'), grading_system, credits, `repeat`) %>% collect()
    group_by(system_key) %>%
    summarize(attmp = sum(credits, na.rm = T)) %>%
    ungroup()
  calc.num.courses <- reg.courses %>%
    group_by(system_key, crs_curric_abbr) %>%
    summarize(num_courses = n_distinct(crs_number)) %>%
    group_by(system_key, add = F) %>%
    summarize(num_courses = sum(num_courses, na.rm = T)) %>%
    ungroup()

  # get.current.quarter.reg <- function(){
  curr.reg <- tbl(con, in_schema('sec', 'registration')) %>%
    # semi_join(currentq, by = c('regis_yr' = 'gl_regis_year', 'regis_qtr' = 'gl_regis_qtr')) %>%
    # changing to hard coded filters for now
    filter(regis_yr == filteryr, regis_qtr == filterqtr) %>%
    # semi_join(db.eop) %>%
    inner_join(calc.attmp) %>%
    inner_join(calc.num.courses) %>%
    mutate(yrq = regis_yr*10 + regis_qtr) %>%
    select(system_key,
           yrq,
           class = regis_class,
           honors_program = regis_hnrs_prg,
           tenth_day_credits,
           attmp,
           num_courses) %>%
    collect()

  result <- bind_rows(transcript, curr.reg) %>%
    group_by(system_key) %>%
    arrange(system_key, yrq) %>%
    mutate(cum.pts = cumsum(pts),
           cum.attmp = cumsum(attmp),
           cum.gpa = cum.pts / cum.attmp) %>%
    ungroup()

  return(result)
}

get.courses.taken <- function(from_yrq = YRQ_0){

  courses.taken <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
    # semi_join(db.eop) %>%
    mutate(yrq = tran_yr*10 + tran_qtr) %>%
    filter(yrq >= from_yrq) %>%
    select(system_key,
           yrq,
           course.index = index1,
           dept_abbrev,
           course_number,
           course_credits,
           course_branch,
           summer_term,
           grade,
           duplicate_indic,
           repeat_course,
           honor_course,
           section_id) %>%
    collect() %>%
    mutate_if(is.character, trimws) %>%         # doesn't work correctly remotely (in R specifically), unsafe to do other string mutations w/ output before trimming
    # but don't want to build the course code w/ the extra whitespace :\
    # trim + concate would work fine in sql server, etc. but for some reason it resolves oddly w/ R
    # rename(course.index = index1) %>%
    mutate(course = paste(dept_abbrev, course_number, sep = "_"),
           duplicate_indic = as.numeric(duplicate_indic),
           numeric.grade = recode(grade,
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
                                  "F"  = "00"),
           numeric.grade = as.numeric(numeric.grade) / 10,
           course.withdraw = if_else(grepl("W", grade), 1, 0),
           course.nogpa = if_else(grade %in% c("", "CR", "H", "HP", "HW", "I", "N", "NC", "NS", "P", "S", "W", "W3", "W4", "W5", "W6", "W7"), 1, 0),
           course.alt.grading = if_else(grade %in% c('CR', 'S', 'NS', 'P', 'HP', 'NC'), 1, 0))

  # combine with current quarter course reg
  curr.qtr <- tbl(con, in_schema('sec', 'registration_courses')) %>%
    # semi_join(db.eop) %>%
    # semi_join(currentq, by = c('regis_yr' = 'gl_regis_year', 'regis_qtr' = 'gl_regis_qtr')) %>%
    # changing to hard coded filters for a while
    filter(regis_yr == filteryr, regis_qtr == filterqtr) %>%
    filter(request_status %in% c('A', 'C', 'R')) %>%
    collect() %>%
    mutate_if(is.character, trimws) %>%
    mutate(yrq = regis_yr*10 + regis_qtr,
           course.alt.grading = if_else(grading_system > 0, 1, 0),
           duplicate_indic = as.numeric(dup_enroll),
           # replace_na(list('duplicate_indic' = 0)),
           repeat_course = if_else(`repeat` %in% c('1', '2', '3'), T, F),
           course = paste(crs_curric_abbr, crs_number, sep = '_')) %>%
    select(system_key,
           yrq,
           course.index = index1,
           dept_abbrev = crs_curric_abbr,
           course_number = crs_number,
           course_credits = credits,
           course_branch,
           summer_term,
           # grade
           duplicate_indic,
           repeat_course,
           honor_course,
           section_id = crs_section_id,
           course,
           # numeric.grade
           # course.withdraw
           # course.nogpa
           #
           course.alt.grading) %>%
    group_by(system_key, course) %>%
    arrange(system_key, course, section_id) %>%
    filter(row_number() == 1) %>%
    ungroup()


  result <- bind_rows(courses.taken, curr.qtr) %>% distinct() %>% replace_na(list('duplicate_indic' = 0))

  return(result)
}

# courses.taken <- get.courses.taken()

# DERIVED COURSES-TAKEN FIELDS ----------------------------------------------

create.derived.courses.taken.tscs.data <- function(course.data){
  # combining the following calculations that have `courses.taken` as a dependency and
  # return ~ equally long combined table

  courses.taken <- course.data

  # total of fees, courses taken in different gen ed categories
  # vlpa, rsn, etc.
  create.fees.gened.reqs <- function(){
    sched <- tbl(con, in_schema("sec", "time_schedule")) %>%
      mutate(yrq = ts_year*10 + ts_quarter) %>%
      filter(yrq >= YRQ_0) %>%
      select(yrq,
             course_branch,
             dept_abbrev,
             course_no,
             section_id,
             sln,
             current_enroll,
             parent_sln,
             current_enroll,
             writing_crs,
             diversity_crs,
             english_comp,
             rsn,
             arts_hum,
             social_science,
             natural_science,
             gen_elective,
             fee_amount) %>%
      collect() %>%
      mutate_if(is.character, trimws) %>%
      mutate(course = paste(dept_abbrev, course_no, sep = "_"))

    result <- courses.taken %>%
      select(system_key, yrq, course, section_id, numeric.grade) %>%
      left_join(sched, by = c('course' = 'course', 'yrq' = 'yrq', 'section_id' = 'section_id')) %>%
      group_by(system_key, yrq) %>%
      summarize(avg.class.size = mean(current_enroll, na.rm = T),
                n.writing = sum(writing_crs, na.rm = T),
                n.diversity = sum(diversity_crs, na.rm = T),
                n.engl_comp = sum(english_comp, na.rm = T),
                n.rsn = sum(rsn, na.rm = T),
                n.vlpa = sum(arts_hum, na.rm = T),
                n.indiv_soc = sum(social_science, na.rm = T),
                n.nat_world = sum(natural_science, na.rm = T),
                n.gen_elective = sum(gen_elective, na.rm = T),
                sum.fees = sum(fee_amount, na.rm = T)) %>%
      ungroup()

    # rm(sched)
    return(result)

  }

  # repeats, alternate grading
  create.repeats.w.alt.grading <- function(){
    result <- courses.taken %>%
      arrange(system_key, yrq) %>%
      group_by(system_key, yrq) %>%
      summarize(rep.courses = sum(repeat_course),
                n.w = sum(course.withdraw),
                n.alt.grading = sum(course.alt.grading)) %>%
      group_by(system_key, add = F) %>%
      mutate(csum.rep.courses = cumsum(rep.courses),
             csum.w = cumsum(n.w),
             csum.alt.grading = cumsum(n.alt.grading)) %>%
      ungroup()

    return(result)
  }


##
# [TODO] get rid of this
##
  # dept-wise courses, grades, etc (v. wide)
  # agg/reduce to 1 row per student + yrq + dept_abbr
  # then pivot wide
  create.stu.dept.wide <- function(percent = 0.9){
    stu.deptwise.data <- courses.taken %>%
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

    # Too many columns w/o reducing the number of depts
    top.n.depts <- stu.deptwise.data %>%            # this might break things later if the top_n shift around, safer to use names() later? Hmmmm, have to think about approach to that
      group_by(dept_abbrev) %>%                     # it doesn't matter now but next time it might be nice to drop these column-wise rather than row-wise
      summarize(nd = n_distinct(system_key)) %>%
      ungroup() %>%
      filter(cume_dist(nd) >= percent) %>%
      select(-nd)

    result <- stu.deptwise.data %>%
      inner_join(top.n.depts) %>%
      pivot_wider(.,
                  id_cols = c(system_key, yrq),
                  names_from = dept_abbrev,
                  values_from = c(cumavg.dept, nclass.dept, csum.dept.creds),
                  values_fill = list(cumavg.dept = NA,
                                     nclass.dept = 0,
                                     csum.dept.creds = 0))

    names(result) <- str_replace_all(names(result), " ", "")

    return(result)
  }

  # "stem.courses"      "stem.credits"      "avg.stem.grade"    "csum.stem.courses" "csum.stem.credits"
  create.stem.data <- function(){
    result <- tbl(con, in_schema(sql("EDWPresentation.sec"), "dmSCH_dimCurriculumCourse")) %>%
      filter(FederalSTEMInd == "Y") %>%
      select(dept_abbrev = CurriculumCode,
             course_number = CourseNbr,
             course_level = CourseLevelNbr,
             cip = CIPCode) %>%
      distinct() %>%
      collect() %>%
      mutate_if(is.character, trimws) %>%
      mutate(course = paste(dept_abbrev, course_number, sep = "_"),
             is.stem = 1) %>%
      right_join( select(.data = courses.taken, system_key, yrq, course, numeric.grade, course_credits) ) %>%
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

    return(result)
  }

  fees <- create.fees.gened.reqs()
  repeats <- create.repeats.w.alt.grading()
  stu.wide <- create.stu.dept.wide()
  stem <- create.stem.data()

  merged.data <- fees %>%
    left_join(repeats) %>%
    left_join(stem) %>%
    left_join(stu.wide)

  return(merged.data)
}


# MAJORS ------------------------------------------------------------------

# removed the pivot
create_major_data <- function() {
  # for newest quarter
  reg.major <-
    tbl(con, in_schema('sec', 'registration_regis_col_major')) %>%
    # semi_join(db.eop) %>%
    # inner_join(currentq, by = c('regis_yr' = 'gl_regis_year', 'regis_qtr' = 'gl_regis_qtr')) %>%
    # changing to filter vars to work around registration time period limitations
    filter(regis_yr == filteryr, regis_qtr == filterqtr) %>%
    mutate(yrq = regis_yr * 10 + regis_qtr) %>%
    select(
      system_key,
      yrq,
      index1,
      tran_yr = regis_yr,
      tran_qtr = regis_qtr,
      tran_major_abbr = regis_major_abbr
    )

  mjr <- tbl(con, in_schema("sec", "transcript_tran_col_major")) %>%
    # semi_join(db.eop) %>%
    mutate(yrq = tran_yr * 10 + tran_qtr) %>%
    filter(yrq >= YRQ_0) %>%
    select(system_key,
           yrq,
           index1,
           tran_yr,
           tran_qtr,
           tran_major_abbr) %>%
    full_join(reg.major) %>%
    mutate_if(is.character, trimws) %>%
    collect()                           # this will get re-used for a number of derivations

  return(mjr)
}


# STUDENT_1, EDW, AGE -------------------------------------------------------

create.stu1 <- function(){

  result <- tbl(con, in_schema("sec", "student_1")) %>%
    filter(last_yr_enrolled >= 2004) %>%
    select(system_key,
           s1_gender,
           child_of_alum,
           running_start,
           resident) %>%
    collect() %>%
    # some fixes
    mutate(running_start = ifelse(running_start == 'Y', 1, 0),
           s1_gender = ifelse(s1_gender == 'F', 1, 0))

  return(result)
}


# Age, calc from birth_dt + calendar ------------------------------------------------------------------

calc.adjusted.age <- function(){

  bd <- tbl(con, in_schema('sec', 'student_1')) %>%
    select(system_key, birth_dt) %>%
    # semi_join(db.eop) %>%
    inner_join( tbl(con, in_schema('sec', 'registration')) %>%
                  select(system_key, regis_yr, regis_qtr) ) %>%
    mutate(table_key = paste0('0', as.character(regis_yr), as.character(regis_qtr), ' '))

  result <- tbl(con, in_schema('sec', 'sys_tbl_39_calendar')) %>%
    select(table_key, tenth_day) %>%
    inner_join(bd) %>%
    collect() %>%
    mutate(age = as.numeric(difftime(tenth_day, birth_dt, units = "days")) / 364.25,
           yrq = regis_yr*10 + regis_qtr) %>%
    select(system_key, yrq, age)

  # rm(bd)
  return(result)
}

# APPLICATIONS ------------------------------------------------------------

# Create application filter  --------------------------------------------------------------

create.application.data <- function(){

  # create an in-db filtering file for the applications
  app.filter <- tbl(con, in_schema("sec", "student_1")) %>%
    select(system_key,
           appl_yr = current_appl_yr,
           appl_qtr = current_appl_qtr,
           appl_no = current_appl_no) %>%
    distinct()

  # ...get (many) --------------------------------------------------------

  gpa <- tbl(con, in_schema('sec', 'sr_adm_appl')) %>%
    group_by(system_key) %>%
    summarize(high_sch_gpa = max(high_sch_gpa, na.rm = T),
              trans_gpa = max(trans_gpa, na.rm = T)) %>%
    ungroup()

  sr.appl <- tbl(con, in_schema("sec", "sr_adm_appl")) %>%
    semi_join(app.filter) %>%
    # filter(appl_type %in% c(1, 2, 4, 6, "R"),
    #        appl_yr >= 2003,
    #        appl_status %in% c(11, 12, 15, 16, 26)) %>%
    select(system_key,
           # trans_gpa,
           conditional,
           provisional,
           with_distinction,
           res_in_question,
           low_family_income,
           # appl_class = class,
           starts_with("hs_"),
           last_school_type,
           -hs_esl_engl) %>%
    inner_join(gpa) %>%
    mutate(trans_gpa = na_if(trans_gpa, 0),
           high_sch_gpa = na_if(high_sch_gpa, 0))
  # ! #
  # test scores no longer required
  # ! #

  # # best test scores
  # appl.hist <- tbl(con, in_schema("sec", "APPLHistApplication")) %>%
  #   filter(appl_type %in% c(1, 2, 4, 6, "R"),
  #          appl_yr >= 2003,
  #          appl_status %in% c(11, 12, 15, 16, 26)) %>%
  #   select(system_key, appl_yr, appl_qtr, appl_no, best_satr_v, best_satr_m, best_satr_c) %>%
  #   semi_join(app.filter)

  appl.guardian <- tbl(con, in_schema("sec", "sr_adm_appl_guardian_data")) %>%
    semi_join(app.filter) %>%
    group_by(system_key) %>%
    summarize(guardian.ed.max = max(guardian_ed_level, na.rm = T),
              guardian.ed.min = min(guardian_ed_level, na.rm = T)) %>%
    ungroup()

  # combine applications:
  appl.data <- sr.appl %>%
    # full_join(appl.req.major) %>%
    # full_join(appl.init.major) %>%
    # full_join(appl.income) %>%
    # full_join(appl.hist) %>%
    full_join(appl.guardian) %>%
    collect() %>%
    mutate_if(is.character, trimws)

  return(appl.data)

}


# UNMET ADD REQ'S ---------------------------------------------------------

create.unmet.requests <- function(){
  # unexpectedly getting a SQL error w/ %/%
  temp_yr0 <- YRQ_0 %/% 10

  unmet.reqs <- tbl(con, in_schema('sec', 'sr_unmet_request')) %>%
    # filter(unmet_yr >= YRQ_0 %/% 10) %>%
    filter(unmet_yr >= temp_yr0) %>%
    group_by(unmet_system_key, unmet_yr, unmet_qtr) %>%
    summarize(n.unmet = n()) %>%
    ungroup() %>%
    collect() %>%
    mutate(yrq = unmet_yr*10 + unmet_qtr) %>%
    select(system_key = unmet_system_key, yrq, n.unmet)
}


# LATE REGISTRATIONS ----------------------------------------------------

# including Day 0 as 'late'
create.late.registrations <- function(){

  syscal <- tbl(con, in_schema("sec", "sys_tbl_39_calendar")) %>%
    filter(first_day >= "2004-01-01") %>%                           # arbitrary, some kind of limit is helpful
    select(table_key, first_day, tenth_day, last_day_add) %>%
    mutate(yrq = as.integer(table_key),
           regis_yr = round(yrq / 10, 0),
           regis_qtr = yrq %% 10)

  # "fixes" still don't eliminate the extremely off dates, so I'll stick with the min and correct the ones I see right now
  regc <- tbl(con, in_schema("sec", "registration_courses")) %>%
    mutate(yrq = regis_yr*10 + regis_qtr) %>%
    filter(yrq >= YRQ_0) %>%
    select(system_key, yrq, add_dt_tuit) %>%
    group_by(system_key, yrq) %>%
    filter(add_dt_tuit == min(add_dt_tuit, na.rm = T)) %>%
    distinct() %>%
    left_join(syscal) %>%
    collect() %>%
    mutate(reg.late.days = as.numeric(difftime(add_dt_tuit, first_day, units = "days"))) %>%
    ungroup()

  regc$reg.late.days[regc$reg.late.days <= -365] <- median(regc$reg.late.days[regc$reg.late.days < 0])
  regc$reg.late.binary <- if_else(regc$reg.late.days >= 0, 1, 0)

  result <- regc %>%
    select(system_key, yrq, reg.late.binary)

  return(result)

}


# HOLDS -------------------------------------------------------------------

create.holds <- function(){

  holds <- tbl(con, in_schema("sec", "student_1_hold_information")) %>%
    inner_join(cal, by = c('hold_dt' = 'dt')) %>%
    collect() %>%
    group_by(system_key, yrq) %>%
    summarize(n.holds = n()) %>%
    ungroup() %>%
    mutate(yrq = as.numeric(yrq))
}


# major transformations ---------------------------------------------------

major_xforms <- function(major_data, courses_taken_data){

  # calc double+ majors
  dual.majors <- major_data %>%
    mutate(dual_major = if_else(index1 >= 1, 1, 0)) %>%
    select(system_key, yrq, dual_major) %>%
    group_by(system_key, yrq) %>%
    summarize(dual_major = max(dual_major)) %>%
    ungroup()

  stu.major <- major_data %>%
    filter(index1 == 1) %>%
    select(system_key, yrq, tran_major_abbr) %>%
    group_by(system_key) %>%
    arrange(system_key, yrq) %>%
    mutate(
      major.change = if_else(
        tran_major_abbr == lag(tran_major_abbr, order_by = yrq),
        0,
        1,
        missing = 0
      ),
      major.change.count = cumsum(major.change
      )
    ) %>%
    ungroup() # %>%

  stu.major$n.majors <- unlist(tapply(stu.major$tran_major_abbr,
                                      stu.major$system_key,
                                      function(x) {
                                        rep(seq_along(rle(x)$lengths), times = rle(x)$lengths)
                                      }))

  # setup some filtered views of the major codes for derived variables
  sr.maj.codes <- tbl(con, in_schema("sec", "sr_major_code")) %>%
    select(-contains('_spare_'),
           -contains('_types_')) %>%
    collect()

  pre.codes <- sr.maj.codes %>% filter(major_premaj == TRUE) %>% distinct(trimws(major_abbr)) %>% pull()
  ext.codes <- sr.maj.codes %>% filter(major_premaj_ext == TRUE) %>% distinct(trimws(major_abbr)) %>% pull()
  # coding does not have same meaning for non-matric

  premajors <- major_data %>%
    mutate(
      tran_major_abbr = trimws(tran_major_abbr),
      premajor = if_else(tran_major_abbr %in% pre.codes, 1, 0),
      ext_premajor = if_else(tran_major_abbr %in% ext.codes, 1, 0),
      nonmatric = if_else(tran_major_abbr %in% c('N MATR', 'T NM', 'B NM'), 1, 0)) %>%
    # 'collapse' to 1 row
    group_by(system_key, yrq) %>%
    summarize(premajor = max(premajor, na.rm = T),
              ext_premajor = max(ext_premajor, na.rm = T),
              nonmatric = max(nonmatric, na.rm = T)) %>%
    ungroup()

  # Then do the 'classes in major' by making a larger table from major + xf.trs.courses, then reducing it
  # I don't like this actually, shouldn't be referencing courses.taken w/in this fun
  maj.courses.taken <- courses_taken_data %>%
    select(system_key, yrq, dept_abbrev) %>%
    left_join(stu.major) %>%
    mutate(course.equals.major = ifelse(tran_major_abbr == dept_abbrev, 1, 0)) %>%
    group_by(system_key, yrq) %>%
    summarize(n.major.courses = sum(course.equals.major)) %>%
    group_by(system_key, add = F) %>%
    arrange(system_key, yrq) %>%
    mutate(csum.major.courses = cumsum(n.major.courses)) %>%
    ungroup()

  # compile into 1 table
  result <- stu.major %>%
    inner_join(dual.majors) %>%
    inner_join(premajors) %>%
    left_join(maj.courses.taken) %>%
    mutate_if(is.numeric, replace_na, 0)

  return(result)
}



# RUN create funcs --------------------------------------------------------------
# useful for verification, could be pushed to server side

transcript <- create.transcripts()
courses.taken <- get.courses.taken()
# derived.courses.taken.data <- create.derived.courses.taken.tscs.data()
raw.major.data <- create_major_data()
stu.age <- calc.adjusted.age() # need to fix this for extra current q
appl.data <- create.application.data()
unmet.reqs <- create.unmet.requests()
late.reg <- create.late.registrations()
holds <- create.holds()
stu1 <- create.stu1()

derived.courses.taken.data <- create.derived.courses.taken.tscs.data(course.data = courses.taken)
majors <- major_xforms(major_data = raw.major.data, courses_taken_data = courses.taken)


mrg.dat <- transcript %>%
  inner_join(stu1) %>%
  left_join(appl.data, by = c('system_key' = 'system_key')) %>%
  left_join(holds) %>%
  left_join(late.reg) %>%
  left_join(majors) %>%
  left_join(stu.age) %>%
  left_join(unmet.reqs) %>%
  # left_join(compass.feats) %>%
  # left_join(compass.weekly)%>%
  left_join(derived.courses.taken.data) %>%
  replace_na(list('n.holds' = 0, 'n.unmet' = 0))

# derived features and corrections for 'fresh' data ----------------------------------

# (i <- sapply(mrg.dat, is.character))
# apply(mrg.dat[,i], 2, unique)

# coerce logical vars for modeling
names(mrg.dat)[sapply(mrg.dat, is.logical)]
mrg.dat <- mrg.dat %>%
  mutate_if(is.logical, as.numeric)

# Full time = 12 credits
# add qtr.sequence for 'time' (rough as that is)
mrg.dat <- mrg.dat %>%
  mutate(ft = if_else(tenth_day_credits >= 12, 1, 0),
         ft.creds.ou = tenth_day_credits - 12) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(qtr.seq = row_number()) %>%
  ungroup()

# aggregate IC variables
# ic <- mrg.dat %>% select(starts_with('IC', ignore.case = F))
# mrg.dat$ic_tot <- rowSums(ic)
# rm(ic)
# mrg.dat <- mrg.dat %>% select(-starts_with('IC', ignore.case = F))


# name check --------------------------------------------------------------
#
# source.names <- names(read_csv('src_r/risk_gpa25/data/data_any-adverse-qtrly-outcome-no-preproc.csv', n_max = 2))
# x <- names(mrg.dat)
#
# print('***?CHECK MODEL DATA NAMES AGAINST OUTPUT FILE?***')
# cbind(setdiff(source.names, x))

# save --------------------------------------------------------------------

# save(mrg.dat, file = paste0('OMAD_adverse_outcome_mod/data/merged-sdb-compass_', Sys.Date(), '.RData'))
save(mrg.dat, file = paste0('OMAD_adverse_outcome_mod/data/ALL-STU-merged_', Sys.Date(), '.RData'))
save(courses.taken, file = 'OMAD_adverse_outcome_mod/data/ALL-STU-Y-courses-taken.RData')
