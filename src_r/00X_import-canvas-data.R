rm(list = ls())
gc()

library(tidyverse)
library(odbc)
library(dbplyr)

  # train <- readr::read_csv("data/pytrain.csv")
  # test <- readr::read_csv("data/pytest.csv")
  #
  # names(test)
  # test <- test[,-1]
  #
  # train.key <- train$system_key
  # test.key <- test$system_key
  #
  # train <- subset(train, select = -c(system_key))
  # test <- subset(test, select = -c(system_key))
  #
  # xy <- subset(train, subset = train$qtr.num == 1,
  #              select = c(qgpa, class.1, class.2, class.3, class.4, tenth_day_credits, num_courses,
  #                         attmp, nongrd, s1_genderF, child_of_alumTRUE, running_startY, HispanicIndN,
  #                         s1_high_satm, s1_high_satv, age, conditional, high_sch_gpa,
  #                         hs_yrs_math, hs_yrs_english, hs_math_level, reg.late.days,
  #                         n.unmet, ftFALSE, ft.creds.over))
  # xy <- xy[complete.cases(xy),]
  # fit <- lm(qgpa ~., data = xy)
  # summary(fit)
  # pred <- predict(fit)
  # sqrt(mean(((xy$qgpa - pred)^2)))
  # plot(xy$qgpa, pred)

# canvas/instructure file ------------------------------------------------------------

path.to.canvas <- "../../../../../UW_FinalGrades/"
can <- readr::read_csv(paste0(path.to.canvas, "uw-grades.csv"), col_names = T,
                       cols(.default = col_double(),
                            course_key = col_character()))

courses.can <- readr::read_csv(paste0(path.to.canvas, "courses-provisioning_08122019.csv"))
stu.can <- readr::read_csv(paste0(path.to.canvas, "student-provisioning_07092019.csv"))

# sdb source data ---------------------------------------------------------

con <- dbConnect(odbc::odbc(), config::get()$sdb$dns, Database = config::get()$sdb$db, UID = config::get()$sdb$uid, PWD = keyring::key_get("sdb"))
link.netid <- tbl(con, in_schema("sec", "student_1")) %>%
  select(system_key, uw_netid) %>%
  collect() %>%
  mutate_if(is.character, trimws)

link.course <- tbl(con, in_schema("sec", "time_schedule")) %>%
  select(ts_year, ts_quarter, course_branch, course_no, dept_abbrev, section_id,
         sln, parent_sln, current_enroll, qsr) %>%
  mutate(ts_yrq = ts_year*10 + ts_quarter) %>%
  filter(ts_yrq >= 20064) %>%
  collect() %>%
  mutate_if(is.character, trimws)


# join canvas courses with time schedule ----------------------------------

can <- can %>% mutate(course_key = str_sub(course_key, start = 4))

table(can$course_key %in% courses.can$canvas_course_id)   # how odd that there would be any FALSEs

check <- can[!(can$course_key %in% courses.can$canvas_course_id),]
unique(check$course_key)
unique(check$user_id)

x <- str_split(courses.can$course_id, "-", n = 5, simplify = T)
x[,2] <- recode(x[,2],
                summer = 3L,
                autumn = 4L,
                winter = 1L,
                spring = 2L)
x <- as.data.frame(x)
names(x) <- c("ts_year", "ts_quarter", "dept_abbrev", "course_no", "section_id")
crss <- courses.can %>%
  select(course_key = canvas_course_id, long_name, account_id, canvas_term_id) %>%
  bind_cols(x) %>%
  mutate(ts_year = as.numeric(ts_year),
         ts_quarter = as.numeric(ts_quarter),
         course_no = as.numeric(course_no)) %>%
  inner_join(link.course, by = c("ts_year" = "ts_year",
                                 "ts_quarter" = "ts_quarter",
                                 "dept_abbrev" = "dept_abbrev",
                                 "course_no" = "course_no",
                                 "section_id" = "section_id"))

# now can link the time schedule with the canvas dump
mg.courses <- can %>%
  mutate(course_key = as.numeric(course_key)) %>%
  inner_join(crss, by = c("course_key" = "course_key")) %>%
  select(-account_id, -id, -start_at, -end_at)
table(mg.courses$ts_yrq)


# join canvas courses w/ netids -------------------------------------------
#
# stu.can <-> link.netid <-> can --> mg.courses

# split netid from email address (not checking to validate domain)
stu.can$uw_netid <- str_split(stu.can$email, "@", n = 2, simplify = T)[,1]

# join to link.netid and courses.can
canvas.data.merged <- link.netid %>%
  inner_join(stu.can) %>%       # by = 'uw_netid'
  select(system_key,
         canvas_user_id) %>%
  inner_join(mg.courses, by = c('canvas_user_id' = 'user_id'))

# little tidying up
rm(can, check, con, courses.can, crss, ids, link.course, link.netid, mg.courses, stu.can, x)


# import other SDB data --------------------------------------------------

# omad students
load('data/merged-dataset.RData')

# join some features with canvas file using sys key and yrq ---------------

sat <- scale(mrg.dat$s1_high_satv + mrg.dat$s1_high_satm, center = T, scale = T)
act <- scale(mrg.dat$s1_high_act, center = T, scale = T)

mrg.dat$std.test.high <- pmax(sat, act, na.rm = T)
rm(act, sat)

mrg.dat <- mrg.dat %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  mutate(lqgpa = lag(qgpa),
         lcgpa = lag(cum.gpa),
         lprobe = lag(probe),
         lqgpa20 = lag(qgpa20),
         s1_gender = ifelse(s1_gender == "F", 1, 0),
         prior.gpa = pmax(trans_gpa, high_sch_gpa, na.rm = T)) %>%
  ungroup() %>%
  select(system_key, yrq, class, honors_program, tenth_day_credits, scholarship_type, num_ind_study,
         num_courses, nongrd, deduct, tot_creds, lqgpa, lcgpa, lprobe, lqgpa20, s1_gender, prior.gpa,
         child_of_alum, std.test.high, starts_with('hs_'), reg.late.days, reg.late.binary,
         major.change, major.change.count, n.unmet, ft, ft.creds.over)

uw.canvas <- canvas.data.merged %>% inner_join(mrg.dat, by = c('system_key' = 'system_key',
                                                               'ts_yrq' = 'yrq'))

# save merged canvas grade file -------------------------------------------

save(uw.canvas, file = 'data/merged-canvas-grade-data-with-sdb.RData')

