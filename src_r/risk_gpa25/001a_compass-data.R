rm(list = ls())
gc()

library(dbplyr)
library(odbc)
library(tidyverse)

setwd(rstudioapi::getActiveProject())

options(tibble.print_min = 30)

# fetch (new) data --------------------------------------------------------

# con <- dbConnect(odbc::odbc(), 'compass', UID = config::get("sdb", file = "config.yml")$uid, PWD = keyring::key_get("sdb"))
#
# dbListTables(con)
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
# con <- dbConnect(odbc(), 'sqlserver01', UID = config::get('sdb', file = 'config.yml')$uid, PWD = keyring::key_get('sdb'))
# cal <- tbl(con, in_schema('EDWPresentation.sec', 'dimDate')) %>%
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
# save(appt, cal, sid, file = 'src_r/risk_gpa25/data/compass.RData')

# merge -------------------------------------------------------------------

rm(list = ls())
gc()

load('src_r/risk_gpa25/data/compass.RData')

dat <- appt %>%
  # mutate(Date = as.POSIXct(Date, tz = 'UTC')) %>%
  mutate(t_in = paste(str_sub(Date, end = 10), str_sub(Time_In, end = 5), sep = ' '),
         Date = as.POSIXct(Date, tz = 'UTC')) %>%
  filter(str_length(t_in) == 16) %>%
  inner_join(cal, by = c('Date' = 'CalendarDate')) %>%
  mutate(AcademicQtrKeyId = as.numeric(AcademicQtrKeyId),
         qtr.num = AcademicQtrKeyId %% 10,
         qtr.label = factor(qtr.num, labels = c('Winter', 'Spring', 'Summer', 'Autumn')), # levels = c('Winter', 'Spring', 'Summer', 'Autumn')),
         t_in = strftime(t_in, format = '%Y-%m-%d %H:%M', tz = 'UTC'))

# dat$staff_id[is.na(dat$staff_id)] <- 'None'
dat <- dat %>%
  mutate_at(vars(.data$AddDropclass : .data$Other), as.numeric) %>%
  mutate(NoShow = as.numeric(if_else(NoShow == 'Y', 1, 0)))

hs <- dat[dat$Source == 'HS',]
# ic <- dat %>%
#   filter(dat$Source %in% c('Arlyn', 'basement', 'Biology', 'chem', 'dev3', 'EpMac', 'FrontDesk', 'Lecture Room', 'mac', 'Math', 'test10' 'Writing Center'))
ic <- dat[grep('^IC', dat$Contact_Type),]
adv <- dat %>% filter(dat$Contact_Type %in% c('Appointment', 'Quick Question', 'Notes', 'Email Contact', 'Email', 'Telephone',
                                              'Email Message', 'Workshop', 'General Question', 'Classroom Presentation', 'ECC Meeting or Event',
                                              'Admin Notes'))


# describe ----------------------------------------------------------------

# ftable(xtabs( ~staff_id + yr, data = adv, addNA = T))
# dat %>%
#   filter(!is.na(staff_id)) %>%
#   group_by(staff_id, AcademicQtrKeyId) %>%
#   summarize(n = n()) %>%
#   ggplot(., aes(x = AcademicQtrKeyId, y = n, group = staff_id, color = staff_id)) +
#   geom_line()

cbind(apply(adv[,8:28], 2, function(x) round(mean(x, na.rm = T), 3)))
#
# adv %>%
#   # group_by(qtr.label, AcademicQtrWeekNum) %>%
#   summarize_at(vars(AddDropclass:NoShow), sum, na.rm = T) %>%
#   # ungroup() %>%
#   # mutate(xvar = paste(qtr.label, AcademicQtrWeekNum, sep = '-')) %>%
#   pivot_longer(., cols = AddDropclass:NoShow) %>%
#   arrange(desc(value))
#
# adv %>%
#   group_by(qtr.label, AcademicQtrWeekNum) %>%
#   summarize_at(vars(AddDropclass:NoShow), sum) %>%
#   ungroup() %>%
#   mutate(xvar = paste(qtr.label, AcademicQtrWeekNum, sep = '-')) %>%
#   pivot_longer(., cols = AddDropclass:NoShow) %>%
#   ggplot(., aes(x = xvar, y = value, group = name, color = name)) +
#   geom_line() +
#   theme(axis.text.x = element_text(angle = 45))
#
# adv %>%
#   mutate(hour = as.numeric(str_sub(Time_In, start = 1, end = 2))) %>%
#   group_by(qtr.label, hour) %>%
#   summarize_at(vars(AddDropclass:NoShow), sum) %>%
#   ungroup() %>%
#   pivot_longer(., col = AddDropclass:NoShow) %>%
#   ggplot(., aes(x = hour, y = name, fill = value)) +
#   geom_raster()


# aggregate ---------------------------------------------------------------

# create weekly data?

adv.agg <- adv %>%
  group_by(student_no, yr, qtr.num) %>%
  summarize_at(vars(AddDropclass:NoShow), sum, na.rm = T) %>%
  ungroup() %>%
  mutate(visit_advising = 1)
ic.activity.agg <- ic %>%
  group_by(student_no, yr, qtr.num, Contact_Type) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  pivot_wider(., names_from = 'Contact_Type', values_from = 'n')
ic.activity.agg[,4:17] <- apply(ic.activity.agg[,4:17], 2, function(x) replace_na(x, 0))

str_clean <- function(x){
  x <- str_remove_all(x, '[[:punct:]]')
  # x <- tolower(x)
  x <- str_replace_all(x, " ", "_")
}

n <- str_clean(names(ic.activity.agg[4:ncol(ic.activity.agg)]))
# n <- str_remove_all(n, '[[:punct:]]')
# n <- c(names(ic.activity.agg[1:3]), n)
names(ic.activity.agg) <- c(names(ic.activity.agg[1:3]), n)
ic.activity.agg$visit_ic <- 1

# by weeks:
# NA weeks will be 99? Probably they should be the next year/qtr and get a 0
next.yrq <- function(x){
  x <- x + ifelse(x %% 10 == 4, 7, 1)
  return(x)
}

# [TODO] fix the yrq sequencing for the above data as well

adv.wk <- adv %>%
  filter(NoShow == 0) %>%
  rename(yrq = AcademicQtrKeyId, week = AcademicQtrWeekNum) %>%
  # mutate(visit_advising = 1) %>%
  group_by(student_no, yrq, week) %>%
  # summarize(visit_advising = sum(visit_advising)) %>%
  summarize(visit_advising = n()) %>%
  ungroup() %>%
  mutate(week = replace_na(week, 0),
         yrq = if_else(week == 0, next.yrq(yrq), yrq)) %>%
  pivot_wider(., names_from = 'week', values_from = 'visit_advising', names_prefix = 'advising_week_')
adv.wk[,3:ncol(adv.wk)] <- apply(adv.wk[,3:ncol(adv.wk)], 2, function(x) replace_na(x, 0))

ic.wk <- ic %>%
  rename(yrq = AcademicQtrKeyId, week = AcademicQtrWeekNum) %>%
  group_by(student_no, yrq, week) %>%
  summarize(visit_ic = n()) %>%
  ungroup() %>%
  mutate(week = replace_na(week, 0),
         yrq = if_else(week == 0, next.yrq(yrq), yrq)) %>%
  pivot_wider(., names_from = 'week', values_from = 'visit_ic', names_prefix = 'ic_week_')
ic.wk[,3:ncol(ic.wk)] <- apply(ic.wk[,3:ncol(ic.wk)], 2, function(x) replace_na(x, 0))


# add system_key; combine ----------------------------------------------------------

compass.feats <- full_join(adv.agg, ic.activity.agg) %>%
  filter(student_no > 1) %>%
  mutate(yrq = (yr*10)+qtr.num) %>%
  select(-yr, -qtr.num) %>%
  inner_join(sid) %>%
  select(-student_no)

compass.weekly <- full_join(adv.wk, ic.wk) %>%
  filter(student_no > 1) %>%
  inner_join(sid) %>%
  select(-student_no)

save(compass.feats, compass.weekly, file = 'src_r/risk_gpa25/data/compass-features.RData')
