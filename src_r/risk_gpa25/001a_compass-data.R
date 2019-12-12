library(dbplyr)
library(odbc)
library(tidyverse)

setwd(rstudioapi::getActiveProject())

con <- dbConnect(odbc::odbc(), 'compass', UID = config::get("sdb", file = "config.yml")$uid, PWD = keyring::key_get("sdb"))

dbListTables(con)

appt <- tbl(con, 'appointment') %>%
  # filter(year(Date) >= 2008) %>%
  select(ID,
         student_no,
         staff_id,
         Contact_Type,
         Date,
         Time_In,
         Time_Out,
         .data$AddDropclass : .data$Notes,
         Event_Type,
         Source,
         TimeDateIn,
         TimeDateOut,
         AutoLogOut) %>%
  collect()

dbDisconnect(con); rm(con)

con <- dbConnect(odbc(), 'sqlserver01', UID = config::get('sdb', file = 'config.yml')$uid, PWD = keyring::key_get('sdb'))
cal <- tbl(con, in_schema('EDWPresentation.sec', 'dimDate')) %>%
  filter(CalendarYr >= 2008) %>%
  select(CalendarDate, AcademicQtrKeyId, AcademicQtrDayNum, AcademicQtrWeekNum, AcademicQtrName, AcademicQtrCensusDayInd, AcademicYrName) %>%
  collect() %>%
  mutate(yr = as.numeric(AcademicQtrKeyId) %/% 10)

# sid <- tbl(con, in_schema('sec', 'student_1')) %>%
#   select(system_key, student_no) %>%
#   collect()

dbDisconnect(con); rm(con)


# merge -------------------------------------------------------------------

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

adv %>%
  group_by(qtr.label, AcademicQtrWeekNum) %>%
  summarize_at(vars(AddDropclass:NoShow), sum) %>%
  ungroup() %>%
  mutate(xvar = paste(qtr.label, AcademicQtrWeekNum, sep = '-')) %>%
  pivot_longer(., cols = AddDropclass:NoShow) %>%
  ggplot(., aes(x = xvar, y = value, group = name, color = name)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45))

adv %>%
  mutate(hour = as.numeric(str_sub(Time_In, start = 1, end = 2))) %>%
  group_by(qtr.label, hour) %>%
  summarize_at(vars(AddDropclass:NoShow), sum) %>%
  ungroup() %>%
  pivot_longer(., col = AddDropclass:NoShow) %>%
  ggplot(., aes(x = hour, y = name, fill = value)) +
  geom_raster()
