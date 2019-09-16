rm(list = ls())
gc()

library(tidyverse)

setwd(rstudioapi::getActiveProject())

load("data/courses-taken.RData")

# dat <- xf.trs.courses[1:50,]
#
# dat$numeric.grade <- as.numeric(dat$numeric.grade) / 10
# dat$dept_abbrev <- str_replace(dat$dept_abbrev, " ", "_")
#
# datl <- dat %>%
#   group_by(system_key, dept_abbrev) %>%
#   summarize(grade = mean(numeric.grade, na.rm = T))
#
# datl %>% spread(dept_abbrev, grade)

dept.grades <- xf.trs.courses %>%
  mutate(numeric.grade = as.numeric(numeric.grade) / 10,
         dept_abbrev = str_replace(dept_abbrev, ' ', '_')) %>%
  group_by(system_key, dept_abbrev) %>%
  summarize(g = mean(numeric.grade, na.rm = T)) %>%
  spread(dept_abbrev, g)

# (n <- nrow(dept.grades))
i <- apply(dept.grades, 2, function(x) sum(!is.na(x)) > 20)
table(i)

dept.grades <- dept.grades[,i == T]

q1.grades <- xf.trs.courses %>%
  group_by(system_key) %>%
  filter(yrq == min(yrq)) %>%
  ungroup() %>%
  mutate(numeric.grade = as.numeric(numeric.grade) / 10,
         dept_abbrev = str_replace(dept_abbrev, ' ', '_')) %>%
  group_by(system_key, dept_abbrev) %>%
  summarize(g = mean(numeric.grade, na.rm = T)) %>%
  spread(dept_abbrev, g)
replace_nan <- function(x, replacement){                # hack up a correction for div/0 errors
  # [TODO] check type
  i <- is.nan(x)
  x[i] <- replacement
  return(x)
}
q1.grades <- data.frame(apply(q1.grades, 2, function(x) replace_nan(x, 0)))

i <- apply(q1.grades, 2, function(x) sum(!is.na(x)) >= 20); cbind(names(q1.grades)[i == F])
q1.grades <- q1.grades[,i == T]


# gen a running tally per student -----------------------------------------

dept.grades.run <- xf.trs.courses %>%
  mutate(numeric.grade = as.numeric(numeric.grade) / 10,
         dept_abbrev = str_replace(dept_abbrev, ' ', '_')) %>%
  group_by(system_key) %>%
  arrange(system_key, yrq) %>%
  # create common quarter num
  mutate(qnum = rep(1:length(unique(yrq)), times = rle(yrq)$lengths)) %>%
  # summarize by dept_key
  group_by(system_key, qnum, dept_abbrev) %>%
  summarize(gr = mean(numeric.grade, na.rm = T)) %>%
  # suffix
  mutate(dept.qtr = paste(dept_abbrev, qnum, sep = "_")) # %>%
  # [DON'T!] spread vals by quarter for each student
  # spread(dept.qtr, gr)

