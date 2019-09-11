rm(list = ls())
gc()

train <- readr::read_csv("data/pytrain.csv")
test <- readr::read_csv("data/pytest.csv")

names(test)
test <- test[,-1]

train.key <- train$system_key
test.key <- test$system_key

train <- subset(train, select = -c(system_key))
test <- subset(test, select = -c(system_key))

xy <- subset(train, subset = train$qtr.num == 1,
             select = c(qgpa, class.1, class.2, class.3, class.4, tenth_day_credits, num_courses,
                        attmp, nongrd, s1_genderF, child_of_alumTRUE, running_startY, HispanicIndN,
                        s1_high_satm, s1_high_satv, age, conditional, high_sch_gpa,
                        hs_yrs_math, hs_yrs_english, hs_math_level, reg.late.days,
                        n.unmet, ftFALSE, ft.creds.over))
xy <- xy[complete.cases(xy),]
fit <- lm(qgpa ~., data = xy)
summary(fit)
pred <- predict(fit)
sqrt(mean(((xy$qgpa - pred)^2)))
plot(xy$qgpa, pred)


# canvas/instructure file ------------------------------------------------------------

path.to.canvas <- "../../../../../UW_FinalGrades/"
can <- readr::read_csv(paste0(path.to.canvas, "uw-grades.csv"), col_names = T,
                       cols(.default = col_double(),
                            course_key = col_character()))

courses.can <- readr::read_csv(paste0(path.to.canvas, "courses-provisioning_08122019.csv"))
stu.can <- readr::read_csv(paste0(path.to.canvas, "student-provisioning_07092019.csv"))

