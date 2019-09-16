rm(list = ls())
gc()

library(tidyverse)
library(caret)
library(xgboost)
library(xgboostExplainer)
library(doParallel)

set.seed(4567)
theme_set(theme_bw(base_size = 14))
options(tibble.print_max = 500)
cl <- makeCluster((detectCores() - 1), type = "PSOCK")
registerDoParallel(cl)

setwd(rstudioapi::getActiveProject())

load('data/merged-canvas-grade-data-with-sdb.RData')

XY.base <- uw.canvas %>%
  mutate(course.level = course_no %/% 100) %>%
  select(target = final_score, course.level, class:ft.creds.over)

uw.weekly <- uw.canvas %>% select(mean_recent_score_1:missing_count_16)

# weekly model comparisons ------------------------------------------------

# not _all_
# more setup but a smarter way to do this would be to split the weekly data into a list and loop over it
# or I could set up some masks v/ a set of 'base' varnames and go through the weekly data iteratively
# the pipeline could stand improvement but that's a lot of up front work for the moment

# anyway:

# Canvas only baseline 3 week model

params = list(max_depth = 9, eta = .1, nthread = 3, objective = 'reg:linear')

baseline.data <- uw.weekly %>%
  select(mean_recent_score_1:missing_count_3) %>%
  mutate(target = XY.base$target)

baseline.lm <- lm(target ~., data = baseline.data)
summary(baseline.lm)

baseline.dmatrix <- xgb.DMatrix(as.matrix(baseline.data[,-31]), label = baseline.data$target)
baseline.xgb <- xgb.train(param = params,
                          baseline.dmatrix,
                          nrounds = 500,
                          verbose = T)
print(baseline.xgb)
plot(predict(baseline.xgb, baseline.dmatrix), baseline.data$target)

# preproc <- preProcess(baseline.data)
# ppbaseline.data <- predict(preproc, baseline.data)

# baseline.ctrl <- trainControl(method = "cv",
#                               number = 5,
#                               verboseIter = T,
#                               allowParallel = T)
# grid <- expand.grid(nrounds = seq(200, 1e3, 50)
#                     eta = c(.025, .05, .1, .3),
#                     max_depth = c(5, 9),
#                     gamma = 0,
#                     colsample_bytree = 1,
#                     min_child_weight = 1,
#                     subsample = 1)
# baseline.mod <- train(target ~.,
#                       data = baseline.data,
#                       method = "xgbTree",
#                       trControl = baseline.ctrl,
#                       family = "binomial",
#                       tuneGrid = grid)

# re-run with weekly data and sdb vars
w <- bind_cols(XY.base, select(uw.weekly, ends_with('_1')))
dat <- xgb.DMatrix(as.matrix(w), label = w$target)

w1.mod <- xgb.train(param = params,
                    dat,
                    nrounds = 500,
                    verbose = T)
print(w1.mod)
plot(predict(w1.mod, dat), dat$target)

w <- bind_cols(XY.base, select(uw.weekly, ends_with('_1'), ends_with('_2')))
dat <- xgb.DMatrix(as.matrix(w[,-1]), label = w$target)

w2.mod <- xgb.train(param = params,
                    dat,
                    nrounds = 500,
                    verbose = T)
print(w2.mod)
plot(predict(w2.mod, dat), w$target)



# Explain week 2 ----------------------------------------------------------

explr = buildExplainer(w2.mod, trainingData = dat, type = "regression")
pred.breakdown = explainPredictions(w2.mod, explr, dat)
i = 37912
w[i,]
showWaterfall(w2.mod, explr, dat, data.matrix(w[,-1]) , i, type = "regression", threshold = .5)

i <- sample(1:nrow(dat), size = 10, replace = F)
# for(j in 1:length(i)){
#   # png(filename = paste0('viz/xgb-course-grade_', i[j], ".png"), 9, 7.5, units = "in", res = 150)
#   showWaterfall(w2.mod, explr, dat, data.matrix(w[,-1]) , i[j], type = "regression", threshold = .5)
#   # dev.off()
# }
showWaterfall(w2.mod, explr, dat, data.matrix(w[,-1]) , 6180, type = "regression", threshold = .5)




# customize breakdown plot ------------------------------------------------

plot.breakdown <- function(idx, breakdown.obj = pred.breakdown, source.data = w[,-1], Y = w[,1], threshold = .001){
  cur <- as.matrix(breakdown.obj)[idx,]
  weight <- sum(cur)
  pred <- weight
  label.data <- source.data[idx,]

  intercept <- cur[names(cur) == 'intercept']
  cur <- cur[names(cur) != 'intercept']
  label.data <- label.data[names(cur) != 'intercept']

  i <- order(abs(cur), decreasing = T)
  cur <- cur[i]
  label.data <- label.data[i]
  i.other <- which(abs(cur) < threshold)
  other.impact <- 0

  if(length(i.other > 0)) {
    other_impact <- sum(cur[i.other])
    names(other.impact) <- 'other'
    cur = cur[-i.other]
    label.data = label.data[-i.other]
  }
  if (abs(other.impact) > 0) {
    cur <- c(intercept, cur, other.impact)
    label.data = c("", label.data, "")
    labels = paste0(names(cur), " = ", label.data)
    labels[1] = 'intercept'
    labels[length(labels)] = 'other'
  }
  else {
    cur <- c(intercept, cur, other.impact)
    label.data <- c("", label.data)
    labels <- paste0(names(cur), " = ", label.data)
    labels[1] <- 'intercept'
  }

  # cat("\nActual: ", Y[idx])
  # cat("\nPrediction: ", pred)
  # cat("\nWeight: ", weight)
  # cat("\nBreakdown")
  # cat("\n")
  # print(cur)


  waterfalls::waterfall(values = cur,
                        rect_text_labels = round(cur, 2),
                        labels = labels,
                        total_rect_text = round(weight, 2),
                        calc_total = T,
                        total_axis_text = 'Prediction') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

}

plot.breakdown(6180, threshold = .5)
plot.breakdown(20964, threshold = .5)
plot.breakdown(17, threshold = .5)


save(w2.mod, dat, w, explr, pred.breakdown, file = "models/xgb-canvas-week2-mod-and-explainer.RData")