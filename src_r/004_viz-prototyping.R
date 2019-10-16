# viz prototype ideas

# UPDATES to the get.pred function...
# [TODO] re-add course name
# [TODO] re-add SID


# setup -------------------------------------------------------------------

rm(list = ls())
gc()

library(tidyverse)
library(xgboostExplainer)
library(xgboost)
library(scales)

setwd(rstudioapi::getActiveProject())

theme_set(theme_bw(base_size = 14))


# functions ---------------------------------------------------------------
# use roxygen2 style doc
# We could use tidyverse funs but I'm assuming that 1) we'll want to reduce dependencies
# later and, 2) this will get translated to python later


#' Fetch a row from the prediction breakdown and return the top-n items along with the intercept.
#'
#' @param i A row index [TODO] map indices to system_key or student_id/netid/etc.
#' @param n The number of columns to return. Columns in \code{i} are ranked and top-n returned.
#' @param df A built prediction object [TODO] build on fly w/ explainer object
#' @return ... [TODO] optimize return format
#' @examples
#' [TODO] i should point to an ID #
get_pred <- function(i = 1, n = 7, pred.df = test.data, orig.df = label.data){
  predicted.value <- sum(pred.df[i,], na.rm = T)
  intercept <- pred.df$intercept[i]
  cur <- subset(pred.df[i,], select = -intercept)
  ord <- order(abs(cur), decreasing = T)
  cur <- cur[, ..ord]
  other <- sum(cur[, (n+1):ncol(cur)], na.rm = T)
  cur <- cur[, 1:n]
  # print(unlist(unname(cur)))
  # print(as.vector(cur, mode = "numeric"))    # simpler

  # create label w/ true data values
  # print(names(cur))
  label.vals <- orig.df[i, names(cur)]

  plot.labels <- c("intercept",
                   paste0(names(label.vals), " = ", round(label.vals, 2)),
                   "other")

  plot.data <- round(c(intercept, as.vector(cur, mode = 'numeric'), other), 2)
  student <- round(c(NA, as.vector(label.vals, mode = 'numeric'), NA), 2)       # 'intercept', ..., 'other'

  # print(plot.labels)
  # print(plot.data)
  # print(student)

  return(data.frame('labels' = plot.labels,
                    'preds' = plot.data,
                    'student' = student))

}


# data init ---------------------------------------------------------------

load("models/xgb-canvas-week2-mod-and-explainer.RData")
load('data/merged-canvas-grade-data-with-sdb.RData')          # get the course and SID for predictor proto

# try w/o intercept

test <- get_pred(n = 12, pred.df = pred.breakdown, orig.df = w)
test <- test[2:nrow(test),]
test$vord <- seq_len(nrow(test))

# reminders
#  -`explr`
#     --table with the feature impact for each leaf of each tree from fitted model
#  -`pred.breakdown`
#     --feature impact breakdown derived from explr, rows correspond to original data
#     --row sum = model prediction


# Plot tests --------------------------------------------------------------

#  bars -------------------------------------------------------------------

ggplot(data = test, aes(x = reorder(labels, rev(vord)), y = preds, label = preds)) +
  geom_col() +
  geom_text() +
  coord_flip()





# waterfall ---------------------------------------------------------------

# waterfall(values = round(rnorm(5), 1), labels = letters[1:5], calc_total = TRUE)
# waterfall(.data = data.frame(category = letters[1:5],
#                              value = c(100, -20, 10, 20, 110)),
#           fill_colours = colorRampPalette(c("#1b7cd6", "#d5e6f2"))(5),
#           fill_by_sign = FALSE)





# xxxx --------------------------------------------------------------------

# dat <- xgb.DMatrix(as.matrix(w[,-1]), label = w$target)
# p <- predict(w2.mod, dat, predleaf = T)
# # predict w/ leaf = T returns i X n_trees matrix where i,j is the leaf where i ends up
#
# pc <- predict(w2.mod, dat, predcontrib = T)
# # predcontrib returns the feature contributions in form of an (i*tree*leafs(?)) X j+1 (+intercept/BIAS) matrix


# save some shiny test data -----------------------------------------------

sd <- bind_cols('system_key' = uw.canvas$system_key,
                'course' = paste(uw.canvas$dept_abbrev, uw.canvas$course_no, sep = "_"),
                'yrq' = as.integer(uw.canvas$ts_yrq),
                pred.breakdown,
                'pred' = rowSums(pred.breakdown))[1:5000,]

sd <- sd %>%
  group_by(system_key) %>%
  filter(yrq == max(yrq)) %>%
  ungroup()

src <- uw.canvas %>%
  mutate(course = paste(uw.canvas$dept_abbrev, uw.canvas$course_no, sep = "_")) %>%
  semi_join(sd, by = c('system_key' = 'system_key',
                       'course' = 'course',
                       'ts_yrq' = 'yrq'))

click_data <- src %>%
  select(system_key, course, starts_with('clicks_per_week')) %>%
  group_by(system_key, course) %>%
  pivot_longer(., cols = starts_with('clicks'),
               names_to = c('week'),
               names_pattern = 'clicks_per_week_(.*)',
               values_to = 'clicks') %>%
  mutate(week = as.integer(week),
         clicks = round(clicks, 0)) %>%
  ungroup()

save(sd, src, click_data, file = "data/shiny-test-data.RData")


# shiny-proto-sandbox -----------------------------------------------------

yhat <- sd[sd$system_key == 1918667,]
xdat <- src[src$system_key == 1918667,]

# plot pred grades by class
ggplot(yhat, aes(x = course, y = pred)) +
  geom_point(size = 5) +
  geom_segment(aes(x = course, xend = course, y = 0, yend = pred))

# plot course activity (clicks) by week
# turned into click_data above to simplify reactive action
xdat %>% select(course,
                starts_with('clicks_per_week')) %>%
  pivot_longer(., cols = starts_with('clicks'),
               names_to = c('week'),
               names_pattern = 'clicks_per_week_(.*)',
               values_to = 'clicks') %>%
               # names_ptypes = list(week = 'numeric')) %>%
  mutate(week = as.integer(week)) %>%
  filter(week <= 8) %>%
  ggplot(data = ., aes(x = week, y = clicks, group = course)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  facet_grid(rows = vars(course), scales = 'free_y')

# prototype viz of top predictions by each class
test <- sd %>% filter(system_key == 1936909)

# for each class in `test` we want to return the top n predictors
# return a list of data frames
proto_pred <- function(df, n = 5){
  df <- subset(df, select = -c(system_key, course, yrq, intercept, pred))
  # js: row-wise largest n values
  js <- apply(df, 1, function(x) {
    x <- unlist(x)
    x <- x[order(abs(x), decreasing = T)][1:n]
    # x <- data.frame(as.list(x[1:5]))
    res <- data.frame('vars' = names(x),
                      'vals' = unname(x),
                      row.names = NULL)
    return(res)
  })
}
(px <- proto_pred(sd[1:3,]))
