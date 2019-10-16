rm(list = ls())
gc()

setwd(rstudioapi::getActiveProject())

library(tidyverse)

load("data/merged-dataset.RData")
load("data/courses-taken.RData")

