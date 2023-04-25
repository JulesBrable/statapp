source(here::here("R/functions/sampling_functions.R"))

## import data ##
# designing our sample #

to_remove <- diff_2_csv("nat2019us.csv", "nat2021us.csv")
n <- 300000

mixsample <- make_sample(n = n, to_remove = to_remove)

write_csv(mixsample, here::here("data", "data.csv"))

rm(list = ls())
