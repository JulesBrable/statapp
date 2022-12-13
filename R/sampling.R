
# SETTING ENVIRONEMENT ----------------------------------------------------
rm(list = ls())

library(data.table)
library(tidyverse)

source(here::here("R/statdesc_functions.R"))
source(here::here("R/sampling_functions.R"))

list.files(path = "data")

# issue with the column names #
ndiff1 <- setdiff(
  (sample_df("nat2019us.csv", n = 1, segment_treat = F) %>% colnames()),
  (sample_df("nat2021us.csv", n = 1, segment_treat = F) %>% colnames())
)

ndiff2 <- setdiff(
  (sample_df("nat2021us.csv", n = 1, segment_treat = F) %>% colnames()),
  (sample_df("nat2019us.csv", n = 1, segment_treat = F) %>% colnames())
)

ndiff3 <- setdiff(
  (sample_df("nat2021us.csv", n = 1, segment_treat = F) %>% colnames()),
  (sample_df("nat2018us.csv", n = 1, segment_treat = F) %>% colnames())
)

to_remove <- c(ndiff1, ndiff2, ndiff3)


## import data ##
# loading all 2021 data in order to compare
tab2021 <- load_data("nat2021us.csv")

# designing our sample
n <- 300000
s2021 <- sample_df("nat2021us.csv", n, remove = to_remove)
s2020 <- sample_df("nat2020us.csv", n, remove = to_remove)
s2019 <- sample_df("nat2019us.csv", n, remove = to_remove)
s2018 <- sample_df("nat2018us.csv", n, remove = to_remove)

mixsample <- s2021 %>% 
  bind_rows(s2020) %>% 
  bind_rows(s2019) %>% 
  bind_rows(s2018)

# STAT DESC ---------------------------------------------------------------

tab2021 %>% 
  get_prop(rf_inftr) %>% 
  bind_rows(tab2021 %>% 
              get_prop(rf_fedrg)) %>% 
  bind_rows(tab2021 %>% 
              get_prop(rf_artec)) %>% 
  mutate(var = c("rf_inftr", "rf_fedrg", "rf_artec")) %>%
  relocate(var) %>% 
  kableExtra::kbl() %>% 
  template()

mixsample %>% 
  get_prop(rf_inftr) %>% 
  bind_rows(tab2021 %>% 
              get_prop(rf_fedrg)) %>% 
  bind_rows(tab2021 %>% 
              get_prop(rf_artec)) %>% 
  mutate(var = c("rf_inftr", "rf_fedrg", "rf_artec")) %>%
  relocate(var) %>% 
  kableExtra::kbl() %>% 
  template()

s2020 %>%
  transform_age() %>% 
  pyrage()

tab2021 %>%
  transform_age() %>% 
  pyrage()

tab2021 %>%
  get_prop(rf_artec) %>%
  kableExtra::kbl() %>% 
  template()

