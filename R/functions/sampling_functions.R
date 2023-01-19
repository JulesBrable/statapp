library(tidyverse)

# ---------------------------------------------------------------------

load_data <- function(file, path = "data"){
  
  tab2021 <- arrow::read_csv_arrow(
    here::here(path, file)
  )
}

# -------------------------------------------------------------------------

sample_df <- function(file, n, segment_treat = TRUE, path = "data",
                      col_to_segment = rf_inftr, remove = NULL){
  
  # Sample rows of data without replacement #
  
  set.seed(42) # reproducibility
  
  if (!isTRUE(segment_treat)) {
    
    if (length(remove) > 0) {
      return(
        load_data(path = path, file = file) %>%
          sample_n(n) %>% 
          select(!any_of(remove))
        )
    } else {
      return(load_data(path = path, file = file) %>% sample_n(n))
    }
    
  } else {
    
    df <- load_data(path = path, file = file)
    
    dft <- df %>%
      filter({{col_to_segment}} == "N") %>% # without treatment
      sample_n(n/2) %>% 
      bind_rows( # with treatment
        df %>% 
          filter({{col_to_segment}} == "Y") %>% 
          sample_n(min(n/2), nrow(df %>% filter({{col_to_segment}} == "Y")))
      )
    
    rm(df)
    
    if (length(remove) > 0) {
      return(dft %>% select(!any_of(remove)))
    } else {
      return(dft)
    }
  }
}

# -------------------------------------------------------------------------

diff_2_csv <- function(csv1, csv2, n = 1, segment_treat = F){
  
  # Checks if there is differences in the column names of 2 given csv
  # Uses the {sample_df} function
  #If so, return a vector of string containing these names
  #Takes about 30 seconds to run.
  
  ndiff1 <- setdiff(
    (sample_df(csv1, n = n, segment_treat = segment_treat) %>% colnames()),
    (sample_df(csv2, n = n, segment_treat = segment_treat) %>% colnames())
  )
  
  ndiff2 <- setdiff(
    (sample_df(csv2, n = n, segment_treat = segment_treat) %>% colnames()),
    (sample_df(csv1, n = n, segment_treat = segment_treat) %>% colnames())
  )
  
  return(c(ndiff1, ndiff2))
}

# -------------------------------------------------------------------------

make_sample <- function(n, to_remove){
  
  # sample the 4 dataset from 2018, 2019, 2020 and 2021
  s2021 <- sample_df("nat2021us.csv",  n, remove = to_remove)
  s2020 <- sample_df("nat2020us.csv", n, remove = to_remove)
  s2019 <- sample_df("nat2019us.csv", n, remove = to_remove)
  s2018 <- sample_df("nat2018us.csv", n, remove = to_remove)
  
  # then bind the rows of each sample all together
  mixsample <- s2021 %>% 
    dplyr::bind_rows(s2020) %>% 
    dplyr::bind_rows(s2019) %>% 
    dplyr::bind_rows(s2018)
  
  rm(s2021, s2020, s2019, s2018)
  
  return(mixsample)
}



