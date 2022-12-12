
# -------------------------------------------------------------------------

sample_df <- function(file, n, segment_treat = TRUE, path = "data",
                      col_to_segment = rf_inftr, remove = NULL){
  
  # Sample rows of data without replacement #
  
  set.seed(42) # reproducibility
  
  if (segment_treat == FALSE){
    
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


