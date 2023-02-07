library(tidyverse)

source("R/functions/sampling_functions.R")
source("R/functions/statdesc_functions.R")

nb_years <- 4
# create and empty dataframe
res <- data.frame(
  matrix(
    nrow = nb_years,
    ncol = 3,
    dimnames = list(NULL, c("Year", "count", "frequency"))
    )
  )

# from 2018 to 2021:
# - load the dataset
# - get the count and proportion of treated
for (i in seq(18, 21)) {
  
  res[i-17, 1] <- glue::glue("20{i}")

  res0 <- load_data(glue::glue("nat20{i}us.csv")) %>% 
    group_by(rf_inftr) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(freq = n * 100 / sum(n)) %>% 
    filter(rf_inftr == "Y")
    
  
  res[i-17, 2] <- res0 %>% 
    pull(n) %>%
    round(2)
  
  res[i-17, 3] <- res0 %>% 
    pull(freq) %>%
    round(2)

}

# convert year to date
res$Year <- lubridate::ymd(paste0(res$Year, "0101"))

res

# plot the result
res %>% 
  ggplot(aes(x = Year, y = perc_T)) +
  geom_line(size = 0.75)+
  geom_point() +
  ylim(0, 3) +
  labs(
    y = "Percentage Treated",
    title = "Changes in the proportion of people treated between 2018 and 2021"
    ) +
  theme_formatted()








