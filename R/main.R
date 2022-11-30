library(data.table)
library(tidyverse)

source("R/statdesc_functions.R")

path <- "Downloads"              ## to replace
file <- "natl2007.csv"           ## to replace

tab2007 <- fread(glue::glue("{path}/{file}"))
# don't directly view the raw table (too large)

head(tab2007)

colnames(tab2007)

tab2007 %>% 
  group_by(birth) %>% 
  count()

tab2007 %>% var_summary("dbwt")
tab2007 %>% density_plot("dbwt")



