library(tidyverse)

path <- "data"
df_reg1 <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))

df_reg1 %>% colnames()

df_reg1 <- df_reg1 %>% 
  rename(c(
    "meduc_less_hs" = "meduc_1",
    "meduc_hs_nodiploma" = "meduc_2",
    "meduc_hs_diploma" = "meduc_3",
    "meduc_no_degree" = "meduc_4",
    "meduc_associate_degree" = "meduc_5",
    "meduc_master" = "meduc_7",
    "meduc_doctorate" = "meduc_8",
    "feduc_less_hs" = "feduc_1",
    "feduc_hs_nodiploma" = "feduc_2",
    "feduc_hs_diploma" = "feduc_3",
    "feduc_no_degree" = "feduc_4",
    "feduc_associatedegree" = "feduc_5",
    "feduc_master" = "feduc_7",
    "feduc_doctorate" = "feduc_8",
    "mrace_black" = "mrace6_2",
    "mrace_aian" = "mrace6_3",
    "mrace_asian" = "mrace6_4",
    "mrace_nhopi" = "mrace6_5",
    "mrace_more_than_one_race" = "mrace6_6",
    "frace_black" = "frace6_2",
    "frace_aian" = "frace6_3", 
    "frace_asian" = "frace6_4",
    "frace_nhopi" = "frace6_5",
    "frace_more_than_one_race" = "frace6_6",
    ))

## logarithmic transformation of birthweight
df_reg1$ldbwt <- df_reg1$dbwt %>% log()
df_reg1 <- df_reg1 %>% select(!dbwt)

reg <- lm(ldbwt ~ .,
          data = df_reg1)

summary(reg)

today <- Sys.Date()
reg %>% 
  stargazer::stargazer(
    type = "html",
    out = glue::glue("reports/reg_{today}.html")
    )
