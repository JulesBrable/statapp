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

## regression of dbwt on all control variables excluding rf_fedrg 

df_reg_dbwt_excluding_re_fedrg <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))

df_reg_dbwt_excluding_re_fedrg  %>% colnames()

df_reg_dbwt_excluding_re_fedrg  <- df_reg_dbwt_excluding_re_fedrg  %>% 
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

# excluding re_fedrg

df_reg_dbwt_excluding_re_fedrg <- df_reg_dbwt_excluding_re_fedrg %>% select(!rf_fedrg)

reg_dbwt_excluding_re_fedrg <- lm(dbwt ~ .,
          data = df_reg_dbwt_excluding_re_fedrg )

summary(reg_dbwt_excluding_re_fedrg )

today <- Sys.Date()
reg %>% 
  stargazer::stargazer(
    type = "html",
    out = glue::glue("reports/reg_{today}.html")
  )

# regression of ldbwt on all control variables excluding rf_fedrg 

df_reg_ldbwt_excluding_re_fedrg <- df_reg_dbwt_excluding_re_fedrg 
df_reg_ldbwt_excluding_re_fedrg$ldbwt <- df_reg_ldbwt_excluding_re_fedrg$dbwt %>% log()
df_reg_ldbwt_excluding_re_fedrg <- df_reg_ldbwt_excluding_re_fedrg %>% select(!dbwt)


reg_ldbwt_excluding_re_fedrg <- lm(ldbwt ~ .,
                                  data = df_reg_ldbwt_excluding_re_fedrg )

summary(reg_ldbwt_excluding_re_fedrg )

today <- Sys.Date()
reg %>% 
  stargazer::stargazer(
    type = "html",
    out = glue::glue("reports/reg_{today}.html")
  )

# taking gest_under_37_weeks as interest variable 

df_reg_premature <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))
df_reg_premature <- df_reg_premature%>% select(!dbwt)

reg_premature <- lm(gest_under_37_weeks ~ .,
                                   data = df_reg_premature)

summary(reg_premature)

today <- Sys.Date()
reg %>%  
  stargazer::stargazer(
    type = "html",
    out = glue::glue("reports/reg_{today}.html")
  )

# Taking gest_under_37_weeks as interest variable but excluding dplural from the control variable

df_reg_premature_drop_dplural <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))
df_reg_premature_drop_dplural <- df_reg_premature_drop_dplural%>% select(!c(dplural,dbwt))

reg_premature_drop_dplural <- lm(gest_under_37_weeks ~ .,
                    data = df_reg_premature_drop_dplural)

summary(reg_premature_drop_dplural)

# Taking under_1500 as interest variable 

df_reg_under_1500<- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))
df_reg_under_1500$dbwt_inf_1500 <- ifelse(df_reg_under_1500$dbwt < 1500 , 1, 0)
df_reg_under_1500 <- df_reg_under_1500%>% select(!c(dplural,dbwt,gest_under_37_weeks))

reg_under_1500 <- lm(dbwt_inf_1500 ~ .,
                                 data = df_reg_under_1500)

summary(reg_under_1500)

# Taking under_2500 as interest variable 

df_reg_under_2500<- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))
df_reg_under_2500$dbwt_inf_2500 <- ifelse(df_reg_under_2500$dbwt < 2500 , 1, 0)
df_reg_under_2500 <- df_reg_under_2500%>% select(!c(dplural,dbwt,gest_under_37_weeks))

reg_under_2500 <- lm(dbwt_inf_2500 ~ .,
                     data = df_reg_under_2500)

summary(reg_under_2500)
