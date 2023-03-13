library(tidyverse)
library(pROC)
library(cobalt)

source(here::here("R/functions/statdesc_functions.R"))
source(here::here("R/functions/sampling_functions.R"))

to_keep <- c("dbwt", "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15",
             "rf_fedrg", "priorlive", "dmar", "cig_rec", "mhisp_r", "fhispx",
             "no_infec", "sex", "gestrec3")

df <- load_data("data.csv") %>%
  select(all_of(to_keep))

# recoding mrace15 into mrace6 (correctly)
df <- df %>%
  mutate(mrace6 = case_when(
    between(mrace15, 4, 10) ~ 4,
    between(mrace15, 11, 14) ~ 5,
    mrace15 == 15 ~ 6,
    TRUE ~ as.numeric(mrace15)
  )) %>% 
  select(!mrace15)

# recoding marital status
df <- df %>%
  mutate(dmar = case_when(
    dmar == 1 ~ 1,
    is.na(dmar) ~ 9,
    TRUE ~ 0
  ))

# remove NA values
df <- df %>% 
  filter(fagecomb != 99,
         meduc != 9,
         dbwt != 9999,
         feduc != 9,
         frace6 != 9,
         mrace6 != 9,
         priorlive != 99,
         mhisp_r != 9,
         fhispx != 9,
         cig_rec != "U",
         dmar != 9,
         no_infec != 9,
         gestrec3 != 3)

df %>% nrow()

## logarithmic transformation of birthweight
df$ldbwt <- df$dbwt %>% log()
df <- df %>% select(!dbwt)

## one hot encoding

# recoding sex of the newborn, infection and period of gestation
df$sex_M <-  ifelse(df$sex == "M" , 1, 0)
df$infection <- ifelse(df$no_infec == 0 , 1, 0)
df$gest_under_37_weeks <- ifelse(df$gestrec3 == 1 , 1, 0) 

# then we have to remove the original variables
df <- df %>% select(!all_of(c("sex", "no_infec", "gestrec3")))

# mother's and fathers's hispanic origin
# 0 = non hispanic, 1 = hispanic origin 
df %>% select(mhisp_r) %>% distinct()
###########df$mhisp_r <- ifelse(df$mhisp_r != 0 , 1, 0)
###########df$fhispx <- ifelse(df$fhispx != 0 , 1, 0)

# recoding cigarette consumption from Y & N to 1 & 0 :
df %>% select(cig_rec) %>% distinct()
df$cig_rec <- ifelse(df$cig_rec == "Y", 1, 0)

# recoding rf_fedrg as follows :  X, U, N -> 0 & Y -> 1
df$rf_fedrg <- ifelse(df$rf_fedrg == "Y", 1, 0)

mylogit <- glm(rf_fedrg ~ ., data = df, family = binomial)

summary(mylogit)

# Estimate the probability of the target variable using the logit coefficients
df$propensity_score <- predict(mylogit, newdata = df, type = "response")

# AUC & ROC curve
roc_curve <- roc(df$rf_fedrg, df$propensity_score)
round(auc(roc_curve), 2)
plot(roc_curve, main = "ROC Curve")

# matching
matched_data <- Matching::Match(Tr = df$rf_fedrg,
                      M = 1,
                      X = df$propensity_score,
                      caliper = 0.05,
                      replace = FALSE,
                      ties = FALSE)

# Evaluate the balance of covariates
bal.tab <- bal.tab(matched_data, df %>% dplyr::select(!rf_fedrg))
summary(bal.tab)



