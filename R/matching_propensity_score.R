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

mylogit <- glm(rf_fedrg ~ .,
               data = df %>% select(!ldbwt),
               family =  binomial(link = "logit"))

summary(mylogit)

# Exponentiate the coefficients to get the odds ratios
coef <- coef(mylogit)
odds_ratio <- exp(coef)
odds_ratio

# Estimate the probability of the target variable using the logit coefficients
df$propensity_score <- mylogit$fitted.values

# AUC & ROC curve
roc_curve <- roc(df$rf_fedrg, df$propensity_score)
round(auc(roc_curve), 2)
plot(roc_curve, main = "ROC Curve")

# matching
matched_data <- Matching::Match(
  Y = df$ldbwt,
  Tr = df$rf_fedrg,
  X = df$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)

save(matched_data, file = paste0("data/", "matched_data.Rdata"))

matched_data <- miceadds::load.Rdata2(paste0("data/", "matched_data.Rdata"))

summary(matched_data)

# Evaluate the balance of covariates
#bal.tab <- bal.tab(toto, df %>% dplyr::select(!rf_fedrg))
#summary(bal.tab)

df_mb <- df %>% select(!c(propensity_score, ldbwt))

set.seed(42)
mb  <- Matching::MatchBalance(rf_fedrg ~ .,
                    data=df_mb,
                    match.out=matched_data,
                    nboots=10)

# ------------------------------------------------------------------------------
# Pour la sous-population des jumeaux et plus : 

to_keep2 <- c("dbwt", "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15",
             "rf_fedrg", "priorlive", "dmar", "cig_rec", "mhisp_r", "fhispx",
             "no_infec", "sex", "gestrec3", "dplural")

df2 <- load_data("data.csv") %>%
  select(all_of(to_keep2))

# on ne garde que les lignes avec dplural > 1 

df2 <- df2[df2$dplural > 1, ] 
unique(df2$dplural) # il ne reste plus que 2,3,4 et 5 comme valeurs (jumeaux, triplets ...)

# recoding mrace15 into mrace6 (correctly)
df2 <- df2 %>%
  mutate(mrace6 = case_when(
    between(mrace15, 4, 10) ~ 4,
    between(mrace15, 11, 14) ~ 5,
    mrace15 == 15 ~ 6,
    TRUE ~ as.numeric(mrace15)
  )) %>% 
  select(!mrace15)

# recoding marital status
df2 <- df2 %>%
  mutate(dmar = case_when(
    dmar == 1 ~ 1,
    is.na(dmar) ~ 9,
    TRUE ~ 0
  ))

# remove NA values
df2 <- df2 %>% 
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

df2 %>% nrow()

## logarithmic transformation of birthweight
df2$ldbwt <- df2$dbwt %>% log()
df2 <- df2 %>% select(!dbwt)

## one hot encoding

# recoding sex of the newborn, infection and period of gestation
df2$sex_M <-  ifelse(df2$sex == "M" , 1, 0)
df2$infection <- ifelse(df2$no_infec == 0 , 1, 0)
df2$gest_under_37_weeks <- ifelse(df2$gestrec3 == 1 , 1, 0) 

# then we have to remove the original variables
df2 <- df2 %>% select(!all_of(c("sex", "no_infec", "gestrec3")))

# mother's and fathers's hispanic origin
# 0 = non hispanic, 1 = hispanic origin 
df2 %>% select(mhisp_r) %>% distinct()
###########df$mhisp_r <- ifelse(df$mhisp_r != 0 , 1, 0)
###########df$fhispx <- ifelse(df$fhispx != 0 , 1, 0)

# recoding cigarette consumption from Y & N to 1 & 0 :
df2 %>% select(cig_rec) %>% distinct()
df2$cig_rec <- ifelse(df2$cig_rec == "Y", 1, 0)

# recoding rf_fedrg as follows :  X, U, N -> 0 & Y -> 1
df2$rf_fedrg <- ifelse(df2$rf_fedrg == "Y", 1, 0)

mylogit2 <- glm(rf_fedrg ~ .,
               data = df2 %>% select(!ldbwt),
               family =  binomial(link = "logit"))

summary(mylogit2)

# Exponentiate the coefficients to get the odds ratios
coef2 <- coef(mylogit2)
odds_ratio2 <- exp(coef2)
odds_ratio2

# Estimate the probability of the target variable using the logit coefficients
df2$propensity_score <- mylogit2$fitted.values

# AUC & ROC curve
roc_curve2 <- roc(df2$rf_fedrg, df2$propensity_score)
round(auc(roc_curve2), 2)
plot(roc_curve2, main = "ROC Curve")

# matching
matched_data2 <- Matching::Match(
  Y = df2$ldbwt,
  Tr = df2$rf_fedrg,
  X = df2$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)

save(matched_data2, file = paste0("data/", "matched_data2.Rdata"))

matched_data2 <- miceadds::load.Rdata2(paste0("data/", "matched_data2.Rdata"))

summary(matched_data2)

# Evaluate the balance of covariates
#bal.tab <- bal.tab(toto, df %>% dplyr::select(!rf_fedrg))
#summary(bal.tab)

df_mb2 <- df2 %>% select(!c(propensity_score, ldbwt))

set.seed(42)
mb2  <- Matching::MatchBalance(rf_fedrg ~ .,
                              data=df_mb,
                              match.out=matched_data,
                              nboots=10)
