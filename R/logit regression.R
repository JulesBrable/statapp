library(tidyverse)
library(pROC)
library(cobalt)
library(magrittr) 
library(dplyr)

source(here::here("R/functions/statdesc_functions.R"))
source(here::here("R/functions/sampling_functions.R"))

to_keep <- c( "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15", "priorlive", "dmar", "cig_rec", "mhisp_r", 
              "fhispx", "no_infec", "dbwt" , "rf_fedrg", "gestrec3")


df <- load_data("data.csv") %>%
  select(all_of(to_keep))

# NETTOYAGE DES DONNEES

# pour mrace6, les labels sont mal codes, donc :
# recoding mrace15 into mrace6 (correctly)
df <- df %>%
  mutate(mrace6 = case_when(
    between(mrace15, 4, 10) ~ 4,
    between(mrace15, 11, 14) ~ 5,
    mrace15 == 15 ~ 6,
    TRUE ~ as.numeric(mrace15)
  )) %>% 
  select(!mrace15)

# recoding rf_fedrg as follows :  X, U, N -> 0 & Y -> 1
df$rf_fedrg <- ifelse(df$rf_fedrg == "Y", 1, 0)

# on recode dmar
df <- df %>%
  mutate(dmar = case_when(
    dmar == 1 ~ 1,
    is.na(dmar) ~ 9,
    TRUE ~ 0))

#on supprime les valeurs unknown
df <- df %>% 
  filter(fagecomb != 99,
         meduc != 9,
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
# recoding cigarette consumption from Y & N to 1 & 0 :
df %>% select(cig_rec) %>% distinct()
df$cig_rec <- ifelse(df$cig_rec == "Y", 1, 0)

# check des valeurs manquantes
df %>%
  map(~sum(is.na(.)))
#aucune valeur manquante

###### Régression logistique sur l'indicatrice under_1500 : 

# On crée la variable under_1500 : 
df$dbwt_inf_1500 <- ifelse(df$dbwt < 1500 , 1, 0)

logit_1500 <- glm(dbwt_inf_1500 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
               dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , family=binomial(link="logit"), data=df) 

summary(logit_1500)

# Odds ratio

logit_1500.or = exp(coef(logit_1500))
logit_1500.or

# Marginal Effects 

install.packages("mfx") 
library(mfx)
logitmfx(dbwt_inf_1500 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
           dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , data=df) 



###### Régression logistique sur l'indicatrice under_2500 : 

# On crée la variable under_2500 : 
df$dbwt_inf_2500 <- ifelse(df$dbwt < 2500 , 1, 0)

logit_2500 <- glm(dbwt_inf_2500 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                    dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , family=binomial(link="logit"), data=df) 

summary(logit_2500)

# Odds ratio

logit_2500.or = exp(coef(logit_2500))
logit_2500.or

# Marginal Effects 

library(mfx)
logitmfx(dbwt_inf_2500 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
           dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , data=df) 


###### Régression logistique sur l'indicatrice gestrec3 : 

# On crée la variable binaire gestrec3 en 0/1 : 
df$gestrec3 <- ifelse(df$gestrec3 == 1, 1, 0)

logit_gestrec3 <- glm(gestrec3 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                    dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , family=binomial(link="logit"), data=df) 

summary(logit_gestrec3)

# Odds ratio

logit_gestrec3.or = exp(coef(logit_gestrec3))
logit_gestrec3.or

# Marginal Effects 

install.packages("mfx") 
library(mfx)
logitmfx(gestrec3 ~  rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
           dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive , data=df) 

