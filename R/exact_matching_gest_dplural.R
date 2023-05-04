library(tidyverse)

source(here::here("R/functions/statdesc_functions.R")),
source(here::here("R/functions/sampling_functions.R"),
  

library(data.table)
library(tidyverse)
library(tcltk)
library(lattice)
library(MatchIt)
library(marginaleffects)

to_keep <- c( "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15", "priorlive", "dmar", "cig_rec", "mhisp_r", 
             "fhispx", "no_infec", "dplural", "rf_fedrg", "gestrec3")

df <- df_final %>%# or modify the data source
  select(all_of(to_keep))

# NETTOYAGE DES DONNEES

# pour mrace6, les labels sont mal codes, donc :
# recoding mrace15 into mrace6 (correctly)
df <- df %>%
  mutate(mrace6 = case_when(
    between(mrace15, 4, 10) ~ 4,
    between(mrace15, 11, 14) ~ 5,
    mrace15 == 15 ~ 6,
    TRUE ~ as.numeric(mrace15))) %>% 
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


#On transforme dplural en variable binaire 0/1 pour le matching exact : 

df$dplural <- ifelse(df$dplural == 1, 0, 1) # Si ce sont des jumeaux alors on code en 1, 0 si singleton
df$dplural
set.seed(100)
df1 <- sample_n(df, 300000)
df1%>%count(dplural) # env 5180 jumeaux pour 300 000  naissances soit 1,73 % de jumeaux

# On calcule la part de jumeaux parmi les traités :
df1 %>% filter(dplural == 1, rf_fedrg ==1) %>% nrow()/df1%>%filter(rf_fedrg==1) %>% nrow()
# On obtient 0,1005 

#On calcule la part de jumeaux parmi les non-traités :
df1 %>% filter(dplural == 1, rf_fedrg ==0) %>% nrow()/df1%>%filter(rf_fedrg==0) %>% nrow()
# On obtient 0,0168 


###### Matching exact avec comme variable expliquée Y = dplural 


m <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
               dmar + cig_rec + mhisp_r + fhispx + no_infec + priorlive,
             data = df1,
             method = "exact",
             k2k = TRUE,
             estimand = "ATT") 
# Effet à la main pour Dplural :
md_T <- match.data(m, group = "treated")
md_C <- match.data(m, group="control")

mean(md_T$dplural) - mean(md_C$dplural) # On obtient comme valeur 0,086

# Effet calculé pour Dplural :
md <- match.data(m)

myreg_dplural=glm(dplural ~ rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                  dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive, data = md, family=binomial(link=logit))
summary(myreg_dplural)
#Dans le summary my_reg on obtient comme coeff associé à rf_fedrg : 2.05

#avg_comparisons(myreg, variables = "rf_fedrg", vcov = ~subclass,
                #newdata = md)

####### POUR GESTREC3 :

#On transforme dplural en variable binaire 0/1 pour le matching exact : 

df$gestrec3 <- ifelse(df$gestrec3 == 1, 1, 0) # Si prématuré alors gestrec3 = 1, 0 sinon 
df$gestrec3
set.seed(100)
df1 <- sample_n(df, 300000)
df1%>%count(gestrec3) # env 31500 prématurés pour 300 000  naissances soit 10,5 % de jumeaux

# On calcule la part de prématurés parmi les traités :
df1 %>% filter(gestrec3 == 1, rf_fedrg ==1) %>% nrow()/df1%>%filter(rf_fedrg==1) %>% nrow()
# On obtient 0,1507

#On calcule la part de prématurés parmi les non-traités :
df1 %>% filter(gestrec3 == 1, rf_fedrg ==0) %>% nrow()/df1%>%filter(rf_fedrg==0) %>% nrow()
# On obtient 0,1045


###### Matching exact avec comme variable expliquée Y = gestrec3


m <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
               dmar + cig_rec + mhisp_r + fhispx + no_infec + priorlive,
             data = df1,
             method = "exact",
             k2k = TRUE,
             estimand = "ATT") 

# Effet à la main pour Gestrec3 :
md_T <- match.data(m, group = "treated")
md_C <- match.data(m, group="control")


# Effet à la main pour gestrec3 :

mean(md_T$gestrec3) - mean(md_C$gestrec3) # On obtient comme valeur 0,0720

# Effet calculé pour gestrec3 : 
md <- match.data(m)

myreg_gestrec3 = glm(gestrec3 ~ rf_fedrg + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
            dmar + cig_rec + mhisp_r + fhispx + no_infec+ priorlive, data = md, family=binomial(link=logit))
summary(myreg_gestrec3 )
#Dans le summary my_reg on obtient comme coeff associé à rf_fedrg : 0,669





