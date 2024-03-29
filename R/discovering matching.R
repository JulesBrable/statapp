
#-----------------------setting environment-------------------------------------
rm(list = ls())

library(data.table)
library(tidyverse)
library(tcltk)
library(lattice)
library(MatchIt)
library(marginaleffects)

library(devtools)
install_github("https://github.com/IQSS/cem.git")

#----------------------------loading dataset------------------------------------
source("R/functions/sampling_functions.R")

path <- "Data"
list.files(path = path)
file <- "data.csv"


to_keep <- c("dbwt", "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15",
             "rf_fedrg", "rf_artec", "priorlive", "dmar", "cig_rec", "mhisp_r", "fhispx",
             "no_infec", "sex", "gestrec3", "dplural")

df <- load_data(file)%>%
  select(all_of(to_keep))


#------------------------ nettoyage des données (comme dans les régressions)----
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

# on recode dmar comme dans la régression
df <- df %>%
  mutate(dmar = case_when(
    dmar == 1 ~ 1,
    is.na(dmar) ~ 9,
    TRUE ~ 0))

# recoding rf_fedrg as follows :  X, U, N -> 0 & Y -> 1
df$rf_fedrg <- ifelse(df$rf_fedrg == "Y", 1, 0)

# recoding rf_artec as follows :  X, U, N -> 0 & Y -> 1
df$rf_artec <- ifelse(df$rf_artec == "Y", 1, 0)

#on supprime les valeurs unknown
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

# recoding cigarette consumption from Y & N to 1 & 0 :
df %>% select(cig_rec) %>% distinct()
df$cig_rec <- ifelse(df$cig_rec == "Y", 1, 0)

#recoding sex
df$sex_M <-  ifelse(df$sex == "M" , 1, 0)
df <- df %>% select(!all_of(c("sex")))

#----------------------------- matching sur rf_fedrg: cem (PREMIER ESSAI)----------------------------------
#on fait le matching
mat <- cem(treatment= "rf_fedrg", data=df, drop = "dbwt")   
mat #affiche le nombre de matchs


#avec un match k2k
matk2k <- cem(treatment= "rf_fedrg", data=df, k2k=TRUE, drop = "dbwt")  
matk2k

#----------------------------- matching sur rf_fedrg: cem avec MatchIt----------
m_fedrg <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
        priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
        data = df,
        method = "cem",
        estimand = "ATT") 

#takes a lot of time
summary(m_fedrg) 

#on plot les distributions
plot(m_fedrg, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
md_fedrg <- match.data(m_fedrg)
summary(md_fedrg)

md_fedrg_T <- match.data(m_fedrg, group = "treated")
summary(md_fedrg_T)

md_fedrg_C <- match.data(m_fedrg, group = "control")
summary(md_fedrg_C)

#puis on estime l'ATT
fit1 <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
           priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
           + gestrec3), data = md_fedrg)

summary(fit1)

avg_comparisons(fit1, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(md_fedrg, rf_fedrg == 1))

#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
               priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dpural,
             data = df,
             method = "cem",
             estimand = "ATT",
             k2k = TRUE) 

#takes a lot of time
summary(mk2k)

#on plot les distributions
plot(mk2k, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_k2k <- match.data(mk2k)
summary(mD_k2k)

#puis on estime l'ATT
fitk2k <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                              + gestrec3), data = mD_k2k)

avg_comparisons(fitk2k, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_k2k, rf_fedrg == 1))


#----------------------matching fedrg EXACT-----------------------
#avec un match exact (essai avec la distance par défaut = Mahalanobis)
mex <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                  priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                data = df,
                method = "exact",
                estimand = "ATT") 

#takes a lot of time
summary(mex)

#on plot les distributions
plot(mex, type = "densiy", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_ex <- match.data(mex)
summary(mD_ex)

mD_ex_T <- match.data(mex, group = "treated")
summary(mD_ex_T)
#à la main, moyenne dbwt pour les traités = 3183

mD_ex_C <- match.data(mex, group="control")
summary(mD_ex_C)
#à la main, moyenne dbwt pour les contrôles = 3312 

#puis on estime l'ATT
fitex <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                  priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                + gestrec3), data = mD_ex)

summary(fitex)

avg_comparisons(fitex, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_ex, rf_fedrg == 1))

#En estimant, on a un coefficient de -77,8

#----------------------matching fedrg JUMEAUX: cem avec MatchIt-----------------------
#on filtre la présence de naissances plural
dfJ <- df %>% filter(dplural != 1)

m_J <- matchit(rf_fedrg ~ + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
               priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
             data = dfJ,
             method = "cem",
             estimand = "ATT") 

summary(m_J) 

#on plot les distributions
plot(m_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cem_J <- match.data(m_J)
summary(mD_cem_J)

mD_cem_J_T <- match.data(m_J, group = "treated")
summary(mD_cem_J_T)
#à la main, moyenne dbwt pour les traités = 2363

mD_cem_J_C <- match.data(m_J, group="control")
summary(mD_cem_J_C)
#à la main, moyenne dbwt pour les contrôles = 2387 

#puis on estime l'ATT
fitcem_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                               + gestrec3), data = mD_cem_J)

avg_comparisons(fitcem_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cem_J, rf_fedrg == 1))

#En estimant, on a un coefficient de -14,1

mex_J <- matchit(rf_fedrg ~ + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
               data = dfJ,
               method = "exact",
               estimand = "ATT") 

summary(mex_J) 

#on plot les distributions
plot(mex_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_ex_J <- match.data(mex_J)
summary(mD_ex_J)

mD_ex_J_T <- match.data(mex_J, group = "treated")
summary(mD_ex_J_T)
#à la main, moyenne dbwt pour les traités = 2363

mD_ex_J_C <- match.data(mex_J, group="control")
summary(mD_ex_J_C)
#à la main, moyenne dbwt pour les contrôles = 2387 

#puis on estime l'ATT
fitex_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                  + gestrec3), data = mD_ex_J)

avg_comparisons(fitex_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_ex_J, rf_fedrg == 1))

#En estimant, on a un coefficient de -14,1



#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k_J <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                  priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                data = dfJ,
                method = "cem",
                estimand = "ATT",
                k2k = TRUE) 

summary(mk2k_J)

plot(mk2k_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cemk2k_J <- match.data(mk2k_J)
summary(mD_cemk2k_J)

mD_cemk2k_J_T <- match.data(mk2k_J, group = "treated")
summary(mD_cemk2k_J_T)

mD_cemk2k_J_C <- match.data(mk2k_J, group="control")
summary(mD_cemk2k_J_C) 

#puis on estime l'ATT
fitcemk2k_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                   priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                 + gestrec3), data = mD_cemk2k_J)

avg_comparisons(fitcemk2k_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cemk2k_J, rf_fedrg == 1))

#----------------------matching SINGLE rf_fedrg : cem avec MatchIt-----------------------
#on filtre la présence de naissances single
dfS <- df %>% filter(dplural == 1)

m_S <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3,
              data = df,
              method = "cem",
              estimand = "ATT") 

summary(m_S) 

#on plot les distributions
plot(m_S, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cem_S <- match.data(m_S)
summary(mD_cem_S)

mD_cem_S_T <- match.data(m_S, group = "treated")
summary(mD_cem_S_T)

mD_cem_S_C <- match.data(m_S, group="control")
summary(mD_cem_S_C)
#à la main, moyenne dbwt pour les contrôles = 3276 

#puis on estime l'ATT
fitcem_S <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                  + gestrec3), data = mD_cem_S)

avg_comparisons(fitcem_S, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cem_S, rf_fedrg == 1))

#En estimant, on a un coefficient de -78.5



#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k_S <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                   priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                 data = df,
                 method = "cem",
                 estimand = "ATT",
                 k2k = TRUE) 

summary(mk2k_S)

plot(mk2k_S, type = "density", interactive = FALSE)





#----------------------------- matching sur rf_artec: cem avec MatchIt----------
m_artec <- matchit(rf_artec ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                     priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                   data = df,
                   method = "cem",
                   estimand = "ATT") 

#takes a lot of time
summary(m_artec) 

#on plot les distributions
plot(m_artec, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
md_artec <- match.data(m_artec)
summary(md_artec)

md_artec_T <- match.data(m_artec, group = "treated")
summary(md_artec_T)

md_artec_C <- match.data(m_artec, group = "control")
summary(md_artec_C)

#puis on estime l'ATT
fit2 <- lm(dbwt ~ rf_artec * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                              + gestrec3), data = md_artec)

avg_comparisons(fit2, variables = "rf_artec", vcov = ~subclass,
                newdata = subset(md_artec, rf_artec == 1))

#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k_artec <- matchit(rf_artec ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                  priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dpural,
                data = df,
                method = "cem",
                estimand = "ATT",
                k2k = TRUE) 

#takes a lot of time
summary(mk2k_artec)

#on plot les distributions
plot(mk2k_artec, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_k2k_artec <- match.data(mk2k_artec)
summary(mD_k2k_artec)

#puis on estime l'ATT
fitk2k_artec <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                  priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                + gestrec3), data = mD_k2k)

avg_comparisons(fitk2k, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_k2k, rf_fedrg == 1))


#----------------------matching artec EXACT-----------------------
#avec un match exact (essai avec la distance par défaut = Mahalanobis)
mex_artec <- matchit(rf_artec ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
               data = df,
               method = "exact",
               estimand = "ATT") 

#takes a lot of time
summary(mex_artec)

#on plot les distributions
plot(mex_artec, type = "densiy", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_ex_artec <- match.data(mex_artec)
summary(mD_ex_artec)

mD_ex_artec_T <- match.data(mex_artec, group = "treated")
summary(mD_ex_artec_T)
#à la main, moyenne dbwt pour les traités = 3183

mD_ex_artec_C <- match.data(mex_artec, group="control")
summary(mD_ex_artec_C)
#à la main, moyenne dbwt pour les contrôles = 3312 

#puis on estime l'ATT
fitex_artec <- lm(dbwt ~ rf_artec * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                               + gestrec3), data = mD_ex_artec)

avg_comparisons(fitex_artec, variables = "rf_artec", vcov = ~subclass,
                newdata = subset(mD_ex_artec, rf_artec == 1))

#En estimant, on a un coefficient de -77,8

#----------------------matching fedrg JUMEAUX: cem avec MatchIt-----------------------
#on filtre la présence de naissances plural
dfJ <- df %>% filter(dplural != 1)

m_J <- matchit(rf_fedrg ~ + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
               data = dfJ,
               method = "cem",
               estimand = "ATT") 

summary(m_J) 

#on plot les distributions
plot(m_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cem_J <- match.data(m_J)
summary(mD_cem_J)

mD_cem_J_T <- match.data(m_J, group = "treated")
summary(mD_cem_J_T)
#à la main, moyenne dbwt pour les traités = 2363

mD_cem_J_C <- match.data(m_J, group="control")
summary(mD_cem_J_C)
#à la main, moyenne dbwt pour les contrôles = 2387 

#puis on estime l'ATT
fitcem_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                  + gestrec3), data = mD_cem_J)

avg_comparisons(fitcem_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cem_J, rf_fedrg == 1))

#En estimant, on a un coefficient de -14,1

mex_J <- matchit(rf_fedrg ~ + mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                   priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                 data = dfJ,
                 method = "exact",
                 estimand = "ATT") 

summary(mex_J) 

#on plot les distributions
plot(mex_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_ex_J <- match.data(mex_J)
summary(mD_ex_J)

mD_ex_J_T <- match.data(mex_J, group = "treated")
summary(mD_ex_J_T)
#à la main, moyenne dbwt pour les traités = 2363

mD_ex_J_C <- match.data(mex_J, group="control")
summary(mD_ex_J_C)
#à la main, moyenne dbwt pour les contrôles = 2387 

#puis on estime l'ATT
fitex_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                   priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                 + gestrec3), data = mD_ex_J)

avg_comparisons(fitex_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_ex_J, rf_fedrg == 1))

#En estimant, on a un coefficient de -14,1



#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k_J <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                  data = dfJ,
                  method = "cem",
                  estimand = "ATT",
                  k2k = TRUE) 

summary(mk2k_J)

plot(mk2k_J, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cemk2k_J <- match.data(mk2k_J)
summary(mD_cemk2k_J)

mD_cemk2k_J_T <- match.data(mk2k_J, group = "treated")
summary(mD_cemk2k_J_T)

mD_cemk2k_J_C <- match.data(mk2k_J, group="control")
summary(mD_cemk2k_J_C) 

#puis on estime l'ATT
fitcemk2k_J <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                       priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                     + gestrec3), data = mD_cemk2k_J)

avg_comparisons(fitcemk2k_J, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cemk2k_J, rf_fedrg == 1))

#----------------------matching SINGLE rf_fedrg : cem avec MatchIt-----------------------
#on filtre la présence de naissances single
dfS <- df %>% filter(dplural == 1)

m_S <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                 priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3,
               data = df,
               method = "cem",
               estimand = "ATT") 

summary(m_S) 

#on plot les distributions
plot(m_S, type = "density", interactive = FALSE)

#on cherche à représenter toutes les variables
mD_cem_S <- match.data(m_S)
summary(mD_cem_S)

mD_cem_S_T <- match.data(m_S, group = "treated")
summary(mD_cem_S_T)

mD_cem_S_C <- match.data(m_S, group="control")
summary(mD_cem_S_C)
#à la main, moyenne dbwt pour les contrôles = 3276 

#puis on estime l'ATT
fitcem_S <- lm(dbwt ~ rf_fedrg * (mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M 
                                  + gestrec3), data = mD_cem_S)

avg_comparisons(fitcem_S, variables = "rf_fedrg", vcov = ~subclass,
                newdata = subset(mD_cem_S, rf_fedrg == 1))

#En estimant, on a un coefficient de -78.5



#avec un match k2k (essai avec la distance par défaut = Mahalanobis)
mk2k_S <- matchit(rf_fedrg ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                    priorlive + dmar + cig_rec + mhisp_r + fhispx + no_infec + sex_M + gestrec3 + dplural,
                  data = df,
                  method = "cem",
                  estimand = "ATT",
                  k2k = TRUE) 

summary(mk2k_S)

plot(mk2k_S, type = "density", interactive = FALSE)