library(tidyverse)

source(here::here("R/functions/statdesc_functions.R"))
source(here::here("R/functions/sampling_functions.R"))

to_keep <- c("dbwt", "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15",
             "rf_fedrg", "dplural", "priorlive", "dmar", "cig_rec", "mhisp_r", "fhispx",
             "no_infec", "sex", "gestrec3", "gestrec10")

df <- df_final %>%# or modify the data source
  select(all_of(to_keep))

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

# check des valeurs manquantes
df %>%
  map(~sum(is.na(.)))

# beaucoup de valeurs manquantes pour le statut marital : on les code en 9
# (on supprimera cette valeur apres) 
# de plus, il faut la recoder en 0 - 1 (initialement : 1 (married) - 2)

df <- df %>%
  mutate(dmar = case_when(
    dmar == 1 ~ 1,
    is.na(dmar) ~ 9,
    TRUE ~ 0
  ))

# les autres valeurs manquantes ne sont pas notees en NA mais sont codées
# (cf documentation) :

df %>% 
  filter(
    fagecomb == 99 |
      meduc == 9 |
      dbwt == 9999 |
      feduc == 9 |
      frace6 == 9 |
      mrace6 == 9 |
      priorlive == 99 |
      mhisp_r == 9 |
      fhispx == 9 |
      cig_rec == "U" |
      dmar == 9 |
      no_infec == 9 |
      gestrec3 == 3) %>% 
  count(rf_fedrg)

df %>% nrow()


# on les supprime (au moins pour cette partie, pour pouvoir faire les regressions)
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
         gestrec3 != 3,
         gestrec10 != 99)

df %>% nrow()

# feature engineering ----------------------------------------------------------------

## transforming the age of the parents by a piecewise linear function
# that is, X1 = max(0, age - s1), etc

df %>% 
  select(fagecomb, mager) %>% 
  map(summary)

# age thresholds :
tres_mage <- seq(15, 45, 5)
tres_fage <- c(tres_mage, seq(50, 90, 10))

piecewise_linear_function <- function(df, col, treshold, new_name){
  
  # for each treshold in the given range of treshold
  for (tres in treshold) {
    
    # apply rowwise the transformation : age_i = max(0, age - si)
    # (and rename automatically the new variable)
    df <- df %>%
      rowwise() %>%
      mutate(
        !!sym(glue::glue("{new_name}_{tres}")) := max(0, {{col}} - tres))
  }
  
  return(df)
  
}

piecewise_linear_function <- function(df, col, threshold, new_name) {
  
  # For each threshold, apply the transformation to the column
  for (tres in threshold) {
    # rename automatically the new variable
    new_col_name <- glue::glue("{new_name}_{tres}")
    
    # piecewise linear transformation : age_i = max(0, age - s_i)
    # apply rowwise (pmax is vectorized) the transformation
    df[[new_col_name]] <- pmax(0, col - tres)
  }
  
  return(df)
}

df <- df %>% piecewise_linear_function(.$mager, tres_mage, "mage")
df <- df %>% piecewise_linear_function(.$fagecomb, tres_fage, "fage")

# to check if it worked :
df %>% select(starts_with("mage"))
df %>% select(starts_with("fage"))

# finally remove mager and fagecomb
#df <- df %>% select(!all_of(c("mager", "fagecomb")))

## one hot encoding

# recoding sex of the newborn, infection and period of gestation
df$sex_M <-  ifelse(df$sex == "M" , 1, 0)
df$infection <- ifelse(df$no_infec == 0 , 1, 0)
df$gest_under_37_weeks <- ifelse(df$gestrec3 == 1 , 1, 0) # 1 c'est oui, 0 c'est non.

# then we have to remove the original variables
df <- df %>% select(!all_of(c("sex", "no_infec", "gestrec3")))

# mother's and fathers's hispanic origin
# 0 = non hispanic, 1 = hispanic origin 
df %>% select(mhisp_r) %>% distinct()
df$mhisp_r <- ifelse(df$mhisp_r != 0 , 1, 0)
df$fhispx <- ifelse(df$fhispx != 0 , 1, 0)

# recoding cigarette consumption from Y & N to 1 & 0 :
df %>% select(cig_rec) %>% distinct()
df$cig_rec <- ifelse(df$cig_rec == "Y", 1, 0)

# recoding rf_fedrg as follows :  X, U, N -> 0 & Y -> 1
df$rf_fedrg <- ifelse(df$rf_fedrg == "Y", 1, 0)

# OHE for education and race for both mother & father
# NB : we creat the dummy by removing the most frequent label
# i.e. the most frequent label becomes the reference
#ohe1 <- c("feduc","meduc","mrace6","frace6")
#df <- df %>%
#  fastDummies::dummy_cols(select_columns =  all_of(ohe1),
#    remove_most_frequent_dummy = TRUE)

# au final, les references prises sont :
# meduc_6 (bachelor), feduc_3 (high school graduated),

# ensuite on enlève les variables recodées en one-hot
# puis on renome les modalites pour plus de lisibilite
#df <- df %>% 
select(!all_of(ohe1))

saveRDS(df, "data/uniquerawgemelity_data.rds") # modify the created file name



# Matching excat avec jumeaux comme variable d'intérêt
df$dplural <- ifelse(df$dplural == 1, 0, 1)
remove(df1)
df1 <- sample_n(df, 300000)
df1%>%count(dplural) # 98 000 SING POUR 1800 JUM 1,8 % DE JUM OK
df$gest_under_37_weeks

m_gem <- matchit(dplural ~ mager + meduc + fagecomb + feduc + frace6 + mrace6 +
                   dmar + cig_rec + mhisp_r + fhispx + infection + sex_M ,
                 data = df1,
                 method = "exact",
                 k2k = TRUE,
                 estimand = "ATT") 
summary(m_gem)

## Propensity score matching

mylogit3 <- glm(rf_fedrg ~ .,
                data = df1 %>% select(!c(dplural,gest_under_37_weeks,gestrec10,dbwt)),
                family =  binomial(link = "logit"))

summary(mylogit3)

df1$propensity_score <- mylogit3$fitted.values
m_gem2<- Matching::Match(
  Y = df1$dplural,# dplural est codé en 0/1
  Tr = df1$rf_fedrg, 
  X = df1$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)
summary(m_gem2) 

# Propensity score avec gest_under_37_weeks comme variable d'intérêt 



m_gem3<- Matching::Match(
  Y = df1$gest_under_37_weeks,# dplural est codé en 0/1
  Tr = df1$rf_fedrg, 
  X = df1$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)
summary(m_gem3)

# Propensity score avec gestrec10 comme variable d'intérêt 
m_gem4<- Matching::Match(
  Y = df1$gestrec10,# dplural est codé en 0/1
  Tr = df1$rf_fedrg, 
  X = df1$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)
summary(m_gem4)


# Propensity score avec dbwt_inf_1500 comme variable d'intérêt :
df1$dbwt_inf_1500 <- ifelse(df1$dbwt < 1500 , 1, 0)
unique(df$dbwt_inf_1500)

m_gem5<- Matching::Match(
  Y = df1$dbwt_inf_1500,# dplural est codé en 0/1
  Tr = df1$rf_fedrg, 
  X = df1$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)
summary(m_gem5)

# Propensity score avec dbwt_inf_2500 comme variable d'intérêt :
df1$dbwt_inf_2500 <- ifelse(df1$dbwt < 2500 , 1, 0)
unique(df1$dbwt_inf_2500)

m_gem6<- Matching::Match(
  Y = df1$dbwt_inf_2500,# dplural est codé en 0/1
  Tr = df1$rf_fedrg, 
  X = df1$propensity_score,
  M = 1, # number of matches which should be found
  caliper = 0.05,
  replace = FALSE,
  ties = FALSE)
summary(m_gem6)



