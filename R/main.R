
# SETTING ENVIRONEMENT ----------------------------------------------------
rm(list = ls())

library(data.table)
library(tidyverse)

source("R/statdesc_functions.R")

path <- "data"
list.files(path = path)
file <- "nat2021us.csv"

# works but takes time
#tab2021 <- fread("https://data.nber.org/nvss/natality/csv/nat2021us.csv")

tab2021 <- load_data(path, file)

# !!!!!WARNING: don't directly view the raw table (way too large)

# FIRST GLANCE AT THE DATA ------------------------------------------------
head(tab2021)
colnames(tab2021)

# columns with all values missing
tab2021 %>% keep(~all(is.na(.x))) %>% names

# month and year of birth count
tab2021 %>% hist_plot("dob_mm")
tab2021 %>% hist_plot("dob_wk")

# place of birth
tab2021 %>% 
  group_by(bfacil) %>% 
  count()

tab2021 %>% hist_plot("bfacil3", 3) # recoded place of birth


# maternal characteristics
tab2021 %>% hist_plot("mager")
tab2021 %>% hist_plot("mrace6")
tab2021 %>% hist_plot("meduc", 9) # to recode??
tab2021 %>% hist_plot("mhisp_r", 9) # recoded

# father characteristics
tab2021 %>% hist_plot("fagerec11", 11) # recoded
tab2021 %>% hist_plot("frace6", 9) # recoded
tab2021 %>% hist_plot("fhisp_r", 9) # recoded
tab2021 %>% hist_plot("feduc", 9) # recoded

# previous births
prev <- c("priorlive", "priordead", "priorterm")
tab2021 %>% 
  select(prev) %>%
  var_summary(prev, nan = c("99 ", 99))

tab2021 %>% hist_plot("tbo_rec", 9)
tab2021 %>% filter(illb_r != 888) %>% density_plot("illb_r", 999)
tab2021 %>% filter(ilop_r11 != 88) %>% hist_plot("ilop_r11", 99)
tab2021 %>% 
  mutate(precare5 = replace(precare5, precare5 == 4, 0)) %>% 
  hist_plot("precare5", 5)
tab2021 %>% hist_plot("previs_rec", 12)

# Special Supplemental Nutrition Program for Women, Infants, and Children
tab2021 %>% 
  group_by(wic) %>% 
  count()

# marital status
tab2021 %>%
  group_by(dmar) %>% 
  count()

# payment source for delivery (recoded)
tab2021 %>% 
  group_by(pay_rec) %>% 
  count()

# paternity acknowledged
tab2021 %>%
  group_by(mar_p) %>% 
  count() # to be combined with the corresponding "flag" variable

# almost no smoker
tab2021 %>% 
  select(c("cig_0", "cig_1", "cig_2", "cig_3")) %>%
  filter_all(all_vars(. != 99)) %>% 
  var_summary(c("cig_0", "cig_1", "cig_2", "cig_3"))
  var_summary(prev)

#imc
tab2021 %>% density_plot("bmi", 99) # can also add weight and heigth...

# risk factors
risk <- c("rf_pdiab", "rf_gdiab", "rf_phype", "rf_ghype", "rf_ehype", "rf_ppterm")
tab2021 %>% select(risk) %>% sapply(function(x){
  table(factor(x,
               levels = unique(unlist(tab2021$rf_pdiab)),
               ordered = TRUE))
  })

# pma
pma <- c("rf_inftr", "rf_fedrg", "rf_artec")
titi %>% 
  get_prop(rf_inftr) %>% 
  bind_rows(titi %>% 
              get_prop(rf_fedrg)) %>% 
  bind_rows(titi %>% 
              get_prop(rf_artec))

#infections
inf <- c("ip_gon", "ip_syph", "ip_chlam", "ip_hepatb", "ip_hepatc")
tab2021 %>% select(all_of(inf)) %>% sapply(function(x){
  table(factor(x,
               levels = unique(unlist(tab2021$ip_gon)),
               ordered = TRUE))
})


# POTENTIAL TARGETS -------------------------------------------------------

# apgar score : very different
tab2021 %>% hist_plot("apgar5", 99)
tab2021 %>% filter(apgar10 != 88) %>% hist_plot("apgar10", 99)

# birth weight
tab2021 %>% density_plot("dbwt", 9999)

tab2021 %>% var_summary("dplural") # single or not

tab2021 %>% hist_plot("combgest", 99)

# Abnormal Conditions of the Newborn
ab <- c("ab_aven1", "ab_aven6", "ab_nicu", "ab_surf", "ab_anti", "ab_seiz")
tab2021 %>% select(all_of(ab)) %>% sapply(function(x){
  table(factor(x,
               levels = unique(unlist(tab2021$ab_aven1)),
               ordered = TRUE))
})

# Congenital Anomalies of the Newborn
ca <- c("ca_anen", "ca_mnsb", "ca_cchd", "ca_cdh", "ca_omph", "ca_gast",
        "ca_limb", "ca_cleft", "ca_clpal", "ca_downs", "ca_disor", "ca_hypo")
tab2021 %>% select(all_of(ca)) %>% sapply(function(x){
  table(factor(x,
               levels = unique(unlist(tab2021$ca_anen)),
               ordered = TRUE))
})

# statistical summary
vars <- c("apgar5", "apgar10", "dbwt")
tab2021 %>%
  filter(apgar5 != 99, apgar10 != 99, dbwt != 9999) %>% 
  var_summary(vars)


###### age pyramid #######
tab2021 %>%
  transform_age() %>% 
  pyrage()

###############
# reste quelques variables geographiques/ sexe de l'enfant
# do it on many years ?

# mother age imputed
tab2021 %>% 
  group_by(mage_impflg) %>% 
  count()

tab2021 %>% 
  group_by(mraceimp) %>% 
  count()

tab2021 %>% 
  group_by(mar_imp) %>% 
  count()

