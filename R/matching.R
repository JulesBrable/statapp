
#setting environment
rm(list = ls())

library(data.table)
library(tidyverse)
library(tcltk)
library(lattice)

install.packages("devtools")
library(devtools)
install_github("https://github.com/IQSS/cem.git")

#loading dataset
source("R/functions/sampling_functions.R")

path <- "Data"
list.files(path = path)
file <- "data.csv"


to_keep <- c("dbwt", "mager", "meduc", "fagecomb", "feduc", "frace6", "mrace15",
             "rf_artec", "priorlive", "dmar", "cig_rec", "mhisp_r", "fhispx",
             "no_infec", "sex", "gestrec3")

df <- load_data(file)%>%
  select(all_of(to_keep))

# recoding rf_artec as follows :  X, U, N -> 0 & Y -> 1
df$rf_artec <- ifelse(df$rf_artec == "Y", 1, 0)

#matching

mat <- cem(treatment= "rf_artec", data=df)   
mat

#résultats:

