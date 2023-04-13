library(Matching)
library(MatchIt)
library(tidyverse)

# Function to estimate the ATE/ATT
# using Abadie-Imbens (2006) matching method
est_Abadie_Imbens <- function(data, treat_var, outcome_var, ...) {
  
  Match(Y = data[[outcome_var]],
        Tr = data[[treat_var]],
        X =  data[, setdiff(names(data), c(outcome_var, treat_var)),
                  with = FALSE],
        ...)
}

path <- "data"

df <- readRDS(paste(path, "preprocessed_data.rds", sep = "/")) %>% 
  sample_n(300000)

res <- est_Abadie_Imbens(
  df, "rf_fedrg", "dbwt", M = 1, caliper = 0.2, estimand = "ATT", ties = FALSE
  )

# AI = Abadie-Imbens
save(res, file = paste0("data/", "matched_AI_300000.Rdata"))

matched_AI1 <- miceadds::load.Rdata2(paste0("data/", "matched_AI_300000.Rdata"))

summary(matched_AI1)

################## regarder egalement les summary proposes par le package

############ COMPARAISON DE L'INFERENCE DES METHODES ############

set.seed(123)
df <- readRDS(paste(path, "preprocessed_data.rds", sep = "/")) %>% 
  sample_n(100000)

res <- est_Abadie_Imbens(
  df, "rf_fedrg", "dbwt", M = 1, caliper = 0.2, estimand = "ATT", ties = FALSE
)

save(res, file = paste0("data/", "matched_AI_100000.Rdata"))
#res <- miceadds::load.Rdata2(paste0("data/", "matched_AI_100000.Rdata"))

mex1 <- matchit(rf_fedrg ~ . -dbwt,
               data = df,
               method = "cem",
               estimand = "ATT")

mex2 <- matchit(rf_fedrg ~ . -dbwt,
               data = df,
               method = "nearest",
               distance = "mahalanobis",
               estimand = "ATT")

mex3 <- matchit(rf_fedrg ~ . -dbwt,
               data = df,
               method = "nearest",
               distance = "glm",
               caliper = .25,
               estimand = "ATT")

matched_data_cem1 <- match.data(mex1)
matched_data_cem2 <- match.data(mex2)
matched_data_cem3 <- match.data(mex3)

res1 <- lm(dbwt ~ ., data = matched_data_cem1 %>% 
            select(all_of(df %>% colnames())))

res2 <- lm(dbwt ~ ., data = matched_data_cem2 %>% 
            select(all_of(df %>% colnames())))

res3 <- lm(dbwt ~ ., data = matched_data_cem3 %>% 
            select(all_of(df %>% colnames())))

summary(res)
summary(res1)
summary(res2)
summary(res3)

