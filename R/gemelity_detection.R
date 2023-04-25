library(tidyverse)

#df <- readRDS(paste(path, "preprocessed_data.rds", sep = "/"))
source("R/functions/sampling_functions.R")

df <- load_data("nat2020us.csv")

# on fixe un index dans le dataframe de depart pour avoir les idees claires
df$index <- 1:nrow(df)

## on selectionne un sous ensemble de variables (supposemment) discrimminantes
df1 <- df %>%
  select(index, dplural, dmar, restatus, fagecomb, frace31, fhispx, dlmp_mm, mar_p,
         combgest, mager, m_ht_in, bmi, wtgain, dwgt_r, mrace31, mhispx, meduc)

df1 %>% count(dplural) # effectifs de gemelite

## construction d'une variable group_id qui va identifier les enfants de la meme fraterie
# ie, le cardinal de cette variable doit etre egal au nombre de fratries differentes
assign_group_id <- function(df) {
  # fonction qui assigne un numero de groupe unique aux observations similaires
  # similaires au sens des variables retenues ci dessus
  # ainsi ces observations sont censees appartenir a la meme fraterie
  group_ids <- df %>%
    group_by_all() %>%
    group_indices()
  
  return(group_ids)
}

check_group <- function(df, nb_gemelity, group = group_id){
  # on regarde si, pour chaque valeur de gemilite (2, 3 ou 4), on a qqchose de coherent
  # ie est ce que le nombre de groupes formes par la notre fonction precedente nous donne des groupes de bonne taille?
  
  # 1) est ce qu'il existe des groupes formes de taille trop petite 
  # exemple : des jumeaux (dplural = 2) mais un seul enfant dans le groupe
  test1 <- df %>%
    count({{ group }}) %>%
    filter(n < nb_gemelity) %>%
    nrow()
  
  # 2) est ce qu'il existe des groupes formes de taille trop grande 
  # exemple : des jumeaux (dplural = 2) mais 3 ou 4 enfants dans le groupe
  test2 <- df %>%
    count({{ group }}) %>%
    filter(n > nb_gemelity) %>%
    nrow()
  
  if (test1 > 0) {
    cat(glue::glue("{test1} groups with n < {nb_gemelity} -> Des freres/soeurs morts?"))
  } else {
    cat(glue::glue("{test1} group with n < {nb_gemelity} -> OK, pas de morts \n"))
  }
  cat("\n \n")
  if (test2 > 0) {
    cat(glue::glue("\n {test2} groups with n > {nb_gemelity} -> Pas assez bien separes, rajouter des variables ?"))
  } else {
    cat(glue::glue("\n {test2} group with n > {nb_gemelity} -> OK, enfants bien separes"))
  }
}

# quadruples ou +
df4 <- df1 %>% filter(dplural == 4)
df4$group_id <- assign_group_id(df4 %>% select(-index))
check_group(df4, 4) # pas besoin de rajouter des variables car possiblement quintuples

# essayons pour les triples
df3 <- df1 %>% filter(dplural == 3)
df3$group_id <- assign_group_id(df3 %>% select(-index))
check_group(df3, 3)

# enfin, les jumeaux
df2 <- df1 %>% filter(dplural == 2)
df2$group_id <- assign_group_id(df2 %>% select(-index))
check_group(df2, 2)

## resolution des problemes :
# pour les jumeaux qui sont tous seuls, c'est pas un probleme a priori
# on peut considerer qu'ils sont "deja" selectionnes
# pour les triples/quadruples qui sont seuls dans l'echantillon, pareil
# 1) cpdt pour les triples/quadruples qui sont seulement 2 dans l'echantillon, probleme !
# on va ici considerer que c'est des jumeaux et on en tirera un au hasard
# 2) probleme egalempent pour les gens censes etre jumeaux/triples
# mais qui, avec les variables discrimminantes utilisees, sont + que 2 ou 3
# on les tirera au sort egalement.

# finalement, integrons une colonne de groupe au dataframe dans son ensemble
df1 <- df1 %>% filter(dplural != 1)
df1$group_id <- assign_group_id(df1 %>% select(-index))

df1 %>%distinct(group_id) %>% nrow()

sum(df2 %>%distinct(group_id) %>% nrow(),
    df3 %>%distinct(group_id) %>% nrow(),
    df4 %>%distinct(group_id) %>% nrow())
# on a bien le meme nombre de groupes en groupant sur le dataframe entier et en groupant sur 3 dataframes separes
# logique car les obs de df2, df3 et df4 sont discrimminees par dplural

## etape finale:
# dans le dataframe de la population totale, reattibruer les observations de jumeaux
# contenant ainsi la colonne de groupe de "portee"
# puis selectionner aleatoirement une observation par "portee":

res <- df %>%
  filter(dplural == 1) %>% 
  mutate(group_id = max(df1$group_id):(nrow(df) - nrow(df1) + max(df1$group_id) - 1)) %>% 
  select(df1 %>% colnames()) %>% 
  rbind(df1) %>% 
  mutate(u = runif(nrow(df))) %>% 
  group_by(group_id) %>% 
  filter(u == max(u)) %>% 
  ungroup()

res %>% distinct(group_id) %>% nrow()

df_final <- res %>% 
  select(index) %>% 
  left_join(df)
