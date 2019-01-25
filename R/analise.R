

# open packages
library(tidyverse)
library(readxl)
library(foreign)
library(dplyr)
library(tidyr)
library(purrr)

# Loading functions
source('R/load_data.R')

# setting the election year
year <- 2006

data <- load_data('data/de_final_data.csv')
data <- dplyr::filter(data, ano_eleicao == year)
data <- clean_data(data)












#############################
# Third function
#############################



##########################################################################################
#### Second Part: running the functions with our dataset
##########################################################################################

# Read the dataset
#de_data <- read.csv("data/new_de_90_14_brasil (3).csv")
#source("R/data_class_correction.R")
#de_data <- de_data %>% filter(sigla_uf == "AC")
#de_data <- de_data %>% filter(ano_eleicao == 2006)

#############################
# Fourth function
#############################



# For example, if we run this function
# for this dataset only for year = 2006,
# it will return a dataset that indicates
# who the elected deputies were by state
results <- generate_elected(de_data, 2006)


# This routine indicates the rows
# with correct prediction for
# deputies
correct_pred <-  results %>%
  filter(both_qp == 1 | both_media == 1) #only 1.031 exactly corrected predicions for 2006


# This routine indicates the candidates
# who we predict as elected
# but they weren't.
# It includes observations in which
# a deputy were elected by remainders
# but we predict they were elected by
# electoral quotient
wrong_1 <-  results %>%
  filter(!correct_pred)  %>%
  filter(!is.na(resultado))


# This routine indicates the candidates
# who were elected but we predict they weren't
wrong_2 <- results %>%
  filter(desc_sit_cand_tot %in% c("MEDIA") | desc_sit_cand_tot %in% c("ELEITO")) %>%
  filter(is.na(resultado))




# For each election
# we will run this procedure
# one time per deputy
# removing him/her 
# in order to test if they are irrelevant or not


# All the elected deputies are relevants 
# by themselves
results <- results %>%
  mutate(relevance = FALSE)

#results <- filter(results, sigla_uf == "AC")

# Generating a list of deputies
deputies_list <- results %>%
  arrange(desc(tot_votos_nominais)) %>%
  distinct(ano_eleicao, sigla_uf, nome_candidato, numero_cand, relevance)
# Now we have a list of around 10,000 
# deputies for 2006


# A vector name with the list of all the elected candidates (according to our prediction)
real_elected_deputies <- results %>%
  filter(resultado %in% c("ELEITO POR MEDIA", "ELEITO QP")) %>%
  arrange(nome_candidato) %>%
  distinct(sigla_uf, numero_cand)

check_prediction <- function(dataset, uf, party, year2) {
  # This function gets the dataset and check if it
  # matches the real_elected_deputies (our first prediction)
  
  ########### warning ##############
  # The correct code for this function
  # requires a filter with the candidate id (candidate number).
  # However, because the dataset includes some candidates
  # with non-available id (NA), we are using the name of the candidates
  
  results <- generate_elected(de_data, year2)
  correct_pred <-  results %>%
    filter(both_qp == 1 | both_media == 1) 
  wrong_1 <-  results %>%
    filter(!correct_pred)  %>%
    filter(!is.na(resultado))
  wrong_2 <- results %>%
    filter(desc_sit_cand_tot %in% c("MEDIA") | desc_sit_cand_tot %in% c("ELEITO")) %>%
    filter(is.na(resultado))
  results <- filter(results, !is.na(tot_votos_nominais))
  results <- results %>%
    mutate(relevance = FALSE)
  deputies_list <- results %>%
    arrange(desc(tot_votos_nominais)) %>%
    distinct(ano_eleicao, sigla_uf, nome_candidato, numero_cand, relevance)
  real_elected_deputies <- results %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA))) %>%
    filter(party_or_colig == party) %>%
    filter(resultado %in% c("ELEITO POR MEDIA", "ELEITO QP")) %>%
    arrange(nome_candidato) %>%
    distinct(sigla_uf, numero_cand)
  
  
  # Filtering real_elected by state
  # and generating a vector of names
  real_elected <- real_elected_deputies %>%
    distinct(numero_cand) %>%
    arrange(numero_cand) %>%
    pull
  
  
  # Now we're testing whether removing this deputies
  # affects the election outcome
  predicted_elected_deputies <- clean_tipo_legenda(dataset) %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA))) %>%
    filter(party_or_colig == party) %>%
    filter(resultado %in% c("ELEITO POR MEDIA", "ELEITO QP")) %>%
    filter(party_or_colig == party) %>%
    arrange(numero_cand) %>%
    pull(numero_cand) 
  
  # When an elected deputy appears,
  # Checking whether the length of each vector
  # is sufficient to return TRUE for relevance
  if ( length(real_elected) != length(predicted_elected_deputies) )
    return(FALSE)
  
  # If some candidate inside real_elected_deputies
  # does not match someone inside predicted elected deputies,
  # the tested deputy is relevant, so we return true
  return(all(predicted_elected_deputies == real_elected))
  
}




generate_relevants_by_party <- function(uf, year2, party) {
  # This functions gets a state
  # and run for each party
  # a routine that checks whether a candidate
  # is irrelevant, removing him/her if the output is true
  
  dataset <- filter(de_data, sigla_uf == uf , year == year2) %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA)))
  
  
  
  party_candidates <- dataset %>%
    filter(party_or_colig == party) %>%
    arrange(tot_votos_nominais) 
  
  
  for (k in 1:nrow(party_candidates)) {
    if ( filter(dataset, numero_cand != party_candidates$numero_cand[k]) %>%
         clean_data(year2) %>%
         check_prediction(uf, party, year2) ) {
      dataset <- filter(dataset, numero_cand != party_candidates$numero_cand[k])
    }
    else {
      break
    }
  }
  
  party_relevants <- dataset %>%
    filter(party_or_colig == party) %>%
    arrange(tot_votos_nominais) 
  
  
  party_relevants   
}


generate_relevants <- function(uf, year2 = 2006) {
  cat(glue::glue("Running for {uf}. year = {year2}\n")) 
  de_data <- de_data %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA)))
  
  parties <- filter(de_data, sigla_uf == uf, year == year2) %>%
    distinct(party_or_colig) %>%
    pull
  
  # It also runs for parties with no elected deputies
  # This is a bug we must fix down the line
  
  relevants <- map_df(parties, ~ generate_relevants_by_party(uf, year2, .x))
  relevants
}




ufs <- de_data %>%
  distinct(sigla_uf) %>%
  pull


relevants <- map_df(ufs, ~ generate_relevants(.x, year2 = 2006))


relevants <- mutate(relevants, relevant = T)


de_data1 <- left_join(de_data, relevants)

table(deputies_list$relevance)

