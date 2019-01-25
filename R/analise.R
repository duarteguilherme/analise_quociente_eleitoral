

# open packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)

# Loading functions
source('R/load_data.R')
source('R/simulate_elections.R')
source('R/generate_relevants.R')



# Define year here
year <- 2006

# Define db filename here
filename <- "data/de_final_data.csv"

# For example, if we run this function
# for this dataset only for year = 2006,
# it will return a dataset that indicates
# who the elected deputies were by state
results <- generate_elected(filename, year)


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


# Generating a list of deputies
deputies_list <- results %>%
  arrange(desc(tot_votos_nominais)) %>%
  distinct(ano_eleicao, sigla_uf, nome_candidato, numero_cand, relevance)
# Now we have a list of around 10,000 
# deputies for 2006


# A vector name with the list of all the elected candidates (according to our prediction)
actual_elected_deputies <- results %>%
  mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                 ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA))) %>%
  filter(resultado %in% c("ELEITO POR MEDIA", "ELEITO QP")) %>%
  arrange(nome_candidato) %>%
  distinct(sigla_uf, numero_cand, party_or_colig)


# Generate a vector of states
ufs <-  results %>%
  distinct(sigla_uf) %>%
  pull


# We have to introduce the real_elected_deputies

ufs <- c("AC","AM","AP")

relevants <- map_df(ufs, ~ generate_relevants(.x, year2 = year, actual_elected_deputies))


relevants <- mutate(relevants, relevance = T)

table(relevants$relevance)



