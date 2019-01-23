# cleaning the environment
rm(list=ls())

# set directory
workd <- setwd("C:/Users/thiago.moreira/Dropbox/TAMU/cheibub/TallziaCode")
workd <- setwd("/Users/thiagomqmoreira/Dropbox/New_Data/Final_Data")
workd <- setwd('~/Downloads/')
getwd()

# open packages
library(tidyverse)
library(readxl)
library(foreign)
library(dplyr)
library(tidyr)
library(purrr)


# setting the election year
year <- 2006


######################################################
###################### Dataset #######################
######################################################

de_data <- read.csv("de_final_data.csv",  encoding = "latin1", stringsAsFactors = F)
de_data <- de_data %>% rename(nome_candidato = nome_candidato.x)
table(de_data$desc_sit_cand_tot)
#de_data <- de_data %>% mutate(desc_sit_cand_tot = recode(desc_sit_cand_tot, "ELEITO POR QP" = "ELEITO"))
source("data_class_correction.R")


##########################################################################################
#### First Part: functions to allocate seats and compare with the actual electoral outcome
##########################################################################################


#######################
# First function
#######################

clean_data <- function(data, year) {
  # Generate how seats are allocated
  # considering the partisan quotient
  
  
  d <- data %>% 
    dplyr::filter(ano_eleicao == year) %>% 
    dplyr::select(ano_eleicao:nome_candidato, 
                  descricao_cargo, desc_sit_cand_tot, 
                  sigla_partido:de_tot_v_nominal_ptd, 
                  de_tot_v_legenda_ptd, 
                  de_tot_v_nominal_colig, 
                  de_tot_v_legenda_colig, 
                  de_tot_v_total_ptd:qtd_votos_legenda, 
                  m_de)
  
  # creating columns
  d <- d %>% 
    group_by(ano_eleicao, sigla_uf) %>% 
    mutate(votos_validos = qtd_votos_nominais + qtd_votos_legenda) %>% 
    mutate(division = votos_validos / m_de) %>% 
    mutate(remainder = division %% 1) %>% 
    mutate(round_remainder = ifelse(remainder <= .5, 0, 1)) %>% 
    mutate(int_div = votos_validos %/% m_de) %>% 
    mutate(quociente_eleitoral = int_div + round_remainder) %>% 
    filter(de_tot_v_total_colig >= quociente_eleitoral) %>% 
    mutate(cand_perc_qe = tot_votos_nominais / quociente_eleitoral) %>% 
    mutate(quociente_partidario = de_tot_v_total_colig %/% quociente_eleitoral) %>%
    ungroup() # check if a group_by is necessary, this might be a source of errors
  
  d <- d %>%
    mutate(desc_sit_cand_tot = ifelse(desc_sit_cand_tot == "ELEITO POR QP",
                                      "ELEITO", 
                                      desc_sit_cand_tot)) %>%
    # create an id for elections and uf
    mutate(id = stringr::str_c(ano_eleicao, "_", sigla_uf))
  
  d
}


#############################
# Second function
#############################


find_leftover <- function(d_final) {
  # Find who is the first leftover
  # This part of the function 
  # allocates the leftover seats
  
  d_final <- d_final %>%
    mutate(media = de_tot_v_total_colig / 
             (d_final$quociente_partidario + d_final$vaga_por_media + 1)) %>%
    mutate(max_media = max(media))
  tipo <- d_final %>% 
    arrange(desc(media)) %>%
    summarise(tipo = first(tipo_legenda)) %>%
    pull
  
  if(tipo == "PARTIDO ISOLADO") {
    
    party <-  d_final %>% 
      arrange(desc(media)) %>%
      summarise(tipo = first(sigla_partido)) %>%
      pull
    
    d_final <- d_final %>%
      mutate(vaga_por_media = (sigla_partido == party) + vaga_por_media)
    
    location_party <- unique(filter(d_final, sigla_partido == party)$quociente_partidario) +
      unique(d_final$vaga_por_media[d_final$sigla_partido == party])
    
    d_final$resultado[d_final$sigla_partido == party][location_party] <- "ELEITO POR MEDIA"
    
  } else {
    # tipo == "COLIGACAO"
    
    colig <- d_final %>% 
      arrange(desc(media)) %>%
      summarise(tipo = first(nome_coligacao)) %>%
      pull
    
    d_final <- d_final %>%
      mutate(vaga_por_media = (nome_coligacao == colig) + vaga_por_media)
    
    location_colig <- unique(d_final$quociente_partidario[d_final$nome_coligacao == colig]) +
      unique(d_final$vaga_por_media[d_final$nome_coligacao == colig])
    
    d_final$resultado[d_final$nome_coligacao == colig][location_colig] = "ELEITO POR MEDIA"
  }
  d_final
}



#############################
# Third function
#############################

clean_tipo_legenda <- function(d) {
  # For a certain year and state
  # it calculates elected deputies
  # and compares it with the actual results
  
  d_isolado <- d %>% 
    filter(tipo_legenda == "PARTIDO ISOLADO") %>% 
    group_by(sigla_partido) %>% 
    arrange(sigla_partido, desc(tot_votos_nominais)) %>% 
    mutate(resultado = ifelse(row_number() <= quociente_partidario, "ELEITO QP", NA)) %>%
    ungroup()  
  
  d_coligacao <- d_coligacao <- d %>% 
    filter(tipo_legenda == "COLIGACAO")  %>% 
    group_by(nome_coligacao) %>% 
    arrange(nome_coligacao, desc(tot_votos_nominais))  %>% 
    mutate(resultado = ifelse(row_number() <= quociente_partidario, "ELEITO QP", NA)) %>%
    ungroup()
  
  d_final <- bind_rows(d_isolado, d_coligacao) %>%  
    mutate(total_elec_qp = sum(!is.na(resultado))) %>% 
    mutate(left_to_distribute = m_de - total_elec_qp) %>% 
    mutate(vaga_por_media = 0) 
  
  if ( d_final$left_to_distribute[1] > 0 )
    # A purrr trick to run the function recursively
    # n times, n = number of leftovers
    d_final <- reduce(rerun(d_final$left_to_distribute[1], find_leftover), compose)(d_final)
  
  n_seats <- d_final %>% distinct(m_de) %>% pull 
  
  all_real_life <- d_final %>% 
    filter(desc_sit_cand_tot == "ELEITO" | desc_sit_cand_tot == "MEDIA") %>% 
    select(ano_eleicao, sigla_uf, nome_candidato, sigla_partido, nome_coligacao, composicao_legenda, tipo_legenda,
           desc_sit_cand_tot, resultado, quociente_eleitoral, quociente_partidario, m_de, total_elec_qp, left_to_distribute,
           vaga_por_media)
  
  all_pred <- d_final %>% 
    filter(resultado == "ELEITO QP" | resultado == "ELEITO POR MEDIA") %>% 
    select(ano_eleicao, sigla_uf, nome_candidato, sigla_partido, nome_coligacao, composicao_legenda, tipo_legenda,
           desc_sit_cand_tot, resultado, quociente_eleitoral, quociente_partidario, m_de, total_elec_qp, left_to_distribute,
           vaga_por_media)
  
  n_seats_real_life <- nrow(all_real_life)
  
  n_seats_match <- n_seats == n_seats_real_life
  
  # This variable test if all_real_life and all_pred are identical
  # and if the number of seats are the same 
  
  d_final <- d_final %>% 
    mutate(match = (identical(all_real_life, all_pred) & n_seats_match)) %>%
    mutate(both_qp = ifelse(desc_sit_cand_tot == "ELEITO" & resultado == "ELEITO QP", 1, 0)) %>% 
    mutate(real_qp_pred_media = ifelse(desc_sit_cand_tot == "ELEITO" & resultado == "ELEITO POR MEDIA", 1, 0)) %>% 
    mutate(real_qp_pred_na = ifelse(desc_sit_cand_tot == "ELEITO" & is.na(resultado), 1, 0)) %>% 
    mutate(both_media = ifelse(desc_sit_cand_tot %in% c("MEDIA")
                               & resultado == "ELEITO POR MEDIA", 1, 0)) %>% 
    mutate(real_media_pred_qp = ifelse(desc_sit_cand_tot %in% c("MEDIA") & resultado == "ELEITO QP", 1, 0)) %>%
    mutate(real_media_pred_na = ifelse(desc_sit_cand_tot %in% c("MEDIA") & is.na(resultado), 1, 0)) %>% 
    mutate(real_not_elec_pred_qp = ifelse(resultado == "ELEITO QP" & 
                                            (desc_sit_cand_tot == "SUPLENTE" | desc_sit_cand_tot == "NAO ELEITO"), 1, 0)) %>% 
    mutate(real_not_elec_pred_media = ifelse(resultado == "ELEITO POR MEDIA" & 
                                               (desc_sit_cand_tot == "SUPLENTE" | desc_sit_cand_tot == "NAO ELEITO"), 1, 0)) %>%
    mutate(correct_pred = both_qp == 1 | both_media == 1)
  
  d_final
  
}    




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

generate_elected <- function(data, year) {
  # This function runs our algorithm
  # for finding elected deputies: 
  # It returns a dataset with
  # 'resultado' column
  # with who were elected in the election
  
  # Cleaning Data for 2006
  dataset <- clean_data(de_data, year)
  
  # Separate dataset by ids
  # Ids represent different elections
  d <- dataset %>%
    split(.$id)
  
  # Running clean_tipo_legenda for all states
  results <- map_df(d, clean_tipo_legenda ) 
  
  results
}


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

