#' In this file, we will put
#' all the functions related
#' to simulate one election
#' 

library(dplyr)
library(tidyr)
library(purrr)

find_leftover <- function(d_final) {
  # Find who is the first leftover
  # This part of the function 
  # allocates the leftover seats
  
  d_final <- d_final %>%
    mutate(media = de_tot_v_total_legenda / 
             (d_final$quociente_partidario + d_final$vaga_por_media + 1)) %>%
    mutate(max_media = max(media))
  
  legenda <-  d_final %>% 
      arrange(desc(media)) %>%
      summarise(tipo = first(party_or_colig)) %>%
      pull
    
  d_final <- d_final %>%
      mutate(vaga_por_media = (party_or_colig == legenda) + vaga_por_media)
    
  location_party <- unique(filter(d_final, party_or_colig == legenda)$quociente_partidario) +
      unique(d_final$vaga_por_media[d_final$party_or_colig == legenda])
    
  d_final$resultado[d_final$party_or_colig == legenda][location_party] <- "ELEITO POR MEDIA"
  
  d_final    
}




clean_tipo_legenda <- function(d_uf, original = F) {
  # For a certain year and state
  # it calculates elected deputies
  # and compares it with the actual results
  # argument original tests if dataset is consistent
  
  d_uf <- d_uf %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA))) 
  d_uf <- d_uf %>%
    mutate(votos_legenda = ifelse(tipo_legenda == "PARTIDO ISOLADO", de_tot_v_legenda_ptd,
                                   ifelse(tipo_legenda == "COLIGACAO", de_tot_v_legenda_colig, NA))) 
  
  d_uf <- d_uf %>%
    group_by(party_or_colig) %>%
    mutate(de_tot_v_total_legenda = sum(tot_votos_nominais) + first(votos_legenda))
    
  
  total_votos_nominais <- sum(d_uf$tot_votos_nominais, na.rm=T)
  total_votos_legenda <- d_uf %>%
    group_by(party_or_colig) %>%
    summarise(votos_legenda = first(votos_legenda))
  total_votos_legenda <- sum(total_votos_legenda$votos_legenda, na.rm=T)
  
  # Test for valid votes
  if ( original ) {
    if ( d_uf$qtd_votos_nominais[1] != (total_votos_nominais)) {
      stop(glue::glue("\nData for {d_uf$sigla_uf[1]} year {d_uf$ano_eleicao[1]} didn't match for nominal votes"))
    }
    if ( d_uf$qtd_votos_legenda[1] != (total_votos_legenda)) {
      stop(glue::glue("\nData for {d_uf$sigla_uf[1]} year {d_uf$ano_eleicao[1]} didn't match for legenda votes"))
    }
  }
  
  d_uf <- d_uf %>% 
    mutate(votos_validos = total_votos_nominais + total_votos_legenda) %>% 
    mutate(division = votos_validos / m_de) %>% 
    mutate(remainder = division %% 1) %>% 
    mutate(round_remainder = ifelse(remainder <= .5, 0, 1)) %>% 
    mutate(int_div = votos_validos %/% m_de) %>% 
    mutate(quociente_eleitoral = int_div + round_remainder) %>% 
#    mutate(cand_perc_qe = tot_votos_nominais / quociente_eleitoral) %>% 
    group_by(party_or_colig) %>%
    mutate(quociente_partidario = de_tot_v_total_legenda %/% quociente_eleitoral) %>%
    ungroup()
  
  d_uf$resultado <- NA
  

  d_final <- d_uf %>% 
    group_by(party_or_colig) %>% 
    arrange(party_or_colig, desc(tot_votos_nominais))  %>% 
    mutate(resultado = ifelse(row_number() <= quociente_partidario, "ELEITO QP", NA)) %>%
    ungroup() %>%
    mutate(total_elec_qp = sum(!is.na(resultado))) %>% 
    mutate(left_to_distribute = m_de - total_elec_qp) %>% 
    mutate(vaga_por_media = 0) 
  

  if (sum(is.na(d_final$party_or_colig))) stop("Error. Some party_or_colig is NA.")

  if ( d_final$left_to_distribute[1] > 0 )
    # A purrr trick to run the function recursively
    # n times, n = number of leftovers
    for (i in 1:d_final$left_to_distribute[1])
      d_final <- find_leftover(d_final)

  
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
  
  
  # This variable test if all_real_life and all_pred are identical
  # and if the number of seats are the same 
  n_seats_match <- n_seats == n_seats_real_life
  if ( n_seats_match == F ) {
    print(glue::glue("\n** For {d_final$sigla_uf[1]} and {d_final$ano_eleicao[1]}, predicted values and
                     actual results didn't match. **\n"))
  }
  
  
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



generate_elected <- function(filename, year, uf = "", original = F) {
# This function runs our algorithm
# for finding elected deputies: 
# It returns a dataset with
# 'resultado' column
# with who were elected in the election
  
  data <- load_data(filename)
  data <- dplyr::filter(data, ano_eleicao == year)
  dataset <- clean_data(data)
  
  if (uf != "")
    dataset <- dplyr::filter(dataset, sigla_uf == uf)
  
  

#  # Separate dataset by ids
#  # Ids represent different elections
    d <- dataset %>%
      split(.$id)

  # Running clean_tipo_legenda for all states
  results <- map_df(d, ~ clean_tipo_legenda(.x, original) ) 
  
  results
}

