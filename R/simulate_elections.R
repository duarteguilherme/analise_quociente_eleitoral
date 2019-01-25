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
    
    suppressWarnings(d_final$resultado[d_final$sigla_partido == party][location_party] <- "ELEITO POR MEDIA")
    
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
    
    suppressWarnings(d_final$resultado[d_final$nome_coligacao == colig][location_colig] <- "ELEITO POR MEDIA")
  }
  d_final
}




clean_tipo_legenda <- function(d_uf) {
  # For a certain year and state
  # it calculates elected deputies
  # and compares it with the actual results
  
  d_isolado <- d_uf %>% 
    filter(tipo_legenda == "PARTIDO ISOLADO") %>% 
    group_by(sigla_partido) %>% 
    arrange(sigla_partido, desc(tot_votos_nominais)) %>% 
    mutate(resultado = ifelse(row_number() <= quociente_partidario, "ELEITO QP", NA)) %>%
    ungroup()  
  
  d_coligacao <- d_uf %>% 
    filter(tipo_legenda == "COLIGACAO")  %>% 
    group_by(nome_coligacao) %>% 
    arrange(nome_coligacao, desc(tot_votos_nominais))  %>% 
    mutate(resultado = ifelse(row_number() <= quociente_partidario, "ELEITO QP", NA)) %>%
    ungroup()
  
  d_final <- bind_rows(d_isolado, d_coligacao) %>%  
    mutate(total_elec_qp = sum(!is.na(resultado))) %>% 
    mutate(left_to_distribute = m_de - total_elec_qp) %>% 
    mutate(vaga_por_media = 0) 
  
  d_final <- d_final %>%
    mutate(nome_coligacao = ifelse(is.na(nome_coligacao), sigla_partido, nome_coligacao))
  
  if (sum(is.na(d_final$nome_coligacao))) stop("Error. Some nome_coligacao is NA.")
  if (sum(is.na(d_final$sigla_partido))) stop("Error. Some nome_coligacao is NA.")
  
  if ( d_final$left_to_distribute[1] > 0 )
    # A purrr trick to run the function recursively
    # n times, n = number of leftovers
    for (i in 1:d_final$left_to_distribute[1])
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



generate_elected <- function(filename, year, uf = "") {
# This function runs our algorithm
# for finding elected deputies: 
# It returns a dataset with
# 'resultado' column
# with who were elected in the election
  
  data <- load_data('data/de_final_data.csv')
  data <- dplyr::filter(data, ano_eleicao == year)
  dataset <- clean_data(data)
  
  if (uf != "")
    dataset <- dplyr::filter(dataset, sigla_uf == uf)
  
  

#  # Separate dataset by ids
#  # Ids represent different elections
    d <- dataset %>%
      split(.$id)

  # Running clean_tipo_legenda for all states
  results <- map_df(d, clean_tipo_legenda ) 
  
  results
}

