#' These are functions
#' to generate information
#' of relevance for representatives


check_prediction <- function(dataset_without_candidate, actual_elected) {
  # This function gets the dataset and check if it
  # matches the real_elected_deputies (our first prediction)
  

  # Now we're testing whether removing this deputies
  # affects the election outcome
  predicted_elected_deputies <- clean_tipo_legenda(dataset_without_candidate) %>%
    filter(resultado %in% c("ELEITO POR MEDIA", "ELEITO QP")) %>%
    arrange(numero_cand) %>%
    pull(numero_cand) 
  
  actual_elected_num <- actual_elected %>%
    arrange(numero_cand) %>%
    pull(numero_cand)
  
  if (length(predicted_elected_deputies) != length(actual_elected_num))
    stop("TRETA - Checar")
  return(all(predicted_elected_deputies == actual_elected_num))
}




generate_relevants_by_party <- function(party, actual_elected, dataset) {
  # This functions gets a state
  # and run for each party
  # a routine that checks whether a candidate
  # is irrelevant, removing him/her if the output is true

  party_candidates <- dataset %>%
    filter(party_or_colig == party) %>%
    arrange(tot_votos_nominais) 
  
  
  for (k in 1:nrow(party_candidates)) {
    if ( filter(dataset, numero_cand != party_candidates$numero_cand[k]) %>% 
         check_prediction(actual_elected)
         ) {
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


generate_relevants <- function(uf, year2 = year, actual_elected) {
  cat(glue::glue("Running for {uf}. year = {year2}\n\n\n")) 

  dataset <- generate_elected(filename, year2, uf) %>%
    mutate(party_or_colig = ifelse(tipo_legenda == "PARTIDO ISOLADO", sigla_partido,
                                   ifelse(tipo_legenda == "COLIGACAO", nome_coligacao, NA))) 

  parties <- dataset %>%
    distinct(party_or_colig) %>%
    pull
  
  actual_elected <- filter(actual_elected, sigla_uf == uf) 
  
  # It also runs for parties with no elected deputies
  # This is a bug we must fix down the line
  

  relevants <- map_df(parties, ~ generate_relevants_by_party(.x, actual_elected, dataset))
  relevants
}

