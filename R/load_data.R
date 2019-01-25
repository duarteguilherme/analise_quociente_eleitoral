#' In this file, we will put
#' all the functions related
#' to load a datafile
#' 
#' 

library(dplyr)
library(tidyr)
library(purrr)



load_data <- function(filename) {
  de_data <- read.csv(filename,  encoding = "latin1", stringsAsFactors = F, sep = ';')
  
  #' Removing not ascii characters
  
  de_data <- de_data %>%
    mutate_at(vars(desc_sit_cand_tot), function(x) iconv(x, from = "latin1", to = "ascii//translit"))
  
  
  #' Encoding test
  if ( sum(de_data$desc_sit_cand_tot == "ELEITO POR MEDIA", na.rm=T) <= 0 ) {
    stop("Encoding error. Possibly problem with OS - Windows, Linux or Mac")  
  }
}


data <- load_data('data/de_final_data.csv')

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
