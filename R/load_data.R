#' In this file, we will put
#' all the functions related
#' to load a datafile
#' 
#' 

library(dplyr)
library(tidyr)
library(purrr)



load_data <- function(filename) {
  de_data <- read.csv(filename,  encoding = "latin1", stringsAsFactors = F, sep = ',')
  
  #' Removing not ascii characters
  
  de_data <- de_data %>%
    mutate_at(vars(desc_sit_cand_tot), function(x) iconv(x, from = "latin1", to = "ascii//translit"))
  
  # This solution will solve problems regarding
  # differences of tipo_legenda for different years
  de_data <- de_data %>%
    mutate_at(vars(tipo_legenda), function(x) iconv(toupper(x), from = "latin1", to = "ascii//translit"))
  
  #' Encoding test
  if ( sum(de_data$desc_sit_cand_tot == "ELEITO POR MEDIA", na.rm=T) <= 0 ) {
    stop("Encoding error. Possibly problem with OS - Windows, Linux or Mac")  
  }
  
  de_data
}




clean_data <- function(data) {
  # Generate how seats are allocated
  # considering the partisan quotient
  
  
  d <- data %>% 
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
    mutate(desc_sit_cand_tot = ifelse(desc_sit_cand_tot == "ELEITO POR QP",
                                      "ELEITO", 
                                      desc_sit_cand_tot)) %>%
    # create an id for elections and uf
    mutate(id = stringr::str_c(ano_eleicao, "_", sigla_uf))
  
  d
}
