library(tidyverse)

create_features_ <- function(path,resposta = 'dist_sede'){
  sound <- readr::read_rds(path) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(date = lubridate::ymd_hms(date)) %>%
    dplyr::group_by(file,local,freq) %>%
    dplyr::summarise(
      hora = lubridate::hour(min(date)),
      mes_dia = format(min(date),"%m-%d"),
      periodo_dia = dplyr::case_when(hora < 4 ~ 1,
                                     hora < 6 ~ 2,
                                     hora < 9 ~ 3,
                                     hora < 12 ~ 4,
                                     hora < 16 ~ 5,
                                     hora < 19 ~ 6,
                                     hora < 21 ~ 7,
                                     hora <= 24 ~ 8),
      estacao = dplyr::case_when(mes_dia >= '03-21' & mes_dia < '06-21' ~ 1,
                                 mes_dia >= '06-21' & mes_dia < '09-23' ~ 2,
                                 mes_dia >= '09-23' & mes_dia < '12-21' ~ 3,
                                 mes_dia >= '12-21' & mes_dia < '03-21' ~ 4),
      amplitudo_max = max(amp),
      amplitudo_min = min(amp),
      q1_amp = quantile(amp,0.25),
      q2_amp = quantile(amp,0.50),
      q3_amp = quantile(amp,0.75),
      sd_amp = sd(amp),
      mean_amp = mean(amp)
    ) %>%
    dplyr::mutate(
      dist_sede = ifelse(stringr::str_detect(local,'onge'),0,1),
      local= ifelse(stringr::str_detect(local,'arb'),0,1),
    ) %>%
    dplyr::select(-mes_dia)

  x <- sound %>%
    dplyr::ungroup() %>%
    dplyr::select(-resposta)

  y <- sound %>%
    dplyr::ungroup() %>%
    dplyr::select(file,resposta) %>%
    dplyr::distinct()

  readr::write_rds(x,
                   paste0('/home/bdaleffi/pos_graduacao/sondaR/Analise/data-raw/rds/feature_enginering_to_keras/x/',
                          resposta,'/',stringr::str_extract(path,'(?<=/)[0-9].*(?=\\.rds)'),'.rds')
  )

  readr::write_rds(y,
                   paste0('/home/bdaleffi/pos_graduacao/sondaR/Analise/data-raw/rds/feature_enginering_to_keras/y/',
                          resposta,'/',stringr::str_extract(path,'(?<=/)[0-9].*(?=\\.rds)'),'.rds')
  )
  gc(reset = TRUE)

}

create_features <- function(files){

  furrr::future_walk(
    .x = files,
    .f = function(x){
      x <- create_features_(x)
      gc(reset = TRUE)
      return(x)
    },
    .progress = TRUE
  )
}

files <- fs::dir_ls('/home/bdaleffi/pos_graduacao/sondaR/Analise/data-raw/rds/completo/')

future::plan('multisession')
create_features(files)

