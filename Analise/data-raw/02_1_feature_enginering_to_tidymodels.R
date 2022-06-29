library(tidyverse)

create_features_ <- function(path){
  sound <- readr::read_rds(path) %>%
    dplyr::ungroup() %>%
    tibble::as_tibble() %>%
    dplyr::arrange(abs(amp)) %>%
    dplyr::mutate(
      freq_amp_max = dplyr::slice(.,1) %>% dplyr::pull(freq),
      freq_amp_min = dplyr::slice(.,nrow(.)) %>% dplyr::pull(freq)
    ) %>%
    dplyr::group_by(file,local,freq_amp_max,freq_amp_min) %>%
    dplyr::summarise(
      hora = lubridate::hour(min(date)),
      mes_dia = format(min(date),"%m-%d"),
      periodo_dia = dplyr::case_when(hora < 4 ~ "[00:00 - 04:00)",
                                     hora < 6 ~ "[04:00 - 06:00)",
                                     hora < 9 ~ "[06:00 - 09:00)",
                                     hora < 12 ~ "[09:00 - 12:00)",
                                     hora < 16 ~ "[12:00 - 16:00)",
                                     hora < 19 ~ "[16:00 - 19:00)",
                                     hora < 21 ~ "[19:00 - 21:00)",
                                     hora <= 24 ~ "[21:00 - 00:00]"),
      estacao = dplyr::case_when(mes_dia >= '03-21' & mes_dia < '06-21' ~ 'Outono',
                                 mes_dia >= '06-21' & mes_dia < '09-23' ~ 'Inverno',
                                 mes_dia >= '09-23' & mes_dia < '12-21' ~ 'Primavera',
                                 mes_dia >= '12-21' & mes_dia < '03-21' ~ 'Verão'),
      amplitudo_max = max(amp),
      amplitudo_min = min(amp),
      q1_amp = quantile(amp,0.25),
      q2_amp = quantile(amp,0.50),
      q3_amp = quantile(amp,0.75),
      sd_amp = sd(amp),
      mean_amp = mean(amp)
    ) %>%
    dplyr::mutate(
      dist_sede = ifelse(stringr::str_detect(local,'onge'),'Longe da Sede','Perto da Sede'),
      local= ifelse(stringr::str_detect(local,'arb'),'Sta Barb','Assis'),

    )

  gc(reset = TRUE)
  return(sound)

}

create_features <- function(files){

  bd <- furrr::future_map_dfr(
    .x = files,
    .f = function(x){
      x <- create_features_(x)
      gc(reset = TRUE)
      return(x)
    },
    .progress = TRUE
  )

  readr::write_rds(bd,'/home/bdaleffi/pos_graduacao/sondaR/data-raw/rds/feature_enginering_to_tidymodels/features_tidymodels.rds')

}

files <- fs::dir_ls('/home/bdaleffi/pos_graduacao/sondaR/data-raw/rds/completo/')

future::plan('multisession')
create_features(files)

