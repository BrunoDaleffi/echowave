library(tidyverse)
library(tuneR)
library(warbleR)
library(seewave)
library(soundgen)
library(Rraven)
library(baRulho)
library(dynaSpec)
library(bioacoustics)
library(monitoR)
library(reshape2)
library(furrr)

audio_to_df <- function(file,path = '.'){

  local = dplyr::case_when(stringr::str_detect(file,stringr::regex('assis',ignore_case = TRUE)) & stringr::str_detect(file,'longe') ~ 'Assis - AS02',
                           stringr::str_detect(file,stringr::regex('assis',ignore_case = TRUE)) & stringr::str_detect(file,'perto') ~ 'Assis - AS01',
                           stringr::str_detect(file,stringr::regex('barb',ignore_case = TRUE)) & stringr::str_detect(file,'longe') ~ 'Sta Barb - SB02',
                           stringr::str_detect(file,stringr::regex('barb',ignore_case = TRUE)) & stringr::str_detect(file,'perto') ~ 'Sta Barb - SB01',
                           stringr::str_detect(file,stringr::regex('iti',ignore_case = TRUE)) & stringr::str_detect(file,'iti05') ~ 'Itirapina - IT05',
                           stringr::str_detect(file,stringr::regex('iti',ignore_case = TRUE)) & stringr::str_detect(file,'iti06') ~ 'Sta Barb - IT06',
  )

  datetime <- file %>%
    stringr::str_extract('/[0-9]{3,}.*') %>%
    stringr::str_remove_all('[^0-9]') %>%
    lubridate::ymd_hms()

  wav <- tuneR::readWave(file) %>%
    tuneR::downsample(samp.rate = 11025)

  spectro <- seewave::spectro(wave = wav,plot = F)

  amp <- reshape2::melt(spectro$amp, value.name = "Amplitude") %>%
    dplyr::select(FrequencyIndex = Var1, TimeIndex = Var2, Amplitude)

  freq <- reshape2::melt(spectro$freq, value.name = "Frequency") %>%
    dplyr::mutate(FrequencyIndex = row_number(), Frequency = Frequency * 1000)

  time <- reshape2::melt(spectro$time, value.name = "Time") %>%
    dplyr::mutate(TimeIndex = row_number())

  df <- amp %>%
    dplyr::left_join(freq, by = "FrequencyIndex") %>%
    dplyr::left_join(time, by = "TimeIndex") %>%
    dplyr::transmute(file = file,
                     date = datetime + lubridate::seconds(Time),
                     local = local,
                     time = Time,
                     freq = Frequency,
                     amp = Amplitude)

  readr::write_rds(x = df,
                   file = paste0(path,stringr::str_replace_all(datetime,'[^0-9 ]',''),'.rds'),
                   compress = 'xz')


}

files <- tibble::tibble(files = fs::dir_ls('data-raw/bruto',recurse = TRUE,glob = '*.WAV')) %>%
  dplyr::filter(!stringr::str_detect(files,'geral')) %>%
  dplyr::pull(files)


# Df Completo ------------------------------------------------------------------

future::plan(future::multisession(workers = 7))

furrr::future_walk(.x = files,
                   .f = function(x){
                     audio_to_df(file = x,path = 'data-raw/rds/completo_downsampling/')
                     gc(reset = TRUE)
                   },
                   .progress = TRUE)
