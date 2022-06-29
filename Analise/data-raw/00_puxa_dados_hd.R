library(tidyverse)
library(progressr)

files <- c(
  fs::dir_ls('../../../../media/daleffi/Acústica/assis 19-20/',recurse = TRUE,glob = '*.WAV'),
  fs::dir_ls('../../../../media/daleffi/Acústica/sta barb 19-20/',recurse = TRUE,glob = '*.WAV')
)


tabela <- tibble::tibble(file = files,
                         novo_local = paste0('/home/bdaleffi/pos_graduacao/sondaR/data-raw',stringr::str_remove(file,'../../../../media/daleffi/Acústica')),
                         regiao = ifelse(stringr::str_detect(file,'assis'),'Assis','Santa Bárbara'),
                         local = ifelse(stringr::str_detect(file,'perto'),'Perto da Sede Primavera','Longe da Sede Primavera'))

set.seed(42)
amostra1 <- tabela %>%
  dplyr::filter(regiao == 'Assis') %>%
  dplyr::sample_n(2500)
amostra2 <- tabela %>%
  dplyr::filter(regiao != 'Assis') %>%
  dplyr::sample_n(2500)

amostra <- dplyr::bind_rows(amostra1,amostra2)


future::plan('multisession')

furrr::future_walk2(amostra$file,amostra$novo_local,file.copy,.progress = TRUE)

