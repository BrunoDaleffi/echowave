library(tidyverse)
library(progressr)

files <- c(
  fs::dir_ls('/media/daleffi/1TB SSD/Iti/Iti05/',recurse = TRUE,glob = '*.WAV'),
  fs::dir_ls('/media/daleffi/1TB SSD/Iti/Iti06/',recurse = TRUE,glob = '*.WAV')
)


tabela <- tibble::tibble(file = files,
                         novo_local = paste0('/home/bdaleffi/pos_graduacao/projeto final/dados_brutos/audios/itirapina 20-21',stringr::str_remove(file,'/media/daleffi/1TB SSD/Iti')),
                         regiao = 'Itirapina',
                         local = ifelse(stringr::str_detect(file,stringr::regex('iti05',ignore_case = TRUE)),'ITI05','ITI06'))

set.seed(42)
amostra <- tabela %>%
  dplyr::filter(regiao == 'Itirapina') %>%
  dplyr::sample_n(2500)

# set.seed(42)
# amostra1 <- tabela %>%
#   dplyr::filter(regiao == 'Assis') %>%
#   dplyr::sample_n(2500)
# amostra2 <- tabela %>%
#   dplyr::filter(regiao != 'Assis') %>%
#   dplyr::sample_n(2500)
# amostra <- dplyr::bind_rows(amostra1,amostra2)


future::plan('multisession')

furrr::future_walk2(amostra$file,amostra$novo_local,file.copy,.progress = TRUE)

