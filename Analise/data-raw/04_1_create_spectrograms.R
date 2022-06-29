library(tidyverse)


create_spectro_ <- function(path,idx){
  sd <- path %>%
    tuneR::readWave()

  file_name <- paste0('file_',idx)

  (seewave::ggspectro(sd) +
      ggplot2::geom_tile(ggplot2::aes(fill = amplitude,color = amplitude)) +
      ggplot2::scale_fill_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"))  +
      ggplot2::scale_color_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"))  +
      ggplot2::labs(x = '',y = '', fill = '')  +
      ggplot2::scale_x_continuous(limits = c(0,180)) +
      ggplot2::scale_y_continuous(limits = c(0,16)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'none',
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.grid =  ggplot2::element_blank()
      )) %>%
    ggplot2::ggsave(
      filename = file_name,
      path = '/home/bdaleffi/pos_graduacao/sondaR/data-raw/spectrograms/img',device = 'jpeg',
      units = 'px',width = 1000,height = 1000
    )

  tibble::tibble(
    local = ifelse(stringr::str_detect(path,'assis'),1,0),
    dist_sede = ifelse(stringr::str_detect(path,'longe'),1,0),
    hora = lubridate::hour(lubridate::ymd_hms(stringr::str_extract(path,'(?<=/)[0-9].*(?=\\.WAV)'))),
    periodo_dia = dplyr::case_when(hora < 4 ~ 1,
                                   hora < 6 ~ 2,
                                   hora < 9 ~ 3,
                                   hora < 12 ~ 4,
                                   hora < 16 ~ 5,
                                   hora < 19 ~ 6,
                                   hora < 21 ~ 7,
                                   hora <= 24 ~ 8)
  ) %>%
    dplyr::select(-hora) %>%
    readr::write_rds(file = paste0( '/home/bdaleffi/pos_graduacao/sondaR/data-raw/spectrograms/features/',file_name,'.rds'))


  gc(reset = TRUE)
}

create_spectro <- function(files){
  furrr::future_walk2(
    files,
    1:length(files),
    create_spectro_,
    .progress = TRUE
  )
}

files <-fs::dir_ls('data-raw/bruto/',recurse = TRUE,regexp = '*[0-9]\\.WAV$')

future::plan(future::multisession(workers = future::availableCores()-1))
create_spectro(files)
