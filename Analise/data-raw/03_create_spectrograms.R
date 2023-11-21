library(tidyverse)
progressr::handlers(global = TRUE)

progressr::handlers(
  list(
    progressr::handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 115,
      complete = "+"
    )
  )
)


create_spectro_ <- function(path,idx){
  sd <- path %>%
    readr::read_rds() %>%
    purrr::pluck(2) %>%
    dplyr::ungroup() %>%
    dplyr::as_tibble()

  file_name =  paste0('file_',stringr::str_pad(string = idx,width = 5,side = 'left',pad = '0'))

  path_to_save <- '../../dados_brutos/rds/20230318_espectogramas/'

  (ggplot2::ggplot(
    data = sd,
    ggplot2::aes(x =time,y = freq)
  ) +
      ggplot2::geom_tile(ggplot2::aes(fill = amp_norm,color = amp_norm)) +
      ggplot2::scale_fill_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"))  +
      ggplot2::scale_color_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"))  +
      ggplot2::labs(x = '',y = '', fill = '')  +
      ggplot2::scale_x_continuous(limits = c(0,60)) +
      ggplot2::scale_y_continuous(limits = c(0,4000)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = 'none',
                     axis.text.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     panel.grid =  ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(t = -11.5,r = -14.9,b = -26.9,l = -26.5)
      )) %>%
    ggplot2::ggsave(
      filename = paste0(file_name,'.jpeg'),
      path = paste0(path_to_save,'jpeg'),
      device = 'jpeg',
      units = 'px',width = 1000,height = 1000
    )

  local = dplyr::case_when(stringr::str_detect(path,stringr::regex('assis',ignore_case = TRUE)) ~ 'Assis',
                           stringr::str_detect(path,stringr::regex('barb',ignore_case = TRUE))  ~ 'Sta Barb',
                           stringr::str_detect(path,stringr::regex('iti',ignore_case = TRUE)) ~ 'Itirapina'
  )

  tibble::tibble(
    file_spec = paste0(path_to_save,'jpeg/',file_name,'.jpeg'),
    local = local
  ) %>%
    readr::write_rds(file = paste0(path_to_save,'rds/',file_name,'.rds'))
}

create_spectro <- function(files){
  p <- progressr::progressor(along = files)

  furrr::future_walk2(
    files,
    1:length(files),
    function(x,y){
      p()
      create_spectro_(x,y)
    }
  )
}

files <-fs::dir_ls('../../dados_brutos/rds/20221108_df_audio/')

future::plan('multisession')
create_spectro(files)

purrr::map_dfr(
  fs::dir_ls('../../dados_brutos/rds/20230318_espectogramas/rds/'),
  readr::read_rds
) %>%
  readr::write_rds('../../dados_brutos/rds/20230318_espectogramas/spectrograms.rds')
