#' Formato de numero
#' @param num numero a ser convertido
format_num <- function(num) {
  scales::number_format(big.mark = ".", decimal.mark = ",", accuracy = 1)(num)
}

#' Formato de porcentagem
#' @param pct numero a ser convertido
format_pct <- function(pct) {
  scales::percent_format(accuracy = 0.1)(pct)
}

#' Formato de dinheiro
#' @param num numero a ser convertido
format_dinhero <- function(num) {
  scales::dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 0.01)(num)
}

#' new scale
#' @param img image
#' @param prop scale (%)
new_scale <- function(img,prop = 0.4){
  imager::resize(im = img,size_x = prop*dim(img)[1],size_y = prop*dim(img)[2])

}

#' cmig to tibble
#' @param path local do arquivo
#' @param scale reducao de escada (%)
cimg2tibble <- function(path,scale = 0.15){

  imager::load.image(file = path) %>%
    new_scale(scale) %>%
    as.data.frame() %>%
    tibble::as_tibble()

}

#' mig to keras array
#' @param path local do arquivo
#' @param scale reducao de escada (%)
imgtibble2array <- function(imgtibble){

  list(
    ar1 <- imgtibble %>%
      tidyr::pivot_wider(names_from = y, values_from = value) %>%
      dplyr::filter(cc == 1) %>%
      dplyr::select(-x,-cc) %>%
      data.matrix() %>%
      unname %>%
      array(dim = c(nrow(.),ncol(.),1)),

    ar2 <- imgtibble %>%
      tidyr::pivot_wider(names_from = y, values_from = value) %>%
      dplyr::filter(cc == 2) %>%
      dplyr::select(-x,-cc) %>%
      data.matrix() %>%
      unname %>%
      array(dim = c(nrow(.),ncol(.),1)),

    ar3 <- imgtibble %>%
      tidyr::pivot_wider(names_from = y, values_from = value) %>%
      dplyr::filter(cc == 3) %>%
      dplyr::select(-x,-cc) %>%
      data.matrix() %>%
      unname %>%
      array(dim = c(nrow(.),ncol(.),1))
  ) %>%
    abind::abind() %>%
    array(dim = dim(.))
}

sound_spec <- function(sd){
  sd %>%
    seewave::ggspectro() +
    ggplot2::geom_tile(ggplot2::aes(fill = amplitude,color = amplitude)) +
    ggplot2::scale_fill_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"))  +
    ggplot2::scale_color_gradientn(colours = c("#ffffff","#0000ff", "#00ff00","#ffff00","#ff0000"),guide = 'none')  +
    ggplot2::labs(x = 'Tempo (s)',y = 'Frequência (KHz)', fill = 'Amplitude', color = '')  +
    ggplot2::theme_minimal()
}
