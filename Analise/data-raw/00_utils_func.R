library(tidyverse)
library(imager)
library(progressr)
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

cimg2tibble <- function(path){

  magick::image_read(path) %>%
    # magick::image_quantize(colorspace = 'gray') %>%
    imager::magick2cimg() %>%
    as.data.frame() %>%
    tibble::as_tibble()

}

imgtibble2array <- function(imgtibble){

  if('cc' %in% names(imgtibble)){
    ar <- list(
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
  } else{
    ar <- imgtibble %>%
      tidyr::pivot_wider(names_from = y, values_from = value) %>%
      dplyr::select(-x) %>%
      data.matrix() %>%
      unname %>%
      array(dim = c(nrow(.),ncol(.),1))
  }

  return(ar)
}


# FEATURES ARRAY (SPECTRO) ------------------------------------------------
cria_array <- function(file_spec) {
  p <- progressr::progressor(along = file_spec)

  furrr::future_map(
    file_spec,
    function(x){
      p()
      b <- cimg2tibble(x) %>%
        imgtibble2array()
      gc(reset = TRUE,full = TRUE)
      return(b)

    }
  )
}
