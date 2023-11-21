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

# Prep --------------------------------------------------------------------

# Sampling
set.seed(42)
files <- readr::read_rds('~/mestrado/20230211_espectogramas/spectrograms.rds') %>%
  dplyr::mutate(
    file_spec = stringr::str_replace(file_spec,'../../dados_brutos/rds','~/mestrado')
  ) %>%
  rsample::initial_split(0.8667)

files_train_valid <- rsample::training(files) %>%
  rsample::initial_split(0.9232)

files_train <- rsample::training(files_train_valid)
files_valid <- rsample::testing(files_train_valid)
files_test <- rsample::testing(files)


# TREINO

future::plan('multisession')
x_train <- cria_array(files_train$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_train,'~/mestrado/20230211_arrays_cnn/x_train.rds')

# VALIDAÇÃO

future::plan('multisession')
x_valid <- cria_array(files_valid$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_valid,'~/mestrado/20230211_arrays_cnn/x_valid.rds')

# TESTE

future::plan('multisession')
x_test <- cria_array(files_test$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_test,'~/mestrado/20230211_arrays_cnn/x_test.rds')


# Cria cnn_audio ----------------------------------------------------------

x_train <- readr::read_rds('~/mestrado/20230211_arrays_cnn/x_train.rds')
x_valid <- readr::read_rds('~/mestrado/20230211_arrays_cnn/x_valid.rds')
x_test <- readr::read_rds('~/mestrado/20230211_arrays_cnn/x_test.rds')

y_train <- as.numeric(factor(files_train$local))-1
y_train <- keras::to_categorical(y = y_train,num_classes = 3)
y_valid <- as.numeric(factor(files_valid$local))-1
y_valid <- keras::to_categorical(y = y_valid,num_classes = 3)
y_test <- as.numeric(factor(files_test$local))-1
y_test <- keras::to_categorical(y = y_test,num_classes = 3)

# SALVA
cnn_audio <- list(
  train = list(
    x = x_train,
    y = y_train
  ),
  valid = list(
    x = x_valid,
    y = y_valid
  ),
  test = list(
    x = x_test,
    y = y_test
  )
)


readr::write_rds(cnn_audio,'~/mestrado/20230211_arrays_cnn/cnn_audio.rds')
