library(tidyverse)
library(imager)
library(progressr)
progressr::handlers(global = TRUE)

# Functions ---------------------------------------------------------------

# Muda escala da imagem
new_scale <- function(img,prop = 0.4){
  imager::resize(im = img,size_x = prop*dim(img)[1],size_y = prop*dim(img)[2])

}

# Imagem para tibble
cimg2tibble <- function(path,scale = 0.15){

  imager::load.image(file = path) %>%
    new_scale(scale) %>%
    as.data.frame() %>%
    tibble::as_tibble()

}

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


# Prep --------------------------------------------------------------------

# Sampling
set.seed(42)
files <- tibble::tibble(
  img = fs::dir_ls('data-raw/spectrograms/img/'),
  features = fs::dir_ls('data-raw/spectrograms/features/')
) %>%
  rsample::initial_split(prop = 0.8)

files_train <- rsample::training(files)
files_test <- rsample::testing(files)


# FEATURES UNICAS ---------------------------------------------------------
# resposta <- 'dist_sede'
# features_train <- purrr::map_dfr(
#   files_train$features,
#   readr::read_rds
# )
#
# features_test <- purrr::map_dfr(
#   files_test$features,
#   readr::read_rds
# )
#
# y_train <- dplyr::pull(features_train, resposta)
# y_test <-  dplyr::pull(features_test, resposta)
#
# x_f_train <- dplyr::select(features_train,-resposta) %>% as.matrix()
# x_f_test <- dplyr::select(features_test,-resposta) %>% as.matrix()
#
# readr::write_rds(y_train,'data-raw/rds/keras_cnn_features/aux/y_train.rds')
# readr::write_rds(y_test,'data-raw/rds/keras_cnn_features/aux/y_test.rds')
# readr::write_rds(x_f_train,'data-raw/rds/keras_cnn_features/aux/x_f_train.rds')
# readr::write_rds(x_f_test,'data-raw/rds/keras_cnn_features/aux/x_f_test.rds')

# FEATURES ARRAY (SPECTRO) ------------------------------------------------


# TEM UM ROLEZINHO AQUI, LEIA OS COMENTARIOS (nao automatizei por PP 'Pura Preguiça')------------------------------

# TREINO
# future::plan(future::multisession(workers = 6))
# x_train <- furrr::future_map(
#   files_train$img,
#   ~ cimg2tibble(.x,scale = 0.2) %>% imgtibble2array,
#   .progress = TRUE)
#
# readr::write_rds(x_train,'data-raw/rds/keras_cnn_features/aux/train_x.rds')

## FACO ISSO PRA NAO QUEBRAR O R POR FALTA DE MEMORIA
# restara a sessao e roda o seguinte (para chunks):
# library(tidyverse)
# x_train <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/train_x.rds')
#
# x1 <- x_train[2001:4000] %>%
#   abind::abind(along = 0)
#
# readr::write_rds(x1,'data-raw/rds/keras_cnn_features/aux/train_x_2.rds')
#
## TIVE QUE RODAR ESSA PARTE NO SERVIDOR DA TERRANOVA

# x_train <- purrr::map(
#   c('data-raw/rds/keras_cnn_features/aux/train_x_1.rds',
#    'data-raw/rds/keras_cnn_features/aux/train_x_2.rds'),
#   readr::read_rds
# )
#
# x_train <- abind::abind(x_train,along = 0)

# TESTE

future::plan(future::multisession(workers = 6))
x_test <- furrr::future_map(
  files_test$img,
  ~ cimg2tibble(.x,scale = 0.2) %>% imgtibble2array,
  .progress = TRUE) %>%
  abind::abind(along = 0)

readr::write_rds(x_test,'data-raw/rds/keras_cnn_features/aux/x_test.rds')


# SALVA TUDO --------------------------------------------------------------

x_train <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/x_train.rds')
x_test <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/x_test.rds')
x_f_train <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/x_f_train.rds')
x_f_test <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/x_f_test.rds')
y_train <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/y_train.rds')
y_test <- readr::read_rds('data-raw/rds/keras_cnn_features/aux/y_test.rds')


cnn_audio <- list(
  train = list(
    xspec = x_train,
    xfeat = x_f_train,
    y = y_train
  ),
  test = list(
    xspec = x_test,
    xfeat = x_f_test,
    y = y_test
  )
)

readr::write_rds(cnn_audio,'data-raw/rds/keras_cnn_features/cnn_audio_dist_sede.rds',compress = 'xz')
