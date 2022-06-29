library(keras)
library(reticulate)
library(tidyverse)
library(progressr)
handlers(global = TRUE)

# Le os arquivos ------------------------------------------------------------
read_files <- function(files_features,tp = 'x'){

  furrr::future_map(
    .x = files_features,
    .f = function(x){

      if(tp == 'x1'){
        b <- readr::read_rds(x) %>%
          dplyr::select(-dplyr::any_of(c('file','local','hora','periodo_dia','estacao','dist_sede'))) %>%
            as.matrix()

      }

      if(tp == 'x2'){
        b <- readr::read_rds(x) %>%
          dplyr::select(dplyr::any_of(c('hora','periodo_dia','estacao','local','dist_sede'))) %>%
          dplyr::distinct() %>%
          as.matrix()
      }

      if(tp == 'y'){
        b <- readr::read_rds(x) %>%
          dplyr::select(-file) %>%
          as.matrix()
      }
      gc(verbose = FALSE,reset = TRUE)

      return(b)
    },
    .progress = TRUE
  ) %>%
    purrr::set_names(stringr::str_extract(files_features,'(?<=/)[0-9].*(?=\\.rds)'))

}

files_x <- fs::dir_ls('data-raw/rds/feature_enginering_to_keras/x/')
files_y <- fs::dir_ls('data-raw/rds/feature_enginering_to_keras/y/')

future::plan('multisession')
x1 <- read_files(files_x,tp = 'x1')
x2 <- read_files(files_x,tp = 'x2')
y <- read_files(files_y,tp = 'y')

n = round(length(x1)*0.7)

set.seed(42)
x1_treino <- sample(x1,n)
nm <- names(x1_treino)

x1_teste <- x1 %>%
  purrr::discard(names(.) %in% nm)

x2_treino <- x2 %>%
  purrr::keep(names(.) %in% nm)
x2_teste <- x2 %>%
  purrr::discard(names(.) %in% nm)

y_treino <- y %>%
  purrr::keep(names(.) %in% nm)
y_teste <- y %>%
  purrr::discard(names(.) %in% nm)

names(x1_treino) <- NULL
names(x1_teste) <- NULL
names(x2_treino) <- NULL
names(x2_teste) <- NULL
names(y_treino) <- NULL
names(y_teste) <- NULL

x1_train <- array(as.numeric(unlist(x1_treino)),dim = c(length(x1_treino),nrow(x1_treino[[1]]),ncol(x1_treino[[1]])))
x1_test <- array(as.numeric(unlist(x1_teste)),dim = c(length(x1_teste),nrow(x1_teste[[1]]),ncol(x1_teste[[1]])))

x2_train <- array(as.numeric(unlist(x2_treino)),dim = c(length(x2_treino),nrow(x2_treino[[1]]),ncol(x2_treino[[1]])))
x2_test <- array(as.numeric(unlist(x2_teste)),dim = c(length(x2_teste),nrow(x2_teste[[1]]),ncol(x2_teste[[1]])))

y_train <- array(as.numeric(unlist(y_treino)))
y_test <- array(as.numeric(unlist(y_teste)))


# dim(x_train)
# dim(y_train)

# Model -------------------------------------------------------------------

x1_train <- reticulate::array_reshape(x1_train,c(dim(x1_train)[1],dim(x1_train)[2]*dim(x1_train)[3]))
x2_train <- reticulate::array_reshape(x2_train,c(dim(x2_train)[1],dim(x2_train)[2]*dim(x2_train)[3]))

readr::write_rds(x1_train,'data-raw/rds/feature_enginering_to_keras/x1_train.rds')
readr::write_rds(x2_train,'data-raw/rds/feature_enginering_to_keras/x2_train.rds')
readr::write_rds(y_train,'data-raw/rds/feature_enginering_to_keras/y_train.rds')


readr::write_rds(x1_test,'data-raw/rds/feature_enginering_to_keras/x1_test.rds')
readr::write_rds(x2_test,'data-raw/rds/feature_enginering_to_keras/x2_test.rds')
readr::write_rds(y_test,'data-raw/rds/feature_enginering_to_keras/y_test.rds')

