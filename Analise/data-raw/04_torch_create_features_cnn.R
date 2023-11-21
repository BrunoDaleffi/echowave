library(tidyverse)
library(progressr)
library(torch)
library(luz)
library(torchvision)
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

create_tensor <- function(specs,resize = c(250,250)) {
  root <- specs$file_spec

  p <- progressr::progressor(along = root)

  tensor_x <- root |>
    purrr::map(function(x){
      p()
      torchvision::base_loader(x)
    }
    )  |>
    purrr::map(torchvision::transform_to_tensor) |>
    purrr::map(torchvision::transform_resize,size = resize)  |>
    torch::torch_stack()

  tensor_y <- torch::torch_tensor(as.factor(specs$local))

  return(list(tensor_x = tensor_x,tensor_y = tensor_y))
}


spec_dataset <- torch::dataset(
  name = 'audio_spec',

  initialize = function(specs, tensor_x, tensor_y){

    y_lab <- specs$local


    self$files <- specs$file_spec
    self$x <- tensor_x
    self$y <- tensor_y
    self$y_lab <- y_lab

  },

  .getitem = function(index) {

    x <- self$x[index,]
    y <- self$y[index]
    return(list(x = x, y = y))
  },

  .length = function() {
    length(self$files)
  }
)

set.seed(42)
specs <- readr::read_rds('../../dados_brutos/rds/20230211_espectogramas/spectrograms.rds') |>
  dplyr::sample_n(4000)

tensors <- create_tensor(specs)

tensor_x <- tensors$tensor_x
tensor_y <- tensors$tensor_y



# Dataset -----------------------------------------------------------------

# Sampling

dtset <-specs |>
  spec_dataset(tensor_x = tensor_x,tensor_y = tensor_y)

batch_size = 250

id_total <- seq_along(dtset)
id_train_valid <- sample(id_total, floor(length(id_total)*0.85))

id_train <- sample(id_train_valid, floor(length(id_train_valid)*0.8824))
id_valid <- setdiff(id_train_valid, id_train)
id_test <- setdiff(id_total, id_train_valid)

rm(list = c('id_total','id_train_valid'))
gc(TRUE)

dl_train <- torch::dataloader(
  torch::dataset_subset(dtset, id_train),
  batch_size = batch_size,
  shuffle = TRUE
)

dl_valid <- torch::dataloader(
  torch::dataset_subset(dtset, id_valid),
  batch_size = batch_size
)


dl_test <- torch::dataloader(
  torch::dataset_subset(dtset, id_test),
  batch_size = batch_size
)
