library(keras)
library(tidyverse)

data_cnn <- readr::read_rds('data-raw/rds/keras_cnn_features/cnn_audio.rds')
dim_cnn = dim(data_cnn$train$xspec)[2:4]

input_spec <- keras::layer_input(shape = dim_cnn)
input_df <- keras::layer_input(shape = ncol(data_cnn$train$xfeat))

output_spec <- input_spec %>%
  keras::layer_conv_2d(filters = 4,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 8,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 16,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 16,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_flatten()

output_df <- input_df %>%
  keras::layer_dense(units = 512,activation = 'relu')

output <- keras::layer_concatenate(list(output_spec,output_df))  %>%
  keras::layer_dropout(0.4) %>%
  keras::layer_dense(units = 1,activation = 'sigmoid')

model <- keras::keras_model(
  list(input_spec, input_df),
  output)

model %>%
  keras::compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = "acc"
  )

summary(model)

model %>%
  keras::fit(
    list(data_cnn$train$xspec,data_cnn$train$xfeat),
    data_cnn$train$y,
    batch_size = 100,
    epochs = 500,
    validation_split = 0.3
  )
