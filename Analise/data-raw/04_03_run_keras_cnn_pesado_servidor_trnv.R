library(keras)
library(tidyverse)

data_cnn <- readr::read_rds('~/mestrado/cnn_audio.rds')
dim_cnn = dim(data_cnn$train$xspec)[2:4]

input_spec <- keras::layer_input(shape = dim_cnn)
input_df <- keras::layer_input(shape = ncol(data_cnn$train$xfeat))

output_spec <- input_spec %>%
  keras::layer_conv_2d(filters = 16,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 128,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 256,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_conv_2d(filters = 512,kernel_size = c(3,3),activation = 'relu') %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_flatten()

output_df <- input_df %>%
  keras::layer_dense(units = 2048,activation = 'relu')

output <- keras::layer_concatenate(list(output_spec,output_df))  %>%
  keras::layer_dropout(0.4) %>%
  keras::layer_dense(units = 1024,activation = 'relu') %>%
  keras::layer_dropout(0.3) %>%
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
    epochs = 200,
    validation_split = 0.3
  )



z<-predict(object = model,x = list(list(data_cnn$test$xspec,data_cnn$test$xfeat)))


# Teste (98.5% de acc) ------------------------------------------------------

t <- tibble(obs = data_cnn$test$y,
            pred = as.numeric(predict(object = model,x = list(list(data_cnn$test$xspec,data_cnn$test$xfeat))))
) %>%
  dplyr::mutate(pred = ifelse(pred <= 0.5,0,1))

tb <- table(t)
sum(diag(tb))/sum(tb)

