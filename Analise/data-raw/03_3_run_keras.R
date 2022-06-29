library(keras)
library(tidyverse)



# data --------------------------------------------------------------------

x1_train <- readr::read_rds('data-raw/rds/feature_enginering_to_keras/x1_train.rds')
x2_train <- readr::read_rds('data-raw/rds/feature_enginering_to_keras/x2_train.rds')
y_train <- readr::read_rds('data-raw/rds/feature_enginering_to_keras/y_train.rds')

input_x1 <- keras::layer_input(shape = ncol(x1_train))
input_x2 <- keras::layer_input(shape = ncol(x2_train))

output_x1 <- input_x1 %>%
  keras::layer_dense(
    units = 128,
    activation = 'softmax'
  )

output_x2 <- input_x2 %>%
  keras::layer_dense(
    units = 128,
    activation = 'softmax'
  )


output <- keras::layer_concatenate(list(output_x1, output_x2)) %>%
  keras::layer_dropout(0.4) %>%
  keras::layer_dense(
    units = 2048,
    activation = 'relu'
  ) %>%
  # keras::layer_dropout(0.4) %>%
  # keras::layer_dense(
  #   units = 1024,
  #   activation = 'relu'
  # ) %>%
  # keras::layer_dropout(0.4) %>%
  # keras::layer_dense(
  #   units = 128,
  #   activation = 'relu'
  # ) %>%
  # keras::layer_dropout(0.4) %>%
  # keras::layer_dense(
  #   units = 8,
  #   activation = 'relu'
  # ) %>%
  keras::layer_dropout(0.4) %>%
  keras::layer_dense(
    units = 1,
    activation = 'sigmoid'
  )

model <- keras::keras_model(
  inputs = list(input_x1,input_x2),
  outputs = output
)

model %>%
  keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(learning_rate = 0.001),
    metrics = c('accuracy')
  )

summary(model)

history <- model %>%
  keras::fit(
    list(x1_train,x2_train),
    y_train,
    epochs = 1000,
    batch_size = 100,
    validation_split = 0.3,
    view_metrics = FALSE
  )

