library(tensorflow)
library(keras)
library(magrittr)

data_cnn <- readr::read_rds('~/mestrado/20230211_arrays_cnn/cnn_audio.rds')
dim_cnn = dim(data_cnn$train$x)[2:4]

input <- keras::layer_input(shape = dim_cnn)

fts <- 32
output <- input %>%
  keras::layer_conv_2d(filters = fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_conv_2d(filters = fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_spatial_dropout_2d(0.3) %>%

  keras::layer_conv_2d(filters = 2*fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_conv_2d(filters = 2*fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_spatial_dropout_2d(0.3) %>%

  keras::layer_conv_2d(filters = 4*fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_conv_2d(filters = 4*fts, kernel_size = c(5,5), activation = 'relu', padding = 'same', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_batch_normalization() %>%
  keras::layer_max_pooling_2d(pool_size = c(2,2)) %>%
  keras::layer_spatial_dropout_2d(0.3) %>%

  keras::layer_global_average_pooling_2d() %>%
  keras::layer_flatten() %>%
  keras::layer_dense(units = 4*fts, activation = 'relu', kernel_regularizer = regularizer_l2(0.001)) %>%
  keras::layer_dropout(0.4) %>%
  keras::layer_dense(units = 3, activation = 'softmax')

model <- keras::keras_model(
  input,
  output)

reduce_lr <- keras::callback_reduce_lr_on_plateau(
  monitor = "val_loss",
  factor = 0.5,
  patience = 5,
  verbose = 1,
  mode = "auto",
  min_delta = 0.0001,
  cooldown = 0,
  min_lr = 0
)

model %>%
  keras::compile(
    optimizer = keras::optimizer_adam(learning_rate = 0.001),
    loss = 'categorical_crossentropy',
    metrics = 'accuracy'
  )

CustomEarlyStopping <- R6::R6Class(
  "CustomEarlyStopping",
  inherit = KerasCallback,

  public = list(
    epochs_above_97 = 0,
    consecutive_epochs = 10,
    target_accuracy = 0.982,

    on_epoch_end = function(epoch, logs = NULL) {
      val_acc <- logs[["val_accuracy"]]

      if (val_acc > 0.97) {
        self$epochs_above_97 <- self$epochs_above_97 + 1
      } else {
        self$epochs_above_97 <- 0
      }

      if (self$epochs_above_97 >= self$consecutive_epochs && val_acc > self$target_accuracy) {
        cat("Atingiu a acurácia alvo de 98% após", self$consecutive_epochs, "épocas consecutivas acima de 97%. Parando o treinamento...\n")
        self$model$stop_training <- TRUE
      }
    }
  )
)

stop_training <- CustomEarlyStopping$new()

# Treinamento do modelo
hist <- model %>%
  keras::fit(
    data_cnn$train$x,
    data_cnn$train$y,
    batch_size = 64,
    epochs = 300,
    validation_data = list(data_cnn$valid$x,data_cnn$valid$y),
    view_metrics = TRUE,
    callbacks = list(stop_training, reduce_lr)
  )

# Teste (98.5% de acc) ------------------------------------------------------
model <-  keras::load_model_hdf5('~/mestrado/modelo_local.h5')
data_cnn <- readr::read_rds('~/mestrado/20230211_arrays_cnn/cnn_audio.rds')

x <- data_cnn$test$x

t <- tibble::as_tibble(data_cnn$test$y) %>%
  dplyr::bind_cols(tibble::as_tibble(predict(object = model,x = x))) %>%
  purrr::set_names(c('obs_0','obs_1','obs_2','pred_0','pred_1','pred_2')) %>%
  dplyr::mutate(
    pred_0 = round(pred_0,2),
    pred_1 = round(pred_1,2),
    pred_2 = round(pred_2,2)
  ) %>%
  dplyr::transmute(
    obs = dplyr::case_when(
      obs_0 == 1 ~ 'obs0',
      obs_1 == 1 ~ 'obs1',
      obs_2 == 1 ~ 'obs2'
    ),
    pred = dplyr::case_when(
      pred_0 > pred_1 & pred_0 > pred_2 ~ 'pred0',
      pred_1 > pred_0 & pred_1 > pred_2 ~ 'pred1',
      pred_2 > pred_0 & pred_2 > pred_1 ~ 'pred2'
    )
  )

tt<-table(t)

round(sum(diag(tt))/sum(tt),2)

keras::save_model_hdf5(model,'~/mestrado/modelo_local.h5')
