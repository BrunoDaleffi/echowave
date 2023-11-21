library(tidyverse)
library(progressr)
library(torch)
library(luz)
library(torchvision)
source('data-raw/04_torch_create_features_cnn.R')


# CNN ---------------------------------------------------------------------

conv_net <- nn_module(
  "conv_net",

  initialize = function(num_classes=3) {
    ini_out <- 4

    self$layer1 <- torch::nn_sequential(
      torch::nn_conv2d(in_channels = 3,out_channels = ini_out,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$layer2 <-  torch::nn_sequential(
      torch::nn_conv2d(in_channels = ini_out,out_channels = ini_out*2,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out*2),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$layer3 <-  torch::nn_sequential(
      torch::nn_conv2d(in_channels = ini_out*2,out_channels = ini_out*4,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out*4),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$layer4 <-  torch::nn_sequential(
      torch::nn_conv2d(in_channels = ini_out*4,out_channels = ini_out*4,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out*4),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$layer5 <-  torch::nn_sequential(
      torch::nn_conv2d(in_channels = ini_out*4,out_channels = ini_out*2,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out*2),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$layer6 <-  torch::nn_sequential(
      torch::nn_conv2d(in_channels = ini_out*2,out_channels = ini_out,kernel_size = 3),
      torch::nn_relu(),
      torch::nn_batch_norm2d(num_features = ini_out),
      torch::nn_max_pool2d(kernel_size = 2,stride = 2)
    )

    self$global_avg_pool = torch::nn_adaptive_avg_pool2d(output_size = c(1, 1))
    self$flat <- torch::nn_flatten()
    self$fc1 = torch::nn_linear(in_features = ini_out,out_features = 1024)
    self$relu = torch::nn_relu()
    self$dropout = torch::nn_dropout(0.5)
    self$fc2 = torch::nn_linear(in_features = 1024, num_classes)
    self$softmax = torch::nn_softmax(dim=1)

  },

  forward = function(x) {
    x |>
      self$layer1() |>
      self$layer2() |>
      self$layer3() |>
      self$layer4() |>
      self$layer5() |>
      self$layer6() |>
      self$global_avg_pool() |>
      self$flat() |>
      self$fc1() |>
      self$dropout() |>
      self$fc2() |>
      self$softmax()
  }
)

model <- conv_net()


fitted <- conv_net %>%
  luz::setup(
    loss = torch::nn_cross_entropy_loss(),
    optimizer = torch::optim_adam,
    metrics = luz::luz_metric_accuracy()
  ) %>%
  luz::fit(dl_train, epochs = 10, valid_data = dl_valid)

