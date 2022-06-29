library(tidyverse)
library(tidymodels)
source(file = 'data-raw/00_tidymodels.R')

resposta <- 'dist_sede'
# Modelo ------------------------------------------------------------------
base_modelo <- readr::read_rds('data-raw/rds/feature_enginering_to_tidymodels/features_glm.rds') %>%
  dplyr::ungroup() %>%
  dplyr::select(-file) %>%
  dplyr::mutate(resp = factor(resposta)) %>%
  dplyr::select(-resposta)



dt_split <- split_data(base_modelo)
train_data <- dt_split$train_data
test_data <- dt_split$test_data

# Cross valid
rsample <- cross_validation(dados = train_data,v = 5)

# recip
recip <- reciping(train_data,resposta = 'resp')

# models
modelos = list(
  xgb_model = parsnip::boost_tree(
    mtry = tune::tune(), min_n = tune::tune(),
    tree_depth = tune::tune(), trees = 1500,
    sample_size = 0.75, learn_rate = tune::tune(),
    loss_reduction = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost"),

  dt_model = parsnip::decision_tree(tree_depth = tune::tune(),cost_complexity = tune::tune(), min_n = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart"),

  rf_model = parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 1000) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger"),

  knn_model = parsnip::nearest_neighbor(neighbors = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("kknn"),

  nb_model = parsnip::naive_Bayes(smoothness = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("klaR"),

  lr_model = parsnip::logistic_reg(penalty = tune::tune(),mixture = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("glmnet")

)

# ajuste
tictoc::tic('Tempo total')
ft <- purrr::map2(
  modelos,
  names(modelos),
  ~ajusta_modelo(recip = recip,modelo = .x,nomes = .y,rsample_cv = rsample)
)
tictoc::toc()

compar <- compara_modelo(dados_teste = test_data,nomes = names(modelos),modelos = ft) %>%
  tibble::rowid_to_column() %>%
  dplyr::arrange(dplyr::desc(acc),dplyr::desc(auc))

compar

acur <- compar %>%
  dplyr::slice(1)

modelo_final <- ft[[acur$rowid]]


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# ---------------------------KERAS-----------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Keras -------------------------------------------------------------------

# AQUI EU RODO UMA ANN COM OS DADOS QUE DERAM ORIGEM AOS MODELOS ACIMA (TIDYMODELS)

library(keras)

file <- readr::read_rds('data-raw/rds/feature_enginering_to_tidymodels/features_glm.rds') %>%
  dplyr::ungroup() %>%
  dplyr::select(-file)

x_train <- file %>%
  dplyr::select(-resposta) %>%
  dplyr::mutate(hora = as.numeric(factor(hora)),
                mes_dia = as.numeric(factor(mes_dia)),
                periodo_dia = as.numeric(factor(periodo_dia)),
                estacao = as.numeric(factor(estacao)),
                local = as.numeric(factor(local))) %>%
  as.matrix()

y_train <- file %>%
  dplyr::select(resp = resposta) %>%
  dplyr::mutate(resp = factor(resp,levels = sort(unique(.$resp))),
                resp = as.numeric(resp),
                resp = ifelse(resp == 1,0,1)) %>%
  as.matrix()

y_train <- keras::to_categorical(y_train,2)

input_x <- keras::layer_input(shape = ncol(x_train))

output <- input_x %>%
  keras::layer_dense(
    units = 32768,
    activation = 'softmax'
  ) %>%
  keras::layer_dropout(0.3) %>%
  # keras::layer_dense(
  #   units = 1048,
  #   activation = 'softplus'
  # ) %>%
  # keras::layer_dropout(0.2) %>%
  # keras::layer_dense(
  #   units = 512,
  #   activation = 'softplus'
  # ) %>%
  # keras::layer_dropout(0.2) %>%
  # keras::layer_dense(
  #   units = 2,
  #   activation = 'sigmoid'
  # ) %>%
  # keras::layer_dropout(0.2) %>%
  keras::layer_dense(
    units = 2,
    activation = 'sigmoid'
  )

model <- keras::keras_model(
  inputs = input_x,
  outputs = output
)

model %>%
  keras::compile(
    loss = 'categorical_crossentropy',
    optimizer = keras::optimizer_adam(learning_rate = 1e-3,decay = 1e-7),
    metrics = list(keras::metric_binary_accuracy(),
                   keras::metric_precision(),
                   keras::metric_recall(),
                   keras::metric_auc()
                   )
  )

summary(model)

history <- model %>%
  keras::fit(
    x_train,
    y_train,
    epochs = 2000,
    batch_size = 200,
    validation_split = 0.3,
    view_metrics = FALSE
  )


