# Carregando as bibliotecas necessárias
library(tidyverse)
library(tidymodels)
source(file = 'data-raw/00_tidymodels.R')  # Carregando um arquivo externo que provavelmente contém funções adicionais relacionadas ao tidymodels.

# Configurando os handlers para progressr para mostrar barra de progresso
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

# Função para preparar os dados para o modelo
base_modelo_ <- function(file_rds){

  # Definindo o local baseado no nome do arquivo
  local = dplyr::case_when(
    stringr::str_detect(file_rds, stringr::regex('assis', ignore_case = TRUE)) ~ 'Assis',
    stringr::str_detect(file_rds, stringr::regex('barb', ignore_case = TRUE))  ~ 'Sta Barb',
    stringr::str_detect(file_rds, stringr::regex('iti', ignore_case = TRUE)) ~ 'Itirapina'
  )

  # Lendo o arquivo RDS e calculando a média dos coeficientes MFCC
  rds <- readr::read_rds(file_rds)
  mfcc <- rds$mfcc %>% tibble::as_tibble() %>% dplyr::summarise_all(mean)

  # Retornando a combinação do local com os coeficientes MFCC
  tibble::tibble(local = local) %>%
    dplyr::bind_cols(mfcc)
}

# Obtendo lista de arquivos e definindo plano para processamento paralelo
files <- fs::dir_ls('../../dados_brutos/rds/20221108_df_audio/')
future::plan('multisession')

# Função para criar base de dados do modelo usando processamento paralelo
cria_base_modelo <- function(files){
  p <- progressr::progressor(along = files)
  furrr::future_map_dfr(
    files,
    function(x) {
      p()
      base_modelo_(x)
    }
  )
}

# Criando e salvando a base do modelo
base_modelo <- cria_base_modelo(files)
readr::write_rds(base_modelo, '../../dados_brutos/rds/20221108_base_modelo/base_modelo_tidymodels.rds')

# Preparando os dados para treinamento e teste
base_modelo <- readr::read_rds('../../dados_brutos/rds/20221108_base_modelo/base_modelo_tidymodels.rds') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(resp = factor(local)) %>%
  dplyr::select(-local)

dt_split <- split_data(base_modelo)
train_data <- dt_split$train_data
test_data <- dt_split$test_data

# Definindo a validação cruzada
rsample <- cross_validation(dados = train_data, v = 5)

# Definindo o reciping
recip <- reciping(train_data, resposta = 'resp')

# Definindo modelos para ajustar (XGBoost, Random Forest e Regressão Logística)
modelos = list(
  xgb_model = parsnip::boost_tree(
    mtry = tune::tune(), min_n = tune::tune(),
    tree_depth = tune::tune(), trees = 1500,
    sample_size = 0.75, learn_rate = tune::tune(),
    loss_reduction = tune::tune()
  ) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("xgboost"),

  rf_model = parsnip::rand_forest(mtry = tune::tune(), min_n = tune::tune(), trees = 1000) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("ranger"),

  lr_model = parsnip::multinom_reg(penalty = tune::tune(), mixture = tune::tune()) %>%
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("glmnet")
)

# Ajustando os modelos
tictoc::tic('Tempo total')
ft <- purrr::map2(
  modelos,
  names(modelos),
  ~ajusta_modelo(recip = recip, modelo = .x, nomes = .y, rsample_cv = rsample)
)
tictoc::toc()

# Comparando modelos e selecionando o melhor
compar <- compara_modelo(dados_teste = test_data, nomes = names(modelos), modelos = ft) %>%
  tibble::rowid_to_column() %>%
  dplyr::arrange(dplyr::desc(acc))

compar %>% knitr::kable()

acur <- compar %>%
  dplyr::slice(1)

modelo_final <- ft[[acur$rowid]]

t <- table(
  pred = predict(modelo_final, test_data)$.pred_class, obs = test_data$resp
)
round(sum(diag(t)) / sum(t), 2)


# KERAS - MLP -------------------------------------------------------------

# Segmento dedicado ao Keras
# Keras: uma biblioteca para definir e treinar redes neurais profundas.

library(keras)

# Preparando os dados para a rede neural
base_modelo <- readr::read_rds('../../dados_brutos/rds/20221108_base_modelo/base_modelo_tidymodels.rds') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(resp = factor(local)) %>%
  dplyr::select(-local)

x_train <- base_modelo  %>%
  dplyr::select(-resp) %>%
  as.matrix()

y_train <- base_modelo %>%
  dplyr::select(resp) %>%
  dplyr::mutate(resp = factor(resp, levels = sort(unique(.$resp))),
                resp = as.numeric(resp) - 1) %>%
  as.matrix()

y_train <- keras::to_categorical(y_train, 3)

# Definindo a arquitetura da rede neural
input_x <- keras::layer_input(shape = ncol(x_train))
output <- input_x %>%
  # Definindo as camadas da rede neural
  keras::layer_dense(units = 30, activation = 'relu') %>%
  keras::layer_dropout(0.5) %>%
  keras::layer_layer_normalization() %>%
  keras::layer_dense(units = 18, activation = 'relu') %>%
  keras::layer_dropout(0.5) %>%
  keras::layer_layer_normalization() %>%
  keras::layer_dense(units = 3, activation = 'softmax')

model <- keras::keras_model(inputs = input_x, outputs = output)

# ## Model: "MLP"
#
# | Layer (type)                         | Output Shape  | Param #    |
# |--------------------------------------|---------------|------------|
#   | input_1 (InputLayer)                 | (None, 12)    | 0          |
#   | dense_2 (Dense)                      | (None, 30)    | 390        |
#   | dropout_1 (Dropout)                  | (None, 30)    | 0          |
#   | layer_normalization_1 (LayerNormalization) | (None, 30)    | 60         |
#   | dense_1 (Dense)                      | (None, 18)    | 558        |
#   | dropout (Dropout)                    | (None, 18)    | 0          |
#   | layer_normalization (LayerNormalization)   | (None, 18)    | 36         |
#   | dense (Dense)                        | (None, 3)     | 57         |
#
#   **Total params**: 1101 (4.30 KB)
# **Trainable params**: 1101 (4.30 KB)
# **Non-trainable params**: 0 (0.00 Byte)

# Compilando o modelo
model %>%
  keras::compile(
    loss = 'categorical_crossentropy',
    optimizer = keras::optimizer_adam(),
    metrics = list(keras::metric_binary_accuracy(),
                   keras::metric_precision(),
                   keras::metric_recall()
    )
  )

summary(model)

# Treinando a rede neural
tictoc::tic()
history <- model %>%
  keras::fit(
    x_train,
    y_train,
    epochs = 2000,
    batch_size = 1000,
    validation_split = 0.3,
    view_metrics = FALSE
  )
tictoc::toc()

# Avaliando a performance da rede neural nos dados de teste
t <- test_data %>%
  tibble::rowid_to_column() %>%
  dplyr::bind_cols(
    tibble::as_tibble(predict(object = model, x = as.matrix(dplyr::select(test_data, -resp)))) %>%
      purrr::set_names(c('Assis', 'Itirapina', 'Sta Barb'))
  ) %>%
  tidyr::pivot_longer(c('Assis', 'Sta Barb', 'Itirapina')) %>%
  dplyr::arrange(rowid, dplyr::desc(value)) %>%
  dplyr::distinct(rowid, .keep_all = TRUE) %>%
  dplyr::select(pred = name, obs = resp)

tb = table(pred = t$pred, obs = t$obs)

round(sum(diag(tb)) / sum(tb), 2)
