source('data-raw/00_utils_func.R')
# Prep --------------------------------------------------------------------

# Sampling
set.seed(42)
files <- readr::read_rds('../../dados_brutos/rds/20230211_espectogramas/spectrograms.rds') %>%
  rsample::initial_split(0.8667)

files_train_valid <- rsample::training(files) %>%
  rsample::initial_split(0.9232)

files_train <- rsample::training(files_train_valid)
files_valid <- rsample::testing(files_train_valid)
files_test <- rsample::testing(files)


# TREINO

future::plan('multisession')
x_train <- cria_array(files_train$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_train,'../../dados_brutos/rds/20230211_arrays_cnn/x_train.rds')

# VALIDAÇÃO

future::plan('multisession')
x_valid <- cria_array(files_valid$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_valid,'../../dados_brutos/rds/20230211_arrays_cnn/x_valid.rds')

# TESTE

future::plan('multisession')
x_test <- cria_array(files_test$file_spec) %>%
  abind::abind(along = 0)
readr::write_rds(x_test,'../../dados_brutos/rds/20230211_arrays_cnn/x_test.rds')


# Cria cnn_audio ----------------------------------------------------------

x_train <- readr::read_rds('../../dados_brutos/rds/20230211_arrays_cnn/x_train.rds')
x_valid <- readr::read_rds('../../dados_brutos/rds/20230211_arrays_cnn/x_valid.rds')
x_test <- readr::read_rds('../../dados_brutos/rds/20230211_arrays_cnn/x_test.rds')

y_train <- as.numeric(factor(files_train$local))-1
y_train <- keras::to_categorical(y = y_train,num_classes = 3)
y_valid <- as.numeric(factor(files_valid$local))-1
y_valid <- keras::to_categorical(y = y_train,num_classes = 3)
y_test <- as.numeric(factor(files_test$local))-1
y_test <- keras::to_categorical(y = y_test,num_classes = 3)

# SALVA
cnn_audio <- list(
  train = list(
    x = x_train,
    y = y_train
  ),
  valid = list(
    x = x_valid,
    y = y_valid
  ),
  test = list(
    x = x_test,
    y = y_test
  )
)


readr::write_rds(cnn_audio,'../../dados_brutos/rds/20230211_arrays_cnn/cnn_audio.rds')
