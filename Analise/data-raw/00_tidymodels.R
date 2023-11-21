library(tidyverse)
library(tidymodels)
library(discrim)
library(DataExplorer)
library(purrr)
library(naniar)
library(kknn)
library(klaR)
library(arules)

dtct <- function(x,rx) stringr::str_detect(x,stringr::regex(rx,ignore_case = TRUE))

# ARRUMANDO DADOS ######################################################################

processadores = future::availableCores() - 3

# SEPARANDO DADOS #####################################################################
split_data <- function(dados,p = 0.7,seed = 42){
  set.seed(seed)

  data_split <- rsample::initial_split(dados, p)

  train_data <- rsample::training(data_split)
  test_data <- rsample::testing(data_split)

  return(list(train_data = train_data,test_data = test_data))

}

# Reciping ----------------------------------------------------------------

reciping <- function(dados,resposta){
  formula <- as.formula(paste0(resposta," ~ ."))
  dados %>%
    recipes::recipe(formula)  %>%
    recipes::step_dummy(recipes::all_nominal(), -recipes::all_outcomes())
}

# modelos finais ---------------------------------------------------------------------------------------

#para modelos de classificacao
meu_fit <- function(tune_grid, model, workflow){
  best_model <- tune::select_best(tune_grid, "accuracy")
  final_model <- tune::finalize_model(model, best_model)
  workflow <- workflow %>% workflows::update_model(final_model)
  fit <- parsnip::fit(workflow, data = train_data)
  fit
}

# #para modelos de regressao
# meu_fit_reg <- function(tune_grid, model, workflow){
#   best_model <- tune::select_best(tune_grid, "rmse")
#   final_model <- tune::finalize_model(model, best_model)
#   workflow <- workflow %>% workflows::update_model(final_model)
#   fit <- parsnip::fit(workflow, data = train_data)
#   fit
# }

# Tune --------------------------------------------------------------------

cross_validation <- function(dados,v= 5,seed = 42){
  set.seed(seed)
  rsample::vfold_cv(dados, v)

}

#para modelos de classificacao
meu_tune_grid <- function(workflow,rsamples_cv, grid = 5) {
  tune_grid(
    workflow,
    resamples = rsamples_cv,
    grid = grid,
    metrics = metric_set(accuracy, precision, recall, roc_auc),
    control = control_grid(verbose = TRUE, allow_par = TRUE)
  )
}

# #para modelos de regressao
# meu_tune_grid_reg <- function(workflow,rsamples_cv, grid = 5) {
#   tune_grid(
#     workflow,
#     resamples = rsamples_cv,
#     grid = grid,
#     control = control_grid(verbose = TRUE, allow_par = TRUE)
#   )
# }

ajusta_modelo <- function(recip,nomes = '',modelo,rsample_cv,tipo = 'class'){

  if(tipo == 'reg'){
    wf <- workflows::workflow() %>%
      workflows::add_recipe(recip) %>%
      workflows::add_model(modelo)

    # ligar processamento paralelo no linux
    tictoc::tic(nomes)
    tune_grid <- meu_tune_grid_reg(wf,rsample_cv)
    tictoc::toc()



    resp <- meu_fit_reg(tune_grid, modelo, wf)

  } else{
    wf <- workflows::workflow() %>%
      workflows::add_recipe(recip) %>%
      workflows::add_model(modelo)

    # ligar processamento paralelo no linux
    tictoc::tic(nomes)
    tune_grid <- meu_tune_grid(wf,rsample_cv)
    tictoc::toc()



    resp <- meu_fit(tune_grid, modelo, wf)

  }

  return(resp)

}

compara_modelo <- function(dados_teste,nomes = '',modelos, tipo = 'class'){

  if(tipo == 'class'){
    purrr::map2_dfr(modelos,
                    nomes,
                    ~predict(.x, dados_teste, type = "class") %>%
                      purrr::set_names('class') %>%
                      dplyr::mutate(modelo = .y %>% as.character() %>% .[1] %>% stringr::str_extract('^[a-z]*'),
                                    resposta = dados_teste$resp) %>%
                      dplyr::group_by(modelo) %>%
                      dplyr::summarise(acc = scales::percent(yardstick::accuracy_vec(resposta, class),accuracy = 1),
                                       prc = scales::percent(yardstick::precision_vec(resposta, class),accuracy = 1),
                                       rec = scales::percent(yardstick::recall_vec(resposta, class),accuracy = 1)) %>%
                      dplyr::arrange(dplyr::desc(acc),dplyr::desc(prc))
    )
  } else{
    purrr::map2_dfr(modelos,
                    nomes,
                    ~predict(.x, dados_teste,type = 'numeric') %>%
                      dplyr::mutate(modelo = .y %>% as.character() %>% .[1] %>% stringr::str_extract('^[a-z]*'),
                                    resposta = dados_teste$resp,
                                    pred = `.pred`) %>%
                      dplyr::group_by(modelo) %>%
                      dplyr::summarise(rmse = yardstick::rmse_vec(resposta,pred),
                                       mae = yardstick::mae_vec(resposta,pred),
                                       mpe = yardstick::mpe_vec(resposta,pred),
                                       msd = yardstick::msd_vec(resposta,pred),
                                       rpd = yardstick::rpd_vec(resposta,pred),
                                       rsq = yardstick::rsq_vec(resposta,pred))
    )
  }


}
