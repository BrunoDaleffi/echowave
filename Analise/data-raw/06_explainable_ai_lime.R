library(tensorflow)
library(keras)
library(lime)
library(magrittr)
source('data-raw/00_utils_func.R')

img_preprocess <- function(file){
  cria_array(file) %>%
    abind::abind(along = 0)
}
# Prep --------------------------------------------------------------------

# Sampling
files <- readr::read_rds('~/mestrado/20230211_espectogramas/spectrograms.rds') %>%
  dplyr::mutate(
    file_spec = stringr::str_replace(file_spec,'../../dados_brutos/rds','~/mestrado')
  )

files_assis <- files %>%
  dplyr::filter(local == 'Assis') %>%
  dplyr::sample_n(5)

files_st_brb <- files %>%
  dplyr::filter(local == 'Sta Barb') %>%
  dplyr::sample_n(5)

files_iti <- files %>%
  dplyr::filter(local == 'Itirapina') %>%
  dplyr::sample_n(5)


# LIME
model <- keras::load_model_hdf5('~/mestrado/modelo_local.h5')

classf <- lime::as_classifier(x = model,labels =  c('Assis','Itirapina',"Sta Barb"))

explainer <- lime::lime(
  x = files$file_spec,
  model = classf,
  img_preprocess
)

lime::plot_superpixels(path = files_assis$file_spec[5],n_superpixels = 50)



# Assis -------------------------------------------------------------------


explanation_assis <- lime::explain(
  x = files_assis$file_spec[1:5],
  explainer = explainer,
  n_labels = 1,
  n_features = 12,
  n_superpixels = 50
)


expl_assis <- explanation_assis %>%
  dplyr::select(-feature_value,-data,-prediction) %>%
  dplyr::mutate(
    prediction = explanation_assis$prediction,
    feature_value = purrr::map(explanation_assis$feature_value,~.),
    data = purrr::map(explanation_assis$data,~.)
  )


expl <- expl_assis %>%
  dplyr::group_split(case)

plot_image_explanation(expl[[5]])


# Sta Barb ----------------------------------------------------------------

explanation_st_brb <- lime::explain(
  x = files_st_brb$file_spec[1:5],
  explainer = explainer,
  n_labels = 1,
  n_features = 12,
  n_superpixels = 50
)

expl_st_brb <- explanation_st_brb %>%
  dplyr::select(-feature_value,-data,-prediction) %>%
  dplyr::mutate(
    prediction = explanation_st_brb$prediction,
    feature_value = purrr::map(explanation_st_brb$feature_value,~.),
    data = purrr::map(explanation_st_brb$data,~.)
  )

expl <- expl_st_brb %>%
  dplyr::group_split(case)

plot_image_explanation(expl[[1]])



# Itirapina ---------------------------------------------------------------

explanation_iti <- lime::explain(
  x = files_iti$file_spec[1:5],
  explainer = explainer,
  n_labels = 1,
  n_features = 12,
  n_superpixels = 50
)


expl_iti <- explanation_iti %>%
  dplyr::select(-feature_value,-data,-prediction) %>%
  dplyr::mutate(
    prediction = explanation_iti$prediction,
    feature_value = purrr::map(explanation_iti$feature_value,~.),
    data = purrr::map(explanation_iti$data,~.)
  )


expl <- expl_iti %>%
  dplyr::group_split(case)

plot_image_explanation(expl[[1]])
