# Carregando as bibliotecas necessárias para manipulação, análise e visualização dos dados.
library(tidyverse)
library(tuneR)
library(seewave)
library(soundgen)
library(reshape2)
library(furrr)
library(progressr)

# Configurando os handlers globais para o progressr, que permite mostrar o progresso de tarefas demoradas.
progressr::handlers(global = TRUE)

# Definindo um handler para mostrar a barra de progresso com um formato específico.
progressr::handlers(
  list(
    progressr::handler_progress(
      format   = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta",
      width    = 115,
      complete = "+"
    )
  )
)

# Função para converter um arquivo de áudio em um DataFrame.
audio_to_df <- function(file, path = '.') {

  # Identifica o local baseado em partes específicas do nome do arquivo.
  local = dplyr::case_when(
    stringr::str_detect(file, stringr::regex('assis', ignore_case = TRUE)) & stringr::str_detect(file, 'longe')  ~ 'Assis - AS02',
    stringr::str_detect(file, stringr::regex('assis', ignore_case = TRUE)) & stringr::str_detect(file, 'perto') ~ 'Assis - AS01',
    stringr::str_detect(file, stringr::regex('barb', ignore_case = TRUE)) & stringr::str_detect(file, 'longe') ~ 'Sta Barb - SB02',
    stringr::str_detect(file, stringr::regex('barb', ignore_case = TRUE)) & stringr::str_detect(file, 'perto') ~ 'Sta Barb - SB01',
    stringr::str_detect(file, stringr::regex('iti', ignore_case = TRUE)) & stringr::str_detect(file, 'Iti05') ~ 'Itirapina - IT05',
    stringr::str_detect(file, stringr::regex('iti', ignore_case = TRUE)) & stringr::str_detect(file, 'Iti06') ~ 'Itirapina - IT06',
  )

  # Extrai data e hora do nome do arquivo.
  datetime <- file %>%
    stringr::str_extract('/[0-9]{3,}.*') %>%
    stringr::str_remove_all('[^0-9]') %>%
    lubridate::ymd_hms()

  # Lê o arquivo de áudio, restringindo a leitura aos primeiros 60 segundos.
  wav <- tuneR::readWave(filename = file, to = 60, units = 'seconds')

  # Se o local for "Assis" ou "Sta", faz downsampling para 8000 Hz.
  if(stringr::str_detect(local, 'Ass|Sta')) {
    wav <- tuneR::downsample(wav, 8000)
  }

  # Gera um espectrograma do áudio.
  spectro <- seewave::spectro(wave = wav, plot = FALSE)

  # Processa e normaliza a amplitude do áudio.
  amp <- reshape2::melt(spectro$amp, value.name = "Amplitude") %>%
    dplyr::select(FrequencyIndex = Var1, TimeIndex = Var2, Amplitude) %>%
    dplyr::mutate(amp_norm = (Amplitude - mean(.$Amplitude)) / sd(.$Amplitude))

  # Processa a frequência do áudio.
  freq <- reshape2::melt(spectro$freq, value.name = "Frequency") %>%
    dplyr::mutate(FrequencyIndex = row_number(), Frequency = Frequency * 1000)

  # Processa o tempo do áudio.
  time <- reshape2::melt(spectro$time, value.name = "Time") %>%
    dplyr::mutate(TimeIndex = row_number())

  # Calcula os coeficientes MFCC (Mel-frequency cepstral coefficients) do áudio.
  mfcc <- tuneR::melfcc(samples = wav)
  mfcc <- (mfcc - mean(mfcc)) / sd(mfcc)

  # Compila todas as informações processadas.
  base_completa <- amp %>%
    dplyr::left_join(freq, by = "FrequencyIndex") %>%
    dplyr::left_join(time, by = "TimeIndex") %>%
    dplyr::transmute(file = file, date = datetime + lubridate::seconds(Time), local = local, time = Time, freq = Frequency, amp = Amplitude, amp_norm)

  # Salva os dados processados em um arquivo RDS.
  list(mfcc = mfcc, base_completa = base_completa) %>%
    readr::write_rds(file = paste0(path, stringr::str_replace_all(datetime, '[^0-9 ]', ''), '_', local, '.rds'), compress = 'xz')
}

# Lista todos os arquivos de áudio na pasta especificada e filtra os arquivos que contêm a palavra "geral".
files <- tibble::tibble(files = fs::dir_ls('../../dados_brutos/audios/', recurse = TRUE, glob = '*.WAV')) %>%
  dplyr::filter(!stringr::str_detect(files, 'geral')) %>%
  dplyr::pull(files)

# Configurando o uso de múltiplas sessões para processamento paralelo.
future::plan('multisession')

# Função para processar e salvar todos os arquivos listados.
salva <- function(files) {
  # Configura a barra de progresso.
  p <- progressr::progressor(along = files)

  # Processa cada arquivo de áudio em paralelo.
  furrr::future_walk2(.x = files, .y = 1:length(files), function(x, y) {
    p(y)
    audio_to_df(x, path = '../../dados_brutos/rds/20221108_df_audio/')
  })
}

# Chama a função salva para processar e salvar todos os arquivos.
salva(files)
