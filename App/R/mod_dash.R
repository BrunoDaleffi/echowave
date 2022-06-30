#' dash UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for.
#'
#' @noRd
mod_dash_ui <- function(id){
  ns <- shiny::NS(id)
  options(shiny.maxRequestSize=30*1024^2)

  shiny::tagList(
    shinydashboard::box(title = 'Upload',width = 12,
                        shiny::fileInput(
                          inputId = ns('file_audio'),
                          label = 'Arquivo de áudio'
                        ),
                        shiny::fluidRow(
                          shiny::column(
                            width = 3,
                            shinyWidgets::radioGroupButtons(
                              inputId = ns('modelo'),
                              label = 'Resposta',
                              choices = c('Localidade','Distância da sede'),
                              selected = 'Localidade',justified = TRUE,status = 'primary'
                              )
                          )
                        )
    ),

    shinydashboard::box(
      title = 'Modelagem',width = 12,
      shiny::uiOutput(outputId = ns('prob_mod'))
    ),

    shinydashboard::box(
      title = 'Spectrograma',width = 12,
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::sliderInput(
            inputId = ns('corte'),
            label = 'Corte do audio (seg)',
            min = 0,
            max = 3600,
            value = c(0,3600)
          )
        )
      ),
      shiny::plotOutput(outputId = ns('spectrogram'),height = '400px') %>% shinycssloaders::withSpinner(type = 4)
    )

  )


}

#' dash Server Functions
#'
#' @noRd
mod_dash_server <- function(input, output, session){
  ns <- session$ns

  shiny::observe({
    reticulate::virtualenv_create(envname = 'r-tensorflow', python = 'python3')
    reticulate::virtualenv_install('r-tensorflow', c('numpy', 'tensorflow', 'keras'), ignore_installed = FALSE)
    reticulate::use_virtualenv(virtualenv = 'r-tensorflow', required = TRUE)
  })


  sound <- shiny::eventReactive(input$file_audio,{
    sd <- input$file_audio$datapath %>%
      tuneR::readWave()

    return(sd)

  })


  shiny::observe({
    time <- seewave::duration(sound())
    shiny::updateSliderInput(
      session = session,
      inputId = 'corte',
      max = time,
      value = c(0,min(60,round(time/3)))
    )
  })

  sound_to_use <- shiny::eventReactive(input$corte,{
    sound() %>%
      seewave::cutw(
        from = input$corte[1],
        to = input$corte[2],
        output = 'Wave'
      )

  })

  sound_features <- shiny::reactive({

    time = seewave::duration(sound())
    n_split = 180
    sound_split = split(0:time,ceiling(seq_along(0:time) / n_split)) %>%
      purrr::discard(function(x)length(x) <= n_split*0.05)

    #Montar as imagens do spectro

    x_spec <- purrr::map(
      sound_split,
      function(x){
        temp <- fs::path_temp()

        (seewave::cutw(
          wave = sound(),
          from = x[1],
          to = x[length(x)],
          output = 'Wave'
        ) %>%
            sound_spec() +
            ggplot2::theme(legend.position = 'none',
                           axis.text.x = ggplot2::element_blank(),
                           axis.text.y = ggplot2::element_blank(),
                           panel.grid =  ggplot2::element_blank()
            ))  %>%
          ggplot2::ggsave(
            filename = 'spectro.jpeg',
            path = temp,
            device = 'jpeg',
            units = 'px',
            width = 1000,height = 1000
          )

        x_spec <- cimg2tibble(paste0(temp,'/spectro.jpeg'),scale = 0.2) %>%
          imgtibble2array

        return(x_spec)

      }
    ) %>%
      abind::abind(along = 0)

    return(x_spec)
  }
  )

  output$spectrogram <- shiny::renderPlot({
    sound_to_use()  %>%
      sound_spec()

  })



  output$prob_mod <- shiny::renderUI({

    if(input$modelo == 'Localidade'){
      md <- keras::load_model_hdf5(system.file('extdata/modelo_local.h5',package = 'echowave'))
      prob <- predict(object = md,x = sound_features())
      cls <- ifelse(prob >= 0.5,1,0)
      resp <- ifelse(mean(cls) >= 0.5,'Assis','Sta Barb')
    }
    else{
      md <- keras::load_model_hdf5(system.file('extdata/modelo_dist_sede.h5',package = 'echowave'))
      prob <- predict(object = md,x = sound_features())
      cls <- ifelse(prob >= 0.5,1,0)
      resp <- ifelse(mean(cls) >= 0.5,'Longe da Sede','Perto da Sede')
    }

    shiny::tags$h2(
      shiny::div(
        shiny::tags$b(
          resp
        ),
        align = 'center'
      )
    )

  })


}
