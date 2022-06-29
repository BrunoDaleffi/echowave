#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # List the first level callModules here
  shiny::callModule(module = mod_dash_server,'dash_ui')

}
