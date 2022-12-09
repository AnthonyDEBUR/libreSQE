#' maj_referentiels UI Function
#'
#' @description Module shiny pour forcer la mise à jour d'un référentiel SANDRE
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maj_referentiels_ui <- function(id){
  ns <- NS(id)
  tagList(
      actionButton(inputId=ns(""))


  )
}

#' maj_referentiels Server Functions
#'
#' @noRd
mod_maj_referentiels_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_maj_referentiels_ui("maj_referentiels_1")

## To be copied in the server
# mod_maj_referentiels_server("maj_referentiels_1")
