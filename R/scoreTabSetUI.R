scoreTabSetUI <- function(id) {
  ns <- NS(id)

  tabPanel <- shiny::tabPanel
  uiOutput <- shiny::uiOutput

  shiny::tabsetPanel(
    id = ns("tabSet"),
    shinyjs::useShinyjs(),
    tabPanel("Score"   , value = "Score"   , uiOutput(ns("score"))),
    tabPanel("Tableaux", value = "Tableaux", uiOutput(ns("tabs" ))),
    tabPanel("Config." , value = "Config." ,
             ovalideTableDesigner::tableDesignerUI(ns("conf"), debug = T))
  )
}

