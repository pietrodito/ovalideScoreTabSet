library(shiny)
library(ovalideScoreTabSet)

if(interactive()) {

  ui <- fluidPage(
    scoreTabSetUI("sts")
  )

  server <- function(input, output, session) {
    tableDesignerServer("sts", ovalide::nature("mco", "dgf"))
  }

  shinyApp(ui, server)
}
