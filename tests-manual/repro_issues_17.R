## Tries to reproduce Github Issue: https://github.com/rstudio/shriner/issues/17

library(shiny)
library(shriner)

body <- dashboardBody(
  uiOutput("ui")
)

server <- function(input, output) {
  output$ui <- renderUI({
    box(title = "Collapse me",
        status = "warning", solidHeader = TRUE, collapsible = TRUE
    )
  })
}

shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    body
  ),
  server = server
)
