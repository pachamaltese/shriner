## app.R ##
library(shriner)
library(highcharter)

ui <- dashboardPage(
  dashboardHeader(
    title = "Basic dashboard",
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Sales Dept",
                   message = "Sales are steady this month."
                 ),
                 messageItem(
                   from = "New User",
                   message = "How do I register?",
                   icon = icon("question"),
                   time = "13:45"
                 ),
                 messageItem(
                   from = "Support",
                   message = "The new server is ready.",
                   icon = icon("life-ring"),
                   time = "2014-12-01"
                 )
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 90, color = "green",
                          "Documentation"
                 ),
                 taskItem(value = 17, color = "aqua",
                          "Project X"
                 ),
                 taskItem(value = 75, color = "yellow",
                          "Server deployment"
                 ),
                 taskItem(value = 80, color = "red",
                          "Overall project"
                 )
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Base plot", tabName = "baseplot", icon = icon("dashboard")),
      menuItem("Highcharter plot", tabName = "highcharterplot", icon = icon("th"))
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "baseplot",
              fluidRow(
                box(h1("This is a title"), width = 12),

                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),

      # Second tab content
      tabItem(tabName = "highcharterplot",
              fluidRow(
                box(h1("This is another title"), width = 12),

                box(
                  selectInput("type", label = "Type", width = "100%",
                              choices = c("line", "column", "bar", "spline")),
                  selectInput("stacked", label = "Stacked",  width = "100%",
                              choices = c(FALSE, "normal", "percent")),
                  selectInput("theme", label = "Theme",  width = "100%",
                              choices = c(FALSE, "fivethirtyeight", "economist",
                                          "darkunica", "gridlight", "sandsignika",
                                          "null", "handdrwran", "chalk")
                  )
                ),

                box(
                  title = "Highcharter output",
                  highchartOutput("plot2",height = "500px")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  output$plot2 <- renderHighchart({

    hc <- highcharts_demo() %>%
      hc_rm_series("Berlin") %>%
      hc_chart(type = input$type)

    if (input$stacked != FALSE) {
      hc <- hc %>%
        hc_plotOptions(series = list(stacking = input$stacked))
    }

    if (input$theme != FALSE) {
      theme <- switch(input$theme,
                      null = hc_theme_null(),
                      darkunica = hc_theme_darkunica(),
                      gridlight = hc_theme_gridlight(),
                      sandsignika = hc_theme_sandsignika(),
                      fivethirtyeight = hc_theme_538(),
                      economist = hc_theme_economist(),
                      chalk = hc_theme_chalk(),
                      handdrwran = hc_theme_handdrawn()
      )

      hc <- hc %>% hc_add_theme(theme)

    }

    hc

  })
}

shinyApp(ui, server)
