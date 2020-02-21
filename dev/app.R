# global ----

library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shriner)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(digest)
library(bit)

rawToInt <- function(bytes) {
  Reduce(function(left, right) {
    bitwShiftL(left, 8) + right
  }, as.integer(bytes), 0L)
}

# Quick and dirty bloom filter. The hashing "functions" are based on choosing
# random sets of bytes out of a single MD5 hash. Seems to work well for normal
# values, but has not been extensively tested for weird situations like very
# small n or very large p.
BloomFilter <- setRefClass("BloomFilter",
                           fields = list(
                             .m = "integer",
                             .bits = "ANY",
                             .k = "integer",
                             .bytesNeeded = "integer",
                             .bytesToTake = "matrix"
                           ),
                           methods = list(
                             # @param n - Set size
                             # @param p - Desired false positive probability (e.g. 0.01 for 1%)
                             initialize = function(n = 10000, p = 0.001) {
                               m = (as.numeric(n) * log(1 / p)) / (log(2)^2)

                               .m <<- as.integer(m)
                               .bits <<- bit(.m)
                               .k <<- max(1L, as.integer(round((as.numeric(.m)/n) * log(2))))

                               # This is how many *bytes* of data we need for *each* of the k indices we need to
                               # generate
                               .bytesNeeded <<- as.integer(ceiling(log2(.m) / 8))
                               .bytesToTake <<- sapply(rep_len(.bytesNeeded, .k), function(byteCount) {
                                 # 16 is number of bytes an md5 hash has
                                 sample.int(16, byteCount, replace = FALSE)
                               })
                             },
                             .hash = function(x) {
                               hash <- digest(x, "md5", serialize = FALSE, raw = TRUE)
                               sapply(1:.k, function(i) {
                                 val <- rawToInt(hash[.bytesToTake[,i]])
                                 # Scale down to fit into the desired range
                                 as.integer(val * (as.numeric(.m) / 2^(.bytesNeeded*8)))
                               })
                             },
                             has = function(x) {
                               all(.bits[.hash(x)])
                             },
                             set = function(x) {
                               .bits[.hash(x)] <<- TRUE
                             }
                           )
)

# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
                        size = numeric(), r_version = character(), r_arch = character(),
                        r_os = character(), package = character(), version = character(),
                        country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session) {
  # Connect to data source
  sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
  # Clean up when session is over
  session$onSessionEnded(function() {
    close(sock)
  })

  # Returns new lines
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(sock)
  })

  # Parses newLines() into data frame
  reactive({
    if (length(newLines()) == 0)
      return()
    read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
             col.names = names(prototype)
    ) %>% mutate(received = as.numeric(Sys.time()))
  })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
  shinySignals::reducePast(pkgStream, function(memo, value) {
    rbind(memo, value) %>%
      filter(received > as.numeric(Sys.time()) - timeWindow)
  }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
  shinySignals::reducePast(pkgStream, function(memo, df) {
    if (is.null(df))
      return(memo)
    memo + nrow(df)
  }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
  # These parameters estimate that with 5000 unique users added to
  # the filter, we'll have a 1% chance of false positive on the next
  # user to be queried.
  bloomFilter <- BloomFilter$new(5000, 0.01)
  total <- 0
  reactive({
    df <- pkgStream()
    if (!is.null(df) && nrow(df) > 0) {
      # ip_id is only unique on a per-day basis. To make them unique
      # across days, include the date. And call unique() to make sure
      # we don't double-count dupes in the current data frame.
      ids <- paste(df$date, df$ip_id) %>% unique()
      # Get indices of IDs we haven't seen before
      newIds <- !sapply(ids, bloomFilter$has)
      # Add the count of new IDs
      total <<- total + length(newIds)
      # Add the new IDs so we know for next time
      sapply(ids[newIds], bloomFilter$set)
    }
    total
  })
}

# ui ----

ui <- dashboardPage(
  dashboardHeader(title = "cran.rstudio.com"),
  dashboardSidebar(
    sliderInput("rateThreshold", "Warn when rate exceeds",
                min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Popularity by package (last 5 min)",
                  bubblesOutput("packagePlot", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "info",
                  title = "Top packages (last 5 min)",
                  tableOutput("packageTable")
                )
              )
      ),
      tabItem("rawdata",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("rawtable"),
              downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

# server ----

server <- function(input, output, session) {

  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)

  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5

  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)

  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  dlCount <- downloadCount(pkgStream)

  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)

  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())

  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)

    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = "Downloads per sec (last 5 min)",
      icon = icon("area-chart"),
      color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })

  output$count <- renderValueBox({
    valueBox(
      value = dlCount(),
      subtitle = "Total downloads",
      icon = icon("download")
    )
  })

  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique users",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()

    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)

    bubbles(df$n, df$package, key = df$package)
  })

  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Package name" = package, "% of downloads" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)

  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows), row.names = FALSE)
    options(orig)
  })
}

shinyApp(ui, server)
