# Add an html dependency, without overwriting existing ones
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)

  old <- attr(x, "html_dependencies", TRUE)

  htmlDependencies(x) <- c(old, value)
  x
}

# Add dashboard dependencies to a tag object
addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    shriner_js <- "shriner.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    shriner_js <- "shriner.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }

  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
      c(file = system.file("AdminLTE", package = "shriner")),
      script = adminLTE_js,
      stylesheet = adminLTE_css
    ),
    htmlDependency("shriner",
      as.character(utils::packageVersion("shriner")),
      c(file = system.file(package = "shriner")),
      script = shriner_js,
      stylesheet = "shriner.min.css"
    )
  )

  appendDependencies(x, dashboardDeps)
}
