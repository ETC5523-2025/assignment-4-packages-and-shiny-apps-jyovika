#' Launch the haiInsight Shiny app
#'
#' Opens the interactive explorer included with the package.
#' @export
run_hai_app <- function() {
  app_dir <- system.file("app", package = "haiInsight")
  shiny::runApp(app_dir, launch.browser = TRUE)
}

