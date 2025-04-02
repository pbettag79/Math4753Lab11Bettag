#' Run MyBoot Shiny App
#'
#' This function runs the MyBoot Shiny app included in the package.
#'
#' @export
run_myboot_app <- function() {
  # Locate the Shiny app file
  app_dir <- system.file("myboot", "app.R", package = "Math4753Lab11PatriceBettag", mustWork = TRUE)

  # Run the app
  shiny::runApp(app_dir, display.mode = "normal")
}

