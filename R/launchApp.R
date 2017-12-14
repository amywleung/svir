#' launchApp
#'
#' Launches Shiny web application using the server.R and ui.R files located in
#' the /application subdirectory.
#'
#' @return Shiny web application
#'
#' @example
#' launchApp()
#'
#' @export

launchApp <- function() {
  runApp(appDir = system.file("application", package = "svir"), display.mode = "normal")
}
