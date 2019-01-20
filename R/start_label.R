#' Start the label app
#'
#' @param path the path to the database
#' @export
#' @importFrom shiny shinyAppDir
start_label <- function(path = "~") {
  Sys.setenv("LABEL_PATH" = path)
  shinyAppDir(system.file("label", package = "soundcluster"))
}
