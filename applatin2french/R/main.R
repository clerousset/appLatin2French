#' @export
runApp <- function(){
  #myshinyapp::populate_table(bucket = "avouacr", object = "diffusion/shiny-template/quakes.csv")
  appDir <- system.file("app", package = "applatin2french")
  shiny::runApp(appDir, display.mode = "normal")
}
