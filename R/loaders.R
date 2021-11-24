#' @export
loadLabBookConfig <- function(folder){
  tmp <- read.table( file=file.path(folder, ".LabBookR.config"))
  LabBookR.config.folder <<- tmp$folder
  LabBookR.config.author <<- tmp$author
}
