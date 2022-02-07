#' @export
loadLabBookConfig <- function(folder, verbose=TRUE){
  tmp <- read.table( file=file.path(folder, ".LabBookR.config"), stringsAsFactor=FALSE)
  LabBookR.config.folder <<- tmp$folder
  LabBookR.config.author <<- tmp$author
  LabBookR.config.dueDate <<- tmp$dueDate
  LabBookR.config.scheduledDate <<- tmp$scheduledDate
  LabBookR.config.reqTime <<- tmp$reqTime

  if(verbose){
    cat("LabBook configuration loaded:\n")
    cat("-----------------------------------\n")
    cat("folder            :", LabBookR.config.folder,"\n")
    cat("author            :", LabBookR.config.author,"\n")
    cat("dueDate           :", LabBookR.config.dueDate, "\n")
    cat("scheduledDate     :", LabBookR.config.scheduledDate, "\n")
    cat("reqTime           :", LabBookR.config.reqTime, "\n")
  }

}
