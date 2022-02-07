#' Check lab book
#'
#' This function checks, if the lab book is properly configured and prepared
#' @return A matrix shoing the test results
#' @export

checkLabBook <- function(folder){
  if( exists("LabBookR.config.folder")){
    folder <- LabBookR.config.folder
  } else {
    stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  cat("Checking for integrity and issues within the LabBook\n")
  cat("----------------------------------------------------\n")
  cat("Folder     : ")
  if(file.exists(folder)){
    cat(paste0("\033[0;32m","found!","\033[0m","\n"))
  } else {
    cat(paste0("\033[0;31m","not found!","\033[0m","\n"))
    stop("     ",folder, " does not exist!", call.=FALSE)
  }

  projects <- getMyProjects(folder)
  cat("ToDo lists : ")
  if(sum(projects$ToDo=="YES")==nrow(projects)){
    cat(paste0("\033[0;32m","found!","\033[0m","\n"))
  } else {
    cat(paste0("\033[0;31m","not found!","\033[0m","\n"))
    for(i in 1:nrow(projects)){
      if(projects$ToDo[i]=="NO"){
        cat("        Check project: ", projects$title[i],"\n")
      }
    }
  }
}
