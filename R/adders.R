#' Add ToDo item
#'
#' This functions adds a ToDo item to a project
#' @param folder Path to LabBookR folder
#' @param project The project name
#' @return a list of projects
#' @export
addToDo <-  function(folder=NA, project=NA, entryDate=NA, dueDate=NA, scheduledDate=NA, reqTime=NA, description=NA){

  if( exists("LabBookR.config.folder")){
    folder <- LabBookR.config.folder
  } else {
    if(is.na(folder)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  all_projects <- getMyProjects(folder)
  project <- match.arg(project, all_projects$title)

  project_file <- paste0(project,".Rmd")

  if(!file.exists(file.path(folder, project_file))) stop("Folder/project not found, please check the correct names. E.g. run getMyProjects() to receive a full list of projects.")

  project_content <- readLines(file.path(folder, project_file))
  todo.start <- grep("# ToDo", project_content)
  todo.end <- grep("# Progress Notes", project_content) - 1

  # Create the new entry
  if(is.na(entryDate)){
    entryDate <- format(Sys.time(), "%Y-%m-%d")
  }

  if(is.na(dueDate)){
    if( exists("LabBookR.config.dueDate")){
      dueDatePlus <- LabBookR.config.dueDate
      dueDate <- as.character(as.Date(entryDate) + dueDatePlus)
    } else {
      dueDate <- " "
    }
  }

  if(is.na(scheduledDate)){
    if( exists("LabBookR.config.scheduledDate")){
      scheduledDatePlus <- LabBookR.config.scheduledDate
      scheduledDate <- as.character(as.Date(entryDate) + scheduledDatePlus)
    } else {
      scheduledDate <- " "
    }
  }

  if(is.na(reqTime)){
    if(exists("LabBookR.config.reqTime")){
      reqTime <- LabBookR.config.reqTime
    } else {
      reqTime <- " "
    }
  }

  if(is.na(description)){
    stop("No task description provided, please describe your task.")
  }

  todo_entry <- paste0("@ ", paste(c(entryDate, dueDate, scheduledDate, reqTime,"FALSE", description), collapse=" @ "))

  project_content_new <- c(project_content[1:(todo.end-1)], todo_entry, project_content[(todo.end):length(project_content)])

  fileConn <- file(file.path(folder, project_file))
  writeLines(project_content_new, fileConn)
  close(fileConn)

}
