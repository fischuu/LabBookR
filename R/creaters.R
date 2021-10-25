#' Create Complete LabBook
#'
#' Create the lab book
#' @param labBook Path to LabBookR folder
#' @return A RMarkdown file
#' @export
createLabBook <- function(labBook=NULL){
  # Input checks
  if(is.null(labBook)) stop("Please provide a labBook address")

  projects <- list.files(labBook, pattern="*.Rmd")

  projectRMD <- list()
  availDates <- structure(list(), class="Date")

  # Import all projects
  for(i in 1:length(projects)){
    projectRMD[[i]] <- readLines(file.path(labBook, projects[i]))
  }

  # Filter out all non- Progress related lines
  for(i in 1:length(projects)){
    progressStart <- which(projectRMD[[i]]=='# Progress Notes')
    progressEnd <- length(projectRMD[[i]])
    if(progressStart>=progressEnd){
      projectRMD[[i]] <- "NA"
    } else {
      projectRMD[[i]] <- projectRMD[[i]][(progressStart+1): progressEnd]
    }
  }
  # Now get all the available dates
  for(i in 1:length(projects)){
    availDates <- c(availDates, as.Date(gsub("## ", "", projectRMD[[i]][grep("##", projectRMD[[i]])]), format="%Y-%m-%d"))
  }
  # Unique and sort the dates
  availDates <- unique(availDates)
  availDates <- availDates[order(availDates, decreasing=TRUE)]

  # Now concatenate the progress entries based on the timestamps
  labBook <- c()
  for(i in 1:length(availDates)){
    # Find projects with that particular timestamp and loop through them
    tmpProject <- grep(availDates[i], projectRMD)
    for(j in 1:length(tmpProject)){
     dateStart <- grep(paste0("## ",as.character(availDates[i])), projectRMD[[tmpProject[j]]])
     otherDates <- grep("## ", projectRMD[[tmpProject[j]]])
     dateEnd <- otherDates[min(which(otherDates==dateStart)+1, length(otherDates))]
     if(dateStart==dateEnd){
       labBook <- c(labBook, c(projects[tmpProject], projectRMD[[tmpProject[j]]][dateStart:length(projectRMD[[tmpProject[j]]])]))
     } else {
       labBook <- c(labBook, c(projects[tmpProject], projectRMD[[tmpProject[j]]][dateStart:dateEnd]))
     }
    }
  }

  labBook
}

debug(createLabBook)
createLabBook("/home/fischuu/git/LabBook/")
undebug(createLabBook)
#' Create Project Report
#'
#' Create a project report
#' @param project Project name
#' @param labBook Path to LabBookR folder
#' @return A RMarkdown dfile
#' @export
createProjectReport <- function(project=NULL, labBook=NULL){
# Input checks
  if(is.null(project)) stop("Please define a project.")
  if(is.null(labBook)) stop("Please provide a labBook address")

  rmarkdown::render(file.path(labBook,paste0(project,".Rmd")))
}

#' Create a new project
#'
#' This function creates a new project
#' @param title Name of the project
#' @param folder Path to LabBookR folder
#' @param author Name of the project report author
#' @return A blank project file
#' @export
createNewProject <- function(title, folder, author){
  blankProject <- c(
    '---',
    paste0('title: "',title,'"'),
    paste0('author: "',author,'"'),
    'output:',
    '  html_document:',
    '  toc: true',
    '  toc_depth: 4',
    '  toc_float:',
    '    toc_collapsed: true',
    'number_sections: false',
    'theme: lumen',
    'df_print: paged',
    'code_folding: hide',
    '---',
    '',
    '```{r setup, include=FALSE}',
    'knitr::opts_chunk$set(echo = TRUE,',
    '                      eval = FALSE)',
    '```',
    '```{}',
    '# Instructions to enter ToDo-list items',
    '# Follow the format:',
    '@ IncomingDate @ DueDate @ PlannedDate @ RequiredTime @ Ready @ Description',
    '@ YYYY.MM.DD @ YYYY.MM.DD @ YYYY.MM.DD @ h:mm @ logical @ Free text',
    '',
    '# Subprojects, just intent 4 whitespaces like this:',
    '@ YYYY.MM.DD @ YYYY.MM.DD @ YYYY.MM.DD @ h:mm @ TRUE/FALSE @ Main task',
    '    @ YYYY.MM.DD @ YYYY.MM.DD @ YYYY.MM.DD @ h:mm @ TRUE/FALSE @ subtask',
    '```',
    '',
    '# Project initialisation',
    '[ ] Create a github repository named "Project - Title"',
    '[ ] Create a CSC project named "Title"',
    '[ ] Initiate Allas backup for scratch space',
    '-> script can be found here:',
    '[ ] Setup Luke project backup for scratch space:',
    '',
    '# ToDo',
    '',
    '# Progress Notes',
    ''
  )
  file <- paste0(gsub(" ","_",title), ".Rmd")
  if(file.exists(file.path(folder, file))){
    stop("Project exists already, nothing was done!")
  } else {
    fileConn <- file(file.path(folder, file))
    writeLines(blankProject, fileConn)
    close(fileConn)
  }
}

