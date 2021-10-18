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
    #'date: "`r format(Sys.time(), '\%d \%B, \%Y')`"',
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
  fileConn <- file(file.path(folder, file))
  writeLines(blankProject, fileConn)
  close(fileConn)
}

