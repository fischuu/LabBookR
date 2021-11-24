#' Create Complete LabBook
#'
#' Create the lab book
#' @param labBook Path to LabBookR folder
#' @param output Define output format, options: "html+pdf", "html", "pdf"
#' @return A RMarkdown file
#' @export
createLabBook <- function(labBook=NULL, sortedByDate=TRUE, title="My LabBook", author="Daniel Fischer", output="html+pdf"){
  # Input checks
  if(is.null(labBook)) stop("Please provide a labBook address")

  projects <- list.files(labBook, pattern="*.Rmd")
  if(length(which(projects=="labBook.complete.Rmd"))>0) projects <- projects[-which(projects=="labBook.complete.Rmd")]
  if(length(which(projects=="labBook.ToDo.Rmd"))>0) projects <- projects[-which(projects=="labBook.ToDo.Rmd")]

  if(output=="html+pdf" || output=="pdf+html"){
    output_render <- c("html_document","pdf_document")
  } else if (output == "html"){
    output_render <- c("html_document")
  } else if (output == "pdf"){
    output_render <- c("pdf_document")
  }

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

    if(length(progressStart)==0) stop("There is a problem with the start of the ## Progress notes section in project ", projects[i])
    if(length(progressEnd)==0) stop("There is a problem with the end of the ## Progress notes section in project ", projects[i])

    if(progressStart>=progressEnd){
      projectRMD[[i]] <- "NA"
    } else {
      projectRMD[[i]] <- projectRMD[[i]][(progressStart+1): progressEnd]
    }
  }
  # Now get all the available dates
  for(i in 1:length(projects)){
    addThis <- as.Date(gsub("## ", "", projectRMD[[i]][grep("^##", projectRMD[[i]])]), format="%Y-%m-%d")
    if(sum(is.na(addThis))>0) stop("Malformated date in project:", projects[i])
    availDates <- c(availDates, addThis)
  }
  # Unique and sort the dates
  availDates <- unique(availDates)
  availDates <- availDates[order(availDates, decreasing=TRUE)]

  # Create data matrices to store information
  projectsPerDay <- matrix(0, nrow=length(availDates), ncol=length(projects))
  rownames(projectsPerDay) <- as.character(availDates)
  colnames(projectsPerDay) <- gsub(".Rmd","",projects)

  # Now concatenate the progress entries based on the timestamps
  labBook.out <- c()
  for(i in 1:length(availDates)){
    # Find projects with that particular timestamp and loop through them
    tmpProject <- grep(availDates[i], projectRMD)
    newDate <- TRUE
    for(j in 1:length(tmpProject)){
     dateStart <- grep(paste0("## ",as.character(availDates[i])), projectRMD[[tmpProject[j]]])
     otherDates <- grep("## ", projectRMD[[tmpProject[j]]])
     dateEnd <- otherDates[min(which(otherDates==dateStart)+1, length(otherDates))]
     if(newDate){
       headLine <- c("",paste0("# ", gsub("## ", "",projectRMD[[tmpProject[j]]][dateStart])),paste0("## ", gsub(".Rmd","",projects[tmpProject[j]])))
     } else {
       headLine <- c("",paste0("## ", gsub(".Rmd","",projects[tmpProject[j]])))
     }

     if(dateStart==dateEnd){
       labBook.out <- c(labBook.out, headLine, projectRMD[[tmpProject[j]]][(dateStart+1):length(projectRMD[[tmpProject[j]]])])
     } else {
       labBook.out <- c(labBook.out, headLine,
                               projectRMD[[tmpProject[j]]][(dateStart+1):(dateEnd-1)])
     }
     newDate <- FALSE
     projectsPerDay[i,tmpProject[j]] <- 1
    }
  }

  if(!sortedByDate){
    headLine <- c("",paste0("# ", gsub(".Rmd","",projects[1])))
    labBook.out <- c(headLine, projectRMD[[1]])
    for(i in 2:length(projects)){
      headLine <- c("",paste0("# ", gsub(".Rmd","",projects[i])))
      labBook.out <- c(labBook.out, headLine, projectRMD[[i]])
    }
  }

  header <- c('---',
              paste0('title: "',title,'"'),
              paste0('author: "',author,'"'),
              'output:',
              '  html_document:',
              '      toc: true',
              '      toc_depth: 4',
              '      toc_float:',
              '        toc_collapsed: true',
              '  pdf_document:',
              '      toc: true',
              '      toc_depth: 4',
              'number_sections: false',
              'theme: lumen',
              'df_print: paged',
              'code_folding: hide',
              '---',
              '',
              '```{r setup, include=FALSE}',
              'knitr::opts_chunk$set(echo = TRUE,',
              '                      eval = FALSE)',
              '```')
  labBook.out <- c(header,labBook.out)
  fileConn <- file(file.path(labBook, "labBook.complete.Rmd"))
    writeLines(labBook.out, fileConn)
  close(fileConn)
  rmarkdown::render(file.path(labBook, "labBook.complete.Rmd"), output_render)

  # Quick and dirty copy+paste from here, with minor modifications:
  # https://stackoverflow.com/questions/65469546/how-to-include-row-col-names-in-image-r
  image.real <- function(mat) {
    mat <- mat[,ncol(mat):1]
    image(mat, axes = FALSE)
    axis(1, at = seq(0, 1, length = nrow(mat)), labels = rownames(mat), las=2)
    axis(2, at = seq(0, 1, length = ncol(mat)), labels = colnames(mat), las=2)
    box()
  }
  par(oma=c(4,15,1,1))
  image.real(projectsPerDay)
}

#' Create Complete ToDoList
#'
#' Create the ToDo List
#' @param labBook Path to LabBookR folder
#' #' @param output Define output format, options: "html+pdf", "html", "pdf"
#' @return A RMarkdown file
#' @export
#' @import kableExtra
createTODOreport <- function(labBook=NULL, sortedByDate=TRUE, title="My TODO", author="Daniel Fischer", output="html+pdf"){
  # Input checks
  if(is.null(labBook)) stop("Please provide a labBook address")

  if(output=="html+pdf" || output=="pdf+html"){
    output_render <- c("html_document","pdf_document")
  } else if (output == "html"){
    output_render <- c("html_document")
  } else if (output == "pdf"){
    output_render <- c("pdf_document")
  }

  projects <- list.files(labBook, pattern="*.Rmd")
  if(length(which(projects=="labBook.complete.Rmd"))>0) projects <- projects[-which(projects=="labBook.complete.Rmd")]
  if(length(which(projects=="labBook.ToDo.Rmd"))>0) projects <- projects[-which(projects=="labBook.ToDo.Rmd")]

  todo <- getMyTODO(folder=labBook)

#  todo.kbl <- kbl(todo)

  todo.kbl <- todo %>%
                kbl() %>%
                kable_styling()

  labBook.out <- c("\n", todo.kbl,"\n")

  header <- c('---',
              paste0('title: "',title,'"'),
              paste0('author: "',author,'"'),
              'output:',
              '  html_document:',
              '      toc: true',
              '      toc_depth: 4',
              '      toc_float:',
              '        toc_collapsed: true',
              '  pdf_document:',
              '      toc: true',
              '      toc_depth: 4',
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
              '',
              '<style type="text/css">',
                'div.main-container {',
                  'max-width: 1800px;',
                  'margin-left: auto;',
                  'margin-right: auto;',
                '}',
              '</style>'
  )
  labBook.out <- c(header,labBook.out)
  fileConn <- file(file.path(labBook, "labBook.ToDo.Rmd"))
  writeLines(labBook.out, fileConn)
  close(fileConn)
  rmarkdown::render(file.path(labBook, "labBook.ToDo.Rmd"), output_render)

}
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
    '      toc: true',
    '      toc_depth: 4',
    '      toc_float:',
    '        toc_collapsed: true',
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
    '```{bash, echo=FALSE, include=FALSE}',
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

