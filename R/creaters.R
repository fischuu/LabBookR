#' Create Complete LabBook
#'
#' Create the lab book
#' @param labBook Path to LabBookR folder
#' @param output Define output format, options: "html+pdf", "html", "pdf"
#' @return A RMarkdown file
#' @export
createLabBook <- function(labBook=NULL, sortedByDate=TRUE, title="My LabBook", author="Daniel Fischer", output="html+pdf"){
  # Input checks
  if( exists("LabBookR.config.labBook")){
    labBook <- LabBookR.config.labBook
    folder <- LabBookR.config.folder
  } else {
    if(is.null(labBook)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  #projects <- list.files(labBook, pattern="*.Rmd")
  projectsOverview <- read.table(file.path(folder, "labBook.projectOverview.tsv"), sep="\t", header=TRUE)
  projects <- projectsOverview$Title


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
    projectRMD[[i]] <- readLines(file.path(labBook, projects[i], paste0(projects[i], ".Rmd")))
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
    tmp <- gsub("## ", "", projectRMD[[i]][grep("^## ", projectRMD[[i]])])
    tmp_date <- sub("\\s.*$", "", tmp)

    addThis <- as.Date(tmp_date, format="%Y-%m-%d")
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
    tmpProject <- grep(gsub("-", ".", availDates[i]), projectRMD)
    newDate <- TRUE
    for(j in 1:length(tmpProject)){
     dateStart <- grep(as.character(gsub("-", ".", availDates[i])), projectRMD[[tmpProject[j]]])
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
createTODOreport <- function(labBook=NULL, sortedBy="Scheduled", title="My TODO", author="Daniel Fischer", output="html", showNonfinished=TRUE, showFinished=FALSE){
# Input checks
  if( exists("LabBookR.config.labBook")){
    labBook <- LabBookR.config.labBook
  } else {
    if(is.null(labBook)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

#if(is.null(labBook)) stop("Please provide a labBook address")

  if( exists("LabBookR.config.folder")){
    folder <- LabBookR.config.folder
  } else {
    if(is.na(folder)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

if(output=="html+pdf" || output=="pdf+html"){
  output_render <- c("html_document","pdf_document")
} else if (output == "html"){
  output_render <- c("html_document")
} else if (output == "pdf"){
  output_render <- c("pdf_document")
}

projectsOverview <- read.table(file.path(folder, "labBook.projectOverview.tsv"), sep="\t", header=TRUE)
projects <- projectsOverview$Title

if(length(which(projects=="labBook.complete.Rmd"))>0) projects <- projects[-which(projects=="labBook.complete.Rmd")]
if(length(which(projects=="labBook.ToDo.Rmd"))>0) projects <- projects[-which(projects=="labBook.ToDo.Rmd")]

todo <- getMyTODO(folder=labBook)

# Remove finished and/or non-finished jobs from the list
showThose1 <- c()
showThose2 <- c()
if(showFinished) showThose1 <- which(toupper(todo$Finished)=="TRUE")
if(showNonfinished) showThose2 <- which(toupper(todo$Finished)=="FALSE")
showThose <- c(showThose1, showThose2)
if(length(showThose)==0) showThose <- 1:nrow(todo)
todo <- todo[showThose,]

write.table(todo, file.path(folder, "labBook.ToDo.data"), sep="\t", quote=FALSE)


todoText <- c('---',
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
            'library("DT")',
            'knitr::opts_chunk$set(echo = TRUE,',
            '                      eval = TRUE)',
            paste0('folder <- "', folder, '"'),
            '```',
            '',
            '```{r, echo=FALSE, include=FALSE}',
            'todo <- read.table(file.path(folder, "labBook.ToDo.data"), sep="\t", header=TRUE)',
            '```',
            '',
            '```{r, include=TRUE, echo=FALSE}',
            'datatable(todo)',
            '```'
)

fileConn <- file(file.path(labBook, "labBook.ToDo.Rmd"))
writeLines(todoText, fileConn)
close(fileConn)
rmarkdown::render(file.path(labBook, "labBook.ToDo.Rmd"), output_render)

}

createTODOreport_old <- function(labBook=NULL, sortedBy="Scheduled", title="My TODO", author="Daniel Fischer", output="html+pdf", showNonfinished=TRUE, showFinished=FALSE){
  # Input checks
  if(is.null(labBook)) stop("Please provide a labBook address")

  if( exists("LabBookR.config.folder")){
    folder <- LabBookR.config.folder
  } else {
    if(is.na(folder)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  if(output=="html+pdf" || output=="pdf+html"){
    output_render <- c("html_document","pdf_document")
  } else if (output == "html"){
    output_render <- c("html_document")
  } else if (output == "pdf"){
    output_render <- c("pdf_document")
  }

  #projects <- list.files(labBook, pattern="*.Rmd")

  projectsOverview <- read.table(file.path(folder, "labBook.projectOverview.tsv"), sep="\t", header=TRUE)
  projects <- projectsOverview$Title

  if(length(which(projects=="labBook.complete.Rmd"))>0) projects <- projects[-which(projects=="labBook.complete.Rmd")]
  if(length(which(projects=="labBook.ToDo.Rmd"))>0) projects <- projects[-which(projects=="labBook.ToDo.Rmd")]

  todo <- getMyTODO(folder=labBook)

  # Remove finished and/or non-finished jobs from the list
  showThose1 <- c()
  showThose2 <- c()
  if(showFinished) showThose1 <- which(todo$Finished=="TRUE")
  if(showNonfinished) showThose2 <- which(todo$Finished=="FALSE")
  showThose <- c(showThose1, showThose2)
  if(length(showThose)==0) showThose <- 1:nrow(todo)
  todo <- todo[showThose,]

  # Sort the table
  todo <- todo[order(todo[sortedBy]),]

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
  if( exists("LabBookR.config.labBook")){
    labBook <- LabBookR.config.labBook
  } else {
    if(is.null(labBook)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  all_projects <- getMyProjects(labBook)

  if(is.null(project)){
    for(i in 1:length(all_projects$title)){
      rmarkdown::render(file.path(labBook, all_projects$title[i], paste0(all_projects$title[i],".Rmd")))
    }
  } else {
    project <- match.arg(project, all_projects$title)
    rmarkdown::render(file.path(labBook, project, paste0(project,".Rmd")))
  }


}

#' Create a new project
#'
#' This function creates a new project
#' @param title Name of the project
#' @param labBook Path to LabBookR folder
#' @param author Name of the project report author
#' @return A blank project file
#' @export
createNewProject <- function(title=NULL, labBook=NULL, author=NULL){

  if( exists("LabBookR.config.labBook")){
    labBook <- LabBookR.config.labBook
  } else {
    if(is.null(labBook)) stop("Please specify the LabBook folder or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

  if( exists("LabBookR.config.author")){
    author <- LabBookR.config.author
  } else {
    if(is.null(author)) stop("Please specify an author name or load your LabBook configuration via `loadLabBookConfig(...)`")
  }

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
    paste0('title <- "', title,'"'),
    '```',
    '',
    '```{bash, echo=FALSE, include=FALSE}',
    '',
    '# Project initialisation',
    '| Task Description                                        | Completed   |',
    '|---------------------------------------------------------|-------------|',
    '| Create a LabBookR project file                          | &#10004;    |',
    '| Create a GitHub repository named "Project - Title"      | &#10060;    |',
    '| Create a CSC project named "Title"                      | &#10060;    |',
    '| Initiate Allas backup for scratch space                 | &#10060;    |',
    '| Setup Luke project backup for scratch space             | &#10060;    |',
    '```',
    '',
    '# Project overview',
    '',
    '# ToDo',
    '',
    '```{r, eval=TRUE, echo=FALSE, include=FALSE}',
    'toDo <- read.table(paste0(title,".todo.tsv"), header=TRUE, sep="\t")',
    '```',
    '',
    '```{r, eval=TRUE, echo=FALSE, include=TRUE}',
    'DT::datatable(toDo)',
    '```',
    '',
    '# Progress Notes',
    ''
  )
# Create progress file
  file <- paste0(gsub(" ","_",title), ".Rmd")
  folder <- gsub(".Rmd", "", file)
  if(dir.exists(file.path(labBook, folder))){
    stop("Project exists already, nothing was done!")
  } else {
    dir.create(folder)
    dir.create(file.path(folder, "Files"))

    fileConn <- file(file.path(labBook, folder, file))
    writeLines(blankProject, fileConn)
    close(fileConn)
  }

# Create project-specific ToDo file
  toDo <- data.frame(Incoming=character(),
                     Due=character(),
                     Scheduled=character(),
                     RequiredTime=character(),
                     Priority=character(),
                     Finished=character(),
                     Task=character()
  )
  write.table(toDo, file=file.path(labBook, folder, gsub(".Rmd", ".todo.tsv", file)), quote=FALSE, sep="\t")

# Create/Add to project overview
  projectData <- data.frame(Incoming=date(),
                            Title=title,
                            Active=TRUE)

# Create progress file
  if(file.exists(file.path(labBook, "labBook.projectOverview.tsv"))){
    tmp <- read.table(file.path(labBook, "labBook.projectOverview.tsv"), header=TRUE, sep="\t")
    write.table(rbind(tmp, projectData), file=file.path(labBook, "labBook.projectOverview.tsv"), sep="\t", quote=FALSE)
  } else {
    write.table(projectData, file=file.path(labBook, "labBook.projectOverview.tsv"), sep="\t", quote=FALSE)
  }

}

#' @export
createLabBookConfig <- function(folder, labBook, author, dueDate=28, scheduledDate=21, reqTime="2:00", overwrite=FALSE){
   LabBookR.config <- data.frame(folder=folder,
                                 labBook=labBook,
                                 author=author,
                                 dueDate=dueDate,
                                 scheduledDate=scheduledDate,
                                 reqTime=reqTime)

   if(file.exists(file.path(folder, ".LabBookR.config"))){
     if(overwrite){
       write.table(LabBookR.config, file=file.path(folder, ".LabBookR.config"))
     } else {
       stop("Config file exists already, please set 'overwrite=TRUE' if you want to overwrite existing configuration")
     }
   } else {
     write.table(LabBookR.config, file=file.path(folder, ".LabBookR.config"))
   }

}
