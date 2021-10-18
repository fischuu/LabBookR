#' Show projects
#'
#' This functions shows exixting project
#' @param folder Path to LabBookR folder
#' @return a list of projects
#' @export
getMyProjects <-  function(folder){
  projectFiles <- list.files(folder, pattern = "*.Rmd")
  projects.list <- list()

  projects <- data.frame(title=character(),
                         ToDo=character(),
                         file=character(),
                         stringsAsFactors=FALSE)

  for(i in 1:length(projectFiles)){
    tmp.project <- readLines(file.path(folder, projectFiles[i]))

    ifelse(length( grep("# ToDo",tmp.project))>0, todo.avail <- "YES", todo.avail <- "NO" )

    tmp <- data.frame(title = gsub('\"','',gsub('title: \"','',tmp.project[grep("title", tmp.project)])),
                      ToDo = todo.avail,
                      file = projectFiles[i],
                      stringsAsFactors=FALSE)

    projects <- rbind(projects,tmp)
  }

  projects
}

getToDo.internal <- function(x){
  subproject <- FALSE
  tmp <- strsplit(x,"@ ")[[1]]
  if(tmp[1]=="    ") subproject <- TRUE
  tmp <- trimws(tmp[-1])
  data.frame(Incoming=tmp[1],
             DueData=tmp[2],
             Scheduled=tmp[3],
             TimeReq=tmp[4],
             Finished=tmp[5],
             Description=tmp[6],
             Subproject=subproject)
}

#' @export
getMyTODO <- function(folder){
   output <- c()
   projectFiles <- list.files(folder, pattern = "*.Rmd")
   projects <- getMyProjects(folder)
   for(i in 1:nrow(projects)){
     tmp.project <- readLines(file.path(folder, projectFiles[i]))
     todo.start <- grep("# ToDo",tmp.project)
     todo.end <- grep("# Progress Notes",tmp.project) - 1

     for(j in (todo.start+1):todo.end){
       if(tmp.project[j]!=""){
         tmpToDo <- getToDo.internal(tmp.project[j])
         tmpToDo$Project <- projects$title[i]
         output <- rbind(output, tmpToDo)
       }
     }
   }

   output
}






