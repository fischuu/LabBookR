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
getMyTODO <- function(folder, verbose=TRUE, sorting=c("Incoming", "Due", "Scheduled")){
   output <- c()
   projects <- getMyProjects(folder)

   project.wo.todo <- projects[projects$ToDo=="NO",]
   projects <- projects[-which(projects$ToDo=="NO"),]

   sorting <- match.arg(sorting)

   if(nrow(project.wo.todo)>0){
     if(verbose){
       cat("Following projects without ToDo are not displayed:\n")
       print(project.wo.todo)
     }
   }


   for(i in 1:nrow(projects)){
     tmp.project <- readLines(file.path(folder, projects$file[i]))
     todo.start <- grep("# ToDo",tmp.project)
     todo.end <- grep("# Progress Notes",tmp.project) - 1

     if(length(todo.start)==0) stop ("No ToDo section start found in project ", projects$title[i])
     if(length(todo.start)==0) stop ("No ToDo section end found in project ", projects$title[i])

     for(j in (todo.start+1):todo.end){
       if(tmp.project[j]!=""){
         tmpToDo <- getToDo.internal(tmp.project[j])
         tmpToDo$Project <- projects$title[i]
         output <- rbind(output, tmpToDo)
       }
     }
   }

   output[,1] <- as.Date(output[,1], format="%Y.%m.%d")
   output[,2] <- as.Date(output[,2], format="%Y.%m.%d")
   output[,3] <- as.Date(output[,3], format="%Y.%m.%d")

   if(sorting=="Incoming"){
     output <- output[order(output[,1]),]
   } else if (sorting=="Due"){
     output <- output[order(output[,2]),]
   } else if(sorting=="Scheduled"){
     output <- output[order(output[,3]),]
   }
  rownames(output) <- NULL
  output
}

#' @export
getTasksPerWeek <- function(folder, plot=TRUE){
  TODO <- getMyTODO(folder=folder, verbose=FALSE)

  years <- unique(format(TODO$Incoming, "%Y"))

  incoming.week <- format(TODO$Incoming, "%V")
  due.week <- format(TODO$DueData, "%V")
  scheduled.week <- format(TODO$Scheduled, "%V")

  incoming.year <- format(TODO$Incoming, "%Y")
  due.year <- format(TODO$DueData, "%Y")
  scheduled.year <- format(TODO$Scheduled, "%Y")

  incoming <- paste0(incoming.year,".",incoming.week)
  due <- paste0(due.year,".",due.week)
  scheduled <- paste0(scheduled.year,".",scheduled.week)

  time.formatted <- as.POSIXlt(TODO$TimeReq,format="%H:%M")

  time <- (time.formatted$hour*60 + time.formatted$min)/60

  tmp <- table(scheduled, time)
  tmp <- as.numeric(colnames(tmp))* tmp
  timePerWeek <- apply(tmp,1,sum)

  if(plot){
    par(mfrow=c(2,2))

    barplot(table(incoming), las=2, main="Incoming tasks per week")
    barplot(table(due), las=2, main="Due tasks per week")
    barplot(table(scheduled), las=2, main="Scheduled tasks per week")
    barplot(timePerWeek, las=2, main="Scheduled working time per week")
  }

  output <- list(incoming = table(incoming),
                 due = table(due),
                 scheduled = table(scheduled),
                 timePerWeek = timePerWeek)
  output
}


