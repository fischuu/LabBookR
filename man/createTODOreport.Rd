% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creaters.R
\name{createTODOreport}
\alias{createTODOreport}
\title{Create Complete ToDoList}
\usage{
createTODOreport(
  labBook = NULL,
  sortedByDate = TRUE,
  title = "My TODO",
  author = "Daniel Fischer",
  output = "html+pdf"
)
}
\arguments{
\item{labBook}{Path to LabBookR folder
#' @param output Define output format, options: "html+pdf", "html", "pdf"}
}
\value{
A RMarkdown file
}
\description{
Create the ToDo List
}
