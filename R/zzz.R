.onAttach <- function(libname, pkgname){
  packageStartupMessage("This is LabBookR version ", utils::packageVersion("LabBookR"))
}
