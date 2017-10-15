#' @importFrom rJava .jpackage
#' @importFrom rJava .jinit

.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
 # tools::vignetteEngine("knitr", pattern = "[.]Rmd$", 
  #                      package = "knitr")
  .jinit(force.init = TRUE)
  
  #assign("WrappeR",.jnew("it/polimi/genomics/r/Hello"),envir=parent.env(environment()))
  
}

.onAttach <- function(libname, pkgname) {
  #packageStartupMessage("GMQL successfully loaded")
}

.onUnload <- function(libpath) {
  #.rscalaPackageUnload()
}
