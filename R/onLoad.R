#' @importFrom rJava .jpackage .jinit
#' @import RGMQLlib
#' 
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname, lib.loc = libname)
  initGMQLscalaAPI()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("GMQL successfully loaded")
}

.onDetach <- function(libpath) { } 

.onUnload <- function(libpath) { }


#' @importFrom rJava .jinit .jaddClassPath
#' 
initGMQLscalaAPI <- function(libLoc, mem = "12G") {
  # Check if library directory is missing
  
  # Starting the java engine
  .jinit(force.init = TRUE)
  if (missing(libLoc)) {
    libLoc = system.file("extdata", "java", package = "RGMQLlib")
  }
  
  path = Sys.glob(paste0(libLoc, "/*.jar"))
  available_local_files <- list.files(
    libLoc, 
    full.names = FALSE, 
    pattern = "\\.jar$"
  )
  
  # Check if all the files are there
  if (!"GMQL.jar" %in% available_local_files) {
    stop("GMQL jar not available")
  }
  
  if (length(path) > 0) {
    rJava::.jaddClassPath(path)
  }
  
  rJava::.jaddClassPath(dirname(path))
}
