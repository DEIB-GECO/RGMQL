#' @importFrom rJava .jpackage .jinit
#' @import RGMQLlib
#' 
.onLoad <- function(libname, pkgname) {
    .jpackage(pkgname, lib.loc = libname)
    # tools::vignetteEngine("knitr", pattern = "[.]Rmd$", package = "knitr")
    initGMQLscalaAPI()
    if(exists("GMQL_credentials", envir = .GlobalEnv))
    {
        packageStartupMessage("Check if GMQL credentials are expired... \n")
        if(exists("remote_url", where = GMQL_credentials))
        {
            user <- GMQL_credentials$username
            psw <- GMQL_credentials$password
            remote_url <- GMQL_credentials$remote_url
            login_gmql(remote_url,user,psw)
        }
    }
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("GMQL successfully loaded")
}

.onDetach <- function(libpath) {
    
} 

.onUnload <- function(libpath) {
}


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
    available_local_files <- list.files(libLoc, full.names = FALSE, 
                                        pattern = "\\.jar$")
    
    # Check if all the files are there
    if (!"GMQL.jar" %in% available_local_files) 
        stop("GMQL jar not available")
    
    if (length(path) > 0)
        rJava::.jaddClassPath(path)

rJava::.jaddClassPath(dirname(path))
}

