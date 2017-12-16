#' Init GMQL server
#'
#' It initializes and runs GMQL server for executing GMQL query
#' It also performs a login to GMQL REST services suite, if needed
#' 
#' @importFrom rJava J
#' 
#' @param output_format string identifies the output format of sample files.
#' It can be TAB, GTF or COLLECT:
#' \itemize{
#' \item{TAB: tab delimited file format}
#' \item{GTF: tab-delimited text fstandard ormat based on the general 
#' feature format}
#' \item{COLLECT: used for storing output in memory}
#' }
#' @param remote_processing logical value specifying the processing mode.
#' True for processing on cluster (remote), false for local processing.
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically.
#' If NULL, no login is performed.
#' You can always perform it by calling the function \code{\link{login_gmql}} 
#' explicitly
#' 
#' @param username string name used during signup 
#' @param password string password used during signup
#' 
#' @return None
#'
#' @examples
#'
#' ## This statement initializes GMQL with local processing with sample files 
#' ## output format as tab delimited
#' 
#' init_gmql("tab", FALSE)
#' 
#' ## initializes GMQL with remote processing
#' 
#' remote_url = "http://genomic.deib.polimi.it/gmql-rest-r/"
#' init_gmql(remote_processing = TRUE, url = remote_url)
#' 
#' @export
#'
init_gmql <- function(output_format = "GTF", remote_processing = FALSE, 
                    url = NULL, username = NULL, password = NULL)
{
    out_format <- toupper(output_format)
    if(!out_format %in% c("TAB", "GTF", "COLLECT"))
        stop("output_format: must be TAB, GTF or COLLECT")
    
    .check_logical(remote_processing)
    
    # mettere attesa da input keyboard, controllare se token già esiste 
    # da sessione precedente
    if(!is.null(url) && !exists("authToken",envir = .GlobalEnv))
        login_gmql(url,username,password)
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    WrappeR$initGMQL(out_format,remote_processing)
}

#' Stop GMQL server
#'
#' Stop GMQL server
#' 
#' @importFrom rJava J
#' 
#' @return None
#'
#' @examples
#'
#' ## These statements initializes GMQL with local processing with sample files 
#' ## output format as tab delimited and then stop it
#' 
#' init_gmql("tab", FALSE)
#' 
#' stop_gmql()
#' 
#' @export
#'
stop_gmql <- function()
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    WrappeR$stopGMQL()
}


#' Disable or Enable remote processing
#'
#' It allows to enable or disable remote processing 
#' 
#' @details 
#' The invocation of this function allows to change mode of processing.
#' After invoking collect() it is not possbile to switch the processing mode. 
#' 
#' @importFrom rJava J
#' 
#' @param is_remote logical value used in order to set the processing mode.
#' TRUE you set a remote query processing mode, otherwise it will be local,
#' 
#' @return None
#' 
#' @examples
#' 
#' ## These statements initializes GMQL with local processing with sample files 
#' ## output format as tab delimited and then change processing mode to remote
#' 
#' init_gmql("tab", remote_processing = FALSE)
#' 
#' remote_processing(TRUE)
#'
#' @export
#' 
remote_processing<-function(is_remote)
{
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    .check_logical(is_remote)
    response <- WrappeR$remote_processing(is_remote)
    print(response)
}

