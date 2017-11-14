if(getRversion() >= "2.15.1")
    utils::globalVariables("authToken")

if(getRversion() >= "3.1.0")
    utils::suppressForeignCheck("authToken")

#' Login to GMQL
#'
#' Login to GMQL REST services suite as a registered user, specifying username 
#' and password, or as guest using the proper GMQL web service available 
#' on a remote server
#' 
#' @import httr
#' @importFrom rJava J
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param username string name used during signup
#' @param password string password used during signup
#'
#' @seealso \code{\link{logout_gmql}}
#'
#' @details
#' if both username and password are NULL you will be logged as guest
#' After login you will receive an authentication token.
#' As token remains vaild on server (until the next login / registration) 
#' a user can safely use a token fora previous session as a convenience, 
#' this token is saved in Global environment to perform subsequent REST call 
#' even on complete R restart (if is environemnt has been saved, of course ...)
#' If error occures a specific error is printed
#'
#' @return None
#'
#' @examples
#' 
#' ### login as guest
#' remote_url = "http://130.186.13.219/gmql-rest"
#' login_gmql(remote_url)
#'
#' @export
#'
login_gmql <- function(url, username = NULL, password = NULL)
{
    as_guest <- TRUE
    
    if(!is.null(username) || !is.null(password))
        as_guest <- FALSE
    
    if(as_guest)
    {
        h <- c('Accept' = "Application/json")
        URL <- paste0(url,"/guest")
        req <- httr::GET(URL,httr::add_headers(h))
    }
    else
    {
        h <- c('Accept'="Application/json",'Content-Type'='Application/json')
        URL <- paste0(url,"/login")
        body <- list('username' = username,'password' = password)
        req <- httr::POST(URL,httr::add_headers(h),body = body,encode = "json")
    }

    content <- httr::content(req)

    if(req$status_code !=200)
        stop(content$errorString)
    else
    {
        assign("authToken",content$authToken,.GlobalEnv)
        WrappeR <- J("it/polimi/genomics/r/Wrapper")
        WrappeR$save_tokenAndUrl(authToken,url)
        print(paste("your Token is",authToken))
    }
}

#' Logout from GMQL
#' 
#' Logout from GMQL REST services suite
#' using the proper GMQL web service available on a remote server
#'
#' @import httr
#' @importFrom rJava J
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#'
#' @seealso \code{\link{login_gmql}}
#'
#' @details
#' After logout the authentication token will be invalidated.
#' The authentication token is removed from Global environment
#' If error occures a specific error is printed
#' 
#' @examples
#'
#' #### login as guest, then logout
#' remote_url = "http://130.186.13.219/gmql-rest"
#' login_gmql(remote_url)
#' logout_gmql(remote_url)
#'
#' @return None
#'
#' @export
#'
logout_gmql <- function(url)
{
    URL <- paste0(url,"/logout")
    h <- c('X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req)
    if(req$status_code !=200)
        stop(content$error)
    else
    {
        print(content)
        #delete token from environment
        WrappeR <- J("it/polimi/genomics/r/Wrapper")
        WrappeR$delete_token()
        rm(authToken, envir = .GlobalEnv)
    }
}



