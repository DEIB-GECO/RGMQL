if(getRversion() >= "2.15.1") {
    utils::globalVariables("GMQL_credentials")
    utils::globalVariables("remote_url")
}

if(getRversion() >= "3.1.0") {
    utils::suppressForeignCheck("GMQL_credentials")
    utils::suppressForeignCheck("remote_url")
}


#############################
#     WEB AUTHENTICATION   #
############################


#' Login to GMQL
#'
#' Login to GMQL REST services suite as a registered user, specifying username 
#' and password, or as guest, using the proper GMQL web service available 
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
#' @details
#' If both username and password are missing, you will be logged as guest.
#' After login you will receive an authentication token.
#' As token remains valid on server (until the next login / registration or 
#' logout), a user can safely use a token for a previous session as a 
#' convenience; this token is saved in R Global environment to perform 
#' subsequent REST call even on complete R restart (if the environment has 
#' been saved). If error occurs, a specific error is printed
#'
#' @return None
#'
#' @examples
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' @name login_gmql
#' @rdname login_gmql
#' @export
#' 
login_gmql <- function(url, username = NULL, password = NULL) {
    if(!.is_login_expired(url)) {
        print("Login still valid")
        return(invisible(NULL))
    }
    
    as_guest <- TRUE
    
    if(!is.null(username) || !is.null(password))
        as_guest <- FALSE
    
    url <- sub("/*[/]$","",url)
    
    if(as_guest) {
        h <- c('Accept' = "Application/json")
        URL <- paste0(url,"/guest")
        req <- httr::GET(URL,httr::add_headers(h))
    } else {
        req <- httr::GET(url)
        real_URL <- sub("/*[/]$","",req$url)
        h <- c('Accept'="Application/json",'Content-Type'='Application/json')
        URL <- paste0(real_URL,"/login")
        body <- list('username' = username,'password' = password)
        req <- httr::POST(
            URL, 
            httr::add_headers(h), 
            body = body, 
            encode = "json")
    }
    
    content <- httr::content(req)
    
    if(req$status_code != 200)
        stop(content$errorString)
    else {
        url <- paste0(url,"/")
        GMQL_remote <- list(
            "remote_url" = url, 
            "authToken" = content$authToken,
            "username" = username,
            "password" = password
        )
        
        assign("GMQL_credentials", GMQL_remote, .GlobalEnv)
        print(paste("your Token is", GMQL_remote$authToken))
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
#' @details
#' After logout the authentication token will be invalidated.
#' The authentication token is removed from R Global environment.
#' If error occurs, a specific error is printed
#' 
#' @examples
#' 
#' ## Login to GMQL REST services suite as guest, then logout
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' logout_gmql(remote_url)
#' 
#' @return None
#'
#' @name logout_gmql
#' @rdname logout_gmql
#' @export
#'
logout_gmql <- function(url) {
    url <- sub("/*[/]$","",url)
    URL <- paste0(url,"/logout")
    
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken)
    
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req)
    
    if(req$status_code !=200)
        stop(content$error)
    else {
        #delete token from environment
        if(exists("authToken", where = GMQL_credentials))
            rm(GMQL_credentials, envir = .GlobalEnv)
        
        print(content)
    }
}
#' Register into remote GMQL
#' 
#' Register to GMQL REST services suite
#' using the proper GMQL web service available on a remote server.
#' 
#' @import httr
#' @importFrom rJava J
#' 
#' @param url string url of server: It must contains the server address 
#' and base url; service name is added automatically
#' 
#' @param username string user name used to login in
#' @param psw string password used to login in
#' @param email string user email 
#' @param first_name string user first name 
#' @param last_name string user last name 
#' 
#' @details
#' After registration you will receive an authentication token.
#' As token remains valid on server (until the next login / registration or 
#' logout), a user can safely use a token for a previous session as a 
#' convenience; this token is saved in R Global environment to perform 
#' subsequent REST calls or batch processing even on complete R restart 
#' (if the environment has been saved). If error occurs, a specific error 
#' is printed.
#' 
#' @examples
#' 
#' ## Register to GMQL REST services suite 
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' \dontrun{
#' register_gmql(remote_url,"foo","foo","foo@foo.com","foo","foo")
#' }
#' 
#' @return None
#'
#' @name register_gmql
#' @rdname register_gmql
#' @export
#'
register_gmql <- function(
    url, 
    username, 
    psw, 
    email, 
    first_name, 
    last_name
) {
    req <- httr::GET(url)
    url <- sub("/*[/]$","",req$url)
    
    URL <- paste0(url,"/register")
    h <- c('Accept' = "Application/json")
    reg_body <- list(
        "firstName" = first_name, 
        "lastName" = last_name,
        "username" = username, 
        "email" = email, 
        "password" = psw
    )
    
    req <- httr::POST(
        URL, 
        body = reg_body, 
        httr::add_headers(h), 
        encode = "json"
    )
    
    content <- httr::content(req,"parsed")
    if(req$status_code != 200) {
        stop(content)
    } else {
        GMQL_remote <- list(
            "remote_url" = url, 
            "authToken" = content$authToken,
            "username" = username,
            "password" = psw
        )
        assign("GMQL_credentials", GMQL_remote, .GlobalEnv)
        print(paste("your Token is", GMQL_remote$authToken))
    }
}


#############################
#       WEB BROWSING       #
############################

#' Show all queries
#'
#' It shows all the GMQL queries saved by the user on remote repository, 
#' using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#'
#' @return List of queries. Every query in the list is described by:
#' \itemize{
#' \item{name: name of query}
#' \item{text: text of GMQL query}
#' }
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' ## Login to GMQL REST services suite
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## List all queries executed on remote GMQL system 
#' list <- show_queries_list(remote_url)
#' 
#' @name show_queries_list
#' @rdname show_queries_list
#' @export
#'
show_queries_list <- function(url) {
    url <- sub("/*[/]$","",url)
    URL <- paste0(url,"/query")
    authToken = GMQL_credentials$authToken
    h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req,"parsed")
    if(req$status_code == 200) {
        return(content)
    } else {
        stop(content$error)
    }
}

#' Save GMQL query
#'
#' It saves a GMQL query into repository, taken from file or inserted as text 
#' string, using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param queryName string name of query
#' @param queryTxt string text of GMQL query
#'
#' @return None
#'
#' @details
#' If you save a query with the same name of another query already stored 
#' in repository, you will overwrite it; if no error occurs, it prints: 
#' "Saved", otherwise it prints the error
#'
#' @examples
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement saves query written directly as input string parameter 
#' ## with name "dna_query" 
#' 
#' save_query(remote_url, "example_query",
#' "DATASET = SELECT() Example_Dataset_1; MATERIALIZE DATASET INTO RESULT_DS;")
#' 
#' ## With system.file() this statement  defines the path to the folder 
#' ## "example" of the package "RGMQL", and then it saves the query written 
#' ## in the text file "query1.txt" into remote repository
#' 
#' test_path <- system.file("example", package = "RGMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' save_query_fromfile(remote_url, "query1", test_query)
#' 
#' @name save_query
#' @rdname save_query
#' @export
#'
save_query <- function(url, queryName, queryTxt) {
    req <- httr::GET(url)
    url <- sub("/*[/]$","",req$url)
    URL <- paste0(url,"/query/",queryName,"/save")
    authToken = GMQL_credentials$authToken
    h <- c(
        'Accept' = 'text/plain',
        'X-Auth-Token' = authToken,
        'Content-Type' = 'text/plain'
    )
    req <- httr::POST(URL, httr::add_headers(h),body = queryTxt)
    content <- httr::content(req)
    
    if(req$status_code == 200) {
        # print Saved
        print(content) 
    } else {
        stop(content$error)
    }
}

#' @param filePath string local file path of a txt file containing a GMQL query
#' 
#' @name save_query_fromfile
#' @rdname save_query
#' @export
#' 
save_query_fromfile <- function(url, queryName, filePath) {
    if(!file.exists(filePath)) {
        stop("file does not exist")
    }
    
    queryTxt <- readLines(filePath)
    save_query(url,queryName,queryTxt)
}

#############################
#       WEB OPERATION      #
############################

#' Run a GMQL query
#' 
#' It runs a GMQL query into repository taken from file or inserted as text 
#' string, using the proper GMQL web service available on a remote server
#' 
#' @import httr
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param queryName string name of the GMQL query file
#' @param query string text of the GMQL query
#' @param output_gtf logical value indicating file format used for 
#' storing samples generated by the query.
#' The possiblities are: 
#' \itemize{
#' \item{GTF}
#' \item{TAB}
#' }
#'
#' @return None
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## Run query as string input parameter
#' ## NOTE: not very suitable for long queries
#' 
#' run_query(remote_url, "query_1", "DATASET = SELECT() Example_Dataset1;
#'     MATERIALIZE DATASET INTO RESULT_DS;", output_gtf = FALSE)
#' 
#' ## With system.file() this statement defines the path to the folder 
#' ## "example" of the package "RGMQL", and then it executes the query 
#' ## written in the text file "query1.txt"
#' 
#' test_path <- system.file("example", package = "RGMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' run_query_fromfile(remote_url, test_query, output_gtf = FALSE)
#' }
#' 
#' @rdname run_query
#' @name run_query
#' @export
#'
run_query <- function(url, queryName, query, output_gtf = TRUE) {
    if(output_gtf)
        out <- "GTF"
    else
        out <- "TAB"
    
    req <- httr::GET(url)
    url <- sub("/*[/]$","",req$url)
    URL <- paste0(url,"/queries/run/",queryName,"/",out)
    authToken = GMQL_credentials$authToken
    h <- c(
        'Accept' = "Application/json",
        'Content-Type' = 'text/plain',
        'X-Auth-Token' = authToken
    )
    
    req <- httr::POST(URL,body = query ,httr::add_headers(h),encode = "json")
    content <- httr::content(req,"parsed")
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#' @import httr
#' @param filePath string path of a txt file containing a GMQL query
#' 
#' @rdname run_query
#' @name run_query
#' @export
#' 
run_query_fromfile <- function(url, filePath, output_gtf = TRUE) {
    if (!file.exists(filePath)) {
        stop("file does not exist")
    }
    
    query <- readLines(filePath)
    queryName <- sub('\\..*$', '', basename(filePath))
    run_query(url, queryName, query, output_gtf)
}

#' Compile GMQL query
#'
#' It compiles a GMQL query taken from file or inserted as text string, 
#' using the proper GMQL web service available on a remote server
#' 
#' 
#' @import httr
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param query string text of a GMQL query
#' @param filePath string path of txt file containing a GMQL query
#' 
#' @return None
#'
#' @examples
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement gets the query as text string and runs the compile 
#' ## web service
#' 
#' compile_query(remote_url, "DATASET = SELECT() Example_Dataset_1;
#'     MATERIALIZE DATASET INTO RESULT_DS;")
#' 
#' 
#' ## This statement defines the path to the file "query1.txt" in the 
#' ## subdirectory "example" of the package "RGMQL" and run the compile 
#' ## web service
#' 
#' test_path <- system.file("example", package = "RGMQL")
#' test_query <- file.path(test_path, "query1.txt")
#' compile_query_fromfile(remote_url, test_query)
#' 
#' ## Logout from GMQL REST services suite
#' 
#' logout_gmql(remote_url)
#' 
#' @name compile_query
#' @rdname compile_query
#' @export
#'
compile_query <- function(url, query) {
    authToken = GMQL_credentials$authToken
    h <- c(
        'Accept' = "Application/json",
        'Content-Type' = 'text/plain',
        'X-Auth-Token' = authToken
    )
    req <- httr::GET(url)
    url <- sub("/*[/]$","",req$url)
    URL <- paste0(url, "/queries/compile")
    req <- httr::POST(
        URL, 
        body = query ,
        httr::add_headers(h), 
        encode = "json"
    )
    content <- httr::content(req, "parsed")
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#' @name compile_query
#' @rdname compile_query
#' @export
#'
compile_query_fromfile <- function(url ,filePath) {
    if (!file.exists(filePath)) {
        stop("file does not exist")
    }
    
    query <- readLines(filePath)
    compile_query(url, query)
}

#' Stop a job
#'
#' It stops a specific current query job
#' 
#' @import httr
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param job_id string id of the job
#'
#' @return None
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ## Login to GMQL REST services suite at remote url
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement shows all jobs at GMQL remote system and selects one 
#' ## running job, saving it into 'jobs_1' (in this case is the first of the 
#' ## list), and then stop it
#' 
#' list_jobs <- show_jobs_list(remote_url)
#' jobs_1 <- list_jobs$jobs[[1]]
#' stop_job(remote_url, jobs_1)
#' }
#' 
#' @name stop_job
#' @rdname stop_job
#' @export
#'
stop_job <- function(url, job_id) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/jobs/", job_id, "/stop")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept' = 'text/plain')
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    if (req$status_code != 200) {
        stop(content)
    } else {
        print(content)
    }
}

#' Show a job log or trace
#'
#' It shows a job log or traces a specific job
#'
#' @import httr
#' @param url string url of server: It must contain the server address
#' and base url; service name is added automatically
#' @param job_id string id of the job
#'
#' @return Log or trace text
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## List all jobs
#' list_jobs <- show_jobs_list(remote_url)
#' 
#' \dontrun{
#' jobs_1 <- list_jobs$jobs[[1]]
#' 
#' ## Show jobs_1 log
#' show_job_log(remote_url, jobs_1)
#' 
#' ## Trace jobs_1
#' trace_job(remote_url, jobs_1)
#' 
#' }
#' 
#' @name log_job
#' @rdname log_job
#' @export
#'
show_job_log <- function(url, job_id) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/jobs/", job_id, "/log")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept' = 'Application/json')
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        print(unlist(content, use.names = FALSE))
    }
}



#' @import httr
#' 
#' @name trace_job
#' @rdname log_job
#' @export
#'
trace_job <- function(url, job_id) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/jobs/", job_id, "/trace")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept' = 'Application/json')
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#' Show all jobs
#'
#' It shows all jobs (run, succeded or failed) invoked by the user on remote 
#' server using, the proper GMQL web service available on a remote server
#' 
#' @import httr
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#'
#' @return List of jobs. Every job in the list is described by:
#' \itemize{
#' \item{id: unique job identifier}
#' }
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## List all jobs
#' list_jobs <- show_jobs_list(remote_url)
#' 
#' @rdname show_jobs_list
#' @name show_jobs_list
#' @export
#' 
show_jobs_list <- function(url) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/jobs")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#############################
#       WEB DATASET        #
############################

#' Show datasets
#'
#' It shows all GMQL datasets stored by the user or public in remote 
#' repository, using the proper GMQL web service available on a remote server
#' 
#' @import httr
#' @param url single string url of server: It must contain the server address 
#' and base url; service name is added automatically
#'
#' @return List of datasets. Every dataset in the list is described by:
#' \itemize{
#' \item{name: name of dataset}
#' \item{owner: public or name of the user}
#' }
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## List all datasets
#' 
#' list <- show_datasets_list(remote_url)
#' 
#' @name show_datasets_list
#' @rdname show_dataset
#' @export
#'
show_datasets_list <- function(url) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/datasets")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed") #JSON
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}


#' Show dataset samples
#'
#' It show all samples from a specific GMQL dataset on remote repository, 
#' using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName name of dataset containing the samples whose list we 
#' like to get; if the dataset is a public dataset, we have to add "public." 
#' as prefix, as shown in the example below, otherwise no prefix is needed
#'
#' @return List of samples in dataset. Every sample in the list is described 
#' by:
#' \itemize{
#' \item{id: id of sample}
#' \item{name: name of sample}
#' \item{path: sample repository path}
#' }
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement shows all samples present into public dataset 
#' ## 'Example_Dataset_1'
#' 
#' list <- show_samples_list(remote_url, "public.Example_Dataset_1")
#' 
#' @name show_samples_list
#' @rdname show_samples_list
#' @export
show_samples_list <- function(url, datasetName) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/datasets/", datasetName)
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#' Show dataset schema
#'
#' It shows the region attribute schema of a specific GMQL dataset on remote 
#' repository, using the proper GMQL web service available on a remote server
#' 
#' @import httr
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName name of dataset to get the schema;
#' if the dataset is a public dataset, we have to add "public." as prefix, 
#' as shown in the example below, otherwise no prefix is needed
#'
#' @return List of region schema fields. Every field in the list is described 
#' by:
#' \itemize{
#' \item{name: name of field (e.g. chr, start, end, strand, ...)}
#' \item{fieldType: (e.g. STRING, DOUBLE, ...)}
#' }
#'
#' @details
#' If error occurs, a specific error is printed
#' 
#' @examples
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## Show schema of public dataset 'Example_Dataset_1'
#' 
#' list <- show_schema(remote_url, "public.Example_Dataset_1")
#' 
#' @name show_schema
#' @rdname show_schema
#' @export
#'
show_schema <- function(url, datasetName) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/datasets/", datasetName, "/schema")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken)
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed")
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}

#' Upload dataset
#'
#'
#' It uploads a folder (GMQL or not) containing sample files using 
#' the proper GMQL web service available on a remote server: 
#' a new dataset is created on remote repository
#' 
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName name of dataset to create in repository
#' @param folderPath string local path to the folder containing the samples 
#' files
#' @param schemaName string name of schema used to parse the samples;
#' schemaName available are:
#' \itemize{
#' \item{NARROWPEAK}
#' \item{BROADPEAK}
#' \item{VCF}
#' \item{BED}
#' \item{BEDGRAPH}
#' }
#' if schemaName is NULL, it looks for a XML schema file to read in the 
#' folderPath
#'
#' @return None
#'
#' @details
#' If no error occurs, it prints "Upload Complete", otherwise a specific error 
#' is printed
#' 
#' NOTE: 
#' The folder layout must obey the following rules and adopt 
#' the following layout:
#' The dataset folder can have any name, but must contains the 
#' sub-folders named: "files".
#' The sub-folder "files" contains the dataset files and 
#' the schema xml file.
#' The schema files adopt the following the naming conventions:
#' 
#' - "schema.xml"
#' - "test.schema"
#' 
#' The names must be in LOWERCASE. Any other schema file 
#' will not be conisdered, if both are present, "test.schema" will be used. 
#' 
#' @examples
#'
#' \dontrun{
#' 
#' ## This statement defines the path to the folder "DATASET_GDM" in the 
#' ## subdirectory "example" of the package "RGMQL"
#' 
#' test_path <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' 
#' ## Login to GMQL REST services suite at remote url
#' 
#' remote_url <- "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## Upload of GMQL dataset with "dataset1" as name, without specifying any 
#' ## schema 
#' 
#' upload_dataset(remote_url, "dataset1", folderPath = test_path)
#' 
#' }
#' @name upload_dataset
#' @rdname upload_dataset
#' @export
#'
upload_dataset <- function(
    url,
    datasetName,
    folderPath,
    schemaName = NULL
) {
    folderPath <- sub("/*[/]$","",folderPath)
    if(basename(folderPath) !="files")
        folderPath <- file.path(folderPath,"files")
    
    files <- list.files(folderPath, pattern = "*(.gtf|.gdm)", full.names = TRUE)
    if (!length(files)) {
        stop("no files present")
    }
    
    count = .counter(0)
    
    list_files <- lapply(files, function(x) {
        file <- httr::upload_file(x)
    })
    
    list_files_names <- vapply(list_files, function(x) {
        paste0("file", count())
    }, character(1))
    
    names(list_files) <- list_files_names
    req <- httr::GET(url)
    real_URL <- sub("/*[/]$","",req$url)
    URL <- paste0(real_URL, "/datasets/", datasetName, "/uploadSample")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept:' = 'Application/json')
    
    schema_name <- tolower(schemaName)
    
    if (is.null(schemaName) || identical(schema_name, "customparser")) {
        schema <- .retrieve_schema(folderPath)
        
        list_files <- list(
            list("schema" = httr::upload_file(schema)),
            list_files
        )
        list_files <- unlist(list_files, recursive = FALSE)
        URL <- paste0(real_URL, "/datasets/", datasetName, "/uploadSample")
    } else {
        schemaList <- c(
            "narrowpeak",
            "vcf",
            "broadpeak",
            "bed",
            "bedgraph"
        )
        if (!schema_name %in% schemaList) {
            stop("schema not admissable")
        }
        
        URL <- paste0(
            real_URL,
            "/datasets/",
            datasetName,
            "/uploadSample?schemaName=",
            schema_name
        )
    }
    
    req <- httr::POST(URL, body = list_files , httr::add_headers(h))
    content <- httr::content(req)
    if (req$status_code != 200)
        stop(content)
    else
        print("upload Complete")
}

#' Delete dataset
#'
#' It deletes single private dataset specified by name from remote repository 
#' using the proper GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName string name of dataset to delete
#'
#' @return None
#' 
#' @details
#' If no error occurs, it prints "Deleted Dataset", otherwise a specific error 
#' is printed
#' 
#' @examples
#'
#' \dontrun{
#' 
#' ## This dataset does not exist
#' 
#' remote_url <- "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' delete_dataset(remote_url, "test1_20170604_180908_RESULT_DS")
#' 
#' }
#' 
#' @name delete_dataset
#' @rdname delete_dataset
#' @export
#'
delete_dataset <- function(url, datasetName) {
    req <- httr::GET(url)
    real_URL <- sub("/*[/]$","",req$url)
    URL <- paste0(real_URL, "/datasets/", datasetName)
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept:' = 'application/json')
    req <- httr::DELETE(URL, httr::add_headers(h))
    content <- httr::content(req, "parsed") #JSON
    
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        print(content$result)
    }
}

#' Download Dataset
#'
#' It donwloads private dataset as zip file from remote repository to local 
#' path, or donwloads and saves it into R environment as GRangesList, using 
#' the proper GMQL web service available on a remote server
#' 
#' @import httr
#' @importFrom utils unzip
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName string name of dataset to download
#' @param path string local path folder where to store dataset,
#' by default it is R working directory
#' @return None
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#'
#' ## Download dataset in R working directory
#' ## In this case we try to download a dataset of the user 
#' ## (public datasets from remote repository cannot be downloaded)
#' 
#' \dontrun{
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' download_dataset(remote_url, "Example_Dataset_1", path = getwd())
#' 
#' ## Create GRangesList from user dataset Example_Dataset1 got 
#' ## from repository
#' 
#' download_as_GRangesList(remote_url, "Example_Dataset_1")
#' }
#' 
#' @name download_dataset
#' @rdname download_dataset
#' @export
#'
download_dataset <- function(url, datasetName, path = getwd()) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/datasets/", datasetName, "/zip")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accept' = 'application/zip')
    req <- httr::GET(URL, httr::add_headers(h))
    
    content <- httr::content(req)
    if (req$status_code != 200) {
        stop(content)
    } else {
        zip_path <- file.path(path, paste0(datasetName, ".zip"))
        dir_out <- file.path(path, "")
        writeBin(content, zip_path)
        unzip(zip_path, exdir = dir_out)
        print("Download Complete")
    }
}

#' @import httr
#' @importClassesFrom GenomicRanges GRangesList
#' @importFrom S4Vectors metadata
#' 
#' @return GRangesList containing all GMQL samples in dataset
#' 
#' @name download_as_GRangesList
#' @rdname download_dataset
#' @export
#'
download_as_GRangesList <- function(url,datasetName) {
    list <- show_samples_list(url, datasetName)
    samples <- list$samples
    sample_list_name <- vapply(samples, function(x) x$name, character(1))
    
    sampleList <- lapply(samples, function(x) {
        name <- x$name
        range <- sample_region(url, datasetName, name)
    })
    
    names(sampleList) <- sample_list_name
    gRange_list <- GenomicRanges::GRangesList(sampleList)
    
    meta_list <- lapply(samples, function(x) {
        name <- x$name
        meta <- sample_metadata(url, datasetName, name)
    })
    names(meta_list) <- sample_list_name
    S4Vectors::metadata(gRange_list) <- meta_list
    return(gRange_list)
}

#' Show metadata list from dataset sample
#'
#' It retrieves metadata of a specific sample in dataset using the proper 
#' GMQL web service available on a remote server
#' 
#' @import httr
#'
#' @param url string url of server: It must contain the server address 
#' and base url; service name is added automatically
#' @param datasetName string name of dataset of interest
#' @param sampleName string name of sample of interest
#'
#' @return List of metadata in the form 'key = value'
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement retrieves metadata of sample 'S_00000' from public 
#' ## dataset 'Example_Dataset_1'
#' 
#' sample_metadata(remote_url, "public.Example_Dataset_1", "S_00000")
#' 
#'
#' @name sample_metadata
#' @rdname sample_metadata
#' @export
#'
sample_metadata <- function(url, datasetName,sampleName) {
    url <- sub("/*[/]$", "", url)
    URL <- paste0(url, "/datasets/", datasetName, "/", sampleName, "/metadata")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, 'text', encoding = "UTF-8")
    
    #trasform text to list
    metadata <- strsplit(content, "\n")
    metadata <- strsplit(unlist(metadata), "\t")
    names(metadata) <- vapply(metadata, `[[`, character(1), 1)
    listMeta <- lapply(metadata, `[`,-1)
    
    if (req$status_code != 200) {
        stop(content)
    } else {
        return(listMeta)
    }
}

#' Show regions data from a dataset sample
#' 
#' It retrieves regions data of a specific sample (whose name is specified in 
#' the parameter "sampleName") in a specific dataset (whose name is specified 
#' in the parameter "datasetName") using the proper GMQL web service 
#' available on a remote server
#' 
#' @import httr
#' @importFrom rtracklayer import
#' @importFrom data.table fread
#' @importFrom GenomicRanges makeGRangesFromDataFrame
#' @importFrom utils write.table
#'
#' @param url string url of server. It must contain the server address
#' and base url; service name is added automatically
#' @param datasetName string name of dataset of interest
#' @param sampleName string name of sample of interest
#'
#' @return GRanges data containing regions of sample
#'
#' @details
#' If error occurs, a specific error is printed
#'
#' @examples
#' 
#' \dontrun{
#' 
#' ## Login to GMQL REST services suite as guest
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' 
#' ## This statement retrieves regions data of sample "S_00000" from public 
#' ## dataset "Example_Dataset_1"
#'  
#' sample_region(remote_url, "public.Example_Dataset_1", "S_00000")
#' 
#' }
#' 
#' @name sample_region
#' @rdname sample_region
#' @export
#'
sample_region <- function(url, datasetName,sampleName) {
    url <- sub("/*[/]$","",url)
    URL <- paste0(url,"/datasets/",datasetName,"/",sampleName,"/region")
    authToken = GMQL_credentials$authToken
    h <- c('X-Auth-Token' = authToken, 'Accpet' = 'text/plain')
    req <- httr::GET(URL, httr::add_headers(h))
    content <- httr::content(req, 'parsed',encoding = "UTF-8")
    
    if(req$status_code != 200) {
        stop(content)
    } else {
        list <- show_schema(url,datasetName)
        schema_type <- list$type
        
        temp <- tempfile("temp") #use temporary files
        write.table(
            content,
            temp,
            quote = FALSE,
            sep = '\t',
            col.names = FALSE,
            row.names = FALSE
        )
        vector_field <- vapply(
            list$fields, function(x) x$name, character(1)
        )
        if (identical(schema_type, "gtf")) {
            attr_col_names <- vector_field[
              !vector_field %in% c(
                "seqname", "seqid", "start", "end", "strand"
              )]
            samples <- rtracklayer::import(
                temp, 
                format = "gtf",
                colnames = attr_col_names
            )
        } else {
            df <- data.table::fread(temp, header = FALSE, sep = "\t")
            a <- df[1, 2]
            if(is.na(as.numeric(a)))
                df <- df[-1]
            data.table::setnames(df,vector_field)
            samples <- GenomicRanges::makeGRangesFromDataFrame(
                df,
                keep.extra.columns = TRUE,
                start.field = "left",
                end.field = "right",
                strand.field="strand"
            )
        }
        unlink(temp)
        return(samples)
    }
}

#############################
#        WEB UTILS         #
############################

# no export
serialize_query <- function(url,output_gtf,base64) {
    if(output_gtf) {
        out <- "gtf"
    } else {
        out <- "tab"
    }
    url <- sub("/*[/]$","",url)
    req <- httr::GET(url)
    real_URL <- sub("/*[/]$","",req$url)
    authToken = GMQL_credentials$authToken
    URL <- paste0(real_URL,"/queries/dag/",out)
    h <- c(
        'Accept' = "Application/json",
        'Content-Type' = 'text/plain',
        'X-Auth-Token' = authToken
    )
    
    req <- httr::POST(URL,body = base64 ,httr::add_headers(h),encode = "json")
    content <- httr::content(req,"parsed")
    if (req$status_code != 200) {
        stop(content$error)
    } else {
        return(content)
    }
}


.retrieve_schema <- function(folderPath, duringReading = FALSE) {
    schema_SCHEMA <- list.files(
        folderPath, pattern = "test.schema$", full.names = TRUE
    )
    
    xml_schema <- list.files(
        folderPath, pattern = "schema.xml$", full.names = TRUE
    )
    
    if(!length(schema_SCHEMA) && !length(xml_schema))
        stop("schema not present")
    
    schema <- if(!length(schema_SCHEMA)) 
        xml_schema 
    else
        if(!duringReading) {
            schema_SCHEMA
        } else {
            folderPath
        }
    
    schema
}

.metadata_matrix <- function(url, datasetName) {
  url <- sub("/*[/]$", "", url)
  URL <- paste0(url, "/metadata/", datasetName, "/", "dataset/matrix")
  authToken = GMQL_credentials$authToken
  h <- c(
      'X-Auth-Token' = authToken, 
      'Accpet' = 'application/json',
      'Content-Type' = 'application/json')
  req <- httr::POST(
      URL, 
      body = '{"attributes": []}' ,
      httr::add_headers(h), 
      encode = "json"
  )
  content <- httr::content(req,"parsed")

  if (req$status_code != 200) {
    stop(content)
  } else {
    return(content)
  }
}
