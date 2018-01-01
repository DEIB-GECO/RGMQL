filter.GMQLDateset <- function(.data, m_predicate = NULL, r_predicate = NULL, 
                                    semijoin = NULL)
{
    val <- value(.data)
    meta_pred <- substitute(m_predicate)
    if(!is.null(meta_pred))
    {
        predicate <- .trasform(deparse(meta_pred))
        predicate <- paste(predicate,collapse = "")
        predicate <- as.character(glue::glue(predicate))
        
    }
    else
        predicate <- .jnull("java/lang/String")
    
    reg_pred <- substitute(r_predicate)
    if(!is.null(reg_pred))
    {
        region_predicate <- .trasform(deparse(reg_pred))
        region_predicate <- paste(region_predicate,collapse = "")
        region_predicate <- as.character(glue::glue(region_predicate))
        
    }
    else
        region_predicate <- .jnull("java/lang/String")
    
    gmql_select(val, predicate, region_predicate, semijoin)
}

#' Method filter
#' 
#' @description Wrapper to GMQL SELECT operator 
#' @description It creates a new dataset from an existing one by extracting a 
#' subset of samples and/or regions from the input dataset according to their 
#' predicate. Each sample in the output dataset has the same region attributes, 
#' values, and metadata as in the input dataset.
#' When semijoin function is defined, it extracts those samples containing 
#' all metadata attributes defined in semijoin clause with at least 
#' one metadata value in common with semijoin dataset.
#' If no metadata in common between input dataset and semijoin dataset, 
#' no sample is extracted.
#'
#' @importFrom rJava J .jnull .jarray
#' @importFrom methods isClass
#' @importFrom glue glue
#' @importFrom dplyr filter
#' 
#' @param .data GMQLDataset class object
#' @param m_predicate logical predicate made up by R logical operations 
#' on metadata attributes. 
#' Only !, |, ||, &, && are admitted.
#' @param r_predicate logical predicate made up by R logical operations 
#' on region attributes. 
#' Only !, |, ||, &, && are admitted.
#' 
#' @param semijoin \code{\link{semijoin}} function to define filter method 
#' with semijoin condition (see examples).
#' 
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" in the subdirectory "example" 
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "data"
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data <- read_dataset(test_path) 
#' 
#' ## This statement selects from input data samples of patients younger 
#' ## than 70 years old, based on filtering on sample metadata attribute 
#' ## 'patient_age'
#' 
#' filter_data <- filter(data, patient_age < 70)
#' 
#' ## This statement defines the path to the folders "DATASET_GDM" in the 
#' ## subdirectory "example" of the package "RGMQL" and opens such folder 
#' ## as a GMQL dataset named "join_data"
#' 
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' join_data <- read_dataset(test_path2) 
#' 
#' ## This statement creates a new dataset called 'jun_tf' by selecting those 
#' ## samples and their regions from the existing 'data' dataset such that:
#' ## Each output sample has a metadata attribute called antibody_target 
#' ## with value JUN.
#' ## Each output sample also has not a metadata attribute called "cell" 
#' ## that has the same value of at least one of the values that a metadata 
#' ## attribute equally called cell has in at least one sample 
#' ## of the 'join_data' dataset.
#' ## For each sample satisfying previous conditions, only its regions that 
#' ## have a region attribute called pValue with the associated value 
#' ## less than 0.01 are conserved in output
#' 
#' jun_tf <- filter(data, antibody_target == "JUN", pValue < 0.01, 
#' semijoin(join_data, TRUE, conds("cell")))
#' 
#' 
#' @name filter
#' @rdname filter
#' @aliases filter,GMQLDataset-method
#' @aliases filter-method
#' @export
setMethod("filter", "GMQLDataset", filter.GMQLDateset)

gmql_select <- function(input_data, predicate, region_predicate, s_join)
{
    if(!is.null(s_join))
    {
        if("semijoin" %in% names(s_join))
            semijoin_data <- s_join$semijoin
        else
            stop("use function semijoin()")
    }
    else
        semijoin_data <- .jnull("java/lang/String")
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$select(predicate,region_predicate, semijoin_data, 
                                    input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error)
        stop(data)
    else
        GMQLDataset(data)
        
}

#' Semijoin condtion
#' 
#' This function is used as support to the filter method to define 
#' semijoin conditions on metadata  
#' 
#' 
#' @param .data GMQLDataset class object
#' 
#' @param not_in logical value: TRUE => for a given sample of input dataset
#' ".data" in \code{\link{filter}} method if and only if there exists at 
#' least one sample in dataset 'data' with metadata attributes defined 
#' in groupBy and these attributes of 'data' have at least one value in 
#' common with the same attributes defined in one sample of '.data'
#' FALSE => semijoin condition is evaluated accordingly.
#' 
#' @param groupBy list of evalation functions to define evaluation on metadata:
#' \itemize{
#' \item{ \code{\link{FN}}(value): Fullname evaluation, two attributes match 
#' if they both end with \emph{value} and, if they have further prefixes,
#' the two prefix sequence are identical.}
#' \item{ \code{\link{EX}}(value): Exact evaluation, only attributes exactly 
#' as \emph{value} match; no further prefixes are allowed.}
#' \item{ \code{\link{DF}}(value): Default evaluation, the two attributes match 
#' if both end with \emph{value}.}
#' }
#' 
#' @examples
#' 
#' ## These statements initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folders "DATASET" and "DATASET_GDM" in the subdirectory 
#' ## "example" of the package "RGMQL" and opens such folders as a GMQL dataset 
#' ## named "data" and "join_data" respectively
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data <- read_dataset(test_path)
#' join_data <-  read_dataset(test_path2)
#' 
#' # It creates a new dataset called 'jun_tf' by selecting those samples and 
#' # their regions from the existing 'data' dataset such that:
#' # Each output sample has a metadata attribute called antibody_target 
#' # with value JUN.
#' # Each output sample also has not a metadata attribute called "cell" 
#' # that has the same value of at least one of the values that a metadata 
#' # attribute equally called cell has in at least one sample 
#' # of the 'join_data' dataset.
#' # For each sample satisfying previous conditions, only its regions that 
#' # have a region attribute called pValue with the associated value 
#' # less than 0.01 are conserved in output
#' 
#' jun_tf <- filter(data, antibody_target == "JUN", pValue < 0.01, 
#' semijoin(join_data, TRUE, conds("cell")))
#' 
#' @return semijoin condition as list
#' @export
#' 
semijoin <- function(.data, not_in = FALSE, groupBy)
{
    if(!is.null(groupBy))
    {
        if("condition" %in% names(groupBy))
        {
            cond <- .join_condition(groupBy)
            if(is.null(cond))
                stop("groupBy cannot be NULL")
        }
        else
            stop("use function conds()")
    }
    else
        stop("groupBy cannot be NULL")
    
    if(is.null(.data))
        stop(".data cannot be NULL")
    
    if(!isClass("GMQLDataset", .data))
        stop("data: Must be a GMQLDataset object") 
    
    .check_logical(not_in)
    ptr_data <- value(.data)
    data_cond <- cbind(ptr_data,not_in)
    all_conds <- rbind(data_cond,cond)
    join_condition_matrix <- .jarray(all_conds, dispatch = TRUE)
    
    semijoin <- list("semijoin" = join_condition_matrix)
}


.trasform <- function(predicate)
{
    predicate <- gsub("&|&&","AND",predicate)
    predicate <- gsub("\\||\\|\\|","OR",predicate)
    #predicate <- gsub("![\\(]+","NOT(",predicate)
}