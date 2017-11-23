#' Method filter
#' 
#' It creates a new dataset from an existing one by extracting a subset of 
#' samples and/or regions from the input dataset according to their predicate.
#' each sample in the output dataset has the same region attributes, 
#' values, and metadata as in the input dataset.
#' When semijoin function is defined, it extracts those samples containing 
#' all metadata attribute defined in semijoin clause with at least 
#' one metadata value in common with semijoin dataset.
#' If no metadata in common between input dataset and semijoin dataset, 
#' no sample is extracted.
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' @importFrom methods isClass
#' 
#' @param .data GMQLDataset class object
#' @param m_predicate logical predicate made up by R logical operation 
#' on metadata attribute. 
#' Only !, |, ||, &, && are admitted.
#' @param r_predicate logical predicate made up by R logical operation 
#' on schema region values. 
#' Only !, |, ||, &, && are admitted.
#' @param ... Additional arguments for use in specific methods.
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
#' ## It selects from input data samples of patients younger than 70 years old, 
#' ## based on filtering on sample metadata attribute Patient_age
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' input <- read_dataset(test_path)
#' s <- filter(input, Patient_age < 70)
#' 
#' \dontrun{
#' 
#' # It creates a new dataset called 'jun_tf' by selecting those samples and 
#' # their regions from the existing 'data' dataset such that:
#' # Each output sample has a metadata attribute called antibody_target 
#' # with value JUN.
#' # Each output sample also has not a metadata attribute called "cell" 
#' # that has the same value of at least one of the values that a metadata 
#' # attribute equally called cell has in at least one sample 
#' # of the 'join_data' dataset.
#' # For each sample satisfying previous condition,only its regions that 
#' # have a region attribute called pValue with the associated value 
#' # less than 0.01 are conserved in output
#' 
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data <- read_dataset(test_path)
#' join_data <-  read_dataset(test_path2)
#' jun_tf <- filter(data, antibody_target == 'JUN', pValue < 0.01, 
#' semijoin(join_data, TRUE, DF("cell")))
#' 
#' }
#' 
#' @aliases filter filter-method
#' @export
setMethod("filter", "GMQLDataset",
            function(.data, m_predicate = NULL, r_predicate = NULL, 
                        semijoin = NULL, ...)
            {
                val <- .data@value
                meta_pred <- substitute(m_predicate)
                if(!is.null(meta_pred))
                {
                    predicate <- .trasform(deparse(meta_pred))
                    predicate <- paste(predicate,collapse = "")
                }
                else
                    predicate <- .jnull("java/lang/String")
                
                reg_pred <- substitute(r_predicate)
                if(!is.null(reg_pred))
                {
                    region_predicate <- .trasform(deparse(reg_pred))
                    region_predicate <- paste(region_predicate,collapse = "")
                }
                else
                    region_predicate <- .jnull("java/lang/String")

                gmql_select(val, predicate, region_predicate, semijoin)
            })


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
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
        
}

#' Semijoin Condtion
#' 
#' This function is use as support to filter method to define 
#' semijoin conditions on metadata  
#' 
#' @param data GMQLDataset class object
#' 
#' @param not_in logical value: T => semijoin is perfomed 
#' considering semi_join NOT IN semi_join_dataset, F => semijoin is performed 
#' considering semi_join IN semi_join_dataset
#' 
#' @param ... Additional arguments for use in specific methods.
#' It is also accpet a functions to define condition evaluation on metadata.
#' \itemize{
#' \item{\code{\link{FN}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EX}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' \item{\code{\link{DF}}: Default evaluation, the two attributes match 
#' if both end with value.}
#' }
#' 
#' @examples
#' 
#' # It creates a new dataset called 'jun_tf' by selecting those samples and 
#' # their regions from the existing 'data' dataset such that:
#' # Each output sample has a metadata attribute called antibody_target 
#' # with value JUN.
#' # Each output sample also has not a metadata attribute called "cell" 
#' # that has the same value of at least one of the values that a metadata 
#' # attribute equally called cell has in at least one sample 
#' # of the 'join_data' dataset.
#' # For each sample satisfying previous condition,only its regions that 
#' # have a region attribute called pValue with the associated value 
#' # less than 0.01 are conserved in output
#' 
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data <- read_dataset(test_path)
#' join_data <-  read_dataset(test_path2)
#' jun_tf <- filter(data,NULL,NULL, semijoin(join_data, TRUE, DF("cell")))
#' 
#' @return semijoin condition as list
#' @export
semijoin <- function(data, not_in = FALSE, ...)
{
    semij_cond = list(...)
    if(is.null(data))
        stop("data cannot be NULL")
    
    if(!isClass("GMQLDataset", data))
        stop("data: Must be a GMQLDataset object") 
    
    .check_logical(not_in)
    ptr_data <- data@value
    
    data_cond <- cbind(ptr_data,not_in)
    cond <- .join_condition(semij_cond)
    cond <- rbind(data_cond,cond)
    join_condition_matrix <- .jarray(cond, dispatch = TRUE)
    
    semijoin <- list("semijoin" = join_condition_matrix)
}


.trasform <- function(predicate=NULL)
{
    predicate <- gsub("&|&&","AND",predicate)
    predicate <- gsub("\\||\\|\\|","OR",predicate)
    #predicate <- gsub("![\\(]+","NOT(",predicate)
}