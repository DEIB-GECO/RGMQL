#' GMQL Operation: SELECT
#'
#' It returns all the samples satisfying the predicate on metadata.
#' If regions are specified, returns regions satisfying the predicate 
#' on regions.
#' If semijoin clauses are specified they are applied, too.
#' When semijoin is defined, it extracts those samples containing all metadata 
#' attribute defined in semijoin clause with at least one metadata value 
#' in common with semi join dataset.
#' If no metadata in common between input dataset and semi join dataset, 
#' no sample is extracted.
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#'
#' @param x GMQLDataset class object
#' @param predicate logical predicate made up by R logical operation 
#' on metadata attribute. 
#' Only !, |, ||, &, && are admitted.
#' @param region_predicate logical predicate made up by R logical operation 
#' on chema region values. 
#' Only !, |, ||, &, && are admitted.
#' @param semi_join list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @param semi_join_negation logical value: T => semijoin is perfomed 
#' considering semi_join NOT IN semi_join_dataset, F => semijoin is performed 
#' considering semi_join IN semi_join_dataset
#' 
#' @param semi_join_dataset GMQLDataset class object
#'
#' @return GMQLDataset class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @examples
#' 
#' ## It selects from input data samples of patients younger than 70 years old, 
#' ## based on filtering on sample metadata attribute Patient_age
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' input <- read_dataset(test_path)
#' s <- subset(input, Patient_age < 70)
#' 
#' 
#' \dontrun{
#' 
#' It creates a new dataset called 'jun_tf' by selecting those samples and 
#' their regions from the existing 'data' dataset such that:
#' Each output sample has a metadata attribute called antibody_target 
#' with value JUN.
#' Each output sample also has not a metadata attribute called cell 
#' that has the same value of at least one of the values that a metadata 
#' attribute equally called cell has in at least one sample 
#' of the 'join_data' dataset.
#' For each sample satisfying previous condition,only its regions that 
#' have a region attribute called pValue with the associated value 
#' less than 0.01 are conserved in output
#' 
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' test_path2 <- system.file("example", "DATASET_GDM", package = "RGMQL")
#' data <- read_dataset(test_path)
#' join_data <-  read_dataset(test_path2)
#' jun_tf <- filter(data, antibody_target == 'JUN', pValue < 0.01, c("cell"), 
#' TRUE, semi_join_dataset = join_data )
#' 
#' }
#' @name filter
#' @rdname filter-methods
#' @aliases filter, GMQLDataset-methods
#' @export
setMethod("filter", "GMQLDataset",
            function(data, m_predicate = NULL, r_predicate = NULL, 
                    semi_join = NULL, semi_join_negation = FALSE, 
                    semi_join_dataset = NULL)
            {
                val <- data@value
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
            
                gmql_select(val, predicate, region_predicate, 
                        semi_join, semi_join_negation, semi_join_dataset)
            })

gmql_select <- function(input_data, predicate = NULL, region_predicate = NULL, 
                    semi_join = NULL, semi_join_negation = FALSE, 
                    semi_join_dataset = NULL)
{
    if(is.null(semi_join) && is.null(semi_join_dataset))
    {
        join_condition_matrix <- .jnull("java/lang/String")
        semi_join_dataset <- .jnull("java/lang/String")
        semi_join_negation <- FALSE
    }
    else if(is.null(semi_join) || is.null(semi_join_dataset) ||
            is.null(semi_join_negation)) 
    {
        warning("All semijoin parameters have to be set.
Function will be invoked with these parameters as NULL")
        semi_join_dataset <- .jnull("java/lang/String")
        semi_join_negation <- FALSE
        join_condition_matrix <- .jnull("java/lang/String")
    }
    else
    {
        if(!isClass("GMQLDataset", semi_join_dataset))
            stop("semi_join_dataset: Must be a GMQLDataset object")
        
        semi_join_dataset <- semi_join_dataset@value
        .check_input(semi_join_dataset)
        .check_logical(semi_join_negation)
        join_condition_matrix <- .jarray(.join_condition(semi_join),
                                            dispatch = TRUE)
    }
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$select(predicate,region_predicate, 
                                join_condition_matrix, semi_join_dataset, 
                                semi_join_negation, input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
        
}

.trasform <- function(predicate=NULL)
{
    predicate <- gsub("&|&&","AND",predicate)
    predicate <- gsub("\\||\\|\\|","OR",predicate)
    #predicate <- gsub("![\\(]+","NOT(",predicate)
}