#############################
#       EVALUATION         #
############################

#' Condition evaluation functions
#'
#' These functions is used to support joinBy and/or groupBy function parameter.
#' It create a list of one element: matrix containing the two coloumn:
#' type of condition evaluation and the metadata attribute
#'
#' \itemize{
#' \item{FN: It defines a FULL (FULLNAME) evaluation of the input values.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical}
#' \item{EX: It defines a EXACT evaluation of the input values.
#' EXACT evaluation: only attributes exactly as value will match; 
#' no further prefixes are allowed. }
#' \item{DF: It defines a DEFAULT evaluation of the input values.
#' DEFAULT evaluation: the two attributes match if both end with value.}
#' }
#'
#' @param ... series of string identifying name of metadata attribute 
#' to be evaluated
#' 
#' @return list of 2-D array containing method of evaluation and metadata
#' 
#' @examples
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' r = read_dataset(test_path)
#' 
#' @name FN
#' @aliases FN
#' @rdname condition_eval_func
#' @export
FN <- function(...)
{
    conds <- c(...)
    conds = conds[!conds %in% ""]
    conds = conds[!duplicated(conds)]
    if(length(conds)<=0)
        join_condition_matrix <- .jnull("java/lang/String")
    else
    {
        join_condition_matrix <- t(sapply(conds, function(x) {
            new_value = c("FULL",x)
            matrix <- matrix(new_value)
        }))
    }
    join_condition_matrix
}

#' @name EX
#' @aliases EX
#' @rdname condition_eval_func
#' @export
EX <- function(...)
{
    conds <- c(...)
    conds = conds[!conds %in% ""]
    conds = conds[!duplicated(conds)]
    if(length(conds)<=0)
        join_condition_matrix <- .jnull("java/lang/String")
    else
    {
        join_condition_matrix <- t(sapply(conds, function(x) {
            new_value = c("EXACT",x)
            matrix <- matrix(new_value)
        }))
    }
    join_condition_matrix
}

#' @name DF
#' @aliases DF
#' @rdname condition_eval_func
#' @export
DF <- function(...)
{
    conds <- c(...)
    conds = conds[!conds %in% ""]
    conds = conds[!duplicated(conds)]
    if(length(conds)<=0)
        join_condition_matrix <- .jnull("java/lang/String")
    else
    {
        join_condition_matrix <- t(sapply(conds, function(x) {
            new_value = c("DEF",x)
            matrix <- matrix(new_value)
        }))
    }
    join_condition_matrix
}

