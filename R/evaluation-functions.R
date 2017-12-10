#############################
#       EVALUATION         #
############################

#' Condition evaluation functions
#'
#' These functions are used to support joinBy and/or groupBy function parameter.
#' They create a 2-D array made up by two coloumn:
#' type of condition evaluation and the metadata attribute name
#'
#' \itemize{
#' \item{FN: It defines a FULL (FULLNAME) evaluation of the input values.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical}
#' \item{EX: It defines a EXACT evaluation of the input values.
#' EXACT evaluation: only attributes exactly as value match; 
#' no further prefixes are allowed. }
#' \item{DF: It defines a DEFAULT evaluation of the input values.
#' DEFAULT evaluation: the two attributes match if both end with value.}
#' }
#'
#' @param ... series of string identifying a name of metadata attribute 
#' to be evaluated
#' 
#' @return 2-D array containing method of evaluation and metadata
#' 
#' @examples
#' 
#' "where is my example?"
#' 
#' @name Evaluation-Function
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

#' @name Evaluation-Function
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

#' @name Evaluation-Function
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

