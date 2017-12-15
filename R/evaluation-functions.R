#############################
#       EVALUATION         #
############################

#' Condition evaluation functions
#'
#' These functions are used to support joinBy and/or groupBy function parameter.
#' They create a 2-D array made up by two coloumn:
#' type of condition evaluation and the metadata attribute name
#'
#'
#' @param default series of string identifying a name of metadata attribute 
#' to be evaluated.
#' It defines a DEFAULT evaluation of the input values.
#' DEFAULT evaluation: the two attributes match if both end with value.
#' 
#' @param full series of string identifying a name of metadata attribute 
#' to be evaluated.
#' It defines a FULL (FULLNAME) evaluation of the input values.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical.
#' 
#' @param exact series of string identifying a name of metadata attribute 
#' to be evaluated.
#' It defines a EXACT evaluation of the input values.
#' EXACT evaluation: only attributes exactly as value match; 
#' no further prefixes are allowed. 
#' 
#' @return list of 2-D array containing method of evaluation and metadata
#' 
#' @examples
#' 
#' "where is my example?"
#' 
#' @name Evaluation-Function
#' @aliases condition_evaluation
#' @rdname condition_eval_func
#' @export
condition_evaluation <- function(default = c(""), full = c(""), exact = c(""))
{
    df <- .condition("DEF",default)
    fn <- .condition("FULL",full)
    ex <- .condition("EXACT",exact)
    list("def" = df, "full" = fn, "exact" = ex)
}

.condition <- function(cond, array)
{
    array = array[!array %in% ""]
    array = array[!duplicated(array)]
    
    if(!length(array))
        join_condition_matrix <- NULL
    else
    {
        join_condition_matrix <- t(sapply(array, function(x) {
            new_value = c(cond, x)
            matrix <- matrix(new_value)
        }))
    }
    join_condition_matrix
}


