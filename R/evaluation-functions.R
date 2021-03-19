#############################
#       EVALUATION         #
############################

#' Condition evaluation functions
#'
#' This function is used to support joinBy and/or groupBy function parameter.
#'
#' @param default concatenation of string identifying a name of metadata 
#' attribute to be evaluated. 
#' It defines a DEFAULT evaluation of the input values. 
#' DEFAULT evaluation: the two attributes match if both end with value.
#' 
#' @param full concatenation of string identifying a name of metadata 
#' attribute to be evaluated.
#' It defines a FULL (FULLNAME) evaluation of the input values.
#' FULL evaluation: two attributes match if they both end with value and,
#' if they have further prefixes, the two prefix sequences are identical.
#' 
#' @param exact concatenation of string identifying a name of metadata 
#' attribute to be evaluated.
#' It defines a EXACT evaluation of the input values.
#' EXACT evaluation: only attributes exactly as value match; 
#' no further prefixes are allowed. 
#' 
#' @return list of 2-D array containing method of evaluation and metadata 
#' attribute name
#' 
#' 
#' @name Evaluation-Function
#' @aliases condition_evaluation
#' @rdname condition_eval_func
#' @export
conds <- function(default = c(""), full = c(""), exact = c("")) {
  df <- .condition("DEF",default)
  fn <- .condition("FULL",full)
  ex <- .condition("EXACT",exact)
  list("condition" = list("def" = df, "full" = fn, "exact" = ex))
}

.condition <- function(cond, array) {
  array = array[!array %in% ""]
  array = array[!duplicated(array)]
  
  if(!length(array))
    join_condition_matrix <- NULL
  else {
    join_condition_matrix <- t(vapply(array, function(x) {
      new_value = c(cond, x)
      matrix <- matrix(new_value)
    },character(2)))
  }
  join_condition_matrix
}
