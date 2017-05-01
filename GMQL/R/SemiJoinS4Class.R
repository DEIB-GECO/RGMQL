

#S4 class SemiJoinParam
setClass(
  # Set the name for the class
  "SemiJoinParam",

  # Define the slots
  slots = c(
    semiJoinMeta = "character",
    operation_in  = "logical",
    dataset_path_join_IN = "character"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    semiJoinMeta = "",
    operation_in  = FALSE,
    dataset_path_join_IN = ""
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    return(TRUE)
  }
)

