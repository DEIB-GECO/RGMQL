#' GMQL Function: EXECUTE
#'
#' execute GMQL query
#'
#'
#' @examples
#' execute()
#'
execute <- function()
{
  out <- frappeR$execute()
  if(grepl("OK",out,ignore.case = T))
    print("Done")
  else
    stop(out)
}

#' GMQL Operation: MATERIALIZE
#'
#'The materialize operation saved dataset in the system to make it usable in other GMQL queries.
#'To preserve the content of any dataset generated during a GMQL query,
#'the dataset must be materialized.
#'Any dataset can be materialized, however the operation is time expensive;
#'for best performance, materialize the relevant data only.
#'
#'
#' @param input_data string pointer taken from GMQL function
#' @param dir_out out path folder for default is working directory
#' @examples
#' materialize(input_data = c, dir_out = "/.../foldername")
#' materialize(c,"/.../foldername")
#'
materialize <- function(input_data, dir_out)
{
  out <- frappeR$materialize(input_data,dir_out)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    NULL
}
