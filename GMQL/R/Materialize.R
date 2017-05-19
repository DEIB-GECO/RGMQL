#' GMQL Function: EXECUTE
#'
#' execute GMQL query.
#' The function works only after invoking at least one materialize
#'
#' @examples
#'
#' startGMQL()
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = "/.../foldername")
#' materialize(s,"/.../foldername")
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
#' It saved the content of a dataset,whose name can be specified,
#' that contains samples metadata and samples regions.
#' To preserve the content of any dataset generated during a GMQL query,
#' the dataset must be materialized.
#' Any dataset can be materialized, however the operation is time expensive;
#' for best performance, materialize the relevant data only.
#'
#'
#' @param input_data string pointer taken from GMQL function
#' @param dir_out out path folder for default is working directory
#' @examples
#'
#' startGMQL()
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = "/.../foldername")
#' materialize(s,"/.../foldername")
#'
materialize <- function(input_data, dir_out)
{
  out <- frappeR$materialize(input_data,dir_out)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    NULL
}
