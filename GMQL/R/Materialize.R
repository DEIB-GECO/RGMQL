#' GMQL Function: EXECUTE
#'
#' execute GMQL query.
#' The function works only after invoking at least one materialize
#'
#' @examples
#'
#' \dontrun{
#' startGMQL()
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = "/.../foldername")
#' materialize(s,"/.../foldername")
#' execute()
#' }
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
#' It saves the contents of a dataset that contains samples metadata and samples regions.
#' It is normally used to persist the contents of any dataset generated during a GMQL query.
#' Any dataset can be materialized, but the operation can be very time-consuming.
#' For best performance, materialize the relevant data only.
#'
#'
#' @param input_data "url-like" string taken from GMQL function
#' @param dir_out destination folder path.
#' by default the working directory is set by R environment
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' \dontrun{
#' startGMQL()
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = "/.../foldername")
#' materialize(s,"/.../foldername")
#' }
materialize <- function(input_data, dir_out = getwd())
{
  out <- frappeR$materialize(input_data,dir_out)
  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    NULL
}
