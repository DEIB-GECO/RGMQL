#' GMQL Function: EXECUTE
#'
#' execute GMQL query.
#' The function works only after invoking at least one materialize
#'
#' @examples
#'
#' \dontrun{
#'
#' initGMQL("gtf")
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
  out <- WrappeR$execute()
  if(grepl("OK",out,ignore.case = TRUE))
    print("Executed")
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
#'
#' initGMQL("gtf")
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = "/.../foldername")
#' materialize(s,"/.../foldername")
#' }
materialize <- function(input_data, dir_out = getwd())
{
  out <- WrappeR$materialize(input_data,dir_out)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    NULL
}

#' GMQL Operation: TAKE
#'
#'
#' @import GenomicRanges
#'
#' @param input_data "url-like" string taken from GMQL function
#' @param rows number of rows that you want to retrieve an stored in memory
#' by default is 0 that means all rows
#'
#' @return GrangesList
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' initGMQL("collect")
#' r = read(path)
#' r2 = read(path2)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' take(input_data = m, rows = 45)
#' }
take <- function(input_data, rows=0L)
{
  rows <- as.integer(rows[1])
  if(rows<0)
    stop("rows cannot be negative")

  out <- WrappeR$take(input_data,rows)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    NULL

  reg <- WrappeR$get_reg()
  meta <- WrappeR$get_meta()
  schema <- WrappeR$get_schema()

}



