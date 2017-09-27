#' GMQL Function: EXECUTE
#'
#' execute GMQL query.
#' The function works only after invoking at least one materialize
#'
#' @details 
#' 
#' After invoking execution function, all varialbe associated to DAG will be removed
#' from scala enviroment, although the associated R variable will remain stored in R environment
#' 
#' @return None
#'
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = readDataset(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = test_path)
#' execute()
#' 
#' @export
#'
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
#' @param input_data returned object from any GMQL function
#' @param dir_out destination folder path.
#' by default is current working directory of the R process
#'
#' @return None
#'
#' @references \url{http://www.bioinformatics.deib.polimi.it/genomic_computing/GMQL/doc/GMQLUserTutorial.pdf}
#'
#' @examples
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' materialize(input_data = m, dir_out = test_path)
#' 
#' @export
#'
materialize <- function(input_data, dir_out = getwd())
{
  out <- WrappeR$materialize(input_data$value,dir_out)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)
  else
    invisible(NULL)
}

#' GMQL Operation: TAKE
#'
#'
#' @import GenomicRanges
#'
#' @param input_data returned object from any GMQL function
#' @param rows number of rows that you want to retrieve and stored in memory
#' by default is 0 that means take all rows
#'
#' @return GrangesList with associated metadata
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' library(rscala)
#'
#' initGMQL("gtf")
#' test_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#' r = read(test_path)
#' s = select(input_data = r)
#' m = merge(groupBy = c("antibody_targer","cell_karyotype"),input_data = s)
#' take(input_data = m, rows = 45)
#' }
#' ""
#' @export
#'
take <- function(input_data, rows=0L)
{
  rows <- as.integer(rows[1])
  if(rows<0)
    stop("rows cannot be negative")

  out <- WrappeR$take(input_data$value,rows)
  if(grepl("No",out,ignore.case = TRUE))
    stop(out)

  reg <- WrappeR$get_reg()
  meta <- WrappeR$get_meta()
  schema <- WrappeR$get_schema()
  "taken"
}



