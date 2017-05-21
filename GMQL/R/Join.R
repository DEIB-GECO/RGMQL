join <- function(genometric_predicate = NULL, output=NULL, joinBy = NULL, right_input_data, left_input_data)
{
  out <- frappeR$join()
  if(grepl("No",out,ignore.case = T) || grepl("expected",out,ignore.case = T))
    stop(out)
  else
    out
}
