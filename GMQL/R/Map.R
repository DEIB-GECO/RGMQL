map <- function(aggregates = NULL,output,joinBy = NULL, right_input_data,left_input_data)
{





  out<-frappeR$map(aggregates,right_input_data,left_input_data)

  if(grepl("No",out,ignore.case = T))
    stop(out)
  else
    out
}
