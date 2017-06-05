
#internal
.counter <- function(zero = 0)
{
  i <- zero
  function() {
    i <<- i + 1
    toString <- as.character(i)
  }
}

#move to internals
.add_metadata <- function(files)
{
  x <- scan(files, what="", sep="\n")
  y <- strsplit(x, "\t")
  names(y) <- sapply(y, `[[`, 1)
  listMeta <- lapply(y, `[`, -1)
}

#move to internals
.schema_header <- function(datasetName)
{
  schema_name <- list.files(datasetName, pattern = "*.schema$",full.names = TRUE)
  if(length(schema_name)==0)
    stop("schema not present")

  xml_schema <- xml2::read_xml(schema_name)
  list_field <- xml2::as_list(xml_schema)
  vector_field <- unlist(list_field)
}


