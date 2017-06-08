
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

# aggregates factory
.aggregates <- function(metadata,class)
{
  if(!is.list(metadata))
    stop("metadata must be a list")

  if(!all(sapply(metadata, function(x) is(x,class))))
    stop("All elements should be META_OPERATOR object")

  names <- names(metadata)
  if(is.null(names))
  {
    warning("You did not assign a names to a list.\nWe build names for you")
    names <- sapply(metadata, take_value.META_OPERATOR)
  }
  else
  {
    if("" %in% names)
      stop("No partial names assignment is allowed")
  }
  aggregate_matrix <- t(sapply(metadata, function(x) {

    new_value = as.character(x)
    matrix <- matrix(new_value)

  }))
  m_names <- matrix(names)
  metadata_matrix <- cbind(m_names,aggregate_matrix)
}


#meta join condition
.join_condition <- function(conditions)
{
  if(is.list(conditions))
  {
    join_condition_matrix <- t(sapply(conditions, function(x) {
      new_value = as.character(x)
      if(length(new_value)==1)
        new_value = c("DEF",new_value)
      else if(!identical("DEF",new_value[1]) && !identical("FULL",new_value[1]) && !identical("EXACT",new_value[1]))
        stop("no valid condition")
      matrix <- matrix(new_value)
    }))
  }
  else if(is.character(conditions))
  {
    conditions = conditions[!conditions %in% ""]
    conditions = conditions[!duplicated(conditions)]
    if(length(conditions)<=0)
      join_condition_matrix <- NULL
    else
    {
      join_condition_matrix <- t(sapply(conditions, function(x) {
        new_value = c("DEF",x)
        matrix <- matrix(new_value)
      }))
    }
  }
  else
    stop("only list or character")
}

#predicate
.check_predicate <- function(predicate_string)
{
  if(!is.character(predicate_string))
    stop("no valid predicate")

  if(length(predicate_string)>1)
    stop("no multiple string")
}



