
.counter <- function(zero = 0) {
  i <- zero
  function() {
    i <<- i + 1
    toString <- as.character(i)
  }
}

.add_metadata <- function(files) {
  x <- scan(files, what="", sep="\n")
  y <- strsplit(x, "\t")
  names(y) <- vapply(y, `[[`,character(1), 1)
  listMeta <- lapply(y, `[`, -1)
}

.schema_header <- function(datasetName) {
  schema_name <- list.files(
    datasetName, 
    pattern = "*.schema$",
    full.names = TRUE)
  
  schema_name_xml <- list.files(
    datasetName, 
    pattern = "*.xml$",
    full.names = TRUE)
  
  if(!length(schema_name) && !length(schema_name_xml))
    stop("schema not present")
  
  if(!length(schema_name))
    xml_schema <- xml2::read_xml(schema_name_xml)
  else
    xml_schema <- xml2::read_xml(schema_name)
  
  list_field <- xml2::as_list(xml_schema)
  vector_field <- unlist(list_field)
}

.schema_type_coordinate <- function(datasetName) {
  schema_name <- list.files(
    datasetName, 
    pattern = "*.schema$",
    full.names = TRUE)
  
  schema_name_xml <- list.files(
    datasetName, 
    pattern = "*.xml$",
    full.names = TRUE)
  
  if(!length(schema_name) && !length(schema_name_xml))
    stop("schema not present")
  
  if(!length(schema_name))
    xml_schema <- xml2::read_xml(schema_name_xml)
  else
    xml_schema <- xml2::read_xml(schema_name)
  
  gmql_schema_tag <- xml2::xml_children(xml_schema)
  all_attrs <- xml2::xml_attrs(gmql_schema_tag)
  all_attrs_list <- as.list(all_attrs[[1]])
}

# aggregates factory
.aggregates <- function(meta_data,class) {
  if(!is.list(meta_data))
    stop("meta_data: invalid input")
  
  if(!all(vapply(meta_data, function(x) is(x,class), logical(1))))
    stop("All elements must be META_AGGREGATES object")
  
  names <- names(meta_data)
  if(is.null(names)) {
    warning("You did not assign a names to a list.\nWe build it for you")
    names <- vapply(meta_data, take_value.META_AGGREGATES,character(1))
  } else {
    if("" %in% names)
      stop("No partial names assignment is allowed")
  }
  aggregate_matrix <- t(vapply(meta_data, function(x) {
    new_value = as.character(x)
    matrix <- matrix(new_value)
  },character(2)))
  
  m_names <- matrix(names)
  metadata_matrix <- cbind(m_names,aggregate_matrix)
}


# meta join condition
.join_condition <- function(cond) {
  cond_matrix <- NULL
  def <- cond$condition$def
  if(!is.null(def))
    cond_matrix <- rbind(cond_matrix, def)
  
  exact <- cond$condition$exact
  if(!is.null(exact))
    cond_matrix <- rbind(cond_matrix, exact)
  
  full <- cond$condition$full
  if(!is.null(full))
    cond_matrix <- rbind(cond_matrix, full)
  cond_matrix
}

.check_input <- function(value) {
  if(!is.character(value))
    stop("no valid data")
  
  if(length(value)>1)
    stop("no multiple string")
}

.check_logical <- function(value) {
  if(!is.logical(value))
    stop("no valid data")
  
  if(length(value)>1)
    stop("no multiple string")
}

.is_login_expired <- function(url) {
  if(exists("GMQL_credentials", envir = .GlobalEnv)) {
    if(exists("authToken", where = GMQL_credentials)) {
      authToken <- GMQL_credentials$authToken
      url <- sub("/*[/]$","",url)
      h <- c('Accept' = 'Application/json', 'X-Auth-Token' = authToken)
      URL <- paste0(url,"/user")
      req <- httr::GET(URL,httr::add_headers(h))
      if(req$status_code != 200)
        return(TRUE)
      else
        return(FALSE)
    }
  }
  return(TRUE)
}
