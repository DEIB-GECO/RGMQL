
.counter <- function(zero = 0)
{
    i <- zero
    function() {
        i <<- i + 1
        toString <- as.character(i)
    }
}

.add_metadata <- function(files)
{
    x <- scan(files, what="", sep="\n")
    y <- strsplit(x, "\t")
    names(y) <- sapply(y, `[[`, 1)
    listMeta <- lapply(y, `[`, -1)
}

.schema_header <- function(datasetName)
{
    schema_name <- list.files(datasetName, pattern = "*.schema$",
                                full.names = TRUE)
    schema_name_xml <- list.files(datasetName, pattern = "*.xml$",
                                full.names = TRUE)
    
    if(length(schema_name)==0 || length(schema_name_xml) == 0)
        stop("schema not present")
    
    xml_schema <- xml2::read_xml(schema_name)
    list_field <- xml2::as_list(xml_schema)
    vector_field <- unlist(list_field)
}

# aggregates factory
.aggregates <- function(meta_data,class)
{
    if(!is.list(meta_data))
        stop("meta_data: invalid input")
    
    if(!all(sapply(meta_data, function(x) is(x,class))))
        stop("All elements must be META_AGGREGATES object")
    
    names <- names(meta_data)
    if(is.null(names))
    {
        warning("You did not assign a names to a list.\nWe build it for you")
        names <- sapply(meta_data, take_value.META_AGGREGATES)
    }
    else
    {
        if("" %in% names)
            stop("No partial names assignment is allowed")
    }
    aggregate_matrix <- t(sapply(meta_data, function(x) {
        new_value = as.character(x)
        matrix <- matrix(new_value)
    }))
    
    m_names <- matrix(names)
    metadata_matrix <- cbind(m_names,aggregate_matrix)
}


# meta join condition
.join_condition <- function(cond)
{
    join_condition_matrix <- do.call(rbind, cond)
    join_condition_matrix
}

.check_input <- function(value)
{
    if(!is.character(value))
        stop("no valid data")
    
    if(length(value)>1)
        stop("no multiple string")
}

.check_logical <- function(value)
{
    if(!is.logical(value))
        stop("no valid data")
    
    if(length(value)>1)
        stop("no multiple string")
}
