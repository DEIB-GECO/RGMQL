#' Function read
#'
#' It reads a GMQL dataset, as a folder containing some homogenus samples on 
#' disk or as a GRangesList, saving it in Scala memory in a way that can be 
#' referenced in R. It is also used to read a repository dataset in case of
#' remote processing.
#' 
#' @importFrom rJava J .jnull .jarray
#' @importFrom methods is
#' 
#' @param dataset folder path for GMQL dataset or dataset name on repository
#' @param parser string used to parsing dataset files.
#' The Parsers available are:
#' \itemize{
#' \item{BedParser}
#' \item{BroadPeakParser}
#' \item{NarrowPeakParser}
#' \item{CustomParser}
#' }
#' Default is CustomParser.
#' @param is_local logical value indicating local or remote dataset
#' @param is_GMQL logical value indicating GMQL dataset or not 
#' 
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
#' 
#' @details
#' Normally, a GMQL dataset contains an XML schema file that contains
#' name of region attributes. (e.g chr, start, stop, strand)
#' The CustomParser reads this XML schema; 
#' if you already know what kind of schema your files have, use one of the 
#' parsers defined, without reading any XML schema.
#' 
#' If GRangesList has no metadata: i.e. metadata() is empty, two metadata are
#' generated:
#' \itemize{
#' \item{"provider" = "PoliMi"}
#' \item{"application" = "RGMQL"}
#' }
#'
#' @examples
#' 
#' ## This statement initializes and runs the GMQL server for local execution 
#' ## and creation of results on disk. Then, with system.file() it defines 
#' ## the path to the folder "DATASET" in the subdirectory "example" 
#' ## of the package "RGMQL" and opens such folder as a GMQL dataset 
#' ## named "data" using CustomParser
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_gmql(test_path)
#' 
#' ## This statement opens such folder as a GMQL dataset named "data" using 
#' ## "NarrowPeakParser" 
#' dataPeak = read_gmql(test_path,"NarrowPeakParser")
#' 
#' ## This statement reads a remote public dataset stored into GMQL system 
#' ## repository. For a public dataset in a (remote) GMQL repository the 
#' ## prefix "public." is needed before dataset name
#' 
#' remote_url = "http://www.gmql.eu/gmql-rest/"
#' login_gmql(remote_url)
#' data1 = read_gmql("public.Example_Dataset_1", is_local = FALSE)
#' 
#' @name read_gmql
#' @rdname read-function
#' @export
#'
read_gmql <- function(
  dataset, 
  parser = "CustomParser", 
  is_local = TRUE, 
  is_GMQL = TRUE
) {
  .check_input(dataset)
  .check_logical(is_local)
  .check_logical(is_GMQL)
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  parser_name <- .check_parser(parser)
  if(is_local) {
    if(!dir.exists(dataset))
      stop("folder does not exist")
    
    dataset <- sub("/*[/]$","",dataset)
    if(basename(dataset) !="files")
      dataset <- file.path(dataset,"files")
    
    schema_SCHEMA <- list.files(
      dataset, pattern = "*.schema$", full.names = TRUE
    )
    
    xml_schema <- list.files(
      dataset, pattern = "*.xml$", full.names = TRUE
    )
    xml_schema = xml_schema[!basename(xml_schema) %in% c("web_profile.xml")]
    
    if(!length(schema_SCHEMA) && !length(xml_schema))
      stop("schema not present")
    
    if(!length(schema_SCHEMA))
      schema_XML <- xml_schema
    else
      schema_XML <- dataset
    
    schema_matrix <- .jnull("java/lang/String")
    url <- .jnull("java/lang/String")
    coords_sys <- .jnull("java/lang/String")
    type <- .jnull("java/lang/String")
  } else {
    url <- WrappeR$get_url()
    if(is.null(url))
      stop("You have to log on using login function")
    
    if(!exists("GMQL_credentials", envir = .GlobalEnv))
      stop("You have to log on using login function")
    
    if(identical(parser_name,"CUSTOMPARSER")) {
      list <- show_schema(url,dataset)
      coords_sys <- list$coordinate_system
      type <- list$type
      schema_names <- vapply(
        list$fields, function(x){x$name},character(1)
      )
      schema_type <- vapply(
        list$fields, function(x){x$type},character(1)
      )
      schema_matrix <- cbind(schema_names,schema_type)
      
      if(is.null(schema_matrix) || !length(schema_matrix))
        schema_matrix <- .jnull("java/lang/String")
      else
        schema_matrix <- .jarray(schema_matrix, dispatch = TRUE)
    }
    else
      schema_matrix <- .jnull("java/lang/String")
    
    schema_XML <- .jnull("java/lang/String")
  }
  
  response <- WrappeR$readDataset(
    dataset, 
    parser_name, 
    is_local, 
    is_GMQL, 
    schema_matrix,
    schema_XML,
    coords_sys, 
    type
  )
  error <- strtoi(response[1])
  data <- response[2]
  if(error)
    stop(data)
  else
    GMQLDataset(data)
}


#' @importFrom S4Vectors metadata
#' @importFrom rJava J .jarray
#' 
#' @param samples GRangesList
#' 
#' @name read_gmql
#' @rdname read-function
#' @export
#'
read_GRangesList <- function(samples) {
  if(!is(samples,"GRangesList"))
    stop("only GrangesList")
  
  meta <- S4Vectors::metadata(samples)
  if(is.null(meta) || !length(meta)) {
    #repeat meta for each sample in samples list
    len <- length(samples)
    warning(
    "No metadata.\nWe provide two metadata for you:
    \n1.provider = PoliMi\n2.application = RGMQL\n"
    )
    index_meta <- rep(seq_len(len),each = len)
    rep_meta <- rep(
      c("provider","PoliMi", "application", "RGMQL"),
      times = len
    )
    meta_matrix <- matrix(rep_meta,ncol = 2,byrow = TRUE)
    meta_matrix <- cbind(index_meta,meta_matrix)
  }else {
    unlist_meta <- unlist(meta)
    names_meta <- names(unlist_meta)
    group_names <- gsub(".*_([0-9]*)\\..*","\\1", names_meta)
    names(unlist_meta) <- NULL
    meta_matrix <- cbind(group_names,names_meta,unlist_meta)
  }
  
  df <- data.frame(samples)
  df <- df[-2] #delete group_name
  len_df <- dim(df)[1] # number of rows
  
  col_types <- vapply(df,class,character(1))
  col_names <- names(col_types)
  #re order the schema?
  # if GTF, change
  if("phase" %in% col_names) {
    col_names <- plyr::revalue(
      col_names,c(type = "feature", phase = "frame", seqnames = "seqname")
    )
    schema_matrix <- cbind(toupper(col_types),col_names)
    schema_matrix<- schema_matrix[
      setdiff(rownames(schema_matrix), c("group","width")),]
  } else {
    col_names <- plyr::revalue(
      col_names,
      c(start = "left", end = "right", seqnames = "chr"))
    schema_matrix <- cbind(col_names,toupper(col_types))
    df$start = df$start - 1
    schema_matrix<- schema_matrix[
      setdiff(rownames(schema_matrix),c("group","width")),]
  }
  region_matrix <- as.matrix(vapply(df, as.character,character(len_df)))
  region_matrix[is.na(region_matrix)] <- "NA"
  region_matrix <- region_matrix[,setdiff(colnames(region_matrix),"width")]
  rownames(schema_matrix) <- NULL
  colnames(schema_matrix) <- NULL
  
  schema_matrix <- .jarray(schema_matrix,dispatch = TRUE)
  meta_matrix <- .jarray(meta_matrix,dispatch = TRUE)
  region_matrix <- .jarray(region_matrix,dispatch = TRUE)
  
  WrappeR <- J("it/polimi/genomics/r/Wrapper")
  response <- WrappeR$read(
    meta_matrix,
    region_matrix,
    schema_matrix,
    "default", "TAB"
  )
  
  GMQLDataset(response)
}

.check_parser <- function(parser) {
  parser <- toupper(parser)
  parsers <- c(
    "BEDPARSER",
    "BROADPEAKPARSER",
    "NARROWPEAKPARSER",
    "CUSTOMPARSER")
  
  if(!parser %in% parsers)
    stop("parser not defined")
  
  parser
}

