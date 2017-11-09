#' GMQL Operation: PROJECT
#'
#' It creates, from an existing dataset, a new dataset with all the samples 
#' from input dataset, but keeping for each sample in the input dataset 
#' only those metadata and/or region attributes expressed in the operator 
#' parameter list.
#' Region coordinates and values of the remaining metadata remain equal to 
#' those in the input dataset. It allows to:
#' \itemize{
#' \item{Remove existing metadata and/or region attributes from a dataset}
#' \item{Create new metadata and/or region attributes in the result}
#' }
#'
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param metadata vector of string made up by metadata attribute
#' @param regions vector of string made up by schema field attribute
#' @param all_but_reg logical value indicating which schema field attribute 
#' you want to exclude; if FALSE only the regions you choose is kept 
#' in the output of the project operation, if TRUE the schema region 
#' are all except ones include in region parameter.
#' if regions is not defined \emph{all_but_reg} is not considerd.
#' @param all_but_meta logical value indicating which metadata 
#' you want to exclude; If FALSE only the metadata you choose is kept 
#' in the output of the project operation, if TRUE the metadata 
#' are all except ones include in region parameter.
#' if metadata is not defined \emph{all_but_meta} is not considerd.
#' @param regions_update list in the form of key = value generating 
#' new genomic region attributes.
#' To specify the new values, the following options are available:
#' \itemize{
#' \item{All aggregation functions already defined by AGGREGATES object}
#' \item{All basic mathematical operations (+, -, *, /), including parenthesis}
#' \item{SQRT, META, NULLABLE constructor object defined by OPERATOR object}
#' }
#' @param metadata_update list in the form of key = value generating 
#' new metadata.
#' To specify the new values, the following options are available:
#' \itemize{
#' \item{All aggregation functions already defined by AGGREGATES object}
#' \item{All basic mathematical operations (+, -, *, /), including parenthesis}
#' \item{SQRT, META, NULLABLE constructor object defined by OPERATOR object}
#' }
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#'
#' @examples
#' 
#' ## It creates a new dataset called CTCF_NORM_SCORE by preserving all 
#' ## region attributes apart from score, and creating a new region attribute 
#' ## called new_score by dividing the existing score value of each region 
#' ## by 1000.0 and incrementing it by 100.
#' ## It also generates, for each sample of the new dataset, 
#' ## a new metadata attribute called normalized with value 1, 
#' ## which can be used in future selections.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' input = read_dataset(test_path)
#' CTCF_NORM_SCORE = project(input, metadata_update = list(normalized = 1), 
#' regions_update = list(new_score = (score / 1000.0) + 100), 
#' regions = c("score"), all_but_reg = TRUE)
#' 
#' 
#' \dontrun{
#' 
#' ## It produces an output dataset that contains the same samples 
#' ## as the input dataset. 
#' ## Each output sample only contains, as region attributes, 
#' ## the four basic coordinates (chr, left, right, strand) and the specified 
#' ## region attributes 'variant_classification' and 'variant_type', 
#' ## and as metadata attributes only the specified ones, 
#' ## i.e. manually_curated__tissue_status and manually_curated__tumor_tag.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' DS_in = read_dataset(test_path)
#' DS_out = project(DS_in, regions = c("variant_classification", 
#' "variant_type"), metadata = c("manually_curated_tissue_status", 
#' "manually_curated_tumor_tag"))
#' 
#' }
#' 
#' @export
#'
#'
project <-function(input_data, metadata = NULL, metadata_update=NULL, 
                    all_but_meta = FALSE, regions = NULL, 
                    regions_update = NULL, all_but_reg=FALSE)
{
    if(!is.null(metadata))
    {
        if(!is.character(metadata))
            stop("metadata: no valid input")
        
        metadata <- metadata[!metadata %in% ""]
        metadata <- metadata[!duplicated(metadata)]
        
        if(length(metadata)==0)
            metadata <- .jnull("java/lang/String")
        
        metadata <- .jarray(metadata)
    }
    else
        metadata <- .jnull("java/lang/String")
    
    if(!is.null(regions))
    {
        if(!is.character(regions))
            stop("regions: no valid input")
        
        regions = regions[!regions %in% ""]
        regions = regions[!duplicated(regions)]
        
        if(length(regions)==0)
            regions <- .jnull("java/lang/String")
        
        regions <- .jarray(regions)
    }
    else
        regions <- .jnull("java/lang/String")
    
    reg_update <- substitute(regions_update)
    if(!is.null(reg_update))
    {
        regions_update <- .trasform_update(deparse(reg_update))
        regions_update <- paste(regions_update,collapse = "")
    }
    else
        regions_update <- .jnull("java/lang/String")
    
    meta_update <- substitute(metadata_update)
    if(!is.null(meta_update))
    {
        metadata_update <- .trasform_update(deparse(meta_update))
        metadata_update <- paste(metadata_update,collapse = "")
    }
    else
        metadata_update <- .jnull("java/lang/String")
    
    if(length(all_but_meta)>1)
        warning("all_but_meta: no multiple values")
    
    if(length(all_but_reg)>1)
        warning("all_but_reg: no multiple values")
    all_but_reg <- all_but_reg[1]
    all_but_meta <- all_but_meta[1]
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$project(metadata,metadata_update,all_but_meta,
                                regions,regions_update,
                                all_but_reg,input_data$value)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        DataSet(data)
}

.trasform_update <- function(predicate=NULL)
{
    predicate <- gsub("list\\(","",predicate)
    predicate <- gsub("\\)$","",predicate)
    predicate <- gsub("=","AS",predicate)
    predicate <- gsub("NIL","NULL",predicate)
    predicate <- gsub("\"","",predicate)
}
