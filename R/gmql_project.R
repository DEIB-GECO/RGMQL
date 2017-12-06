select.GMQLDataset <- function(.data, metadata = NULL, metadata_update = NULL, 
                            all_but_meta = FALSE, regions = NULL, 
                            regions_update = NULL, all_but_reg = FALSE) 
{
    data = .data@value
    r_update <- substitute(regions_update)
    if(!is.null(r_update))
    {
        reg_update <- .trasform_update(deparse(r_update))
        reg_update <- paste(reg_update,collapse = "")
    }
    else
        reg_update <- .jnull("java/lang/String")
    
    m_update <- substitute(metadata_update)
    if(!is.null(m_update))
    {
        meta_update <- .trasform_update(deparse(m_update))
        meta_update <- paste(meta_update,collapse = "")
    }
    else
        meta_update <- .jnull("java/lang/String")
    
    gmql_project(data, metadata, meta_update, all_but_meta, regions, 
                    reg_update, all_but_reg)
}

#' Method select
#' 
#' @description Wrapper to GMQL PROJECT operator 
#' 
#' @description It creates, from an existing dataset, a new dataset with all 
#' the samples from input dataset, but keeping for each sample in the input 
#' dataset only those metadata and/or region attributes expressed.
#' Region coordinates and values of the remaining metadata remain equal to 
#' those in the input dataset. It allows to:
#' \itemize{
#' \item{Remove existing metadata and/or region attributes from a dataset}
#' \item{Update new metadata and/or region attributes in the result}
#' }
#' 
#' @importFrom rJava J .jnull .jarray
#' @importFrom dplyr select
#' 
#' @param .data GMQLDataset class object
#' 
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
#' @param regions_update list of updating rules in the form of 
#' key = value generating new genomic region attributes.
#' To specify the new values, the following options are available:
#' \itemize{
#' \item{All aggregation functions already defined by AGGREGATES object}
#' \item{All basic mathematical operations (+, -, *, /), including parenthesis}
#' \item{SQRT, META, NIL constructor object defined by OPERATOR object}
#' }
#' @param metadata_update list of updating rules in the form of 
#' key = value generating new metadata.
#' To specify the new values, the following options are available:
#' \itemize{
#' \item{All aggregation functions already defined by AGGREGATES object}
#' \item{All basic mathematical operations (+, -, *, /), including parenthesis}
#' \item{SQRT, META, NIL constructor object defined by OPERATOR object}
#' }
#'  
#' @return GMQLDataset object. It contains the value to use as input 
#' for the subsequent GMQLDataset method
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
#' CTCF_NORM_SCORE = select(input, metadata_update = list(normalized = 1), 
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
#' ## i.e. manually_curated_tissue_status and manually_curated_tumor_tag.
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' DS_in = read_dataset(test_path)
#' DS_out = select(DS_in, regions = c("variant_classification", 
#' "variant_type"), metadata = c("manually_curated_tissue_status", 
#' "manually_curated_tumor_tag"))
#' 
#' }
#' 
#' @name select
#' @rdname select
#' @aliases select,GMQLDataset-method
#' @aliases select-method
#' @export
setMethod("select", "GMQLDataset",select.GMQLDataset)

gmql_project <-function(input_data, metadata, metadata_update, all_but_meta, 
                            regions, regions_update, all_but_reg)
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
    

    if(length(all_but_meta)>1)
        warning("all_but_meta: no multiple values")
    
    if(length(all_but_reg)>1)
        warning("all_but_reg: no multiple values")
    
    all_but_reg <- all_but_reg[1]
    all_but_meta <- all_but_meta[1]
    
    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- WrappeR$project(metadata, metadata_update, all_but_meta, 
                    regions, regions_update, all_but_reg, input_data)
    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        GMQLDataset(data)
}

.trasform_update <- function(predicate=NULL)
{
    predicate <- gsub("list\\(","",predicate)
    predicate <- gsub("\\)$","",predicate)
    predicate <- gsub("=","AS",predicate)
    predicate <- gsub("NIL","NULL",predicate)
    predicate <- gsub("\"","",predicate)
}
