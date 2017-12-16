#' Filter and extract function
#'
#' This function lets user to create a new GRangesList with fixed information:
#' seqnames, ranges and strand, and a variable part made up by the regions
#' defined as input. The metadata and metadata_prefix are used to filter 
#' the data and choose only the samples that match at least one metdatata 
#' with its prefix. The input regions are shown for each sample obtained 
#' from filtering.
#'
#' @import xml2
#' @importFrom dplyr bind_cols
#' @importFrom data.table fread
#' @importFrom rtracklayer import
#'
#' @param data string GMQL dataset folder path or GRangesList
#' object
#' @param metadata vector of strings containing names of metadata attributes
#' to be searched for in metadata files.
#' Data will be extracted if at least one condition is satisfied:
#' this condition is logically "ANDed" with prefix filtering (see below)
#' if NULL no filtering action occures
#' (i.e every sample is taken for region filtering)
#' @param metadata_prefix vector of strings that will support the metadata
#' filtering. If defined, each 'metadata' are concatenated with the 
#' corresponding prefix.
#' @param regions vector of strings that extracts only region attribute 
#' specified; if NULL no regions attribute is taken and the output is only 
#' GRanges made up by the region coordinate attributes 
#' (seqnames, start, end, strand)
#' @param suffix name for each metadata column of GRanges. by default is the 
#' "antibody_target". This string is taken from sample metadata file or from
#' metadata() associated. If not present, the column name is the name of 
#' selected regions
#'
#' @details
#' This function works only with datatset or GRangesList which samples or 
#' Granges have the same regions coordinates (chr, ranges, strand)
#' 
#' In case of Grangeslist data input the function will search for metadata
#' into metadata() function associated to Grangeslist.
#'
#' @return GRanges with selected regions
#'
#' @examples
#' 
#' ## This statement defines the path to the folders "DATASET" in the 
#' ## subdirectory "example" of the package "RGMQL" and filter such folder 
#' ## dataset including at output only "pvalue" and "peak" regions
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' filter_and_extract(test_path, regions = c("pvalue", "peak"))
#' 
#' ## This statement import a GMQL dataset as GRangesList and filter it 
#' ## including at output only "pvalue" and "peak" regions
#' 
#' grl = import_gmql(test_path, TRUE)
#' filter_and_extract(grl, regions = c("pvalue", "peak"))
#'
#'
#' @export
#'
filter_and_extract <- function(data, metadata = NULL,
                                metadata_prefix = NULL, regions = NULL, 
                                suffix = "antibody_target")
{
    if(is(data,"GRangesList"))
        .extract_from_GRangesList(data, metadata, metadata_prefix, regions, 
                                    suffix)
    else
        .extract_from_dataset(data, metadata, metadata_prefix, regions, 
                                    suffix)
}

.extract_from_dataset <- function(datasetName, metadata, metadata_prefix, 
                                        regions, suffix)
{
    datasetName <- sub("/*[/]$","",datasetName)
    if(basename(datasetName) !="files")
        datasetName <- paste0(datasetName,"/files")
    
    if(!dir.exists(datasetName))
        stop("Directory does not exists")
    
    gdm_meta_files <- list.files(datasetName, pattern = "*.gdm.meta$",
                                    full.names = TRUE)
    gtf_meta_files <- list.files(datasetName, pattern = "*.gtf.meta$",
                                    full.names = TRUE)
    
    if(length(gdm_meta_files)==0 && length(gtf_meta_files)==0)
        stop("no samples present or no files format supported")
    
    if(length(gdm_meta_files)>=1 && length(gtf_meta_files)>=1)
        stop("GMQL dataset cannot be mixed dataset: no GTF and GDM together")
    
    vector_field <- .schema_header(datasetName)
    
    if(length(gdm_meta_files)>0)
    {
        samples_with_suffix <- .check_metadata_files(metadata,metadata_prefix,
                                                gdm_meta_files, suffix)
        
        samples_file <- sapply(samples_with_suffix, function(x) x$sample)
        suffix_vec <- sapply(samples_with_suffix, function(x) x$suffix)
        suffixes <- unlist(suffix_vec)
        samples_to_read <- unlist(samples_file)
        
        if(length(samples_to_read)>0)
            samples_to_read <- gsub(".meta$", "", samples_to_read)
        else
            samples_to_read <- gsub(".meta$", "", gdm_meta_files)
        
        granges <- .parse_gdm_files(vector_field,samples_to_read,regions,
                                                        suffixes)
    }
    else
    {
        samples_with_suffix <- .check_metadata_files(metadata,metadata_prefix,
                                                    gtf_meta_files, suffix)
        
        samples_file <- sapply(samples_with_suffix, function(x) x$sample)
        suffix_vec <- sapply(samples_with_suffix, function(x) x$suffix)
        suffixes <- unlist(suffix_vec)
        samples_to_read <- unlist(samples_file)
        
        if(length(samples_to_read)>0)
            samples_to_read <- gsub(".meta$", "", samples_to_read)
        else
            samples_to_read <- gsub(".meta$", "", gtf_meta_files)
        
        granges <- .parse_gtf_files(samples_to_read, regions, suffixes)
    }
}

.extract_from_GRangesList <- function(rangesList, metadata, metadata_prefix, 
                                        regions, suffix)
{
    if(!is(rangesList,"GRangesList"))
        stop("only GrangesList admitted")
    
    if(length(rangesList)<=0)
        stop("rangesList empty")
    
    meta_list <- metadata(rangesList)
    samples <- .check_metadata_list(metadata, metadata_prefix, meta_list, 
                                        suffix)
    if(length(unlist(samples))<=0)
        samples <- rangesList
    else
    {
        index <- unlist(samples)
        samples <- rangesList[c(index)]
    }
    granges <- .parse_Granges(samples,regions)
}

.parse_Granges <- function(region_list,regions)
{
    g1 <- region_list[[1]]
    elementMetadata(g1) <- NULL
    if(!is.null(regions))
    {
        DF_list <- lapply(region_list, function(g_x){
            meta <- elementMetadata(g_x)[regions]
            data.frame(meta)
        })
        DF_only_regions <- dplyr::bind_cols(DF_list)
        elementMetadata(g1) <- DF_only_regions
    }
    g1
}

.check_metadata_list <- function(metadata,metadata_prefix,meta_list,col_name)
{
    vec_meta <- paste0(metadata_prefix,metadata)
    list <- mapply(function(x,index){
        vec_names <- names(x)
        a <- sapply(vec_meta, function(y) {
            which(y==vec_names)
        })
        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        #if found retrieve samples that has at least one choosen metadata
        if(found){index}
    }, meta_list, seq_along(meta_list))
}

.check_metadata_files <- function(metadata,metadata_prefix,meta_files,col_name)
{
    suffix <- paste0(col_name,"$")
    vec_meta <- paste0(metadata_prefix,metadata)
    meta_list <- lapply(meta_files, function(x){
        list <- .add_metadata(x)
        vec_names <- names(list)
        a <- sapply(vec_meta, function(y) {
            grep(y,vec_names)
        })
        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        index <- grep(suffix,vec_names)
        suffix <- unlist(list[index])[1] # ne prendo solo uno
        names(suffix) <- NULL
        #if found retrieve samples that has at least one choosen metadata
        if(found)
            list("sample" = x, "suffix" = suffix )
        else
            list("sample" = NULL, "suffix" = suffix )
            
    })
}


.parse_gtf_files <- function(gtf_region_files, regions, suffixes)
{
    g1 <- rtracklayer::import(con = gtf_region_files[1], format = "gtf")
    elementMetadata(g1) <- NULL
    if(is.null(suffixes))
        suffixes = ""
    
    if(!is.null(regions))
    {
        DF_list <- mapply(function(x,h){
            g_x <- rtracklayer::import(con = x, format = "gtf")
            meta <- elementMetadata(g_x)[regions]
            if(h!="")
                names(meta) <- paste(regions,h,sep = ".")
            data.frame(meta)
        },gtf_region_files, suffixes, SIMPLIFY = FALSE)
        DF_only_regions <- dplyr::bind_cols(DF_list)
        elementMetadata(g1) <- DF_only_regions
    }
    g1
}

.parse_gdm_files <- function(vector_field,gdm_region_files,regions,suffixes)
{
    #read first sample cause chromosome regions are the same for all samples
    df <- data.table::fread(gdm_region_files[1],col.names = vector_field,
    header = FALSE,sep = '\t')
    col_names <- names(df)
    df <-  subset(df, TRUE, c("chr","left","right","strand"))
    
    if(!is.null(regions))
    {
        df_list <- lapply(gdm_region_files,function(x,regions,vector_field){
            region_frame <- data.table::fread(x,col.names = vector_field,
                    header = FALSE,sep = '\t')
            col_names <- names(region_frame)
            #delete column not choosen by input
            if(!is.null(regions))
                col_names <- col_names[col_names %in% regions]
            
            if(length(col_names)!=0)
                r <- subset(region_frame,TRUE,col_names)
        },regions,vector_field)

        df_only_regions <- dplyr::bind_cols(df_list)
        complete_df <- dplyr::bind_cols(df,df_only_regions)
        
        region_names <- names(complete_df)[-(1:4)]
        region_names <- gsub('[0-9]+', '',region_names)
        region_names <- paste(region_names,suffixes,sep = ".")
        region_names <- c(names(complete_df)[(1:4)],region_names )
        names(complete_df) <- region_names
        g <- GenomicRanges::makeGRangesFromDataFrame(complete_df,
                            keep.extra.columns = TRUE,
                            start.field = "left",
                            end.field = "right")
    }
    else
        g <- GenomicRanges::makeGRangesFromDataFrame(df, 
                            keep.extra.columns = TRUE,
                            start.field = "left",
                            end.field = "right")
    
}
