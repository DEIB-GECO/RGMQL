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
#' filtering. If defined, each 'metadata' is concatenated with the 
#' corresponding prefix.
#' @param region_attributes vector of strings that extracts only region 
#' attributes  specified; if NULL no regions attribute is taken and the output 
#' is only GRanges made up by the region coordinate attributes 
#' (seqnames, start, end, strand)
#' @param suffix name for each metadata column of GRanges. By default it is the 
#' value of the metadata attribute named "antibody_target". This string is 
#' taken from sample metadata file or from metadata() associated. 
#' If not present, the column name is the name of selected regions specified
#' by 'region_attributes' input parameter
#'
#' @details
#' This function works only with datatset or GRangesList all whose samples or 
#' Granges have the same region coordinates (chr, ranges, strand) ordered in 
#' the same way for each sample 
#' 
#' In case of GRangesList data input, the function searches for metadata
#' into metadata() function associated to GRangesList.
#'
#' @return GRanges with selected regions
#'
#' @examples
#' 
#' ## This statement defines the path to the folder "DATASET" in the 
#' ## subdirectory "example" of the package "RGMQL" and filters such folder 
#' ## dataset including at output only "pvalue" and "peak" region attributes
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' filter_and_extract(test_path, region_attributes = c("pvalue", "peak"))
#' 
#' ## This statement imports a GMQL dataset as GRangesList and filters it 
#' ## including at output only "pvalue" and "peak" region attributes, the sort
#' ## function makes sure that the region coordinates (chr, ranges, strand) 
#' ## of all samples are ordered correctly
#' 
#' 
#' grl = import_gmql(test_path, TRUE)
#' sorted_grl = sort(grl)
#' filter_and_extract(sorted_grl, region_attributes = c("pvalue", "peak"))
#'
#'
#' @export
#'
filter_and_extract <- function(data, metadata = NULL,
                    metadata_prefix = NULL, region_attributes = NULL, 
                    suffix = "antibody_target")
{
    if(is(data,"GRangesList"))
        .extract_from_GRangesList(data, metadata, metadata_prefix, 
            region_attributes, suffix)
    else
        .extract_from_dataset(data, metadata, metadata_prefix, 
            region_attributes, suffix)
}

.extract_from_dataset <- function(datasetName, metadata, metadata_prefix, 
                                        regions, suffix)
{
    datasetName <- sub("/*[/]$","",datasetName)
    if(basename(datasetName) !="files")
        datasetName <- file.path(datasetName,"files")

    if(!dir.exists(datasetName))
        stop("Directory does not exists")
    
    gdm_meta_files <- list.files(datasetName, pattern = "*.gdm.meta$",
                                    full.names = TRUE)
    gtf_meta_files <- list.files(datasetName, pattern = "*.gtf.meta$",
                                    full.names = TRUE)
    
    if(!length(gdm_meta_files) && !length(gtf_meta_files))
        stop("no samples present or no files format supported")
    
    if(length(gdm_meta_files) && length(gtf_meta_files))
        stop("GMQL dataset cannot be mixed dataset: no GTF and GDM together")
    
    vector_field <- .schema_header(datasetName)
    
    
    if(length(gdm_meta_files))
    {
        samples_file <- .check_metadata_files(metadata,metadata_prefix,
                                                gdm_meta_files)
        
        samples_meta_to_read <- unlist(samples_file)
        
        if(length(samples_meta_to_read))
            samples_to_read <- gsub(".meta$", "", samples_meta_to_read)
        else
        {
            samples_to_read <- gsub(".meta$", "", gdm_meta_files)
            samples_meta_to_read <- gtf_meta_files
            
        }
        
        suffix_vec <- .get_suffix(suffix, FALSE, samples_meta_to_read)
        granges <- .parse_gdm_files(vector_field,samples_to_read,regions,
                                        suffix_vec)
    }
    else
    {
        samples_file <- .check_metadata_files(metadata,metadata_prefix,
                                                    gtf_meta_files)
        samples_meta_to_read <- unlist(samples_file)
        
        if(length(samples_meta_to_read))
            samples_to_read <- gsub(".meta$", "", samples_meta_to_read)
        else
        {
            samples_to_read <- gsub(".meta$", "", gtf_meta_files)
            samples_meta_to_read <- gtf_meta_files
        }
        
        suffix_vec <- .get_suffix(suffix, FALSE, samples_meta_to_read)
        granges <- .parse_gtf_files(samples_to_read, regions,suffix_vec)
    }
}

.extract_from_GRangesList <- function(rangesList, metadata, metadata_prefix, 
                                        regions, suffix)
{
    if(!is(rangesList,"GRangesList"))
        stop("only GrangesList admitted")
    
    if(!length(rangesList))
        stop("rangesList empty")
    
    meta_list <- metadata(rangesList)
    samples <- .check_metadata_list(metadata, metadata_prefix, meta_list)
    if(!length(unlist(samples)))
        samples <- rangesList
    else
    {
        index <- unlist(samples)
        samples <- rangesList[c(index)]
    }
    new_meta_list <- metadata(samples)
    suffix_vec <- .get_suffix(suffix, TRUE, new_meta_list)
    granges <- .parse_Granges(samples,regions,suffix_vec)
}

.parse_Granges <- function(region_list,regions,suffixes)
{
    if(is.null(suffixes))
        suffixes = ""
    
    g1 <- region_list[[1]]
    elementMetadata(g1) <- NULL
    if(!is.null(regions))
    {
        DF_list <- mapply(function(g_x,h){
            meta <- elementMetadata(g_x)[regions]
            if(h!="")
                names(meta) <- paste(regions,h,sep = ".")
            data.frame(meta)
        },region_list, suffixes, SIMPLIFY = FALSE)
        DF_only_regions <- dplyr::bind_cols(DF_list)
        elementMetadata(g1) <- DF_only_regions
    }
    g1
}

.get_suffix <- function(col_name, from_list, meta_fl)
{
    suffix <- paste0(col_name,"$")
    
    if(from_list)
    {
        meta_list <- mapply(function(x,index){
            vec_names <- names(x)
            s_index <- grep(suffix,vec_names)
            first_index <- s_index[1]
            suffix <- unlist(x[first_index]) # ne prendo solo uno
            names(suffix) <- NULL
        
            #if found retrieve samples that has at least one choosen metadata
            if(first_index && !is.na(first_index))
                suffix
            else
                ""
        }, meta_fl, seq_along(meta_fl)) 
    }
    else
    {
        meta_list <- vapply(meta_fl, function(x){
            list <- .add_metadata(x)
            vec_names <- names(list)
            index <- grep(suffix,vec_names)
            first_index <- index[1]
            suffix <- unlist(list[first_index]) # ne prendo solo uno
            names(suffix) <- NULL
            #if found retrieve samples that has at least one choosen metadata
            if(first_index && !is.na(first_index))
                suffix
            else
                ""
        },character(1))
    }
    names(meta_list) <- NULL
    meta_list
}

.check_metadata_list <- function(metadata,metadata_prefix,meta_list)
{
    vec_meta <- paste0(metadata_prefix,metadata)
    list <- mapply(function(x,index){
        vec_names <- names(x)
        a <- lapply(vec_meta, function(y) {
            which(y == vec_names)
        })
        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        #if found retrieve samples that has at least one choosen metadata
        if(found){index}
    }, meta_list, seq_along(meta_list))
}

.check_metadata_files <- function(metadata,metadata_prefix,meta_files)
{
    vec_meta <- paste0(metadata_prefix,metadata)
    meta_list <- lapply(meta_files, function(x){
        list <- .add_metadata(x)
        vec_names <- names(list)
        a <- lapply(vec_meta, function(y)grep(y,vec_names))
        ## we would like that manage more index from grep
        found <- as.logical(length(unlist(a)))
        #if found retrieve samples that has at least one choosen metadata
        if(found){x}
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

.parse_gdm_files <- function(vector_field,gdm_region_files,regions,
                                suffixes)
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
            
            if(length(col_names))
                r <- subset(region_frame,TRUE,col_names)
        },regions,vector_field)

        df_only_regions <- dplyr::bind_cols(df_list)
        complete_df <- dplyr::bind_cols(df,df_only_regions)
        
        region_names <- names(complete_df)[-(seq_len(4))]
        region_names <- gsub('[0-9]+', '',region_names)
        region_names <- paste(region_names,suffixes,sep = ".")
        region_names <- c(names(complete_df)[(seq_len(4))],region_names )
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
