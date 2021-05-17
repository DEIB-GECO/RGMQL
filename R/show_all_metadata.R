#' Show a metadata cartesian product of all metadata present into the 
#' dataset and the region sets
#'
#' It show the presence of the metadata keys in that specific regions set, 
#' showing its value or just the logical value TRUE.
#'
#' @param dataset string with GMQL dataset folder path or remote dataset.
#' In case of remote dataset to distinguish among private or public repository
#' each name must be prefixed with "private." or "public." respectively.
#' @param show_value whether or not show the value associated to metadata,
#' otherwise only logical value (TRUE or FALSE) are shown.
#' 
#' @return A Dataframe containing the mapping between metadata and 
#' the regions set
#'
#' @seealso \code{\link{show_all_metadata}}
#'
#' @examples
#' 
#' ## This statement defines the path to the sub-directory "example" of the 
#' ## package "RGMQL" and show all the metadata inside the GMQL dataset among
#' ## all the meta files and return a data-frame, viewing as logical value 
#' ## representing its presence or not for each region set.
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' show_all_metadata(test_path)
#' 
#' ## This statement defines the path to the sub-directory "example" of the 
#' ## package "RGMQL" and show all the metadata inside the GMQL dataset among
#' ## all the meta files and return a data-frame, viewing also its value.
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' show_all_metadata(test_path, show_value = TRUE)
#' 
#' ## This statement the remote dataset called "Example_Dataset_1" on public
#' ## repository and show all the metadata inside the GMQL dataset among
#' ## all the meta files and return a data-frame, viewing also its value.
#' 
#' show_all_metadata("public.Example_Dataset_1", show_value = TRUE)
#'
#' @export
#' 
show_all_metadata <- function(dataset, show_value = FALSE) {
    isRemote <- startsWith(dataset, c("public.", "private."))
    
    if(isRemote[2] && isRemote[1] == FALSE) {
        dataset <- gsub("private.","",dataset)
    }
    
    # since it returns an array we perform the OR condition on the response
    # if at least one is TRUE means that is remote
    if(isRemote[1] | isRemote[2]) {
        .show_all_metadata_remote_dataset(dataset, show_value)
    } else {
        .show_all_metadata_downloaded_dataset(dataset, show_value)
    }
}

.show_all_metadata_remote_dataset <- function(dataset, show_value) {
    url <- GMQL_credentials$remote_url
    
    #first we download all the region file name and its ID
    region_list <- show_samples_list(url, dataset)
    
    metadata_list <-lapply(region_list$samples, function(x) {
        sample_metadata(url, dataset, x$name)
    })  
    
    name_samples <- vapply(region_list$samples, function(x) {
        x$name
    })  
    
    .create_dataFrame(metadata_list, name_samples, show_value)
}

.show_all_metadata_downloaded_dataset <- function(dataset, show_value) {
    datasetName <- sub("/*[/]$","",dataset)
    if(basename(datasetName) !="files")
        datasetName <- file.path(datasetName,"files")
    
    if(!dir.exists(datasetName))
        stop("Directory does not exists")
    
    if(!length(list.files(datasetName)))
        stop("no samples present in this dataset")
    
    regions <- list.files(
        datasetName,
        pattern = "*.gtf$|*.gdm$",
        full.names = TRUE
    )
    
    if(length(regions)) {
        name_samples <- gsub("*.gtf$|*.gdm$", "", basename(regions))
    } else
        stop("No regions files present")
    
    meta <- list.files(
        datasetName, 
        pattern = "*.gtf.meta$|*.gdm.meta$",
        full.names = TRUE
    )
    
    if(length(meta)) {
        meta_list <- lapply(meta, .add_metadata)
    } else
        stop("No meta files present")
    
    .create_dataFrame(meta_list, name_samples, show_value)
}

.create_dataFrame <- function(meta_list, name_samples, show_value) {
    names(meta_list) <- name_samples
    
    set_meta <- unique(
        unlist(
            vapply(meta_list, names)
        )
    )
    
    complete_list <- mapply(function(x, y){
        # get missing keys
        missing <- set_meta[!(set_meta %in% names(meta_list[[y]]))]
        list <- meta_list[[y]]
        # fill list with missing keys
        list[missing] <- NA
        list <- list[set_meta]
    },meta_list, names(meta_list))
    row.names(complete_list) <- set_meta
    data_frame <- data.frame(complete_list)
    
    # show logical data frame
    if(!show_value) {
        data_frame <- as.data.frame(!is.na(data_frame))
    }
    
    return(data_frame)
}
