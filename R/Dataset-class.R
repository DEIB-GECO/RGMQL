#'
#' Abstract class representing GMQL dataset
#' 
#' @importClassesFrom S4Vectors DataTable
#' 
#' @name GMQLDataset-class
#' @rdname GMQLDataset-class
#' 
setClass("GMQLDataset",
            contains = c("DataTable"),
            representation(value = "character"))

# Constructor GMQLDataset
GMQLDataset <- function(value) {
    dataset <- new("GMQLDataset",value = value)
    return(dataset)
}
    
    
setMethod("show", "GMQLDataset",
            function(object)
            {
                cat("GMQL Dataset \n")
                cat(" value :",paste(object@value))
            })

## insted of GMQL select
setGeneric("filter", function(data, m_predicate = NULL, r_predicate = NULL, 
                                semi_join = NULL, semi_join_negation = FALSE, 
                                semi_join_dataset = NULL) 
                            standardGeneric("filter"))

## insted of GMQL extend
setGeneric("mutate", function(.data, metadata = NULL) 
                                standardGeneric("mutate"))

# insted of GMQL order
# setGeneric("sort", function(data, metadata_ordering = NULL, 
# regions_ordering = NULL, fetch_opt = NULL, 
# num_fetch = 0, reg_fetch_opt = NULL, 
# reg_num_fetch = 0) standardGeneric("sort"))

## insted of GMQL merge
setGeneric("aggregate", function(data, groupBy = NULL) 
                                    standardGeneric("aggregate"))


