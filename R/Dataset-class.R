#' Class GMQLDataset
#' 
#' Abstract class representing GMQL dataset
#'
#' @importClassesFrom S4Vectors DataTable
#' @slot value value associated to GMQL dataset
#' @name GMQLDataset-class
#' @rdname GMQLDataset-class
#' 
setClass("GMQLDataset",
            contains = c("DataTable"),
            representation(value = "character"))

#' @name GMQLDataset
#' @importFrom methods new
#' 
#' @param value value associated to GMQL dataset
#' @rdname GMQLDataset-class
#' 
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


#' Method mutate
#' 
#' Wrapper to GMQL extend function
#' 
#' @name mutate
#' @rdname mutate-methods
#' 
setGeneric("mutate", function(.data, metadata = NULL) 
                                standardGeneric("mutate"))

# insted of GMQL order
# setGeneric("sort", function(data, metadata_ordering = NULL, 
# regions_ordering = NULL, fetch_opt = NULL, 
# num_fetch = 0, reg_fetch_opt = NULL, 
# reg_num_fetch = 0) standardGeneric("sort"))

#' Method mutate
#' 
#' Wrapper to GMQL merge function
#' 
#' @name aggregate
#' @rdname aggregate-methods
#' 
setGeneric("aggregate", function(data, groupBy = NULL) 
                                    standardGeneric("aggregate"))


#' Method join
#' 
#' Wrapper to GMQL join function
#' 
#' @name join
#' @rdname join-methods
#' @aliases join
#' 
setGeneric("join", function(x, y, by = NULL, ...) standardGeneric("join"))


