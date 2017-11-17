#' Class GMQLDataset
#' 
#' Abstract class representing GMQL dataset
#'
#' @importClassesFrom S4Vectors DataTable
#' @slot value value associated to GMQL dataset
#' @name GMQLDataset-class
#' @rdname GMQLDataset-class
#' 
#' @return instance of GMQL dataset

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



# insted of GMQL order
# setGeneric("sort", function(data, metadata_ordering = NULL, 
# regions_ordering = NULL, fetch_opt = NULL, 
# num_fetch = 0, reg_fetch_opt = NULL, 
# reg_num_fetch = 0) standardGeneric("sort"))

#' Method aggregate
#' 
#' Wrapper to GMQL merge function
#' 
#' @name aggregate
#' @rdname aggregate-GMQLDataset-method
#' @aliases aggregate
#'  
setGeneric("aggregate", function(data, ...) 
                                    standardGeneric("aggregate"))


#' Method join
#' 
#' Wrapper to GMQL join function
#' 
#' @name join
#' @rdname join-GMQLDataset-method
#' @aliases join
#' 
setGeneric("join", function(x, y, ...) standardGeneric("join"))


#' Method filter
#' 
#' Wrapper to GMQL select function
#' 
#' @name filter
#' @rdname filter-GMQLDataset-method
#' @aliases filter
#' 
setGeneric("filter", function(.data, ...) standardGeneric("filter"))

#' Method cover
#' 
#' Wrapper to GMQL cover function
#' 
#' @name cover
#' @rdname cover-GMQLDataset-method
#' @aliases cover
#' 
setGeneric("cover", function(data, min_acc, max_acc, ...)
                        standardGeneric("cover"))

#' Method map
#' 
#' Wrapper to GMQL map function
#' 
#' @name map
#' @rdname map-GMQLDataset-method
#' @aliases map
#' 
setGeneric("map", function(x, y, ...) standardGeneric("map"))


#' Method materialize
#' 
#' Wrapper to GMQL materialize function
#' 
#' @name materialize
#' @rdname materialize-GMQLDataset-method
#' @export
setGeneric("materialize", function(data, ...) standardGeneric("materialize"))


#' Method take
#' 
#' GMQL Operation: TAKE
#' 
#' @name take
#' @rdname take-GMQLDataset-method
#' @export
setGeneric("take", function(data, ...) standardGeneric("take"))


#' Method extend
#' 
#' Wrapper to GMQL extend function
#' 
#' @name extend
#' @rdname extend-GMQLDataset-method
#' @aliases extend, GMQLDataset-method
#' @exportMethod extend
setGeneric("extend", function(.data, ...) standardGeneric("extend"))



