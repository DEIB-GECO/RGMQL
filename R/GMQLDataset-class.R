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
setGeneric("aggregate", function(x, ...) 
                                    standardGeneric("aggregate"))



#' Method filter
#' 
#' Wrapper to GMQL select function
#' 
#' @name filter
#' @rdname filter-GMQLDataset-method
#' @aliases filter
#' 
setGeneric("filter", function(.data, m_predicate = NULL, r_predicate = NULL, 
                            semijoin = NULL, ...) standardGeneric("filter"))

#' Method cover
#' 
#' Wrapper to GMQL cover function
#' 
#' @name cover
#' @rdname cover-GMQLDataset-method
#' @aliases cover
#' 
setGeneric("cover", function(data, min_acc, max_acc, groupBy = NULL, 
                                variation = "cover", ...)
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


#' Method collect
#' 
#' Wrapper to GMQL materialize function
#' 
#' @name collect
#' @rdname collect-GMQLDataset-method
#' @export
setGeneric("collect", function(x, dir_out = getwd(), name = "ds1", ...) 
                standardGeneric("collect"))


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
#' @aliases extend GMQLDataset-method
#' @exportMethod extend
setGeneric("extend", function(.data, ...) standardGeneric("extend"))


#' Method select
#' 
#' Wrapper to GMQL project function
#' 
#' @name select
#' @rdname select-GMQLDataset-method
#' @aliases select 
#' @export
setGeneric("select", function(.data, ...) standardGeneric("select"))

#' Method arrange
#' 
#' Wrapper to GMQL order function
#' 
#' @name arrange
#' @rdname arrange-GMQLDataset-method
#' @aliases arrange 
#' @export
#' 
setGeneric("arrange", function(.data, metadata_ordering = NULL, 
                    regions_ordering = NULL, fetch_opt = NULL, num_fetch = 0, 
                    reg_fetch_opt = NULL, reg_num_fetch = 0, ...) 
                            standardGeneric("arrange"))

