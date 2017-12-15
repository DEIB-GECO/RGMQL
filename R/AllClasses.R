#' Class GMQLDataset
#' 
#' Abstract class representing GMQL dataset
#'
#' @importClassesFrom S4Vectors DataTable
#' @slot value value associated to GMQL dataset
#' @name GMQLDataset-class
#' @rdname GMQLDataset-class
#' @noRd
#' @return instance of GMQL dataset
#' 
setClass("GMQLDataset",
            contains = c("DataTable"),
            representation(value = "character"))

#' GMQLDataset alloc Function
#' 
#' Alloc GMQLDataset object with its value
#' 
#' @name GMQLDataset
#' @importFrom methods new
#' 
#' @param value value associated to GMQL dataset
#' @rdname GMQLDataset-class
#' @noRd
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

setGeneric("value", function(.dataset) standardGeneric("value"))

setMethod("value", "GMQLDataset", function(.dataset) .dataset@value)


