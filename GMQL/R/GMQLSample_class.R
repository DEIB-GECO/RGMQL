### =========================================================================
### GMQLSample objects
### -------------------------------------------------------------------------
###

setClass("GMQLSample",
         slots = c(
           id_sample ="numeric",
           regions="GRanges",
           metadata="list"
         )
)


### -------------------------------------------------------------------------
### Constructor
###


GMQLSample <- function(id_sample, regions=GRanges(), metadata=NULL)
{
  if(is.null(metadata))
    metadata = list()

  new("GMQLSample", id_sample = id_sample,regions=regions, metadata = metadata)
}

### -------------------------------------------------------------------------
### Accessor
###

setGeneric("Regions", function(object, ...) standardGeneric("Regions"))
setMethod("Regions", "GMQLSample",function(object) object@regions)

setGeneric("ID", function(object, ...) standardGeneric("ID"))
setMethod("ID", "GMQLSample",function(object) object@id_sample)


#setGeneric("metadata", function(object, ...) standardGeneric("metadata"))
#setMethod("metadata", "GMQLSample",function(object) object@metadata)


### -------------------------------------------------------------------------
### Methods
###

setMethod("show",
          signature = "GMQLSample",
          definition =function(object){
            cat("An object of class ", class(object), "\n",sep = "")
            cat("Sample number: ",object@id_sample,"\n", sep = "")
            do.call(show,list(object@regions),envir = as.environment("package:GenomicRanges"))

            cat(length(object@metadata)," metadata associated to this sample")
            invisible(NULL)
          })


