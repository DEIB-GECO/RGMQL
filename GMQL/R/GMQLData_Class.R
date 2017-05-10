### =========================================================================
### GMQLData objects
### -------------------------------------------------------------------------
###

setClass("GMQLData",
         contains=c("CompressedList", "List"),
         slots = c(
           name_dataset = "character",
           sample="GMQLSample"
         ))

setClass("SimpleGMQLData",
         contains = c("GMQLData", "SimpleList"))

### -------------------------------------------------------------------------
### Constructor
###


GMQLData <- function(name_dataset) {

  if(missing(name_dataset))
    name_dataset = "no dataset name"

  new("GMQLData", name_dataset = name_dataset)
}



### -------------------------------------------------------------------------
### Accessor
###




### -------------------------------------------------------------------------
### Methods
###
