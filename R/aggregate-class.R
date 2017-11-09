#############################
#       AGGREGATES          #
#############################

AGGREGATES <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "AGGREGATES"
    return(op_list)
}

check.META_AGGREGATES <- function(value)
{
    if(is.character(value) && length(value)>1)
        stop("value: no multiple string")
    
    if(!is.character(value))
        stop("value: is not a string")
}

META_AGGREGATES <- function(value)
{
    op_list <- list(value = value)
    ## Set the name for the class
    class(op_list) <- "META_AGGREGATES"
    return(op_list)
}

print.META_AGGREGATES <- function(obj) {
    res <- as.character(obj)
    cat(res)
}

as.character.META_AGGREGATES <- function(obj) {
    class <- class(obj)[1]
    val <- obj$value
    c(class,val)
}

take_value.META_AGGREGATES <- function(obj){
    class <- class(obj)[1]
    val <- obj$value
    text <- switch(class,
                "SUM" = paste0("sum_",val),
                "MIN" = paste0("min_",val),
                "MAX" = paste0("max_",val),
                "COUNT" = paste0("count"),
                "BAG" = paste0("bag_",val),
                "BAGD" = paste0("bagd_",val),
                "AVG" = paste0("avg_",val),
                "STD" = paste0("std_"),
                "MEDIAN" = paste0("median_",val),
                "Q1" = paste0("q1_",val),
                "Q2" = paste0("q2_"),
                "Q3" = paste0("q3_",val)
                )
    text
}

#' AGGREGATES object class constructor
#' 
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function sum,
#' performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return SUM aggregate object
#' 
#' @seealso \code{\link{COUNT}} \code{\link{MIN}} \code{\link{MAX}} 
#' \code{\link{AVG}} \code{\link{MEDIAN}} \code{\link{STD}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, and 
#' ## then calculates new metadata attributes for each of them: 
#' ## sum_score is the sum of score of the sample regions.
#' 
#' res = extend(input_data = exp, list(sum_score = SUM("score")))
#' 
#' @export
#'
SUM <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("SUM","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function minimum,
#' performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return MIN aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MAX}} 
#' \code{\link{AVG}} \code{\link{MEDIAN}} \code{\link{STD}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## MinP is the minimum pvalue of the sample regions.
#' 
#' res = extend(input_data = exp, list(minP = MIN("pvalue")))
#' 
#' @export
#'
MIN <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("MIN","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function maximum,
#' performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return MAX aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{AVG}} \code{\link{MEDIAN}} \code{\link{STD}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' 
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## max_score is the maximum score of the sample regions.
#' 
#' res = extend(input_data = exp, list(max_score = MAX("score")))
#' 
#' 
#' @export
#'
MAX <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("MAX","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function aritmetic
#' mean, performing all the type conversions needed
#' 
#' @param value string identifying name of metadata or region attribute
#'
#' @return AVG aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{MEDIAN}} \code{\link{STD}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#'
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## The following cover operation produces output regions where at least 2 
#' ## and at most 3 regions ofexp overlap, having as resulting region 
#' ## attributes the average signal of the overlapping regions; 
#' ## the result has one sample for each input cell.
#' 
#' res = cover(input_data = exp,2,3, c("cell"), 
#' list(avg_signal = AVG("signal")))
#'
#' @export
#'
AVG <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("AVG","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function bag,
#' this function creates comma-separated strings of distinct attribute values,
#' performing all the types conversions needed
#' 
#' @param value string identifying name of metadata or region attribute
#'
#' @return BAG aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' @examples
#' 
#' ## local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_dataset(test_path)
#' 
#' ## It copies all samples of DATA into OUT dataset, and then for each of 
#' ## them it adds another metadata attribute, allScores, 
#' ## which is the aggregation comma-separated list of all the values 
#' ## that the region attribute score takes in the sample.
#' 
#' out = extend(input_data = data, list(allScore = BAG("score")))
#'
#' @export
#'
BAG <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("BAG","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function count,
#' performing all the type conversions needed
#'
#' @return COUNT aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' @examples
#' 
#' ## local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## counts the regions in each sample and stores their number as value 
#' ## of the new metadata RegionCount attribute of the sample.
#' 
#' out = extend(input_data = exp, list(RegionCount = COUNT()))
#'
#' @export
#'
COUNT <- function()
{
    list <- list()
    ## Set the name for the class
    class(list) <- c("COUNT","AGGREGATES","META_AGGREGATES")
    return(list)
}
as.character.COUNT <- function(obj) {
    class <- class(obj)[1]
    c(class,"")
}
check.COUNT <- function(obj){}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function 
#' standard deviation, performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return STD aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## std_score is the standard deviation score of the sample regions.
#' 
#' res = extend(input_data = exp, list(std_score = STD("score")))
#'
#' @export
#'
STD <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("STD","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function median,
#' performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return MEDIAN aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{STD}} 
#' \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## m_score is the median score of the sample regions.
#' 
#' res = extend(input_data = exp, list(m_score = MEDIAN("score")))
#'
#' @export
#'
MEDIAN <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("MEDIAN","AGGREGATES","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function 
#' first quartile, performing all the type conversions needed
#' 
#' @param value string identifying name of metadata attribute
#'
#' @return Q1 aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q2}} \code{\link{Q3}}
#'  
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## q1_score is the first quartile of score of the sample regions.
#' 
#' res = extend(input_data = exp, list(q1_score = Q1("score")))
#'
#'
#' @export
#'
Q1 <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("Q1","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function
#' second quartile, performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return Q2 aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q3}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## q2_score is the second quartile of score of the sample regions.
#' 
#' res = extend(input_data = exp, list(q2_score = Q2("score")))
#'
#' @export
#'
Q2 <- function(value)
{
    check.META_AGGREGATES(value)
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("Q2","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function 
#' third quartile, performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return Q3 aggregate object
#'
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAG}} \code{\link{BAGD}}
#' \code{\link{Q1}} \code{\link{Q2}}
#' 
#' @examples
#' 
#' ### local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' 
#' ## This statement copies all samples of exp into res dataset, 
#' ## and then calculates new metadata attributes for each of them: 
#' ## q3_score is the third quartile of score of the sample regions.
#' 
#' res = extend(input_data = exp, list(q3_score = Q3("score")))
#' 
#' @export
#'
Q3 <- function(value)
{
    check.META_AGGREGATES(value)

    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("Q3","META_AGGREGATES")
    return(list)
}

#' AGGREGATES object class constructor
#'
#' This class constructor is used to create instances of AGGREGATES object,
#' to be used in GMQL functions that require aggregate on value.
#' It prepares input parameter to be passed to the library function bagd,
#' this function creates comma-separated strings of distinct attribute values
#' performing all the type conversions needed
#' 
#' @param value string identifying name of region attribute
#'
#' @return BAGD aggregate object
#' 
#' @seealso \code{\link{SUM}} \code{\link{COUNT}} \code{\link{MIN}} 
#' \code{\link{MAX}} \code{\link{AVG}} \code{\link{MEDIAN}} 
#' \code{\link{STD}} \code{\link{BAG}}
#' \code{\link{Q1}} \code{\link{Q2}} \code{\link{Q3}}
#' 
#' @examples
#' 
#' ## local with CustomParser
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' data = read_dataset(test_path)
#' 
#' ## It copies all samples of DATA into OUT dataset, and then for each of 
#' ## them it adds another metadata attribute, allScores, which is the 
#' ## aggregation comma-separated list of all the distinct values that 
#' ## the region attribute score takes in the sample.
#' 
#' out = extend(input_data = data, list(allScore = BAGD("score")))
#'
#' @export
#'
BAGD <- function(value)
{
    check.META_AGGREGATES(value)
    
    list <- list(value = value)
    ## Set the name for the class
    class(list) <- c("BAGD","AGGREGATES","META_AGGREGATES")
    return(list)
}

