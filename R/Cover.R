#' GMQL Operation: COVER
#'
#' It takes as input a dataset containing one or more samples and returns 
#' another dataset (with a single sample, if no \emph{groupby} option is 
#' specified) by “collapsing” the input dataset samples and their regions 
#' according to certain rules specified by the input parameters.
#' The attributes of the output genomic regions are only the region 
#' coordinates, and Jaccard indexes (JaccardIntersect and JaccardResult).
#' Jaccard Indexes are standard measures of similarity of the contributing 
#' regions, added as default region attributes.
#' The JaccardIntersect index is calculated as the ratio between the lengths 
#' of the intersection and of the union of the contributing regions; 
#' the JaccardResult index is calculated as the ratio between the lengths 
#' of the result and the union of the contributing regions.
#' If aggregate functions are specified, a new attributes is added for 
#' each aggregate function specified.
#' Output metadata are the union of the input ones.
#' If \emph{groupby} clause is specified, the input samples are partitioned 
#' in groups, each with distinct values of the grouping metadata attributes, 
#' and the \emph{cover} operation is separately applied to each group, 
#' yielding to one sample in the result for each group.
#' Input samples that do not satisfy the \emph{groupby} condition 
#' are disregarded.
#'
#' @importFrom methods is
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' minAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param maxAcc maximum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER calss object: \code{\link{ANY}}} that acts as a wildcard, 
#' considering any amount of overlapping.
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param groupBy list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @seealso \code{\link{summit}} \code{\link{flat}} \code{\link{histogram}}
#'
#' @examples
#' 
#' ## This statement produces an output dataset with a single output sample. 
#' ## The COVER operation considers all areas defined by a minimum 
#' ## of two overlapping regions in the input samples, 
#' ## up to any amount of overlapping regions.
#' 
#' init_gmql()
#' test_path <- system.file("example","DATASET",package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(input_data = exp, 2, ANY())
#'
#' \dontrun{
#' ## This GMQL statement computes the result grouping the input exp samples 
#' ## by the values of their cell metadata attribute, 
#' ## thus one output res sample is generated for each cell type; 
#' ## output regions are produced where at least 2 and at most 3 regions 
#' ## of grouped exp samples overlap, setting as attributes of the resulting 
#' ## regions the minimum pvalue of the overlapping regions (min_pvalue) 
#' ## and their Jaccard indexes (JaccardIntersect and JaccardResult).
#' 
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = cover(exp, 2, 3, c("cell"), list(min_pValue = MIN("pvalue")))
#' }
#' @export
#'
cover <- function(input_data, minAcc, maxAcc, groupBy = NULL, 
                    aggregates = NULL)
{
    minAcc <- .trasform_cover(deparse(substitute(minAcc)))
    maxAcc <- .trasform_cover(deparse(substitute(maxAcc)))
    
    min <- .check_cover_param(minAcc,TRUE)
    max <- .check_cover_param(maxAcc,FALSE)
    
    .doVariant("COVER",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: HISTOGRAM
#'
#' returns the non-overlapping regions contributing to the cover,
#' each with its accumulation index value, which is assigned to 
#' the AccIndex region attribute.
#'
#' @importFrom methods is
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' minAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param maxAcc maximum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER calss object: \code{\link{ANY}}} that acts as a wildcard, 
#' considering any amount of overlapping.
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param groupBy list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{summit}}
#'
#' @examples
#'
#' ## This GMQL statement computes the result grouping the input \emph{exp} 
#' ## samples by the values of their \emph{cell} metadata attribute, 
#' ## thus one output \emph{res} sample is generated for each cell type. 
#' ## Output regions are produced by dividing results from COVER in contiguous 
#' ## subregions according to the varying accumulation values 
#' ## (from 2 to 4 in this case): one region for each accumulation value;
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = histogram(exp, 2, 4, groupBy = c("cell"))
#' 
#' @export
#'
histogram <- function(input_data, minAcc, maxAcc, groupBy = NULL, 
                        aggregates = NULL)
{
    minAcc <- .trasform_cover(deparse(substitute(minAcc)))
    maxAcc <- .trasform_cover(deparse(substitute(maxAcc)))
    
    min <- .check_cover_param(minAcc,TRUE)
    max <- .check_cover_param(maxAcc,FALSE)
    
    .doVariant("HISTOGRAM",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: SUMMIT
#'
#' returns regions that start from a position
#' where the number of intersecting regions is not increasing afterwards and
#' stops at a position where either the number of intersecting regions 
#' decreases, or it violates the max accumulation index.
#'
#' @importFrom methods is
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param minAcc minimum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' minAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param maxAcc maximum number of overlapping regions to be considered 
#' during execution
#' Is a integer number, declared also as string.
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER calss object: \code{\link{ANY}}} that acts as a wildcard, 
#' considering any amount of overlapping.
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param groupBy list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @seealso \code{\link{flat}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#'
#' ## This GMQL statement computes the result grouping the input \emph{exp} 
#' ## samples by the values of their \emph{cell} metadata attribute, 
#' ## thus one output \emph{res} sample is generated for each cell type.
#' ## Output regions are produced by extracting the highest accumulation 
#' ## overlapping (sub)regions according to the methodologies described above;
#'
#'
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = summit(input_data = exp, 2, 4, c("cell"))
#' 
#' @export
#'
summit <- function(input_data, minAcc, maxAcc, groupBy = NULL,
                    aggregates = NULL)
{
    minAcc <- .trasform_cover(deparse(substitute(minAcc)))
    maxAcc <- .trasform_cover(deparse(substitute(maxAcc)))
    
    min <- .check_cover_param(minAcc,TRUE)
    max <- .check_cover_param(maxAcc,FALSE)
    
    .doVariant("SUMMIT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

#' GMQL Operation: FLAT
#'
#' returns the contiguous region that starts from the first end and stops at
#' the last end of the regions which would contribute to each region 
#' of the COVER
#'
#' @importFrom methods is
#' @importFrom rJava J
#' @importFrom rJava .jnull
#' @importFrom rJava .jarray
#' 
#' @param input_data returned object from any GMQL function
#' @param minAcc integer number representing minimum number of overlapping 
#' regions to be considered during execution
#' minAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param maxAcc integer number representing maximum number of overlapping 
#' regions to be considered during execution
#' maxAcc accept also:
#' \itemize{
#' \item{PARAMETER class object: \code{\link{ALL}} that represents the number 
#' of samples in the input dataset}
#' \item{PARAMETER calss object: \code{\link{ANY}}} that acts as a wildcard, 
#' considering any amount of overlapping.
#' \item{and expression built using PARAMETER object: (ALL() + N) / K or
#' ALL() / K }
#' }
#' @param groupBy list of CONDITION objects where every object contains 
#' the name of metadata to be used in semijoin, or simple string concatenation 
#' of name of metadata, e.g. c("cell_type", "attribute_tag", "size") 
#' without declaring condition.
#' The CONDITION's available are:
#' \itemize{
#' \item{\code{\link{FULL}}: Fullname evaluation, two attributes match 
#' if they both end with value and, if they have a further prefixes,
#' the two prefix sequence are identical}
#' \item{\code{\link{EXACT}}: Exact evaluation, only attributes exactly 
#' as value will match; no further prefixes are allowed. }
#' }
#' Every condition accepts only one string value. (e.g. FULL("cell_type") )
#' In case of single concatenation with no CONDITION or list with some value 
#' without conditon, the metadata are considered having default 
#' evaluation: the two attributes match if both end with value.
#' 
#' @param aggregates list of element in the form \emph{key} = \emph{aggregate}.
#' The \emph{aggregate} is an object of class AGGREGATES
#' The aggregate functions available are: \code{\link{SUM}}, 
#' \code{\link{COUNT}}, \code{\link{MIN}}, \code{\link{MAX}}, 
#' \code{\link{AVG}}, \code{\link{MEDIAN}}, \code{\link{STD}}, 
#' \code{\link{BAG}}, \code{\link{BAGD}}, \code{\link{Q1}}, 
#' \code{\link{Q2}}, \code{\link{Q3}}.
#' Every aggregate accepts a string value, execet for COUNT, which does not 
#' have any value.
#' Argument of 'aggregate function' must exist in schema, i.e. among region 
#' attributes. Two style are allowed:
#' \itemize{
#' \item list of key-value pairs: e.g. sum = SUM("pvalue")
#' \item list of values: e.g. SUM("pvalue")
#' }
#' "mixed style" is not allowed
#'
#' @return DataSet class object. It contains the value to use as input 
#' for the subsequent GMQL function
#' 
#' @seealso \code{\link{summit}} \code{\link{cover}} \code{\link{histogram}}
#'
#' @examples
#' 
#' ## This GMQL statement computes the result grouping the input \emph{exp} 
#' ## samples by the values of their \emph{cell} metadata attribute, 
#' ## thus one output \emph{res} sample is generated for each cell type. 
#' ## Output regions are produced by concatenating all regions which would 
#' ## have been used to construct a COVER(2,4) statement on the same dataset; 
#' 
#' init_gmql()
#' test_path <- system.file("example", "DATASET", package = "RGMQL")
#' exp = read_dataset(test_path)
#' res = flat(input_data = exp, 2, 4, c("cell"))
#'
#' @export
#'
flat <- function(input_data, minAcc, maxAcc, groupBy = NULL, aggregates = NULL)
{
    minAcc <- .trasform_cover(deparse(substitute(minAcc)))
    maxAcc <- .trasform_cover(deparse(substitute(maxAcc)))
    
    min <- .check_cover_param(minAcc,TRUE)
    max <- .check_cover_param(maxAcc,FALSE)
    
    .doVariant("FLAT",minAcc,maxAcc,groupBy,aggregates,input_data)
}

.doVariant <- function(flag,minAcc,maxAcc,groupBy,aggregates,input_data)
{
    if(!is.null(groupBy))
        join_condition_matrix <- .jarray(.join_condition(groupBy),
                                            dispatch = TRUE)
    else
        join_condition_matrix <- .jnull("java/lang/String")

    if(!is.null(aggregates))
        metadata_matrix <- .jarray(.aggregates(aggregates,"AGGREGATES"),
                                    dispatch = TRUE)
    else
        metadata_matrix <- .jnull("java/lang/String")

    WrappeR <- J("it/polimi/genomics/r/Wrapper")
    response <- switch(flag,
                "COVER" = WrappeR$cover(minAcc, maxAcc, join_condition_matrix,
                                    metadata_matrix, input_data$value),
                "FLAT" = WrappeR$flat(minAcc, maxAcc, join_condition_matrix,
                                    metadata_matrix,input_data$value),
                "SUMMIT" = WrappeR$summit(minAcc,maxAcc, join_condition_matrix,
                                    metadata_matrix, input_data$value),
                "HISTOGRAM" = WrappeR$histogram(minAcc, maxAcc, 
                                join_condition_matrix, metadata_matrix,
                                input_data$value))

    error <- strtoi(response[1])
    data <- response[2]
    if(error!=0)
        stop(data)
    else
        DataSet(data)
}

.check_cover_param <- function(param,is_min)
{
    if(length(param)>1)
        stop("length > 1")

    if(is.numeric(param))
    {
        if(param<=0)
            stop("No negative value")
        else
            return(as.character(param))
    }
    else if(is.character(param))
    {
        if(is_min && identical(param,"ANY"))
            stop("min cannot assume ANY as value")
        return(param)
    }
    else
        stop("invalid input data")
}

.trasform_cover <- function(predicate=NULL)
{
    predicate <- gsub("\\(\\)","",predicate)
}

