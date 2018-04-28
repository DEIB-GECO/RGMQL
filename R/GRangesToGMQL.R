#' Create GMQL dataset from GRangesList
#'
#' It creates GMQL dataset from GRangesList.
#' All samples are in GDM (tab-separated values) or GTF file format
#'
#' @import xml2
#' @importFrom plyr revalue
#' @importFrom rtracklayer export
#' @importFrom utils write.table
#' @importFrom methods is
#' @importFrom S4Vectors metadata
#' @import GenomicRanges
#'
#' @param samples GRangesList
#' @param dir_out folder path where to create a folder and write the sample 
#' files
#' @param is_gtf logical value indicating if samples have to be exported
#' with GTF or GDM format
#'
#' @return None
#'
#' @seealso \code{\link{import_gmql}} 
#'
#' @details
#' The GMQL dataset is made up by two different file types:
#'
#' \itemize{
#' \item{metadata files: they contain metadata associated with corrisponding 
#' sample.}
#' \item{region files: they contain genomic regions data.}
#' \item{region schema file: XML file that contains region attribute names 
#' (e.g. chr, start, end, pvalue)}
#' }
#' Sample region files and metadata files are associated through file name:
#' for example S_0001.gdm for region file and S_0001.gdm.meta for 
#' its metadata file
#'
#'
#' @examples
#' 
#' ## Load and attach add-on GenomicRanges package
#' library(GenomicRanges)
#' 
#' ## These statemens create two GRanges with the region attributes: seqnames, 
#' ## ranges (region coordinates) and strand, plus two column elements:  
#' ## score and GC
#' 
#' gr1 <- GRanges(seqnames = "chr2", ranges = IRanges(3, 6), strand = "+", 
#'     score = 5L, GC = 0.45)
#' gr2 <- GRanges(seqnames = c("chr1", "chr1"),
#'     ranges = IRanges(c(7,13), width = 3), strand = c("+", "-"), 
#'     score = 3:4, GC = c(0.3, 0.5))
#' 
#' ## This statement creates a GRangesList using the previous GRanges 
#' 
#' grl = GRangesList(gr1, gr2)
#' 
#' ## This statement defines the path to the subdirectory "example" of the 
#' ## package "RGMQL" and exports the GRangesList as GMQL datasets with sample 
#' ## files in GTF file format, using the last name of 'dir_out' path as 
#' ## dataset name
#' 
#' test_out_path <- system.file("example", package = "RGMQL")
#' export_gmql(grl, test_out_path, TRUE)
#' 
#' 
#' @export
#'
export_gmql <- function(samples, dir_out, is_gtf)
{
    if(is_gtf)
        .exportGMQL.gtf(samples,dir_out,is_gtf)
    else
        .exportGMQL.gdm(samples,dir_out,is_gtf)
}

.exportGMQL.gdm <- function(samples, dir_out, to_GTF)
{
    .exportGMQL(samples,dir_out,to_GTF)
    print("Export to GDM complete")
}

.exportGMQL.gtf <- function(samples, dir_out,to_GTF)
{
    .exportGMQL(samples, dir_out, to_GTF)
    print("Export to GTF complete")
}


.exportGMQL <- function(samples, dir_out, to_GTF)
{
    if(!is(samples,"GRangesList"))
        stop("samples must be a GrangesList")

    if(!dir.exists(dir_out))
        dir.create(dir_out)
    
    files_sub_dir <- file.path(dir_out,"files")
    dir.create(files_sub_dir)
    cnt = .counter()
    #col_names <- .get_schema_names(samples)
    if(to_GTF)
    {
        #write region
        lapply(samples,function(x,dir){
            anonymusFile <- file()
            sample_name <- file.path(dir,paste0("S_",cnt(),".gtf"))
            g <- rtracklayer::export(x,sample_name,format = "gtf")
            lines <- readLines(anonymusFile)
            lines <- lines[-(1:3)] #delete first 3 lines
            writeLines(lines,sample_name)
            close(anonymusFile)
        },files_sub_dir)
        cnt = .counter(0)
        meta <- metadata(samples)

        #write metadata
        lapply(meta,function(x,dir){
            sample_name <- file.path(dir,paste0("S_",cnt(),".gtf"))
            .write_metadata(x,sample_name)
        },files_sub_dir)
    }
    else
    {
        #write region
        lapply(samples,function(x,dir){
            sample_name <- file.path(dir,paste0("S_",cnt(),".gdm"))
            region_frame <- data.frame(x)
            region_frame$start = region_frame$start - 1
            write.table(region_frame,sample_name,col.names = FALSE,
                            row.names = FALSE, sep = '\t',quote = FALSE)
        },files_sub_dir)

        cnt = .counter(0)
        meta <- metadata(samples)

        #write metadata
        lapply(meta,function(x,dir){
            sample_name <- file.path(dir,paste0("S_",cnt(),".gdm"))
            .write_metadata(x,sample_name)
        },files_sub_dir)
    }
    # first regions to get column names
    col_names <- vapply(elementMetadata(samples[[1]]),class,character(1)) 
    # write schema XML
    .write_schema(col_names,files_sub_dir,to_GTF)
    c = .counter(0)
}



.write_metadata <- function(meta_list,sample_name)
{
    #create my own list if metadata empty
    if(!length(meta_list))
        meta_list <- list(Provider = "Polimi", Application = "R-GMQL")
    
    names_list <- names(meta_list)
    value_list <- unlist(meta_list)
    file_meta_name = paste0(sample_name,".meta")
    data <- data.frame(names_list,value_list)
    names(data) <- NULL
    write.table(data,file_meta_name,row.names = FALSE,
                    col.names = FALSE, quote = FALSE,sep = '\t')
}

.write_schema <- function(columns,directory,to_GTF)
{
    if(to_GTF)
    {
        names(columns) <- plyr::revalue(names(columns),c(type = "feature",
                                            phase = "frame"))
        fixed_element = c(seqname = "character", source = "character", 
                        feature = "character",start = "long", end = "long", 
                            score = "numeric", strand = "character",
                            frame = "character")
        node_list <- c(fixed_element, columns)
        node_list <- node_list[!duplicated(names(node_list))]
    }
    else
    {
        fixed_element = c(chr = "factor", left = "long", right = "long", 
                            strand = "character")
        node_list <- c(fixed_element, columns)
    }

    schema <- file.path(directory,"granges.xml")
    root <- xml2::xml_new_root("gmqlSchemaCollection")
    xml2::xml_attr(root,"name") <- "DatasetName_SCHEMAS"
    xml2::xml_attr(root,"xmlns") <- "http://genomic.elet.polimi.it/entities"
    xml2::xml_add_child(root,"gmqlSchema")
    if(to_GTF)
    {
        xml2::xml_attr(root,"type") <- "gtf"
        xml2::xml_attr(root,"coordinate_system") <- "1-based"
    }
    else
    {
        xml2::xml_attr(root,"type") <- "tab"
        xml2::xml_attr(root,"coordinate_system") <- "0-based"
    }
    
    gmqlSchema <- xml2::xml_child(root,1)

    names_node <- names(node_list)

    mapply(function(type,text){
        field <- xml2::xml_add_child(gmqlSchema,"field")
        if(identical(type,"factor") || identical(type,"character"))
            xml2::xml_attr(field,"type") <- "STRING"
        else if(identical(type,"numeric") || identical(type,"integer"))
            xml2::xml_attr(field,"type") <- "DOUBLE"
        else if(identical(type,"long"))
            xml2::xml_attr(field,"type") <- "LONG"
        else
            xml2::xml_attr(field,"type") <- "NULL"
        xml2::xml_text(field) <- text

    },node_list,names_node)
    xml2::write_xml(root,schema)
}



