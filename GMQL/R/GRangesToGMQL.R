#' Create GMQL dataset from GrangesList
#'
#' It create Gmql dataset from GRangesList
#' All sample are in GDM (tab-separated values) file format
#'
#' @import xml2
#' @importFrom plyr revalue
#' @importFrom rtracklayer export
#' @importFrom utils write.table
#' @importFrom methods is
#' @importFrom S4Vectors metadata
#' @import GenomicRanges
#'
#' @param samples GrangesList
#' @param dir_out folder path where create a folder and write all the sample files
#'
#' @return no value return
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gtf}}
#'
#'
#'
#' @details
#' The GMQL dataset is made up by two differet file type
#'
#' \itemize{
#' \item{metadata files: contains metadata associated to corrisponding sample}
#' \item{region files: contains many chromosome regions }
#' \item{region schema file: XML file contains region attribute (e.g. chr, start, end, pvalue)}
#' }
#' regions sample file and metadata file are associated through file name:
#' for example S_0001.gdm for regions file and S_0001.gdm.meta for its metadata
#'
#'
#' @examples
#'
#' library(GenomicRanges)
#' gr1 <- GRanges(seqnames = "chr2", ranges = IRanges(3, 6), strand = "+", score = 5L, GC = 0.45)
#' gr2 <- GRanges(seqnames = c("chr1", "chr1"),
#' ranges = IRanges(c(7,13), width = 3), strand = c("+", "-"), score = 3:4, GC = c(0.3, 0.5))
#' grl = GRangesList(gr1,gr2)
#' test_out_path <- system.file("example",package = "GMQL")
#' exportGMQL.gdm(grl,test_out_path)
#'
#'
#' @export
#'
exportGMQL.gdm <- function(samples, dir_out)
{
  .exportGMQL(samples,dir_out,to_GTF = FALSE)
  print("Export to GDM complete")
}

#' Create GMQL dataset from GrangesList
#'
#' It create Gmql dataset from GRangesList
#' All sample are in GTF file format
#'
#' @import xml2
#' @importFrom plyr revalue
#' @importFrom rtracklayer export
#' @importFrom methods is
#' @importFrom utils write.table
#' @importFrom S4Vectors metadata
#' @import GenomicRanges
#'
#' @param samples GrangesList
#' @param dir_out folder path where create a folder and write all the sample files
#'
#' @return no value return
#'
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf}} \code{\link{importGMQL.gdm} }
#'
#' @details
#' The GMQL dataset is made up by two differet file type
#' \itemize{
#' \item{metadata files: contains metadata associated to corrisponding sample}
#' \item{region files: contains many chromosome regions }
#' \item{region schema file: XML file contains region attribute (e.g. chr, left, right, qvalue)}
#' }
#' regions sample file and metadata file are associated through file name:
#' for example S_0001.gtf for regions file and S_0001.gtf.meta for its metadata
#'
#'
#' @examples
#'
#' library(GenomicRanges)
#' gr1 <- GRanges(seqnames = "chr2", ranges = IRanges(3, 6), strand = "+", score = 5L, GC = 0.45)
#' gr2 <- GRanges(seqnames = c("chr1", "chr1"),
#' ranges = IRanges(c(7,13), width = 3), strand = c("+", "-"), score = 3:4, GC = c(0.3, 0.5))
#' grl = GRangesList(gr1,gr2)
#' test_out_path <- system.file("example",package = "GMQL")
#' exportGMQL.gtf(grl,test_out_path)
#'
#' @export
#'
#'
exportGMQL.gtf <- function(samples, dir_out)
{
  .exportGMQL(samples,dir_out,to_GTF = TRUE)
  print("Export to GTF complete")
}


.exportGMQL <- function(samples, dir_out,to_GTF)
{
  if(!is(samples,"GRangesList"))
    stop("samples must be a GrangesList")

  dir.create(dir_out)
  files_sub_dir <- paste0(dir_out,"/files")
  dir.create(files_sub_dir)
  c = .counter()
  #col_names <- .get_schema_names(samples)
  if(to_GTF)
  {
   #write region
    lapply(samples,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gtf")
      rtracklayer::export(x,sample_name,format = "gtf")
    },files_sub_dir)

    c = .counter(0)
    meta <- metadata(samples)

    #write metadata
    lapply(meta,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gtf")
      .write_metadata(x,sample_name)
    },files_sub_dir)
  }
  else
  {
    #write region
    lapply(samples,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gdm")
      region_frame <- data.frame(x)
      write.table(region_frame,sample_name,col.names = FALSE,row.names = FALSE, sep = '\t',quote = FALSE)
    },files_sub_dir)

    c = .counter(0)
    meta <- metadata(samples)

    #write metadata
    lapply(meta,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gdm")
      .write_metadata(x,sample_name)
    },files_sub_dir)
  }

  col_names <- sapply(elementMetadata(samples[[1]]),class) # first regions to get column names

  #write schema XML
  .write_schema(col_names,files_sub_dir,to_GTF)
  c = .counter(0)
}



.write_metadata <- function(meta_list,sample_name)
{
  #create my own list if metadata empty
  if(length(meta_list)==0){
    meta_list <- list(Provider = "Polimi", Application = "R-GMQL")
  }
  names_list <- names(meta_list)
  value_list <- unlist(meta_list)
  file_meta_name = paste0(sample_name,".meta")
  data <- data.frame(names_list,value_list)
  names(data) <- NULL
  write.table(data,file_meta_name,row.names = FALSE,col.names = FALSE, quote = FALSE,sep = '\t')
}

.write_schema <- function(columns,directory,to_GTF)
{
  if(to_GTF)
  {
    names(columns) <- plyr::revalue(names(columns),c(type = "feature",phase = "frame"))
    fixed_element = c(seqname = "character", source = "character", feature = "character",
                      start = "long", end = "long", score = "numeric", strand = "character",
                      frame = "character")
    node_list <- c(fixed_element, columns)
    node_list <- node_list[!duplicated(names(node_list))]
    #node_list <- node_list[base::order(match(node_list,correct_list_GTF))]
  }
  else
  {
    fixed_element = c(chr = "factor", left = "long", right = "long", strand = "character")
    node_list <- c(fixed_element, columns)
    #node_list <- plyr::revalue(node_list,c(seqnames = "chr",start = "left",end = "right"))
    #node_list <- node_list[base::order(match(node_list,correct_list_GDM))]
  }

  schema <- paste0(directory,"/test.schema")
  root <- xml2::xml_new_root("gmqlSchemaCollection")
  xml2::xml_attr(root,"name") <- "DatasetName_SCHEMAS"
  xml2::xml_attr(root,"xmlns") <- "http://genomic.elet.polimi.it/entities"
  xml2::xml_add_child(root,"gmqlSchema")
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




