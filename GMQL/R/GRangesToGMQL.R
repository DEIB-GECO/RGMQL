#' Create GMQL dataset from GrangesList
#'
#' It create Gmql dataset from GRangesList
#' All sample are in GDM (tab-separated values) file format
#'
#'
#' @param samples GrangesList
#' @param dir_out folder path where create a folder and write all the sample files
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf} \code{\link{importGMQL.gtf} }
#'
#'
#' @details
#' The GMQL dataset is made up by two differet file type
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
#' \dontrun{
#' g = Granges()
#' g1 = Granges()
#' grl = GRangesList(g,g1)
#' path = "<path_folder_output>"
#' exportGMQL.gdm(grl,path)
#' }
#'
exportGMQL.gdm <- function(samples, dir_out)
{
  .exportGMQL(samples,dir_out,to_GTF = F)
}

#' Create GMQL dataset from GrangesList
#'
#' It create Gmql dataset from GRangesList
#' All sample are in GTF file format
#'
#'
#' @param samples GrangesList
#' @param dir_out folder path where create a folder and write all the sample files
#'
#' @seealso \code{\link{exportGMQL.gdm}} \code{\link{exportGMQL.gtf} \code{\link{importGMQL.gdm} }
#'
#' @details
#' The GMQL dataset is made up by two differet file type
#' \itemize{
#' \item{metadata files: contains metadata associated to corrisponding sample}
#' \item{region files: contains many chromosome regions }
#' \item{region schema file: XML file contains region attribute (e.g. chr, start, end, pvalue)}
#' }
#' regions sample file and metadata file are associated through file name:
#' for example S_0001.gtf for regions file and S_0001.gtf.meta for its metadata
#'
#'
#' @examples
#' \dontrun{
#' g = Granges()
#' g1 = Granges()
#' grl = GRangesList(g,g1)
#' path = "<path_folder_output>"
#' exportGMQL.gtf(grl,path)
#' }
#'
#'
exportGMQL.gtf <- function(samples, dir_out)
{
  .exportGMQL(samples,dir_out,to_GTF = T)
}



.exportGMQL <- function(samples, dir_out,to_GTF)
{
  if(dir.exists(dir_out))
    stop("Directory already exists")

  dir.create(dir_out)
  files_sub_dir <- paste0(dir_out,"/files")
  dir.create(files_sub_dir)

  if(!is(samples,"GRangesList"))
    stop("samples must be a GrangesList")

  c = .counter()

  col_names <- .get_schema_names(samples)

  if(to_GTF)
  {
   #write region
    lapply(samples,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gtf")
      export.gff2(x,sample_name)
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
      write.table(region_frame,sample_name,col.names = F,row.names = F, sep = '\t',quote = F)
    },files_sub_dir)

    c = .counter(0)
    meta <- metadata(samples)

    #write metadata
    lapply(meta,function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gdm")
      .write_metadata(x,sample_name)
    },files_sub_dir)
  }

  .write_schema(col_names,files_sub_dir)
  c = .counter(0)
}


#move to internals

.counter <- function(zero = 0)
{
  i <- zero
  function() {
    i <<- i + 1
    toString <- as.character(i)
  }
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
  write.table(data,file_meta_name,row.names = F,col.names = F, quote = F,sep = '\t')
}


.get_schema_names <- function(samples)
{
  region_frame <- data.frame(samples[[1]]) # first regions to get column names
  col_names <- names(region_frame)
  col_names[!col_names %in% "width"] # elimino width
}

.write_schema <- function(node_list,directory)
{
  schema <- paste0(directory,"/test.schema")
  root <- xml_new_root("gmqlSchemaCollection")
  xml_attr(root,"name") <- "DatasetName_SCHEMAS"
  xml_attr(root,"xmlns") <- "http://genomic.elet.polimi.it/entities"
  xml_add_child(root,"gmqlSchema")
  gmqlSchema <- xml_child(root,1)

  lapply(node_list, function(x,gmqlSchema){
    field <- xml_add_child(gmqlSchema,"field")
    xml_text(field) <- x
  },gmqlSchema)
  write_xml(root,schema)
}

