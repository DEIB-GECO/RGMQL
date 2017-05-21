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

  dots <- list(...)
  if(length(dots)==1 && class(dots[[1]])=="GRangesList") #prendo solo un Grangeslist
    dots <- dots[[1]]

  c = .counter()

  region_frame <- data.frame(dots[[1]]) # first regions to get column names
  col_names <- names(region_frame)

  if(to_GTF)
  {
    apply(dots, function(x,dir){
      sample_name = paste0(dir,"/S_",c(),".gtf")
      export.gff2(x,sample_name)
      meta_list <- metadata(x)
      .write_metadata(meta_list,sample_name)
    },files_sub_dir)
  }
  else
  {
    apply(dots, function(x){
      sample_name = paste0(dir,"/S_",c(),".gdm")
      region_frame <- data.frame(x)
      write.table(region_frame,sample_name,col.names = F,row.names = F)
      meta_list <- metadata(x)
      .write_metadata(meta_list,sample_name)
    })
  }
  #.write_schema(col_names,files_sub_dir)
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
  if(length(meta_list)==0){
    #crea lista con metadati
  }
  names_list <- names(meta_list)
  value_list <- unlist(meta_list)
  file_meta_name = paste0(sample_name,".meta")
  data <- data.frame(names_list,value_list)
  write.table(data,file_meta_name)
}


.write_schema <- function(schema_names,sample_name)
{

}

