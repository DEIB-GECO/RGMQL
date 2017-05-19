#'
#' Create GMQL dataset from Granges or GrangesList
#'
#'
#'
#'@param ... set of Granges or a single GrangesList
#'@param dir_out folder path where create a folder and write all the sample files
#'
exportGMQL.gdm <- function(..., dir_out)
{
  .exportGMQL(...,dir_out,to_GTF = F)
}

#'
#' Create GMQL dataset from Granges or GrangesList
#'
#'
#'
#'@param ... set of Granges or a single GrangesList
#'@param dir_out folder path where create a folder and write all the sample files
#'
exportGMQL.gtf <- function(..., dir_out)
{
  .exportGMQL(...,dir_out,to_GTF = T)
}



.exportGMQL <- function(..., dir_out,to_GTF)
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

