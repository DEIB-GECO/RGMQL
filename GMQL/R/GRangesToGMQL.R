#'
#'
#'create GMQL dataset from Granges or GrangesList
#'
#'
GRangesToGMQL <- function(..., dir_out,file_ouput_gtf = T)
{
  if(dir.exists(dir_out))
    stop("Directory already exists")

  dir.create(dir_out)
  files_sub_dir <- paste0(dir_out,"/files")
  dir.create(files_sub_dir)

  dots <- list(...)
  if(length(dots)==1) #prendo solo un Grangeslist
    dots <- dots[[1]]

  c = .counter()
  if(file_ouput_gtf)
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
      region_frame <- DataFrame(x)
      write.table(region_frame,sample_name)
      meta_list <- metadata(x)
      .write_metadata(meta_list,sample_name)
    })
  }

  .write_schema("",files_sub_dir)

}

#move to internals
.counter <- function(zero = 0) {
  i <- zero
  function() {
    i <<- i + 1
    toString <- as.character(i)
  }
}

.write_metadata(meta_list,sample_name)
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

.write_schema(region, folder_name)
{

}
