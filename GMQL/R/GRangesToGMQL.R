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

  if(class(...)!="GRangesList")
    dots <- list(...)
    #names(dots) <- as.character(c(1:length(dots)))
  else
    dots <- ...

  if(file_ouput_gtf)
    sapply(dots,function(x){
      #export.gff2(x,)
    },files_sub_dir)

}
