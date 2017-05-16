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
  c = counter()
  if(file_ouput_gtf)
  {
    apply(dots, function(x){
      sample_name = paste0("S_",c())
      export.gff2(x,sample_name)
    })
  }
  else
  {
    apply(dots, function(x){

    })
  }

}

counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    toString <- as.character(i)
  }
}

