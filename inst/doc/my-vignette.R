## ---- eval=FALSE---------------------------------------------------------
#  source("https://bioconductor.org/biocLite.R")
#  biocLite("RGMQL")

## ---- initialization, eval=TRUE------------------------------------------
library('RGMQL')

## ---- init, eval=FALSE---------------------------------------------------
#  initGMQL()

## ----read GMQL dataset, eval=FALSE---------------------------------------
#  gmql_dataset_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#  data_out = readDataset("gmql_dataset_path")

## ---- download dataset, eval=FALSE---------------------------------------
#  test_url <- "http://130.186.13.219/gmql-rest"
#  login.GMQL(test_url)
#  downloadDataset(test_url,"dataset_test",path = getwd())

## ----read remote dataset, eval=FALSE-------------------------------------
#  data_out = readDataset("dataset_name_on_repo")

## ---- read GRangesList, eval=FALSE---------------------------------------
#  library("GenomicRanges")
#  gr1 <- GRanges(seqnames = "chr2",
#  ranges = IRanges(103, 106),
#  strand = "+",
#  score = 5L, GC = 0.45)
#  
#  gr2 <- GRanges(seqnames = c("chr1", "chr1"),
#  ranges = IRanges(c(107, 113), width = 3),
#  strand = c("+", "-"),
#  score = 3:4, GC = c(0.3, 0.5))
#  
#  grl <- GRangesList("txA" = gr1, "txB" = gr2)
#  data_out <- read(grl)

## ----query, eval=FALSE---------------------------------------------------
#  initGMQL("gtf")
#  test_path <- system.file("example","DATA_SET_VAR_GTF",package = "RGMQL")
#  input = readDataset(test_path)
#  
#  ## it selects from input data samples of patients younger than 70 years old,
#  ## based on filtering on sample metadata attribute Patient_age
#  #s=select(input,"Patient_age < 70")
#  
#  ## it counts the regions in each sample and stores their number as value of the new metadata
#  ## RegionCount attribute of the sample.
#  e = extend(input_data = s, list(RegionCount = COUNT()))
#  
#  ## materialize the result dataset on disk
#  #m = materialize(e)

## ----execute, eval=FALSE-------------------------------------------------
#  execute()

## ----take, eval=FALSE----------------------------------------------------
#  g <- take(input_data = m, rows = 45)

## ---- eval=TRUE----------------------------------------------------------
library("RGMQL")

test_url = "http://130.186.13.219/gmql-rest"
login.GMQL(test_url)

## ---- eval=TRUE----------------------------------------------------------
test_url = "http://130.186.13.219/gmql-rest"
login.GMQL(test_url)
runQuery(test_url, "query_1", "DATA_SET_VAR = SELECT() HG19_TCGA_dnaseq; 
         MATERIALIZE DATA_SET_VAR INTO RESULT_DS;", output_gtf = FALSE)

