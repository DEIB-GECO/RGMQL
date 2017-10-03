## ---- eval=FALSE---------------------------------------------------------
#  source("https://bioconductor.org/biocLite.R")
#  biocLite("GMQL")

## ---- initialization, eval=FALSE-----------------------------------------
#  initGMQL()

## ----read GMQL dataset, eval=FALSE---------------------------------------
#  gmql_dataset_path <- system.file("example","DATA_SET_VAR_GTF",package = "GMQL")
#  data_out = readDataset("gmql_dataset_path")

## ---- download dataset, eval=FALSE---------------------------------------
#  test_url <- "http://130.186.13.219/gmql-rest"
#  login.GMQL(test_url)
#  downloadDataset(test_url,"dataset_test",path = getwd())

## ---- read GRangesList, eval=FALSE---------------------------------------
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

## ---- eval=TRUE----------------------------------------------------------
library("GMQL")

test_url = "http://130.186.13.219/gmql-rest"
login.GMQL(test_url)

