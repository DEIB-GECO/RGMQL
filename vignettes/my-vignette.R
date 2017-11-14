## ---- initialization, eval = TRUE----------------------------------------
library('RGMQL')

## ---- init, eval = TRUE--------------------------------------------------
init_gmql()

## ---- read GMQL dataset, eval = TRUE-------------------------------------
gmql_dataset_path <- system.file("example", "EXON", package = "RGMQL")
data_out = read_dataset(gmql_dataset_path)

## ---- read GRangesList, eval = TRUE--------------------------------------
library("GenomicRanges")
gr1 <- GRanges(seqnames = "chr2",
ranges = IRanges(103, 106), strand = "+",score = 5L, GC = 0.45)

gr2 <- GRanges(seqnames = c("chr1", "chr1"),
ranges = IRanges(c(107, 113), width = 3),strand = c("+", "-"),
score = 3:4, GC = c(0.3, 0.5))

grl <- GRangesList("txA" = gr1, "txB" = gr2)
data_out <- read(grl)

