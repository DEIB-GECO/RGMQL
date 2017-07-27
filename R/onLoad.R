.onLoad <- function(libname, pkgname) {
  .rscalaPackage(pkgname, heap.maximum = "4g",serialize.output=TRUE)
  ## Assign 'WrappeR' to package environment before it is sealed, but don't fulfill promise until needed.
  assign("WrappeR",s$.it.polimi.genomics.r.Wrapper,envir=parent.env(environment()))

}

.onUnload <- function(libpath) {
  .rscalaPackageUnload()
}

