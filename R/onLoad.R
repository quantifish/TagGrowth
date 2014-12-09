#' On load hook
#'
#' This is a load hook that is called by R when the package is loaded. This
#' should not be exported
#' 
.onLoad <- function(libname, pkgname)
{
  cat("\n")
  cat("==============================================\n")
  cat("Mixed-effects tag-recapture-age library\n")
  cat(TagGrowth.version())
  cat("For help contact darcy@quantifish.co.nz\n")
  cat("==============================================\n")
  cat("\n")
}


