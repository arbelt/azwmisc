.pkgEnv <- new.env()


.onLoad <- function(libname, pkgname){
  .pkgEnv$ggplot2_extended <- exists("plot.caption", ggplot2::theme_minimal())
  invisible()
}
