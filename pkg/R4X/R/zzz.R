.onAttach <- function(libname, pkgname){
  options( R4X.distill = TRUE )
}

getOptionAsFunction <- function(...){
  match.fun(getOption(...))
}

