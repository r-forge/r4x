.onAttach <- function(libname, pkgname){
  options( R4X.distill = TRUE )
	options( R4X.drop = TRUE )
}

getOptionAsFunction <- function(...){
  match.fun(getOption(...))
}

