width <- function( x ){
  kids <- xmlChildren(x)
  if( is.null(kids) ) return(1)
  sum( sapply( kids, width ) )
}

depth <- function(x){
  kids <- xmlChildren(x)
  if( is.null(kids) ) return(1)
  1 + max( sapply( kids, depth ) )
}

