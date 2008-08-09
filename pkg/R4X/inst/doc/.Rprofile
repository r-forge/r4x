source( "~/.Rprofile" )
require( XML )
require( brew )
require( operators )
require( gsubfn )
fresh <- function( ){
  for( i in list.files( "/home/romain/cvs/Rpackages/R4X/R4X/R", pattern = "\\.[rR]$", full = T)){
    source( i )
  }
  .onAttach( )
}
fresh( )

