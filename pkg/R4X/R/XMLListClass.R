`[.XMLList` <-
function( x, i ){
   if( !missing( i )){ 
     class( x ) <- "list"
     structure( x[i], class = c("XMLList", "list" ) )
   } else x 
}

