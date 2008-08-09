`[[.XMLNode` <- function(x, i ){
  if( is.character(i) ){
    i <- sub("^/+", "", i )
    if( length(grep("/", i)) ){
      i <- strsplit( i, "/" )[[1]]
      i <- i[i!=""]
      thisOne <- i[1]
      others <- paste(i[-1], collapse="/" )
      x[[thisOne]][[others]]
    } else {
      if( length(grep("^@", i) ) ){
        i <- sub("^@", "", i )
        attrs <- xmlAttrs( x )
        if( length(grep("^~", i))){
          i <- sub("^~", "", i )
          i <- grep( i, names(attrs) )
        }
        attrs[i]
      } else if( length( grep("^~", i) )) {
        i <- sub("^~", "", i )
        i <- grep( i, names( x$children ) )[1]
        x$children[[i]]
      } else if(i=="#" ) {
        xmlValue(x)
      } else {
        x$children[[i]]
      }
    }
  } else x$children[[i]]
}

`[[<-.XMLNode` <- function (x, i, value) {
  if( !length(grep("@", i)) ) {
    value <- .convertToNode( value, env = parent.frame(1) )
  } 
  
  if( is.numeric(i) ){
    x$children[[i]] <- value
    names(x$children)[i] = if (inherits(value, "XMLNode")) 
        xmlName(value)
    else sapply(value, xmlName)
  } else if( is.character( i) ){
    i <- sub("^/+", "", i )
    if( length( grep("/", i))){
      parts <- strsplit( i , "/+" )[[1]]
      parts <- parts[parts!= ""]
      first <- parts[1]
      rest  <- paste( parts[-1], collapse = "/" )
      if( !first %in% names( x$children) ){
        x[[ first ]] <- xmlNode( first )
      } 
      y <- x[[ first ]]
      y[[ rest ]] <- value
      x[[ first ]] <- y
      
    } else {
      if( length(grep("^@", i)) ){
        i <- sub("^@", "", i )
        attrs <- xmlAttrs( x )
        if( length(grep("^~", i)) ){
          i <- sub("^~", "", i )
          i <- grep( i, names(attrs), value = TRUE)[1]
        } 
        names(value) <- i
        xmlAttrs( x ) <- value
        # TODO:  this does not seem to work
        #        check the "xmlAttrs<-" method for XMLNode
      } else {
        if( "XMLNode" %in% class(value) ){
          if(i == xmlName( value) ) {
            x$children[[i]] <- value
          } else {
            x$children[[i]] <- xmlNode( i, value )
          }  
        } else x$children[[i]] <- value
        
      }
    }
  }
  x
}

