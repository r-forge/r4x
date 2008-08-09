

`[.XMLNode` <- function(x, i, verbose = TRUE ){
  out <- if( is.character(i) ){
    if( i %~% "/" ){ # split i and continue the subset
      i <- i %/~% "/"
      thisOne <- i[1]
      others <- paste(i[-1], collapse="/" )
      y <- x[thisOne]
      if( "XMLNode" %in% class(y) ) {
        y[ others ]
      } else { 
        if(length(y)==1){                
          y[[1]][others]
        } else{
          .lap <- lapply( y, function(item) item[others])
          if( "XMLNode" %in% sapply(.lap, class) ){
            .lap
          } else { 
            do.call(c,.lap)
          }
        }
      }                                         
    } else { # no / in i
      out <- if( i=="" ){
        x$children[]
      } else if( i %~% "^~" ) {            # use regular expressions
        .xmlnode_extract_regex(x,i)        
      } else if( i %~% "^@" ){             # deal with attributes
        .xmlnode_extract_attributes(x,i)   
      } else if( i %~% "^[0-9:]+$") {      # extract numeric positions
        .xmlnode_extract_position(x,i)     
      } else if( i %~% "^#") {             # content (as character or numeric)
        .xmlnode_extract_content(x,i)
      } else if( i %~% "[\\+\\-][^\\[\\]]+$") {
        .xmlnode_extract_plus(x,i)
      } else if( i %~% "\\[[^\\]]*\\]" ){  # i contains a square bracket expression
        .xmlnode_extract_square(x,i)
      } else { 
        .xmlnode_extract_default(x,i)
      }        
      if( length(out)==1 ) out[[1]] else out
    }
    
  } else x$children[i]
  out
}

.xmlnode_extract_regex <- function(x,i){   
  i <- sub("^~", "", i )
  i <- grep( i, names( x$children ) )
  x$children[i]
}

.xmlnode_extract_attributes <- function(x,i){
  i <- sub("^@", "", i )
  attrs <- xmlAttrs( x )
  if( length(grep("^~", i))){
    i <- sub("^~", "", i )
    i <- grep( i, names(attrs) )
  }
  attrs[i]
}

.xmlnode_extract_position <- function(x,i){
  i <- try( eval( parse( text = i ) ), silent = TRUE )
  if( class(i) != "try-error") x$children[i]
}

.xmlnode_extract_content <- function(x,i,as.numeric = i %~% "n"){
  text.test <- xmlSApply( x, function(y) "XMLTextNode" %in% class(y) )
  value <- if(any(text.test)) sapply( xmlChildren(x)[text.test], xmlValue ) else ""
  if( as.numeric ) {
    test <- try(as.numeric(value), silent = TRUE)
    if( test %of% "try-error") value else test
  } else value
}

.xmlnode_extract_default <- function(x,i){
  x$children[i == names(x$children) ]
}

.xmlnode_extract_square <- function(x,i){
  before <- i %-~% "\\[.*" %-~% "[[:space:]]+"       # what is before the square brackets
  inside <- i %o~|% "\\[(.*)\\]" # what is inside the square brackets
  if( before == "" ){
    .xmlnode_extract_position(x,inside)
  } else {
    tmp. <- x[before]
    subset. <- try( eval(parse(text=inside)), silent = TRUE )
    if( "XMLNode" %in% class(tmp.) && class(subset.) != "try-error" ) tmp. else tmp.[ subset. ]
  }
}

.xmlnode_extract_plus <- function(x,i){
  
  plus <- i %~% "\\+[^\\[\\]]*$"
  before <- i %-~% "[+\\-][^\\[\\]]*$" %-~% "[[:space:]]+"
  after  <- i %o~|% "[+\\-].*$" %-~% "[[:space:]]+"
  if(before == "" || after == ""){
    stop( gettext("`+` or `-` should not be at the end of the part of the path") )
  }
  
  after <- try( eval(parse(text=after)), silent = TRUE )
  if( after %of% "try-error" ){
    stop(gettext("the part of the path does not convert into numeric data"))
  }
  
  bracket <- before %~% "\\[.*\\]"
  idx <- if( bracket ){
    inside <- i %o~|% "\\[(.*)\\]"
    inside <- try( eval(parse(text=inside)), silent=TRUE)
    if(inside %of% "try-error"){
      stop( "wrong content inside square brackets" )
    }
    before <- before %-~% "\\[.*" %-~% "[[:space:]]+"
    which( names(x) == before )[inside]
  } else which( names(x) == before )
  if( length(idx) == 0 ){
    stop( "problem" )
  }
  
  idx <- idx + after
  
  if(any(idx < 1 | idx > length(x)) ){
    stop( gettext("outside correct index") )
  }
  x[ idx ]
  
}

`[<-.XMLNode` <- `[[<-.XMLNode`


