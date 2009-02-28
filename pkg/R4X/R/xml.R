# {{{ xml S3 generic
xml <- function(x,..., env = sys.frame(sys.parent()) ){
  UseMethod("xml")
}
# }}}

# {{{ xml.default
#' Creates an xml tree from a character vector
#' 
#' @param x the character vector 
#' @param dots extra parameters for xmlNode
#' @param env the environment to use for brewing and distilling
#' @param distill logical, do we do any distilling
xml.default <- function(x,..., env = sys.frame(sys.parent()), distill=getOption("R4X.distill") ){
  
  # {{{ is.file
  # utility function that checks if x is a file
  # '@param x text to check
  # '@return TRUE if x is a file
  is.file <- function( x ){
	if( length(x) != 1 ) return(FALSE)
	if( x %~% "^<" ) return(FALSE)
	
	if( x %~% "^(ftp|http|file)" ) {
	  return( TRUE )
	} else{ 
	  out <- try( file.exists(x), silent = TRUE )
	  return( !(out %of% "try-error") && out )	  
	}
  }
  # }}}
  
  # {{{ check if x is a file and use the appropriate method
  if( is.file( x ) ){
	conn <- file( x, open = "rt" ); on.exit( close( conn ) )
	xml.connection(conn,..., env=env, distill = distill)
  } else{
	xmlNode( x, ..., env=env, distill = distill )
  }
  # }}}
}
# }}}

# {{{ xml.connection
#' Creates an xml tree by reading lines from a connection
#' 
#' @param x connection to read from
#' @param dots passed to xml.default
#' @param env the environment in which the brewing and distilling takes place
#' @param distill logical, do we do any brewing
xml.connection <- function(x, ..., env = sys.frame(sys.parent()), distill = FALSE ){
  xml.default( readLines(x) , ..., distill = distill) 
}
# }}}

# {{{ xml.matrix
xml.matrix <- function(x,..., distill = TRUE){
  
	# {{{ grab dimnames information
  	rn <- rownames(x)
  	cn <- colnames(x)
  	env <- environment( ) 
  	# }}}
  
	# {{{ build the xml 
  	xml.default( '##((xml
  	  <matrix>
  	    <ncol>{ncol(x)}</ncol>
  	    <nrow>{nrow(x)}</nrow>
  	    <@i|ncol(x)>
  	      <col  id="{i}" name="{cn[i]}" >
  	        <@j|nrow(x)>
  	          <cell>{x[j,i]}</cell>
  	        </@>
  	      </col>
  	    </@>
  	  </matrix>
  	'##xml))
  	, env = env, distill = TRUE )
	# }}}
}
# }}}

# :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1:

