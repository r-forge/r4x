# TODO: depreciate xmlNode to avoid confusion with the one of the XML package
`xmlNode` <- function (name, ..., attrs = NULL, namespace = "", .children = list(...), env = sys.frame(sys.parent()), distill = getOption("R4X.distill") ) {
	if( length(grep("[<]", name))){
		.convertToNode( name, env = env , fail = TRUE, distill = distill )
	} else{
		kids <- lapply(.children, asXMLNode)
		kids = XML:::addNames(kids)
		class(kids) <- c("XMLList", "list")
		node <- list(name = name, attributes = attrs, children = kids, 
				namespace = namespace)
		class(node) <- c("XMLNode")
		node
	}
}


.convertToNode <- function( 
		txt,                            #@ some text to try to convert into xml 
		env = sys.frame(sys.parent()),  #@ where to brew
		fail = FALSE,
		distill = getOption("R4X.distill") ){
	if( !is.character(txt) && class( txt) == "XMLNode") return(txt)
	if( is.character(txt) ){
		
		if(length(txt) > 1) {
			txt <- paste( txt, collapse = "\n" )
		}
		# this removes the first line if it starts with a hash sign, 
		# it is only there to help text editors to recognize when to switch from 
		# R source to xml source, I'm currently using these in jedit: 
		# '##((xml            to start writing xml code
		# '##xml))            to end the xml
		# I'm hoping that in the future we will change the R parser so that it 
		# recognizes xml structures without having to embed them in a string, 
		# like E4X is doing in javascript
		txt <- txt %-~% "^[^<]*"
		if(distill){
			txt <- distill( txt )
		}
		
		# brew the distill'ed text and store the result in the variable `brewed`
		con <- textConnection( "brewed", "w" )
		on.exit( close(con) ) 
		brew.try <- try( brew(text=txt, env = env, output = con), silent = TRUE )
		if( class( brew.try) == "try-error" ){
			stop( brew.try )
		}
		txt <- brewed
		
		.stop <- FALSE
		.msg <- ""
		catchFun <- function(w){
			if(fail){
				.msg <<- w
				.stop <<- TRUE
			}
			txt
		}
		### converts warnings into erros and reports the error if fail is TRUE
		
		node <- tryCatch(          
				xmlTreeParse( txt, asText = TRUE )$doc$children[[1]], 
				error = catchFun, warning = catchFun )
		# Change by FG, Nov 9 2008:
		# changed the following line. It previously read
		# paste("problem in parsing node:", as.character(w)), but w did not exist in this context
		# hence an error would occur.
		if(.stop) stop(paste("problem in parsing node:", paste(as.character(txt), collapse = "\n")))  
		if( class(node) %of%  "XMLNode" ){
			node
		} else asXMLNode( txt )
	}
	
}
