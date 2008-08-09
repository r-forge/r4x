`addChildren.XMLNode` <- function (node, ..., kids = list(...)) {
    kids = lapply(kids, function(i) {
        if (!is(i, "XMLNode")) 
            xmlTextNode(as.character(i))
        else i
    })
    node$children = c(node$children, kids)
    node$children = XML:::addNames(node$children)
    class(node$children) <- c("XMLList", "list" )
    node
}


`+.XMLNode` <- function( e1, e2 ){
  addChildren( e1, .convertToNode( e2, env = parent.frame(1) ) )
}

`+.XMLList` <- function(e1 , e2 ){
  
  e2 <- list( .convertToNode( e2, env = parent.frame(1) ) )
  names(e2) <- xmlName(e2[[1]])
  
  structure( c( e1, e2 ), class = c("XMLList", "list") )
}

