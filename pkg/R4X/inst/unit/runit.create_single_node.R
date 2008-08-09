
test.createNode <- function( ){
  x <- xmlNode("test")
  checkTrue( class(x) == "XMLNode", msg = "checking the creation of a simple node" )
  checkEquals( xmlName(x), "test", msg= "checking the value of a simple node" ) 
  checkEquals( xmlValue(x), NULL, msg = "empty node" )
}

test.createNode.xmlNotation <- function(){
  x <- xmlNode("<test />")
  checkTrue( class(x) == "XMLNode", msg = "simple node - using xml format" )
  checkEquals( xmlName(x), "test", msg= "checking the value of a simple node (2)" ) 
  checkEquals( xmlValue(x), NULL, msg = "empty node (2)" )
}

test.createNode.brew <- function(){
  y <- "test"
  x <- xmlNode("<<%=y%> />")
  checkTrue( class(x) == "XMLNode", msg = "simple node - using xml format with brew" )
  checkEquals( xmlName(x), "test", msg= "checking the value of a simple node (3)" ) 
  checkEquals( xmlValue(x), NULL, msg = "empty node (3)" )
}

test.createNode.xmlNotation.skiphash <- function(){
  x <- xmlNode("#<test />")
  checkTrue( class(x) == "XMLNode", msg = "simple node - using xml format" )
  checkEquals( xmlName(x), "test", msg= "checking the value of a simple node (4)" ) 
  checkEquals( xmlValue(x), NULL, msg = "empty node (4)" )
}

test.wrongnodes <- function(){
  checkException( xmlNode("<foo") , "not closing tag" )
  checkException( xmlNode("<foo>"), "premature end of data" )
  checkException( xmlNode("<foo/><ba"), "extra content" )
}

