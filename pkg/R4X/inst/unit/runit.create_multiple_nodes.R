test.createNode <- function( ){
  a <- "mango"
  x <- xmlNode('<test><foo><bar fruit="<%=a%>"/><bob/></foo></test>')
  checkTrue( class(x) == "XMLNode", msg = "creation of xml structure within xmlNode" )
  checkEquals( xmlName(x), "test", msg= "checking the value of a simple node" ) 
  checkEquals( xmlValue(x), NULL, msg = "empty node" )

  y <- xmlChildren(x)
  checkEquals( length(y),1,"checking number of childs")
  checkEquals( names(y),"foo", "names of childs" )
  
  z <- xmlChildren(y[[1]])
  checkEquals( length(z),2,"checking number of childs")
  checkEquals( names(z),c("bar","bob"), "names of childs")
  
  at <- xmlAttrs( z$bar )
  checkEquals(names(at),"fruit", "checkattribute")
  checkTrue( at == "mango", "check attribute value" )
}
# TODO: [, [[, [<-, [[<-
# TODO: xPath-like syntax
