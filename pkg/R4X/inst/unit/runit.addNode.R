test.addNode <- function( ){
  x <- xmlNode( "test" ) + '<foo/>'
  checkEquals( xmlName(xmlChildren(x)[[1]]), "foo", 
    msg = "checking adding a node" )
    
  x <- xmlNode( "test" ) + '<foo><bar blah="aa" /></foo>'
  checkEquals( xmlName( xmlChildren( xmlChildren(x)[[1]] )[[1]] ), "bar", 
    msg = "checking adding a nested node" )
  checkTrue( xmlAttrs( xmlChildren( xmlChildren(x)[[1]] )[[1]] ) == "aa",  
    msg = "checking adding a nested node (attrbiute)" )
    
}

test.addNode.brew <- function( ){
  y <- "foo"
  x <- xmlNode( "test" ) + '<<%=y%>/>'
  checkEquals( xmlName(xmlChildren(x)[[1]]), "foo", 
    msg = "checking adding a node with brew" )
}

test.addNode.operator <- function( ){
  x <- xmlNode( "test" )
  x %+=% '<foo/>'
  checkEquals( xmlName(xmlChildren(x)[[1]]), "foo", 
    msg = "checking adding a node" )
  
  x <- xmlNode( "test" )
  x %+=% '<foo><bar blah="aa" /></foo>'
  checkEquals( xmlName( xmlChildren( xmlChildren(x)[[1]] )[[1]] ), "bar", 
    msg = "checking adding a nested node" )
  checkTrue( xmlAttrs( xmlChildren( xmlChildren(x)[[1]] )[[1]] ) == "aa",  
    msg = "checking adding a nested node (attrbiute)" )
    
}

test.addNode.operator.brew <- function( ){
  y <- "foo"
  x <- xmlNode( "test" )
  x %+=% '<<%=y%>/>'
  checkEquals( xmlName(xmlChildren(x)[[1]]), "foo", 
    msg = "checking adding a node with operator + brew" )

  x <- xmlNode( "test" )
  x %+=% '<<%=y%>><bar blah="aa" /></<%=y%>>'
  checkEquals( xmlName( xmlChildren( xmlChildren(x)[[1]] )[[1]] ), "bar", 
    msg = "checking adding a nested node" )
  checkTrue( xmlAttrs( xmlChildren( xmlChildren(x)[[1]] )[[1]] ) == "aa",  
    msg = "checking adding a nested node (attrbiute)" )
    
}



