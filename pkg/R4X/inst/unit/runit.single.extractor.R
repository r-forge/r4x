
test.single.extractor <- function( ){

  x <- xml( 
'<a>something
  <b>bla</b>
  <c><d>test</d></c>
</a>' )
  checkEquals( x["c/d/#"] , "test" , checkNames = FALSE, msg = "" )
  checkEquals( x["#"] , "something"  , checkNames = FALSE, msg = "" )
  checkEquals( x["b/#"] , "bla"  , checkNames = FALSE, msg = "" )

  sales <- xml( 
'<sales vendor="John">
    <item type="peas" price="4" quantity="6"/>
    <item type="carrot" price="3" quantity="10"/>
    <item type="chips" price="5" quantity="3"/>
</sales>' )  

  checkEquals( sales[ "item/@price" ] , c("4", "3", "5") , checkNames = FALSE, msg = "" )
  checkEquals( sales[ "item/@quantity" ] , c("6", "10", "3") , checkNames = FALSE, msg = "" )
  checkEquals( sales[ "item/@type" ] , c("peas","carrot","chips") , checkNames = FALSE, msg = "" )
  checkEquals( sales[ "@vendor" ], "John"  , checkNames = FALSE, msg = "" )
  
  x <- '##((xml
<root>
  <child id="1">
    <subchild id = "sub1" >foo</subchild>
    <subchild id = "sub2" >bar</subchild>
  </child>
  <child id="2">
    <subchild id="a">blah</subchild>
    <subchild id="b">bob</subchild>
    <something id="c" />
  </child>
  <fruits>
     <fruit>banana</fruit>
     <fruit>mango</fruit>
  </fruits>
</root>    
  '##xml))
  x <- xml( x )
  
  checkEquals( x[ "child/subchild/@id" ] , c( "sub1","sub2","a","b"), checkNames = FALSE, msg = "" )
  checkEquals( x[ "child//@id" ] , c("sub1","sub2","a","b","c") , checkNames = FALSE, msg = "" )
  checkEquals( x[ "fruits/fruit/#" ] , c( "banana","mango") , checkNames = FALSE, msg = "" )
  checkEquals( x[ "child/subchild[1]/@id" ] , c("sub1", "a")  , checkNames = FALSE, msg = "" )
  
  out <- x[ "child/subchild/" ]
  checkTrue( is.list( out) , msg = "" )
  checkEquals( length(out) , 4 , checkNames = FALSE, msg = "" )
  
  checkEquals( x[ "child/3/@id" ] , "c" , checkNames = FALSE, msg = "" )
  checkEquals( x[ "child/1:2/@id" ] , 
    c("sub1","sub2","a","b")   , checkNames = FALSE, msg = "" )
  
  checkEquals( x[ "child/~^sub/@id" ] ,  
    c("sub1","sub2","a","b"), checkNames = FALSE, msg = "" )
  checkEquals( x[ "child/~^s.*i/@id" ] , 
    c( "sub1","sub2","a","b","c"), checkNames = FALSE, msg = "" )
  
}

