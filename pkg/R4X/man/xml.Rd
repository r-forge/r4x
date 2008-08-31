\name{xml}   
\alias{xml} 
\alias{[.XMLNode}
\alias{[[.XMLNode}
\alias{[<-.XMLNode}
\alias{[[<-.XMLNode}
\title{ Generic XML creation  }
\description{
  \code{xml} is a S3-generic function that builds xml objects
}
\usage{
xml(x, ..., env = sys.frame(sys.parent()))
\method{[}{XMLNode}(x, i  , verbose = TRUE, drop = getOption("R4X.drop") )
\method{[[}{XMLNode}(x, i)
\method{[}{XMLNode}(x, i) <- value
\method{[[}{XMLNode}(x, i) <- value
}
\arguments{
  \item{x}{ Object of which to build an xml representation }
  \item{\dots}{ Additional arguments potentially used in methods }
  \item{env}{ Environment in which to work. It is set by default to the 
    environment from which the function is called. }
  \item{i}{index of extraction}             
  \item{value}{replacement}  
  \item{verbose}{Print additional information}
	\item{drop}{Used with the single extractor. When TRUE and the result of the extract is a single node, 
	it is returned as an \code{XMLNode} object, otherwise as a list of one \code{XMLNode}. 
	The \code{drop} argument is set by the option \code{R4X.drop} which is set to TRUE when the 
	R4X package is attached. }
}
\details{
  Distilling as defined in \code{\link{distill}} can be used to build 
  the xml object. 
  
  The extractors for XMLNode objects as defined here use a pseudo-xpath syntax
  to define which part of the XML structure is extracted. For full xpath capabilities, 
  see the \code{\link[XML]{xpathApply}} function. 
  
  The string inside the square bracket is divided into several tokens separated by a
  forward slash. Each token allows to go down on the XML structure from the top level.
  Unlike xpath, there is no need for the initial slash is meaningless and will be 
  ignored.
              
  Tokens have the following meaning: 
\tabular{cc}{
  Description \tab use \cr
  Standard character string \tab name of the child XML tag \cr
  Character string starting with @ \tab attribute \cr
  Character string starting with \~ \tab Regular expression to identify the tags \cr
  Character string containing square bracket \tab Extract given tags and subset them \cr
  Hash symbol \tab Content of the node \cr 
}
  
  
}
\value{
  An object of class \code{XMLNode} from the XML package is returned. 
}
\author{ Romain Francois <rfrancois@mango-solutions.com> }
\seealso{ \link{distill} }
\examples{         
sales <- xml( 
  '<sales vendor="John">
      <item type="peas" price="4" quantity="6"/>
      <item type="carrot" price="3" quantity="10"/>
      <item type="chips" price="5" quantity="3"/>
  </sales>' )  
      
### example of distilling, see ?distill for details 
y <- "peas" 
sales <- xml( 
  '<sales vendor="John">
  <item type="{y}"/>
  </sales>' )  
sales

y <- c( "peas", "carrot", "chips" ) 
sales <- xml( 
'<sales vendor="John">
  <@j|length(y)>
    <item type="{y[j]}"/>
  </@>  
</sales>' )  
sales

y <- c( "peas", "carrot", "chips" ) 
sales <- xml( 
'<sales vendor="John">
  <@item~y>
    <item type="{item}"/>
  </@>  
</sales>' )  
sales

y <- c( "peas", "carrot", "chips" ) 
sales <- xml( 
'<sales vendor="John">
  <@index?y>
    <item type="{y[index]}"/>
  </@>  
</sales>' )  
sales

### examples of extraction
x <- xml( '<root>
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
</root>' )
x[ 'fruits/fruit' ]            # list of two XMLNode
x[ 'fruits/fruit/#' ]          # c("banana", "mango" )
x[ 'fruits' ]                  # one XMLNode, example with drop set to TRUE
x[ 'fruits', drop = TRUE  ]    # same, but explicit
x[ 'fruits', drop = FALSE ]    # same, but the result is returned as a list of one XMLNode
x[ 'child/subchild' ]          # list of 4 XMLNode
x[ 'child/subchild/@id' ]      # vector of id attributes
x[ 'child/~^s' ]               # list of 5 XMLNode
x[ 'child[1]/subchild' ]       # list of 2 XMLNode
x[ 'child[1]/subchild[1]/#' ]  # "foo"
x[ 'child[1]/1/@id' ]          # "sub1"

}
\keyword{ manip }
\keyword{ utilities }


