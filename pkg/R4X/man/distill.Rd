\name{distill}
\alias{distill}
\title{ distill: simple brew wrapper }
\description{
  distill is a simple wrapper for brew. distill provides 
  functionality to generate brew code from simpler 
  markup closer to the E4X language.
}
\usage{
distill( txt, brew = FALSE, env = sys.frame(sys.parent()) )
}
\arguments{
  \item{txt}{ The text to distill }
  \item{brew}{ Is \code{\link[brew]{brew}} called ? }
  \item{env}{Environment where distilling is performed.}
}
\details{
  \code{distill} consist of generating 
  \code{\link[brew]{brew}} ready templates
  from a different markup that is (arguably) simpler to use
  in an XML setting. The following table shows the 
  possible transformations from distill code to brew code.
  
  \tabular{ccc}{
    description \tab distill code \tab brew code \cr
    R code to be \code{cat} \tab \{x\} \tab <\%=x\%> \cr
    Loop generator starter \tab <@i|n> \tab <\%for(i in 1:n)\{ \%> \cr %}
    Loop generator starter \tab <@i~n> \tab <\%for(i in n)\{ \%> \cr   %}
    Loop generator starter \tab <@i?n> \tab <\%for(i in seq(along = n))\{ \%> \cr %}{
    Loop generator closer  \tab </@>   \tab <\%}\%>
  }
  
}
\value{
  a character vector ready to be \code{brew}ed. 
}
\references{ 
  Jeffrey Horner (2007). brew: Templating Framework for Report
  Generation.
}
\author{ Romain Francois <rfrancois@mango-solutions.com> }
\seealso{ \code{\link{xml}} uses distill to modify the content 
of XML nodes before parsing. \code{\link[brew]{brew}}}
\examples{
  y <- "foo"
  txt <- "<test>{x}</test>"
  distill( txt )
  
  cat. <- function(x){
    cat( x, sep = "\n" )
  }
  
  y <- c("foo", "bar")
  txt <- "
    <test>
      <@i~y>
        <foo>{i}</foo>
      </@>
    </test>
    "
  cat.( distill( txt ) )

  txt <- "
    <test>
      <@i|length(y)>
        <foo>{y[i]}</foo>
      </@>
    </test>
    "
  cat.( distill( txt ) )

  txt <- "
    <test>
      <@i?y>
        <foo>{y[i]}</foo>
      </@>
    </test>
    "
  cat.( distill( txt ) )

  
}
\keyword{ file }

