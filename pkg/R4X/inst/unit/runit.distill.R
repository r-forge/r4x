
test.distill.withinFun <- function( ){
  f <- function( ){
    x <- "foo"
    y <- xml( "<test>{x}</test>" )
    y[[ "#" ]] == "foo"
  }
  checkTrue( f(), msg = "check that distilling works within a function" )
  
}

test.distill.parentenv <- function( ){
  x <- "foo" 
  f <- function( ){
    y <- xml( "<test>{x}</test>" )
    y[[ "#" ]] == "foo"
  }
  checkTrue( f(), msg = "check that brew can use variables in parent environments" )

  f <- function( ){
    y <- xml( "<test/>" ) + "<bar>{x}</bar>"
    y[[ "bar/#" ]] == "foo"
  }
  checkTrue( f(), msg = "check that brew can use variables in parent environments in the +.XMLNode context" )

  f <- function(){
    x <- "foo"
    g <- function(){
      z <- "blah"
      h <- function(){
        y <- xml( "<test>{x}{z}</test>" )
        y[[ "#" ]] == "fooblah"
      }
      h()
    }
    g()
  }
  checkTrue( f(), "check nested environments and distill" )

  f <- function(){
    x <- "foo"
    g <- function(){
      z <- "blah"
      h <- function(){
        y <- xml( "<test></test>" ) + "<bar>{x}{z}</bar>"
        y[[ "bar/#" ]] == "fooblah"
      }
      h()
    }
    g()
  }
  checkTrue( f(), "check nested environments, +.XMLNode and distill" )
  
}

test.distill <- function(){
  y <- "blah"
  x <- xml( '##((xml
    <a>
      <b>
        {paste(y,y,sep="")}
      </b>
    </a>
  '##xml))
  )
  checkEquals( x[["b/#"]], "blahblah", msg = "simple use of distill" )
}

test.distill.and.brew <- function( ){
  z <- "test"
  y <- "blah"
  x <- xml( '##((xml
    <a>
      <b>
        {paste(y,y,sep="")}
      </b>
      <c>
        <%= z %>
      </c>
    </a>
  '##xml))
  )
  checkEquals( x[["b/#"]], "blahblah", msg = "simple use of distill" )
  checkEquals( x[["c/#"]], "test", msg = "distill and brew" )
}


