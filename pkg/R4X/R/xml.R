xml <- function(x,..., env = sys.frame(sys.parent()) ){
  UseMethod("xml")
}

xml.default <- function(x,..., env = sys.frame(sys.parent()), distill=getOption("R4X.distill") ){
  xmlNode(x,..., env=env, distill = distill)
}

xml.connection <- function(x, ..., env = sys.frame(sys.parent()), distill = FALSE ){
  xml.default( readLines(x) , ..., distill = distill) 
}

xml.matrix <- function(x,..., distill = TRUE){
  
  rn <- rownames(x)
  cn <- colnames(x)
  env <- environment( ) 
  
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
  
}
