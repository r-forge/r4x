
### distill from a character vector
distill <- function( txt, brew = FALSE, env = sys.frame(sys.parent()) ){
  
  ### check for right number of templates openers and closers
  openers <- gregexpr( "<@[^>]+>", txt )[[1]]
  closers <- gregexpr( "</@>", txt )[[1]]
  if( length( openers ) != length(closers) ){
    stop( gettext( "unmatching number of template openers and closers" ) )
  }
  
  txt <- gsub( "\\{(.+?)\\}", "<%=\\1%>", txt, perl = TRUE )
  
  if( length(openers ) ){      
    ### change the openers to brew syntax
    txt <- gsub( "<@([^|]+)\\|([^>]+)>", "<% for( \\1 in 1:\\2){%>",  txt, perl = T)
    txt <- gsub( "<@([^~]+)\\~([^>]+)>", "<% for( \\1 in \\2){%>",  txt, perl = T)
    txt <- gsub( "<@([^?]+)\\?([^>]+)>", "<% for( \\1 in seq(along=\\2) ){%>",  txt, perl = T)
    
    ### change the closers to brew syntax
    txt <- gsub( "</@>", "<%}%>", txt )    
  }
  
  if( brew ){
    txt <- capture.output( brew(text=txt, env = env) )
  }
  txt
}

