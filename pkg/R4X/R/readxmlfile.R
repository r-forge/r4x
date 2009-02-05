

#' A simple wrapper for reading the contents of an XML file
#' @param fileName 
#' @returnType 
#' @return 
#' @author fgochez
#' @export
xmlFromFile <- function
(
	fileName, ...
)
{
	if(!file.exists(fileName))
		stop(paste("File", fileName, "does not exist"))
	fileConn <- file(fileName, open = "rt")
	result <- xml(fileConn, ...)
	close(fileConn)
	result
}
