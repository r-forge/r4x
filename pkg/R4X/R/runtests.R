# TODO: Add comment
# 
# Author: Arcanix
###############################################################################


runR4XTests <- function(testpath, printProtocol = FALSE, protocolFile = "R4Xtestreport.html")
{
	stopifnot(require(RUnit))
	if(missing(testpath))
		testpath <- system.file(package = "R4X", "unit")
	R4XSuite <- defineTestSuite(dirs = testpath, "R4X unit test suite")
	results <- runTestSuite(R4XSuite)
	if(printProtocol)
	{
		printHTMLProtocol(results, file = protocolFile)
	}
	
	results
}
