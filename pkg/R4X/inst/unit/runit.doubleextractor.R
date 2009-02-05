# TODO: Add comment
# 
# Author: Frank
###############################################################################


test.double.extractor <- function()
{
	x <- xmlFromFile(system.file(package = "R4X", "unit/book.xml"))
	checkEquals(x[["book/author/#"]], "Capinsky, Marek")
	checkEquals(x[["book/price/#"]], "30.00")
}
