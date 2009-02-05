test.xmlfromfile <- function()
{
	targetFile <- system.file(package = "R4X", "unit/book.xml")
	contents <- xmlFromFile(targetFile)
	checkEquals(contents["book/author/#"], c("Capinsky, Marek","Tuckman, Bruce"), 
			checkNames = FALSE)
 	# checkEquals(contents, xml(readLines(targetFile)))
}

#> bar["book/author/#"]
#book              book 
#"Capinsky, Marek"  "Tuckman, Bruce" 