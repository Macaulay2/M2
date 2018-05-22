needsPackage "Text"
-- we are going eventually to fix the documentation maker so it gets headlines from the raw documentation database, not from loading the package
assert ( match ( 
	  "<a href=\"file:///.*FirstPackage/html/index\\.html\" title=\"an example Macaulay2 package\">FirstPackage</a>",
	  html TO "FirstPackage::FirstPackage"))
assert ( match (
	  "<span><a href=\"file:///.*/FirstPackage/html/index.html\" title=\"an example Macaulay2 package\">FirstPackage</a> -- an example Macaulay2 package</span>",
	  html TOH "FirstPackage::FirstPackage"))
needsPackage "FirstPackage"
f = FirstPackage#"raw documentation database"
assert ( keys f === {"FirstPackage","firstFunction","firstFunction(ZZ)"} )
