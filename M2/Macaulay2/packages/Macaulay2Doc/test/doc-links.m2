needsPackage "Text"
-- we are going eventually to fix the documentation maker so it gets headlines from the raw documentation database, not from loading the package
html TO "FirstPackage::FirstPackage"
assert ( match ( 
	  "<a href=\".*FirstPackage/html/index\\.html\" title=\"an example Macaulay2 package\">FirstPackage</a>",
	  html TO "FirstPackage::FirstPackage"))
assert ( match (
	  "<span><a href=\".*/FirstPackage/html/index.html\" title=\"an example Macaulay2 package\">FirstPackage</a> -- an example Macaulay2 package</span>",
	  html TOH "FirstPackage::FirstPackage"))
needsPackage "FirstPackage"
f = FirstPackage#"raw documentation database"
nodes = {"FirstPackage","firstFunction","firstFunction(ZZ)"}
nodes / (n -> f#?n)
assert( oo === {true,true,true} )
keys f
assert ( set keys f === set nodes )
