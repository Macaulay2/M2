-- -*- coding: utf-8 -*-
newPackage("XML",
    	Version => "1.0", 
    	Date => "July 13, 2009",
    	Authors => {{Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"}},
    	Headline => "an XML parser",
    	DebuggingMode => true)
export {"xmlParse", "XMLnode"}
needsPackage "Parsing"
idP = concatenate % *letterParser
stringP = concatenate % ...
xmlParse = idP : charAnalyzer
