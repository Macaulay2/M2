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
ampP = (s -> "&") % constParser "amp"
ltP = (s -> "<") % constParser "lt"
gtP = (s -> ">") % constParser "gt"
aposP = (s -> "'") % constParser "apos"
quotP = (s -> "\"") % constParser "quot"
entityP = (s -> s#1) % andP("&",orP(ampP,ltP,gtP,aposP,quotP),";")
nonquoteP = new Parser from (c -> if c =!= "\"" then new Parser from (b -> if b === null then c))
stringP = concatenate % * orP(entityP, nonquoteP)
xmlParse = idP : charAnalyzer
