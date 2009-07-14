-- -*- coding: utf-8 -*-

   -- use Parsing package to write an XML parser at top level (handle entities)
   -- 	    result:
   -- 	    	 each node is a hash table
   -- 		 some keys are strings, representing attributes
   -- 		 a special non-string key (symbol children) will provide the list of children (hashtables) and content pieces (strings), if there are any
   -- 		 a special non-string key (symbol name) for the name of the node
   -- 	    these print easily, with < & > " &quot; ' &apos;

newPackage("XML",
    	Version => "1.0", 
    	Date => "July 13, 2009",
    	Authors => {{Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"}},
    	Headline => "an XML parser",
    	DebuggingMode => true)
export {"xmlParse", "XMLnode", "name", "children"}
XMLnode = new Type of HashTable
needsPackage "Parsing"
returns = t -> s -> t
second = s -> s#1
idP = concatenate % +letterParser
ampP = returns "&" % constParser "amp"
ltP = returns "<" % constParser "lt"
gtP = returns ">" % constParser "gt"
aposP = returns "'" % constParser "apos"
quotP = returns "\"" % constParser "quot"
entityP = second % andP("&",orP(ampP,ltP,gtP,aposP,quotP),";")
nonquoteP = new Parser from (c -> if c =!= "\"" and c =!= null then new Parser from (b -> if b === null then c))
stringP = concatenate % * orP(entityP, nonquoteP)
quotedstringP = (s -> s#1) % andP("\"",stringP,"\"")
pairP = (s -> s#0 => s#2) % andP(idP,"=",quotedstringP)
pairsP = * (last % andP(+ constParser " ",pairP))
tagstartP = andP("<",idP,pairsP)
xmlParse = tagstartP : charAnalyzer
print xmlParse ///<foo bar="5" foo="asdf &amp; asdf "///
