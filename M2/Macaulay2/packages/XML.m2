-- -*- coding: utf-8 -*-

newPackage("XML",
    	Version => "1.0", 
    	Date => "July 13, 2009",
    	Authors => {{Name => "Dan Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/"}},
    	Headline => "an XML parser",
    	DebuggingMode => false)
export {"XMLnode", "tag", "children","parse", "toXMLnode", "Trim","toLIBXMLnode"}
scan(pairs Core#"private dictionary", (k,v) -> if match("^xml[A-Z]",k) then (
	  XML#"private dictionary"#k = v;
	  export {v}))

XMLnode = new Type of HashTable
net XMLnode := n -> stack(
     n.tag | concatenate apply(pairs n, (k,v) -> if instance(k,String) then (" ",k,"=",format v) else ""),
     if n.?children then "  " | stack apply(n.children,c -> if instance(c,String) then format c else net c))

nonnull = x -> select(x, i -> i =!= null)
trimwhite = s -> (
     s = replace("^[[:space:]]+","",s);
     s = replace("[[:space:]]+$","",s);
     if s =!= "" then s)
trimopt := identity
settrim = opts -> trimopt = if opts.Trim then trimwhite else identity
toXMLnode0 = node -> (
     --  result:
     --    each node is a hash table of type XMLnode
     --    some keys are strings, representing attributes
     --    a special non-string key (symbol children) will provide the list of children (hashtables) and content pieces (strings), if there are any
     --    a special non-string key (symbol name) for the name of the node
     if xmlIsElement node then (
	  attr := xmlAttributes node;
	  child := xmlGetChildren node;
	  new XMLnode from splice nonnull {
	       symbol tag => xmlGetName node,
	       if attr =!= null then toSequence (
		    while attr =!= null list first(
		    	 xmlGetName attr => ( c := xmlGetChildren attr; concatenate while null =!= c list first( xmlGetContent c, c = xmlGetNext c)),
		    	 attr = xmlGetNext attr)),
	       if child =!= null then symbol children => nonnull while child =!= null list first(toXMLnode child, child = xmlGetNext child)
	       }
	  )
     else if xmlIsText node then trimopt xmlGetContent node)
toXMLnode = method(Options => { Trim => true })
toXMLnode LibxmlNode := opts -> node -> (
     settrim opts;
     toXMLnode0 node)

toLIBXMLnode = method()
populate = (d,x) -> (
     scanPairs(x, (k,v) -> if instance(k,String) then xmlNewProp(d,k,v));
     if x.?children then scan(x.children, child -> (
	       if instance(child,String) then xmlNewText(d,child)
	       else if instance(child,XMLnode) then populate(xmlNewChild(d,child.tag),child)
	       else error "unrecognized child type"));
     d)
toLIBXMLnode XMLnode := x -> populate(xmlNewDoc x.tag,x)
parse = method(Options => { Trim => true })
parse String := opts -> s -> (
     settrim opts;
     toXMLnode xmlParse s)


end

{*
this parser is a failure because Parsing doesn't provide lookahead at all!

needsPackage "Parsing"
returns = t -> s -> t
second = s -> s#1
idP = concatenate % +letterParser
ampP = returns "&" % constParser "amp"
ltP = returns "<" % constParser "lt"
gtP = returns ">" % constParser "gt"
aposP = returns "'" % constParser "apos"
whitespace = * orP(" ","\t","\n","\r")
quotP = returns "\"" % constParser "quot"
entityP = second % andP("&",orP(ampP,ltP,gtP,aposP,quotP),";")
nonquoteP = new Parser from (c -> if c =!= "\"" and c =!= null then new Parser from (b -> if b === null then c))
stringP = concatenate % * orP(entityP, nonquoteP)
quotedstringP = (s -> s#1) % andP("\"",stringP,"\"")
pairP = (s -> s#0 => s#2) % andP(idP,"=",quotedstringP)
pairsP = * (first % andP(pairP,whitespace))
tagstartP = (s -> prepend(s#1,s#3)) % andP("<",(s -> symbol tag => s) % idP,whitespace,pairsP)
tagP = (s -> s) % andP(tagstartP, orP("/>",* futureParser tagP, "</", idP, ">"))
xmlParse = tagP : charAnalyzer
print xmlParse ///<foo bar="5" foo="asdf &amp; asdf"></foo>///
*}

restart
debug Core
p = xmlParse ///<foo> aabc <bar id="foo" name="too"> asdf </bar> <coo/> </foo>///
examine p
x = toXMLnode p
q = toLIBXMLnode x
examine q
