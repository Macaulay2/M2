--		Copyright 1997 by Daniel R. Grayson

errorDepth = 0

documentationMemo = memoize documentation

maximumCodeWidth = 60

getNameFromNumber = new MutableHashTable
otherNodes = new MutableHashTable
getNumberFromName = new MutableHashTable
sectionNumberTable = new MutableHashTable
-----------------------------------------------------------------------------
-- we have to keep track of the part and chapter numbers, and *not* reset the
-- chapter number to zero when starting a new part, so:
--     3,8        part 3 (next chapter is chapter 8)
--     3,8,8      part 3, chapter 8
--     3,8,8,5    part 3, chapter 8, section 5
--     3,8,8,5,2  part 3, chapter 8, section 5, subsection 2

sectionNumber = {0,0}
descend := () -> sectionNumber = (
     if #sectionNumber === 2
     then ( sectionNumber#0, sectionNumber#1, sectionNumber#1 )
     else append(sectionNumber,0)
     )
ascend := () -> (
     if # sectionNumber === 1 then error "oops: ascending too high, producing empty section number";
     if # sectionNumber === 0 then error "oops: empty section number";
     sectionNumber = drop(sectionNumber,-1)
     )
next := sectionNumber -> (
     if #sectionNumber === 0 then sectionNumber
     else if #sectionNumber === 2 then ( sectionNumber#0 + 1, sectionNumber#1 )
     else if #sectionNumber === 3 
     then ( sectionNumber#0, sectionNumber#1 + 1, sectionNumber#2 + 1 )
     else append( drop(sectionNumber, -1), sectionNumber#-1 + 1 )
     )
fmt := sectionNumber -> (
     demark(".",
	  prepend(
	       ROMAN sectionNumber#0,
	       apply(drop(sectionNumber,2),toString)
	       )
	  )
     )
-----------------------------------------------------------------------------
String + ZZ := (s,i) -> s

needs "roman.m2"

record = (
     counter := 0;
     node -> (
	  counter = counter + 1;
	  getNumberFromName#node = counter;
	  getNameFromNumber#counter = node;
	  sectionNumber = next sectionNumber;
	  n := sectionNumberTable#counter = fmt sectionNumber;
	  stderr << "node : " << node << " [" << n << "]" << endl;
	  )
     )

reach1 = method(Dispatch => Thing) 
reach2 = method(Dispatch => Thing)
reach3 = method(Dispatch => Thing)

reach1 Thing := identity
reach1 Sequence := reach1 BasicList := x -> scan(x,reach1)
-- reach1 NOCONTENTS := x -> scan(x,reach3)
reach1 UL := x -> scan(x,reach2)
reach1 TO := reach1 TOH := (x) -> (
     node := formatDocumentTag x#0;
     if not getNumberFromName#?node and not otherNodes#?node 
     then otherNodes#node = documentationMemo x#0;
     )
reach2 Thing := reach1
goOver := node -> (
     record node;
     descend();
     reach1 documentationMemo node;
     ascend();
     )
reach2 TO := reach2 TOH := (x) -> (
     node := formatDocumentTag x#0;
     if not getNumberFromName#?node
     then (
	  if otherNodes#?node then remove(otherNodes,node);
     	  goOver node;
	  ))
reach3 Thing := reach1
reach3 UL := x -> scan(x,reach1)
--------------- body of book
reach1 documentationMemo "Macaulay2"
--------------- appendix
-- sectionNumber = {"A"}
-- document { "Appendix",
--      "We present various footnotes in this appendix.",
--      }
-- reach2 TO "Appendix";
--------------- cover everything else
oldreach2 = reach2
reach2 = reach1
more := true
while more do (
     more = false;
     scan(keys otherNodes, node -> (
	       doc = otherNodes#node;
	       if doc =!= true then ( 
		    reach1 doc;
		    more = true;
		    otherNodes#node = true;
		    )
	       )
	  )
     )
reach2 = oldreach2
--------------- fill in Appendix
-- docDatabase = openDatabase "../cache/Macaulay2-doc"
document {
     Key => "Miscellaneous documentation",
     "We present various additional documentation in this chapter.",
     UL apply(sort unique join(
	       formatDocumentTag \ value \ keys docDatabase,
	       keys otherNodes
	       ),
	  node -> if not getNumberFromName#?node then TO node )
     }
document {
     Key => "Appendix",
     "This appendix contains additional information about the following topics.",
     UL {
	  TO "Miscellaneous documentation"
	  }
     }
reach2 TO "Appendix"
 ------------- index
 -- sectionNumber = {"B"}
 -- document { "Symbol Index",
 -- --     TEX ///\begintwocolumn
 -- --///,
 --      apply(
 -- 	  sort join(keys docDatabase, {"Appendix"}), 
 -- 	  node -> ( TO node, PARA{})),
 -- --     TEX ///
 -- --\endtwocolumn
 -- --///,
 --      }
 -- reach2 TO "Symbol Index"
---------------
close docDatabase
---------------
UnknownReference := "???"

crossReference := (key,text,optional) -> (
     sectionNumber := (
	  if getNumberFromName#?key
	  then sectionNumberTable#(getNumberFromName#key)
	  else (
	       -- error("warning: documentation for key '", key, "' not found");
	       -- stderr << "warning: documentation for key '" << key << "' not found" << endl;
	       UnknownReference
	       )
	  );
     if sectionNumber === UnknownReference
     then if optional 
     then (                                  "{\\bf ", tex text,  "}" )
     else (                                  "{\\bf ", tex text,  "} [", sectionNumber, "]" )
     else ( "\\hyperlink{", sectionNumber, "}{{\\bf ", tex text, "}} [", sectionNumber, "]" )
     )

booktex = method(Dispatch => Thing)
booktex TO := booktex TOH := x -> crossReference(formatDocumentTag x#0, formatDocumentTag x#0,false) 

menuLevel := 2

booktex UL := x -> concatenate(
     ///
\begingroup
\parskip=0pt
///,
     apply(x, x -> if x =!= null then (
	       (menuLevel = menuLevel + 1;),
	       ///%
\par
///,
	       apply(menuLevel-1, i -> ///\indent///),
	       ///\hangindent///, toString menuLevel, ///\parindent
///,
	       -- ///\textindent{$\bullet$}///,
	       booktex if instance(x,TO) then SPAN{ x, headline x#0 } else x,
	       (menuLevel = menuLevel - 1;),
	       ///%
\par
///	       
	       )
	  ),
     ///%
\endgroup
///
     )

booktex HREF := s -> concatenate( "\\href{", tex s#0, "}{", booktex s#-1, "}" )
booktex TEX := identity
///

booktex HR := (x) -> ///\par
\hbox to\hsize{\leaders\hrule\hfill}
///

booktex PARA := (x) -> concatenate(newline,newline)
booktex BR := (x) -> ///\hfil\break
///
booktex IMG := x -> ""
booktex Nothing := x -> ""
booktex Boolean := booktex Symbol := toString
booktex BasicList := booktex Sequence := x -> concatenate apply(x,booktex)
booktex String := tex
booktex ITALIC := x -> concatenate("{\\sl ",booktex toList x,"}")
shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

