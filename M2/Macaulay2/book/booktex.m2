--		Copyright 1997 by Daniel R. Grayson

errorDepth 0

documentationMemo = memoize documentation

asciiLineWidth = 60

getNameFromNumber = new MutableHashTable
otherNodes = new MutableHashTable
getNumberFromName = new MutableHashTable
sectionNumberTable = new MutableHashTable
sectionNumber = {0}
descend = () -> sectionNumber = append(sectionNumber,0)
ascend = () -> (
     if # sectionNumber === 1 then error "oops: ascending too high, producing empty section number";
     if # sectionNumber === 0 then error "oops: empty section number";
     sectionNumber = drop(sectionNumber,-1)
     )
String + ZZ := (s,i) -> s
record = (
     counter := 0;
     node -> (
	  counter = counter + 1;
	  getNumberFromName#node = counter;
	  getNameFromNumber#counter = node;
	  if # sectionNumber =!= 0 then (
	       sectionNumber = append( drop(sectionNumber, -1), sectionNumber#-1 + 1 );
	       );
	  n := sectionNumberTable#counter = concatenate between(".",apply(sectionNumber,toString));
	  stderr << "node : " << node << " [" << n << "]" << endl;
	  )
     )

reach1 = method(SingleArgumentDispatch=>true) 
reach2 = method(SingleArgumentDispatch=>true)
reach3 = method(SingleArgumentDispatch=>true)

reach1 Thing := identity
reach1 Sequence := reach1 BasicList := x -> scan(x,reach1)
reach1 SHIELD := x -> scan(x,reach3)
reach1 MENU := x -> scan(x,reach2)
reach1 optionalTO := identity
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
reach2 optionalTO := identity
reach2 TO := reach2 TOH := (x) -> (
     node := formatDocumentTag x#0;
     if not getNumberFromName#?node
     then (
	  if otherNodes#?node then remove(otherNodes,node);
     	  goOver node;
	  ))
reach3 Thing := reach1
reach3 MENU := x -> scan(x,reach1)
--------------- body of book
reach1 documentationMemo "Macaulay 2"
--------------- appendix
-- sectionNumber = {"A"}
document { "Appendix",
     "We present various footnotes in this appendix.",
     }
reach2 TO "Appendix";
--------------- cover everything else
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
--------------- fill in Appendix
docDatabase = openDatabase "../cache/Macaulay2-doc"
descend()
descend()
scan(sort join(
	  formatDocumentTag \ value \ keys docDatabase,
	  keys otherNodes
	  ),
     node -> if not getNumberFromName#?node then goOver node )
ascend()
ascend()
--------------- index
 -- sectionNumber = {"B"}
 -- document { "Combined Index",
 -- --     TEX ///\begintwocolumn
 -- --///,
 --      apply(
 -- 	  sort join(keys docDatabase, {"Appendix"}), 
 -- 	  node -> (NOINDENT{}, TO node, PARA{})),
 -- --     TEX ///
 -- --\endtwocolumn
 -- --///,
 --      }
 -- reach2 TO "Combined Index"
---------------
close docDatabase
---------------
ttLiteralTable = new MutableHashTable
scan(characters ascii(0 .. 255), 
     c -> ttLiteralTable#c = concatenate(///{\char///, toString first ascii c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
scan(characters "\\{}$&#^_%~", c -> ttLiteralTable#c = concatenate("{\\char", toString first ascii c, "}"))
-- scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))
ttLiteralTable#"\n" = "\n"
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
     	       	    	-- see page 381 of TeX Book
ttLiteral = s -> concatenate apply(characters s, c -> ttLiteralTable#c)
---------------
cmrLiteralTable = copy ttLiteralTable
scan(characters "^=_\\<>|{}", c -> cmrLiteralTable#c = concatenate("{\\tt\\char",toString first ascii c,"}"))
cmrLiteral = s -> concatenate apply(characters s, c -> cmrLiteralTable#c)
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
     then (                                  "{\\bf ", cmrLiteral text,  "}" )
     else (                                  "{\\bf ", cmrLiteral text,  "} [", sectionNumber, "]" )
     else ( "\\hyperlink{", sectionNumber, "}{{\\bf ", cmrLiteral text, "}} [", sectionNumber, "]" )
     )

booktex = method(SingleArgumentDispatch=>true)
booktex TO := booktex TOH := x -> crossReference(formatDocumentTag x#0, formatDocumentTag x#0,false) 
booktex optionalTO := x -> crossReference(formatDocumentTag x#0, formatDocumentTag x#0,true) 

menuLevel := 2

booktex MENU := x -> concatenate(
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
	       booktex if instance(x,TO) then SEQ{ x, headline x#0 } else x,
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

booktex HREF := s -> concatenate( "\\href{", ttLiteral s#0, "}{", booktex s#-1, "}" )
booktex HEADLINE := s -> ""
booktex TEX := identity
booktex NOINDENT := (x) -> ///\noindent\ignorespaces
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
booktex String := cmrLiteral
booktex ITALIC := x -> concatenate("{\\sl ",booktex toList x,"}")
verbatim = x -> concatenate (
     "\\beginverbatim%", newline,
     ttLiteral concatenate x,
     "\\endverbatim{}"
     )
booktex TT := verbatim
shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

booktex H1 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontOne%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
booktex H2 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontTwo%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
booktex H3 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontThree%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
booktex H4 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontFour%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
booktex H5 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontFive%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
booktex H6 := x -> concatenate (
     ///\medskip\noindent\begingroup\headerFontSix%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )

booktex ExampleTABLE := x -> concatenate apply(x,y -> booktex y#1)

booktex CODE :=
booktex PRE := x -> concatenate (
     ///
\beginverbatim%
\penalty-200
///,
     between(newline, 
	  shorten lines concatenate x
	  / (line ->
	       if #line <= asciiLineWidth then line
	       else concatenate(substring(line,0,asciiLineWidth), " ..."))
	  / ttLiteral
	  / (line -> if line === "" then ///\penalty-170/// else line)
	  ),
     ///
\endverbatim
\penalty-200
\noindent
///
     )
