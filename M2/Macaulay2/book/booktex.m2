--		Copyright 1997 by Daniel R. Grayson

asciiLineWidth = 80

nodeTable = new MutableHashTable
nodeTable2 = new MutableHashTable
fileNumberTable = new MutableHashTable
sectionNumberTable = new MutableHashTable
sectionNumber = {}
descend = () -> sectionNumber = append(sectionNumber,0)
ascend = () -> sectionNumber = drop(sectionNumber,-1)
String + ZZ := (s,i) -> s
record = (
     counter := 0;
     node -> (
	  counter = counter + 1;
	  if # sectionNumber > 0 then (
	       sectionNumber = append( 
		    drop(sectionNumber, -1), 
		    sectionNumber#-1 + 1 );
	       );
	  fileNumberTable#node = counter;
	  nodeTable#counter = node;
	  sectionNumberTable#counter = concatenate between(".",apply(sectionNumber,toString));
	  )
     )

reach1 = method(SingleArgumentDispatch=>true) 
reach2 = method(SingleArgumentDispatch=>true)
reach3 = method(SingleArgumentDispatch=>true)

reach1 Thing := identity
reach1 Sequence := reach1 BasicList := x -> scan(x,reach1)
reach1 SHIELD := x -> scan(x,reach3)
reach1 MENU := x -> scan(x,reach2)
reach1 TO := reach1 TOH := (x) -> (
     node := x#0;
     nodeTable2#node = true;
     )
reach2 Thing := reach1
reach2 TO := reach2 TOH := (x) -> (
     node := formatDocumentTag x#0;
     if not fileNumberTable#?node
     then (
	  record node;
     	  descend();
	  reach1 documentation node;
     	  ascend();
	  ))
reach3 Thing := reach1
reach3 MENU := x -> scan(x,reach1)
--------------- body of book
reach2 TO "Macaulay 2"
--------------- appendix
docDatabase = openDatabase "../cache/Macaulay2-doc"
unreachedNodes = new MutableHashTable
scanKeys(docDatabase, 
     node -> (
	  if not fileNumberTable#?node 
	  then unreachedNodes#node = true))
scan(keys nodeTable2, 
     node -> (
	  if not fileNumberTable#?node 
	  then unreachedNodes#node = true))
sectionNumber = {"A"}
document { "Appendix",
     "We present various footnotes in this appendix.",
     }
(
     reach2 TO "Appendix";
     descend();
     reach2 apply(sort keys unreachedNodes, node -> TO node);
     ascend()
     )
--------------- index
sectionNumber = {"B"}
document { "Combined Index",
     TEX ///\begintwocolumn
///,
     apply(
	  sort join(keys docDatabase, {"Appendix"}), 
	  node -> (NOINDENT{}, TO node, PARA{})),
     TEX ///
\endtwocolumn
///,
     }
reach2 TO "Combined Index"
---------------
close docDatabase
---------------
ttLiteralTable = new MutableHashTable
scan(characters ascii(0 .. 255), 
     c -> ttLiteralTable#c = concatenate(///{\char///, toString first ascii c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
scan(characters "\\{}$&#^_%~", 
     c -> ttLiteralTable#c = concatenate("{\\char", toString first ascii c, "}"))
scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))
ttLiteralTable#"\n" = "\n"
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
     	       	    	-- see page 381 of TeX Book
ttLiteral = s -> concatenate apply(characters s, c -> ttLiteralTable#c)
---------------
cmrLiteralTable = copy ttLiteralTable
cmrLiteralTable#"\\" = "{\\tt\\char`\\\\}"
cmrLiteralTable# "<" = "{\\tt\\char`\\<}"
cmrLiteralTable# ">" = "{\\tt\\char`\\>}"
cmrLiteralTable# "|" = "{\\tt\\char`\\|}"
cmrLiteralTable# "{" = "{\\tt\\char`\\{}"
cmrLiteralTable# "}" = "{\\tt\\char`\\}}"
cmrLiteral = s -> concatenate apply(characters s, c -> cmrLiteralTable#c)
---------------
crossReference := (key,text) -> (
     sectionNumber := (
	  if fileNumberTable#?key
	  then sectionNumberTable#(fileNumberTable#key)
	  else (
	       error("warning: documentation for key '", key, "' not found");
	       -- stderr << "warning: documentation for key '" << key << "' not found" << endl;
	       "???"
	       )
	  );
     if hypertex then ( ///\null\special{html:<A href="#///, sectionNumber, ///">}/// ),
     "\\cite{", cmrLiteral text, "}{", sectionNumber, "}",
     if hypertex then   ///\special{html:</A>}///,
     "%", newline
     )

booktex = method(SingleArgumentDispatch=>true)
booktex TO := x -> crossReference(formatDocumentTag x#0, formatDocumentTag x#0) 

menuLevel := 2

booktex MENU := x -> concatenate(
     ///
\begingroup
\parskip=0pt
///,
     apply(x, x -> (
	       (menuLevel = menuLevel + 1;),
	       ///%
\par
///,
	       apply(menuLevel-1, i -> ///\indent///),
	       ///\hangindent///, toString menuLevel, ///\parindent
///,
	       -- ///\textindent{$\bullet$}///,
	       booktex x,
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

booktex HREF := s -> (
     if hypertex then concatenate(
	  "\\special{html:<A href=\"",
	  ttLiteral s#0,
	  "\">}",
	  booktex s#-1,
	  "\\special{html:</A>}"
	  )
     else (
	  if #s === 2
	  then booktex {s#1, " (the URL is ", TT s#0, ")"}
	  else booktex TT s#0
	  )
     )

booktex TEX := identity
booktex NOINDENT := (x) -> ///\noindent\ignorespaces
///

booktex HR := (x) -> ///\par
\line{\leaders\hrule\hfill}
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
     ///\par
\beginverbatim%
\penalty-500
///,
     between(newline, 
	  shorten lines concatenate x
	  / (line ->
	       if #line <= asciiLineWidth then line
	       else concatenate(substring(line,0,asciiLineWidth), " ..."))
	  / ttLiteral
	  / (line -> if line === "" then ///\penalty-500/// else line)
	  ),
     ///
\endverbatim
\noindent
///
     )
