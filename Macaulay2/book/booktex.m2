--		Copyright 1997 by Daniel R. Grayson

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
	  sectionNumberTable#counter = concatenate between(".",apply(sectionNumber,string));
	  )
     )

 -- ignore TO; find MENU, switch, descend
reach1 = method(SingleArgumentDispatch=>true) 

 -- record TO items, switch, descend
reach2 = method(SingleArgumentDispatch=>true)

reach1(Thing   ) := identity
reach1(Sequence) :=
reach1(BasicList):= x -> scan(x,reach1)
reach1(MENU    ) := reach2
reach1(TO      ) := (x) -> (
     node := x#0;
     nodeTable2#node = true;
     )

reach2(Thing   ) := identity
reach2(Sequence) :=
reach2(BasicList):= x -> scan(x,reach2)
reach2(SHIELD)   := reach1
reach2(TO      ) := (x) -> (
     node := x#0;
     if not fileNumberTable#?node
     then (
	  record node;
     	  descend();
	  reach1 doc node;
     	  ascend();
	  ))
--------------- body of book
reach2 TO "Macaulay 2"
--------------- appendix
docDatabase = openDatabase "../cache/Macaulay2.doc"
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
	  node -> (NOINDENT, TO node, PARA)),
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
     c -> ttLiteralTable#c = concatenate(///{\char///, string first ascii c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
scan(characters "\\{}$&#^_%~#", 
     c -> ttLiteralTable#c = concatenate("{\\char", string first ascii c, "}"))
scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))
ttLiteralTable#"\n" = "\n"
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
     	       	    	-- see page 381 of TeX Book
ttLiteral = s -> concatenate apply(characters s, c -> ttLiteralTable#c)
---------------
cmrLiteralTable = copy ttLiteralTable
cmrLiteralTable#"<" = "$<$"
cmrLiteralTable#">" = "$>$"
cmrLiteralTable#"|" = "$|$"
cmrLiteralTable#"{" = "$\\{$"
cmrLiteralTable#"}" = "$\\}$"
cmrLiteral = s -> apply(characters s, c -> cmrLiteralTable#c)
---------------
booktex = method(SingleArgumentDispatch=>true)
booktex TO  := x -> (
     sectionNumber := (
	  if fileNumberTable#?(x#0)
	  then sectionNumberTable#(fileNumberTable#(x#0))
	  else (
	       stderr << "warning: documentation for key '" << x#0 << "' not found" << endl;
	       "???"
	       )
	  );
     if hypertex then (
	  ///\null\special{html:<A href="#///,
	  sectionNumber, 
	  ///">}///
	  ),
     "\\cite{",
     cmrLiteral concatenate x,
     "}{",
     sectionNumber,
     "}",
     if hypertex then ///\special{html:</A>}///
     )
booktex MENU := x -> (
     ///

\begingroup\parindent=40pt

///,
     apply(x, x -> (
	       ///\item{$\bullet$}///,
	       booktex x,
	       "\n"
	       )
	  ),
     "\\endgroup\n\n")
booktex HREF := s -> (
     if hypertex then concatenate(
	  "\\special{html:<A href=\"",
	  ttLiteral s#0,
	  "\">}",
	  booktex s#1,
	  "\\special{html:</A>}"
	  )
     else booktex {s#1, " (the URL is ", TT s#0, ")"}
     )
booktex TEX := identity
(class NOINDENT)#booktex = (x) -> ///
\noindent\ignorespaces
///

(class HR)#booktex   = (x) -> ///
\line{\leaders\hrule\hfill}
///

(class PARA)#booktex = (x) -> "\n\n"
(class BR)#booktex  = (x) -> ///\hfil\break
///
booktex IMG := x -> ""
booktex Nothing := x -> ""
booktex Boolean := booktex Symbol := string
booktex BasicList := booktex Sequence := x -> apply(x,booktex)
booktex String := cmrLiteral
booktex ITALIC := x -> ("{\\sl ",booktex elements x,"}")
verbatim = x -> ("\\beginverbatim%\n", ttLiteral concatenate x, "\\endverbatim{}")
booktex TT := verbatim
shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)
booktex PRE := x -> concatenate (
     ///\par
\vskip 4 pt
\beginverbatim%
///,
     between("\n", 
	  shorten lines concatenate x
	  / (line ->
	       if #line <= 71 then line
	       else concatenate(substring(line,0,71), " ..."))
	  / ttLiteral
	  / (line -> if line === "" then ///\penalty-500/// else line)
	  ),
     ///\endverbatim
\par
\noindent
///
     )
