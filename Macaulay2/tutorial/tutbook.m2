--		Copyright 1995 by Daniel R. Grayson

if hypertex === quote hypertex then hypertex = false

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
reach1 = method(SingleArgumentDispatch=>true) 
reach2 = method(SingleArgumentDispatch=>true)
reach1 Thing := identity
reach1 Sequence := reach1 BasicList := x -> scan(x,reach1)
reach2 Thing := identity
reach2 Sequence := reach2 BasicList := x -> scan(x,reach2)
reach1 MENU := reach2
reach1 TO := (x) -> (
     node := x#0;
     nodeTable2#node = true;
     )
reach2 TO := (x) -> (
     node := x#0;
     if not fileNumberTable#?node
     then (
	  record node;
     	  descend();
	  reach1 doc node;
     	  ascend();
	  ))
--------------- body of book
--load "tutorial.m2"
document { "tutorials",
     MENU {
          TO "Tutorial:Elementary uses of Groebner bases",
          TO "Example:Binomial Ideals",
	  TO "Tutorial:Canonical Embeddings of Plane Curves and Gonality",
	  TO "Tutorial:Fano varieties",
	  TO "Tutorial:Divisors"
--	  TO "Tutorial:Multi-gradings",
--	  TO "Tutorial:Rational Normal Scrolls",
--	  TO "Example:Enriques surface"
	  }
     }

load "final/elementary.out"
load "final/binomial.out"
load "final/canEmbed.out"
load "final/Fano.out"
load "final/divisors.out"

reach2 TO "tutorials"
--------------- appendix
-- docDatabase = openDatabase "../libexec/Macaulay2.doc"
-- unreachedNodes = new MutableHashTable
-- scankeys(docDatabase, 
--     node -> (
--	  if not fileNumberTable#?node 
--	  then unreachedNodes#node = true))
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
     -- reach2 apply(rsort keys unreachedNodes, node -> TO node);
     ascend()
     )
--------------- index
sectionNumber = {"B"}
document { "Combined Index",
     TEX "\\begintwocolumn\n",
     apply(
	  rsort join(
		-- keys docDatabase, 
		{"Appendix"}
		), 
	  node -> (NOINDENT, TO node, PARA)),
     TEX "\\endtwocolumn\n",
     }
reach2 TO "Combined Index"
---------------
-- close docDatabase
---------------
ttLiteralTable = new MutableHashTable
scan(characters ascii(0 .. 255), 
     c -> ttLiteralTable#c = concatenate("{\\char", string first ascii c, "}"))
scan(characters ascii(32 .. 126), c -> ttLiteralTable#c = c)
scan(characters "\\{}$&#^_%~#", 
     c -> ttLiteralTable#c = concatenate("{\\char", string first ascii c, "}"))
scan(characters "$%&#_", c -> ttLiteralTable#c = concatenate("\\",c))
ttLiteralTable#"\n" = "\n"
ttLiteralTable#"\t" = "\t"
ttLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
     	       	    	-- see page 381 of TeX Book
ttLiteral = s -> apply(characters s, c -> ttLiteralTable#c)
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
TO#booktex   = (x) -> (
     sectionNumber := "XXX";
     if fileNumberTable#?(x#0) 
     then sectionNumber = sectionNumberTable#(fileNumberTable#(x#0))
     else stderr << "warning : kay '" << x#0 << "' not found" << endl;
     if hypertex then ("\\null\\special{html:<A href=\"#", sectionNumber, "\">}"),
     "\\cite{", cmrLiteral concatenate x, "}{", sectionNumber, "}",
     if hypertex then "\\special{html:</A>}"
     )
MENU#booktex = (x) -> ("\n\n\\begingroup\\parindent=40pt\n",
     apply(x, x->("\\item{$\\bullet$} ",booktex x,"\n")),
     "\\endgroup\n\n")
TEX#booktex = identity
(class NOINDENT)#booktex = (x) -> "\\noindent\\ignorespaces\n"
(class HR)#booktex   = (x) -> "\n\n\\line{\\leaders\\hrule\\hfill}\n\n"
(class PARA)#booktex = (x) -> "\n\n"
(class BR)#booktex  = (x) -> "\\hfil\\break\n"
IMG#booktex = (x) -> ""
Nothing#booktex = x -> ""
Symbol#booktex = x -> string x
BasicList#booktex = Sequence#booktex = x -> apply(x,booktex)
String#booktex = cmrLiteral
ITALIC#booktex = x -> ("{\\sl ",booktex list x,"}")
verbatim = x -> ("\\beginverbatim%\n", ttLiteral concatenate x, "\\endverbatim{}")
TT#booktex = verbatim

chop := x -> (
     concatenate between(
	  "\n",
	  apply(lines concatenate x, 
	       y -> if #y <= 71 then y else (substring(y,0,71), " ..."))))
PRE#booktex = x -> ("\\par\\vskip 7 pt\n", verbatim chop x, "\\par\\noindent\n")
--------------------------------------------------- make the tex file
bk = openOut (if hypertex then "tuthbook.tex" else "tutbook.tex")
--------------------------------------------
bk << "
%% Macaulay 2 tutorial
%% Copyright 1995, by Daniel R. Grayson and Michael E. Stillman

\\input ../book/macros.tex
\\def\\P{\\bf P}

";
--------------------------------------------

if hypertex then (
     bk << "\\def\\sectionhdr#1#2{%
	\\bigskip\\goodbreak
	\\special{html:<A name=\"#2\">}
	\\line{\\bf#2\\ \\ \\leaders\\hrule\\hfill\\ \\ #1\\ \\ \\leaders\\hrule\\hfill\\ \\ #2}%
	\\special{html:</A>}
	\\medskip\\noindent\\ignorespaces
	}
") else (
     bk << "\\def\\sectionhdr#1#2{%
	\\bigskip\\goodbreak
	\\line{\\bf#2\\ \\ \\leaders\\hrule\\hfill\\ \\ #1\\ \\ \\leaders\\hrule\\hfill\\ \\ #2}%
	\\medskip\\noindent\\ignorespaces
	}
")
--------------------------------------------
bk << "

\\input titlepage.tex

"
--------------------------------------------
-- this depends on the feature of hash tables that when the keys
-- are consecutive integers starting at 0, the keys are scanned
-- in the natural order, which in turn depends on the hash number of
-- a small integer being the integer itself

scan(pairs nodeTable, (i,node) -> (
	  bk 
	  << "\n\n"
	  << "\\sectionhdr{" 
	  << concatenate cmrLiteral node << "}{" << sectionNumberTable#i 
	  << "}\n"
	  << concatenate booktex doc node << endl;
	  ))
bk << "\\end\n" << close
