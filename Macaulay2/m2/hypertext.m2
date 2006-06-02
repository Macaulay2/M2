--		Copyright 1993-2004 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

htmlLiteralTable := new MutableHashTable
scan(characters ascii(0 .. 255), c -> htmlLiteralTable#c = c)
htmlLiteralTable#"\"" = "&quot;"
htmlLiteralTable#"<" = "&lt;"
htmlLiteralTable#"&" = "&amp;"
htmlLiteralTable#">" = "&gt;"
htmlLiteral = s -> concatenate apply(characters s, c -> htmlLiteralTable#c)

htmlExtraLiteralTable := copy htmlLiteralTable
htmlExtraLiteralTable#" " = "&nbsp;"
htmlExtraLiteral = s -> concatenate apply(characters s, c -> htmlExtraLiteralTable#c)
-----------------------------------------------------------------------------
texLiteralTable := new MutableHashTable
    scan(0 .. 255, c -> texLiteralTable#(ascii{c}) = concatenate(///{\char ///, toString c, "}"))
    scan(characters ascii(32 .. 126), c -> texLiteralTable#c = c)
    scan(characters "\\{}$&#^_%~|<>", c -> texLiteralTable#c = concatenate("{\\char ", toString (ascii c)#0, "}"))
    texLiteralTable#"\n" = "\n"
    texLiteralTable#"\r" = "\r"
    texLiteralTable#"\t" = "\t"
    texLiteralTable#"`" = "{`}"     -- break ligatures ?` and !` in font \tt
				   -- see page 381 of TeX Book
texLiteral = s -> concatenate apply(characters s, c -> texLiteralTable#c)

HALFLINE := ///\vskip 4.75pt
///
ENDLINE := ///\leavevmode\hss\endgraf
///
VERBATIM := ///\begingroup\tt\parskip=0pt
///
ENDVERBATIM := ///\endgroup{}///

texExtraLiteralTable := copy texLiteralTable
texExtraLiteralTable#" " = "\\ "
texExtraLiteral := s -> demark(ENDLINE,
     apply(lines s, l -> apply(characters l, c -> texExtraLiteralTable#c))
     )
-----------------------------------------------------------------------------
-- the default case
scan((
	  (info,horizontalJoin),
	  (net,horizontalJoin),
	  (html,concatenate),
	  (tex,concatenate),
	  (texMath,concatenate),
	  (mathML,concatenate)
	  ),
     (op,joiner) -> op MarkUpList := x -> joiner apply(x,op))

-- defop := (joiner,op) -> x -> joiner apply(x,op)
-- info MarkUpList := defop(horizontalJoin,info)
-- net MarkUpList := defop(horizontalJoin,net)
-- html MarkUpList := defop(concatenate,html)
-- tex MarkUpList := defop(concatenate,tex)
-- texMath MarkUpList := defop(concatenate,texMath)
-- mathML MarkUpList := defop(concatenate,mathML)

info TITLE := net TITLE := x -> ""

Hop := (op,filler) -> x -> ( 
     r := horizontalJoin apply(x,op);
     if width r === 1 then r = horizontalJoin(r," ");
     r || concatenate( width r : filler ) )
net  HEADER1 := Hop(net,"*")
net  HEADER2 := Hop(net,"=")
net  HEADER3 := Hop(net,"-")
info HEADER1 := Hop(info,"*")
info HEADER2 := Hop(info,"=")
info HEADER3 := Hop(info,"-")

html String := htmlLiteral
mathML String := x -> concatenate("<mtext>",htmlLiteral x,"</mtext>")
tex String := texLiteral
texMath String := s -> (
     if #s === 1 then s
     else concatenate("\\text{", texLiteral s, "}")
     )
info String := identity

texMath List := x -> concatenate("\\{", between(",", apply(x,texMath)), "\\}")
texMath Array := x -> concatenate("[", between(",", apply(x,texMath)), "]")
texMath Sequence := x -> concatenate("(", between(",", apply(x,texMath)), ")")

texMath HashTable := x -> if x.?texMath then x.texMath else texMath expression x
tex HashTable := x -> (
     if x.?tex then x.tex 
     else if x.?texMath then concatenate("$",x.texMath,"$")
     else tex expression x
     )
html HashTable := x -> html expression x

mathML Nothing := texMath Nothing := tex Nothing := html Nothing := x -> ""

specials := new HashTable from {
     symbol ii => "&ii;"
     }

mathML Symbol := x -> concatenate("<mi>",if specials#?x then specials#x else toString x,"</mi>")

tex Function := x -> "--Function--"

tex Boolean := tex Symbol := 
html Symbol := html Boolean := toString

texMath Function := texMath Boolean := x -> "\\text{" | tex x | "}"

noopts := x -> select(x,e -> class e =!= Option)

wrapAndStack := x -> (
     x = toList x;
     x = net \ x;					    -- convert each to net
     x = apply(x,p -> wrap net p);
     stack x)

vert := (op,post) -> x -> wrapAndStack post select( -- wrapping gets done by the "net" on this line
     sublists(
	  toList x,
	  i -> instance(i,MarkUpListParagraph),
	  op,
	  i -> horizontalJoin(op \ i)),
     n -> width n > 0)
net PARA := net DIV := (vert(net,x -> between("",x))) @@ noopts -- doublespacing
info PARA := info DIV := (vert(info,x -> x)) @@ noopts
info LI := net LI := info DIV1 := net DIV1 := (vert(net,identity)) @@ noopts				    -- singlespacing

tex  BR := x -> ///
\hfil\break
///

html NOINDENT := x -> ""
tex  NOINDENT := x -> ///
\noindent\ignorespaces
///

html HEAD  := x -> concatenate( "<head>", newline, apply(x, html), newline, "</head>", newline )
html TITLE := x -> concatenate( "<title>", apply(x, html), "</title>", newline )

html HR := x -> "\n<hr/>\n"
tex  HR := x -> ///
\hfill\break
\hbox to\hsize{\leaders\hrule\hfill}
///

html PARA := x -> (
     if #x === 0 then "\n<p/>\n"
     else concatenate("\n<p>\n", apply(x,html),"\n</p>\n")
     )

tex PARA := x -> concatenate(///
\par
///,
     apply(x,tex))

html ButtonTABLE := x -> concatenate(
     newline,
     "<table class=\"buttons\">", newline,
     apply(x, row -> ( 
	       "  <tr>", newline,
	       apply(row, item -> (
			 "    <td>", html item, newline,
			 "    </td>", newline
			 )),
	       "  </tr>", newline)),
     "</table>", newline )			 

truncWidth := 0
truncateString := s -> if printWidth == 0 or width s <= printWidth then s else concatenate(substring(s,0,truncWidth-1),"$")
truncateNet    := n -> if printWidth == 0 or width n <= printWidth then n else stack(apply(unstack n,truncateString))

tex TABLE := x -> concatenate applyTable(x,tex)
texMath TABLE := x -> concatenate (
     ///
\matrix{
///,
     apply(x,
	  row -> (
	       apply(row,item -> (texMath item, "&")),
	       ///\cr
///
	       )
	  ),
     ///}
///
     )

info TABLE := x -> boxTable applyTable(toList \ noopts \\ toList x,info)
net  TABLE := x -> boxTable applyTable(toList \ noopts \\ toList x,net)
-- html TABLE := x -> concatenate(
--      newline,
--      "<table>", newline,
--      apply(x, row -> ( 
-- 	       "  <tr>", newline,
-- 	       apply(row, item -> (
-- 			 "    <td>", html item, newline,
-- 			 "    </td>",newline
-- 			 )),
-- 	       "  </tr>", newline)),
--      "</table>", newline )

info PRE := net PRE := x -> net concatenate x
html PRE   := x -> concatenate( 
     "<pre>", 
     html demark(newline,
	  apply(lines concatenate x, s -> concatenate("     ",s))),
     "</pre>"
     )

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

verbatim := x -> concatenate ( VERBATIM, texExtraLiteral concatenate x, ENDVERBATIM )

tex TT := texMath TT := verbatim
tex CODE :=
tex PRE := x -> concatenate ( VERBATIM,
     ///\penalty-200
///,
     HALFLINE,
     shorten lines concatenate x
     / (line ->
	  if #line <= maximumCodeWidth then line
	  else concatenate(substring(0,maximumCodeWidth,line), " ..."))
     / texExtraLiteral
     / (line -> if line === "" then ///\penalty-170/// else line)
     / (line -> (line, ENDLINE)),
     ENDVERBATIM,
     HALFLINE,
     ///\penalty-200\par{}
///
     )

net TT := info TT := x -> concatenate toSequence x   -- should just be strings here
html LISTING := t -> "<listing>" | concatenate toSequence t | "</listing>";
texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(x,tex),"}")
texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(x,tex),"}")
html ITALIC := x -> concatenate("<i>",apply(x,html),"</i>")
texMath TEX := tex TEX := x -> concatenate toList x

-- these seem questionable:
tex Sequence := tex List := tex Array := x -> concatenate("$",texMath x,"$")
html Sequence := x -> concatenate("(", between(",", apply(x,html)), ")")
html List := x -> concatenate("{", between(",", apply(x,html)), "}")

info CODE := net CODE := x -> stack lines concatenate x
html CODE   := x -> concatenate( 
     "<pre>", 
     demark(newline, apply(lines concatenate x, htmlExtraLiteral) ),
     "</pre>"
     )

html ANCHOR := x -> (
     "\n<a id=\"" | x#0 | "\">" | html x#-1 | "</a>"
     )
info ANCHOR := net ANCHOR := x -> net last x
tex ANCHOR := x -> (
     concatenate(
	  ///\special{html:<a id="///, texLiteral x#0, ///">}///,
	  tex x#-1,
	  ///\special{html:</a>}///
	  )
     )

commentize := s -> if s =!= null then concatenate(" -- ",s)

info HR := net HR := x -> concatenate(printWidth:"-")

info Nothing := net

ULop := op -> x -> (
     s := "  * ";
     printWidth = printWidth - #s;
     r := stack apply(toList x, i -> s | op i);
     printWidth = printWidth + #s;
     r)
info UL := ULop info
net UL := ULop net

* String := x -> help x					    -- so the user can cut paste the menu line to get help!

tex UL := x -> concatenate( ///\begin{itemize}///, newline, apply(x, x -> ( ///\item ///, tex x, newline)), ///\end{itemize}///, newline)

texMath SUP := x -> concatenate( "^{", apply(x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(x, tex), "}" )

opSU := (op,n) -> x -> (horizontalJoin apply(x, op))^n
net SUP := opSU(net,1)
info SUP := opSU(info,1)
net SUB := opSU(net,-1)
info SUB := opSU(info,-1)

tex TO := x -> tex TT DocumentTag.FormattedKey x#0

tex TO2 := x -> (
     tag := x#0;
     text := x#1;
     tex TT text )

net TO  := x -> (
     if class x#0 === DocumentTag 
     then concatenate( "\"", DocumentTag.FormattedKey x#0, "\"", if x#?1 then x#1)
     else horizontalJoin( "\"", net x#0, "\"", if x#?1 then x#1)
     )
net TO2 := x -> x#1

-- node names in info files are delimited by commas and parentheses somehow...
infoLiteral := new MutableHashTable
scan(characters ascii(0 .. 255), c -> infoLiteral#c = c)
infoLiteral#"(" = "_lp"
infoLiteral#"_" = "_us"
infoLiteral#")" = "_rp"
infoLiteral#"," = "_cm"
infoLiteral#"*" = "_st"
infoLiteral#":" = "_co"
infoLit := n -> concatenate apply(characters n, c -> infoLiteral#c);
infoTagConvert = method()
tagConvert := n -> infoLit if n#0 === " " or n#-1 === " " then concatenate("\"",n,"\"") else n
infoTagConvert String := tagConvert
infoTagConvert DocumentTag := tag -> (
     pkg := DocumentTag.Package tag;
     fkey := DocumentTag.FormattedKey tag;
     if currentPackage === pkg 
     then tagConvert fkey
     else concatenate("(",pkgTitle pkg,")",tagConvert fkey))
info TO  := x -> concatenate(format DocumentTag.FormattedKey x#0, if x#?1 then x#1, "  (*note ", infoTagConvert x#0, "::)")
info TO2 := x -> concatenate(x#1, "  (*note ", x#1, ":", infoTagConvert x#0, ".)")
info TOH := x -> concatenate(DocumentTag.FormattedKey x#0, if x#?1 then x#1, commentize headline x#0,, "  (*note ", infoTagConvert x#0, "::)" )

info IMG := net IMG := tex IMG  := x -> ""
info HREF := net HREF := x -> net last x

scan( (net,html,tex), op -> op TOH := x -> op SPAN{ new TO from toList x, commentize headline x#0 } )

info LITERAL := tex LITERAL := net LITERAL := x -> ""
html LITERAL := x -> concatenate x
html ITALIC := t -> concatenate("<i>", apply(t,html), "</i>")
html BOLD := t -> concatenate("<b>", apply(t,html), "</b>")

html Option := x -> error("attempted to convert option '", toString x, "' to html")

--     \rm     Roman
--     \sf     sans-serif
--     \tt     typewriter

--     \tiny		5
--     \scriptsize	7
--     \footnotesize	8
--     \small		9
--     \normalsize	10
--     \large		12
--     \Large		14
--     \LARGE		18
--     \huge		20
--     \Huge		24

tex HEADER1 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\Large\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )
tex HEADER2 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\Large\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )
tex HEADER3 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\large\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )
tex HEADER4 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\large\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )
tex HEADER5 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\normal\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )
tex HEADER6 := x -> concatenate (
     ///
\par\medskip\noindent\begingroup\normal\bf
///,
     apply(toList x, tex),
     ///\endgroup
\par\smallskip%
///
     )

html HEADER1 := x -> concatenate (
     ///
<h1>///,
     apply(toList x, html),
     ///</h1>
///
     )

html HEADER2 := x -> concatenate (
     ///
<h2>///,
     apply(toList x, html),
     ///</h2>
///
     )

html HEADER3 := x -> concatenate (
     ///
<h3>///,
     apply(toList x, html),
     ///</h3>
///
     )

html HEADER4 := x -> concatenate (
     ///
<h4>///,
     apply(toList x, html),
     ///</h4>
///
     )

html HEADER5 := x -> concatenate (
     ///
<h5>///,
     apply(toList x, html),
     ///</h5>
///
     )

html HEADER6 := x -> concatenate (
     ///
<h6>///,
     apply(toList x, html),
     ///</h6>
///
     )

redoMENU := r -> DIV prepend(
     HEADER3 "Menu",
     nonnull sublists(toList r, 
	  x -> not ( class x === TO ),
	  x -> HEADER4 {x},
	  v -> if #v != 0 then UL apply(v, i -> (
		    t := optTO i#0;
		    if t === null then error("undocumented menu item ",toString i#0);
		    last t
		    ))))
net MENU := x -> net redoMENU x
html MENU := x -> html redoMENU x

info MENU := r -> (
     pre := "* ";
     printWidth = printWidth - #pre;
     ret := sublists(toList r, 
	  x -> not ( class x === TO ),
	  x -> stack("",info PARA x),
	  v -> stack apply(v, i -> pre | (
		    t := DocumentTag.FormattedKey i#0 | "::";
		    h := headline i#0;
		    if h =!= null then (
		    	 t = concatenate(t,28-#t:" ","  ");
			 wt := #t;
		    	 printWidth = printWidth - wt;
		    	 t = t | info PARA h;
		    	 printWidth = printWidth + wt;
			 );
		    t)));
     printWidth = printWidth + #pre;
     stack join({"* Menu:",""}, ret))

-- the main idea of these comparisons is so sorting will sort by the way things will print:
TO ? TO := TO ? TOH := TOH ? TO := TOH ? TOH := (x,y) -> x#0 ? y#0
TO2 ? TO2 := (x,y) -> x#1 ? y#1
TO ? TO2 := TOH ? TO2 := (x,y) -> x#0 ? y#1
TO2 ? TO := TO2 ? TOH := (x,y) -> x#1 ? y#0

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
