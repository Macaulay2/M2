--		Copyright 1993-2004 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- sublists, might be worthy making public
-----------------------------------------------------------------------------
sublists := (x,f,g,h) -> (
     -- x is a list with elements i
     -- apply g to those i for which f i is true
     -- apply h to the sublists, possibly empty, including those at the beginning and end, of elements between the ones for which f i is true
     -- return the results in the same order
     p := positions(x, f);
     mingle(
	  apply( prepend(-1,p), append(p,#x), (i,j) -> h take(x,{i+1,j-1})),
	  apply( p, i -> g x#i)))

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
texLiteral := s -> concatenate apply(characters s, c -> texLiteralTable#c)

HALFLINE := ///\vskip 4.75pt
///
ENDLINE := ///\leavevmode\hss\endgraf
///
VERBATIM := ///\begingroup\baselineskip=9.5pt\tt\parskip=0pt
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
     else if x.?name then x.name
     else tex expression x
     )

mathML Nothing := texMath Nothing := tex Nothing := html Nothing := x -> ""

specials := new HashTable from {
     symbol ii => "&ii;"
     }

mathML Symbol := x -> concatenate("<mi>",if specials#?x then specials#x else toString x,"</mi>")

tex Function := x -> "--Function--"

tex Boolean := tex Symbol := 
html Symbol := html Boolean := toString

texMath Function := texMath Boolean := x -> "\\text{" | tex x | "}"

vert := (op,post) -> x -> net new ParagraphList from post select(
     sublists(
	  toList x,
	  i -> instance(i,MarkUpListParagraph),
	  op,
	  i -> horizontalJoin(op \ i)),
     n -> width n > 0)
net SEQ := net PARA := net Hypertext := vert(net,x -> between("",x)) -- doublespacing
info SEQ := info PARA := info Hypertext := vert(info,x -> between("",x))
net PARA1 := vert(net,identity)				    -- singlespacing
info PARA1 := vert(info,identity)

html BR := x -> ///
<br>
///
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

html Hypertext := x -> concatenate(apply(x,html))	    -- used to have "<p>" in here

html PARA1 := x -> concatenate("\n", apply(x,html),"\n")
html PARA := x -> (
     if #x === 0 then "\n<p/>\n"
     else concatenate("\n<p>\n", apply(x,html),"\n</p>\n")
     )

tex PARA := x -> concatenate(///
\par
///,
     apply(x,tex))

html ExampleTABLE := x -> concatenate(
     newline,
     "<table class=\"examples\">", newline,
     apply(x, 
	  item -> (
	       " <tr>", newline,
	       "  <td>", html item#1, newline,
	       "  </td>", newline,
	       " </tr>", newline
	       )
	  ),
     "</table>", newline)			 
html EXAMPLE := x -> concatenate html ExampleTABLE apply(#x, i -> {x#i, CODE concatenate("i",toString (i+1)," : ",x#i)})
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

truncateString := s -> if printWidth == 0 or width s <= printWidth then s else concatenate(substring(s,0,printWidth-1),"$")
truncateNet    := n -> if printWidth == 0 or width n <= printWidth then n else stack(apply(unstack n,truncateString))

info ExampleTABLE := net ExampleTABLE := x -> (
     p := "    ";
     if printWidth != 0 then printWidth = printWidth - #p -2;
     r := p | boxList apply(toList x, y -> truncateNet net y#1);
     if printWidth != 0 then printWidth = printWidth + #p + 2;
     r)
info EXAMPLE := net EXAMPLE := x -> net ExampleTABLE apply(#x, i -> {x#i, CODE concatenate("i",toString (i+1)," : ",x#i)})

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

tex ExampleTABLE := x -> concatenate apply(x,y -> tex y#1)

info TABLE := x -> boxTable applyTable(toList x,info)
net TABLE := x -> boxTable applyTable(toList x,net)
html TABLE := x -> concatenate(
     newline,
     "<table>", newline,
     apply(x, row -> ( 
	       "  <tr>", newline,
	       apply(row, item -> (
			 "    <td>", html item, newline,
			 "    </td>",newline
			 )),
	       "  </tr>", newline)),
     "</table>", newline )			 

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

info TT := net TT := x -> horizontalJoin splice (net  \ toSequence x)

htmlDefaults = new MutableHashTable from {
     -- "BODY" => "bgcolor='#e4e4ff'"
     "BODY" => ""
     }

html BODY := x -> concatenate(
     "<body ", htmlDefaults#"BODY", ">", newline,
     apply(x, html), newline,
     "</body>", newline
     )

html LISTING := t -> "<listing>" | concatenate toSequence t | "</listing>";

texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(x,tex),"}")

texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(x,tex),"}")
html ITALIC := x -> concatenate("<i>",apply(x,html),"</i>")

texMath TEX := tex TEX := x -> concatenate toList x

texMath SEQ := tex SEQ := x -> concatenate(apply(toList x, tex))
html SEQ := x -> concatenate(apply(toList x, html))

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

html TEX := x -> x#0

commentize := s -> if s =!= null then concatenate(" -- ",s)

addHeadlines := x -> apply(x, i -> if instance(i,TO) then SEQ{ i, commentize headline i#0 } else i)

addHeadlines1 := x -> apply(x, i -> if instance(i,TO) then SEQ{ "help ", i, commentize headline i#0 } else i)

info HR := net HR := x -> "-----------------------------------------------------------------------------"

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

NLop := op -> x -> stack apply(#x, i -> toString (i+1) | " : " | wrap(printWidth - 10, op x#i))
net NL := NLop net
info NL := NLop info

tex UL := x -> concatenate(
     ///\begin{itemize}///, newline,
     apply(addHeadlines x, x -> if x =!= null then ( ///\item ///, tex x, newline)),
     ///\end{itemize}///, newline)

html UL := x -> concatenate (
     newline,
     "<ul>", newline,
     apply(addHeadlines x, s -> if s =!= null then ("<li>", html s, "</li>", newline)),
     "</ul>", newline)

html NL   := x -> concatenate( "<nl>", newline, apply(x,s -> ("<li>", html s, "</li>", newline)), "</nl>", newline)

texMath SUP := x -> concatenate( "^{", apply(x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(x, tex), "}" )

opSU := (op,n) -> x -> (horizontalJoin apply(x, op))^n
net SUP := opSU(net,1)
info SUP := opSU(info,1)
net SUB := opSU(net,-1)
info SUB := opSU(info,-1)

tex TO := x -> (
     tag := x#0;
     tex SEQ {
     	  TT DocumentTag.FormattedKey tag,
     	  " [", LITERAL { 
	       "\ref{", 
	       -- need something here
	       "}" },
	  "]"
	  }
     )

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

scan( (net,html,tex), op -> op TOH := x -> op SEQ{ new TO from x, commentize headline x#0 } )

tex LITERAL := html LITERAL := x -> concatenate x
html EmptyMarkUpType := html MarkUpType := X -> html X{}
html ITALIC := t -> concatenate("<i>", apply(t,html), "</i>")
html UNDERLINE := t -> concatenate("<u>", apply(t,html), "</u>")
html BOLD := t -> concatenate("<b>", apply(t,html), "</b>")
html TEX := x -> x#0	    -- should do something else!

html Option := x -> toString x

tex HEADER1 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontOne=cmbx12 scaled \magstep 1\headerFontOne%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
tex HEADER2 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontTwo=cmbx12 scaled \magstep 1\headerFontTwo%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
tex HEADER3 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontThree=cmbx12\headerFontThree%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
tex HEADER4 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontFour=cmbx12\headerFontFour%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
tex HEADER5 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontFive=cmbx10\headerFontFive%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
///
     )
tex HEADER6 := x -> concatenate (
     ///\medskip\noindent\begingroup\font\headerFontSix=cmbx10\headerFontSix%
///,
     apply(toList x, tex),
     ///\endgroup\par\smallskip%
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

redoMENU := r -> SEQ prepend(
     HEADER3 "Menu",
     sublists(toList r, 
	  x -> not ( class x === TO ),
	  x -> HEADER4 {x},
	  v -> UL apply(v, i -> TOH i#0 )))
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
