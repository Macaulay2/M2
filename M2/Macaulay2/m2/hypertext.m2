--		Copyright 1993-2004 by Daniel R. Grayson

-----------------------------------------------------------------------------
-- html output
-----------------------------------------------------------------------------

htmlLiteral = s -> if not match("<|&|]]>|\42",s) then s else (
     s = replace("&","&amp;",s);			    -- do this one first
     s = replace("<","&lt;",s);
     s = replace("]]>","]]&gt;",s);
     s = replace("\42","&quot;",s);  -- note: \42 is "
     s )

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

noopts := x -> select(x,e -> class e =!= Option)

scan((
	  (info,horizontalJoin),
	  (net,horizontalJoin),
	  (html,concatenate),
	  (tex,concatenate),
	  (texMath,concatenate),
	  (mathML,concatenate)
	  ),
     (op,joiner) -> op Hypertext := x -> joiner apply(noopts x,op))

scan((
	  (info,horizontalJoin),
	  (net,horizontalJoin)
	  ),
     (op,joiner) -> op HypertextParagraph := x -> wrap joiner apply(noopts x,op))

-- defop := (joiner,op) -> x -> joiner apply(x,op)
-- info Hypertext := defop(horizontalJoin,info)
-- net Hypertext := defop(horizontalJoin,net)
-- html Hypertext := defop(concatenate,html)
-- tex Hypertext := defop(concatenate,tex)
-- texMath Hypertext := defop(concatenate,texMath)
-- mathML Hypertext := defop(concatenate,mathML)

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
tex String := texLiteral
texMath String := s -> (
     if #s === 1 then s
     else concatenate("\\text{", texLiteral s, "}")
     )
info String := identity

texMath List := x -> concatenate("\\{", between(",", apply(x,texMath)), "\\}")
texMath Array := x -> concatenate("[", between(",", apply(x,texMath)), "]")
texMath Sequence := x -> concatenate("(", between(",", apply(x,texMath)), ")")

-- texMath HashTable := x -> if x.?texMath then x.texMath else texMath expression x
-- tex HashTable := x -> (
--      if x.?tex then x.tex 
--      else if x.?texMath then concatenate("$",x.texMath,"$")
--      else tex expression x
--      )
-- html HashTable := x -> html expression x

specials := new HashTable from {
     symbol ii => "&ii;"
     }

texMath Function := texMath Boolean := x -> "\\text{" | tex x | "}"

{*
 spacing between lines and paragraphs:
 observation of browsers reveals:
     nonempty PARA items get at least one blank line above and below
     empty PARA items produce just one blank line
     multiple consecutive empty PARA items have the same effect as one
     empty BR items produce one line break, forcing the current line to terminate, and a second one does it again
     empty DIV items produce one line break, forcing the current line to terminate, but a second one has no new effect
     DIV items are single spaced on separate lines
     nested DIV items don't space more widely
     multiple empty BR items produce multiple line breaks
     PARA "a", BR {}, PARA "c"        leads to "\na\n\n\n\nc\n"
     PARA "a", "b", BR {}, PARA "c"   leads to "\na\n\nb\n\nc\n"
     but: DIV elements can contain DIV elements and PARA elements
     and: DIV{DIV PARA "a", DIV PARA "b", DIV PARA "c" } should format just like DIV{ PARA "a", PARA "b", PARA "c" }
     that means the conversion to nets cannot be a totally recursive algorithm

     that leads to this algorithm:
      introduce new symbols: BK SP
      expand PARA{x} to SP x SP
      expand BR{}    to "" BK
      expand DIV{x}  to BK x BK
      do the expansions above recursively, do the following collapses at top level:
           collapse each sequence of consecutive SPs and BKs to BK "" BK if there is at least one SP in there, else to BK
      collect things between BKs and wrap them into nets, with empty sequences, 
           if we didn't collapse each BK...BK, becoming empty nets of height 0 and depth 0
      discard each BK
      stack all the nets

     We modify that slightly, by removing all the initial and final BKs and SPs at top level

     One more consideration: info MENUs should not be wrapped, but they can be contained in a DIV, which
     must arrange for the wrapping of strings contained in it.  Also, HypertextParagraphs have already been
     wrapped, so they don't need to be wrapped again.
*}

BK := local BK
SP := local SP

scan( ((net,net'), (info,info')), (f,f') -> (
	  f' = f' <- method(Dispatch => Thing); -- this will return either a f (or string), or a sequence of fs and BKs, for later splicing
	  f' Option := o -> ();
	  f' String := identity;
	  f' BR := br -> ("", BK);
	  f' Hypertext := f;
	  f' HypertextParagraph := x -> (SP, {f x}, SP);    -- use { } to indicate wrapping is already done (or not desired)
	  f' UL := x -> (BK, {f x}, BK);
	  f' HypertextContainer := x -> (BK, apply(toSequence x, f'), BK);
     	  f' Thing := x -> error("no hypertext conversion method for: ",toString x," of class ",toString class x);
	  f HypertextContainer := x -> (
	       x = deepSplice f' x;
	       n := 0;
	       while x#?n and (x#n === SP or x#n === BK) do n = n+1;
	       x = drop(x,n);
	       m := -1;
	       while x#?m and (x#m === SP or x#m === BK) do m = m-1;
	       x = drop(x,m+1);
	       x = splice sublists(x, i -> i === BK or i === SP, 
		    SPBKs -> if member(SP,SPBKs) then (BK,"",BK) else BK);
	       x = splice sublists(x, i -> i =!= BK,
		    x -> if #x===1 and instance(x#0,List) then horizontalJoin x#0 else wrap horizontalJoin x, 
		    BK -> ());
	       stack x);
	  ))
     
tex  BR := x -> ///
\hfil\break
///

tex  HR := x -> ///
\hfill\break
\hbox to\hsize{\leaders\hrule\hfill}
///

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

info TABLE := x -> (
     s := printWidth;
     if printWidth > 2 then printWidth = printWidth - 2;
     ret := netList(Boxes=>true, applyTable(toList \ noopts \\ toList x,info));
     printWidth = s;
     ret)

net TABLE :=  x -> (
     (op,ag) := override(options TABLE, toSequence x);
     save := printWidth;
     printWidth = printWidth - 2;
     r := netList(Boxes => op#"class" === "examples", toList \ toList ag);
     printWidth = save;
     r)

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

verbatim := x -> concatenate ( VERBATIM, texExtraLiteral concatenate x, ENDVERBATIM )

maximumCodeWidth = 60					    -- see also booktex.m2, an old file that sets the same variable

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
texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(x,tex),"}")
texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(x,tex),"}")
texMath TEX := tex TEX := x -> concatenate toList x

info PRE := x -> wrap(printWidth,"-",net concatenate x)
net PRE := x -> net concatenate x
html PRE   := x -> concatenate( 
     "<pre>", 
     demark(newline, apply(lines concatenate x, htmlLiteral)),
     "</pre>\n"
     )

info CODE := net CODE := x -> stack lines concatenate x

-- this is wrong now
-- tex ANCHOR := x -> (
--      concatenate(
-- 	  ///\special{html:<a id="///, texLiteral x#0, ///">}///,
-- 	  tex x#-1,
-- 	  ///\special{html:</a>}///
-- 	  )
--      )

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

net LATER := x -> net x#0()
info LATER := x -> info x#0()
html LATER := x -> html x#0()
toExternalString LATER := x -> toExternalString x#0()

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
infoLiteral#"." = "_pd"
infoLiteral#"*" = "_st"
infoLiteral#":" = "_co"
infoLit := n -> concatenate apply(characters n, c -> infoLiteral#c);
infoTagConvert = method()
tagConvert := n -> infoLit if n#0 === " " or n#-1 === " " then concatenate("\"",n,"\"") else n
infoTagConvert String := tagConvert
infoTagConvert DocumentTag := tag -> (
     pkgname := DocumentTag.Title tag;
     fkey := DocumentTag.FormattedKey tag;
     if pkgname === fkey then fkey = "Top";
     fkey = tagConvert fkey;
     if pkgname =!= currentPackage#"title" then fkey = concatenate("(",pkgname,")",fkey);
     fkey)
infoLinkConvert := s -> replace(":","_colon_",s)
info TO  := x -> (
     tag := x#0;
     f := DocumentTag.FormattedKey tag;
     if x#?1 then f = f|x#1;
     tag = getPrimary tag;
     concatenate(
	  if fetchRawDocumentation tag === null
	  then (f, " (missing documentation)")
	  else ("*note ", infoLinkConvert f, ": ", infoTagConvert tag, ",")))
info TO2 := x -> (
     tag := getPrimary x#0;
     concatenate(
	  if fetchRawDocumentation tag === null
	  then (x#1, " (missing documentation)")
	  else ("*note ", infoLinkConvert x#1, ": ", infoTagConvert tag, ",")
	  )
     )
info TOH := x -> (
     tag := x#0;
     f := DocumentTag.FormattedKey tag;
     if x#?1 then f = f|x#1;
     tag = getPrimary tag;
     concatenate(
	  if fetchRawDocumentation tag === null
	  then (f," (missing documentation)")
	  else ("*note ", infoLinkConvert f, ": ", infoTagConvert tag, ","),
	  commentize headline tag
	  )
     )

info IMG := net IMG := tex IMG  := x -> (
     (o,cn) := override(IMG.Options,toSequence x);
     if o#"alt" === null then error ("IMG item is missing alt attribute");
     o#"alt")

info HREF := net HREF := x -> net last x

scan( (net,html,tex), op -> op TOH := x -> op SPAN nonnull { new TO from toList x, commentize headline x#0 } )

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

redoMENU := r -> DIV prepend(
     HEADER3 "Menu",
     nonnull sublists(toList r, 
	  x -> class x === TO,
	  v -> if #v != 0 then UL apply(v, i -> (
		    t := optTO i#0;
		    if t === null then error("undocumented menu item ",toString i#0);
		    last t)),
	  x -> HEADER4 {x}
	  )
     )
net MENU := x -> net redoMENU x
html MENU := x -> html redoMENU x

info MENU := r -> (
     pre := "* ";
     savePW := printWidth;
     printWidth = 0; -- wrapping a menu item makes it hard for emacs info to follow links
     ret := sublists(toList r, 
	  x -> class x === TO,
	  v -> stack apply(v, i -> pre | wrap (
		    fkey := DocumentTag.FormattedKey i#0;
		    icon := infoTagConvert getPrimary i#0;
		    cfkey := infoLinkConvert fkey;
		    t := cfkey | if cfkey === icon then "::" else ": " | icon | ".";
		    h := headline i#0;
		    if h =!= null then t = concatenate(t,28-#t:" ","  ") | h;
		    t)),
	  x -> stack("",info DIV x)
	  );
     printWidth = savePW;
     stack join({"* Menu:",""}, ret))

html COMMENT := x -> concatenate("<!--",x,"-->")
html CDATA := x -> concatenate("<![CDATA[",x,"]]>")
tex COMMENT := x -> newline | concatenate apply(lines concatenate x,line -> "% " | line | newline)
info COMMENT := net COMMENT := x -> ""

-- the main idea of these comparisons is so sorting will sort by the way things will print:
TO ? TO := TO ? TOH := TOH ? TO := TOH ? TOH := (x,y) -> x#0 ? y#0
TO2 ? TO2 := (x,y) -> x#1 ? y#1
TO ? TO2 := TOH ? TO2 := (x,y) -> x#0 ? y#1
TO2 ? TO := TO2 ? TOH := (x,y) -> x#1 ? y#0

texMath STYLE := tex STYLE := net STYLE := x -> ""

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
