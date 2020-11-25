--		Copyright 1993-2004 by Daniel R. Grayson
-----------------------------------------------------------------------------
-- tex, and texMath output
-----------------------------------------------------------------------------

-- TODO: remove as duplicate
noopts := x -> select(x,e -> class e =!= Option)

texLiteralTable := new MutableHashTable
scan(0 .. 255, c -> texLiteralTable#(ascii{c}) = concatenate(///{\char ///, toString c, "}"))
scan(characters ascii(32 .. 126), c -> texLiteralTable#c = c)
scan(characters "\\{}$&#^_%~|<>", c -> texLiteralTable#c = concatenate("{\\char ", toString (ascii c)#0, "}"))
texLiteralTable#"\n" = "\n"
texLiteralTable#"\r" = "\r"
texLiteralTable#"\t" = "\t"
texLiteralTable#"`"  = "{`}" -- break ligatures ?` and !` in font \tt. See page 381 of TeX Book.
texLiteral = s -> concatenate apply(characters s, c -> texLiteralTable#c)

HALFLINE    := "\\vskip 4.75pt\n"
ENDLINE     := "\\leavevmode\\hss\\endgraf\n"
VERBATIM    := "\\begingroup\\tt\\parskip=0pt\n"
ENDVERBATIM := "\\endgroup{}"

texExtraLiteralTable := copy texLiteralTable
texExtraLiteralTable#" " = "\\ "
texExtraLiteral := s -> demark(ENDLINE, apply(lines s, l -> apply(characters l, c -> texExtraLiteralTable#c)))

-----------------------------------------------------------------------------
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
    "\n\\par\\medskip\\noindent\\begingroup\\Large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")
tex HEADER2 := x -> concatenate (
    "\n\\par\\medskip\\noindent\\begingroup\\Large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")
tex HEADER3 := x -> concatenate (
    "\n\\par\\medskip\\noindent\\begingroup\\large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")
tex HEADER4 := x -> concatenate (
    "\n\\par\\medskip\\noindent\\begingroup\\large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")
tex HEADER5 := x -> concatenate (
    "\n\\par\\medskip\\noindent\\begingroup\\normal\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")
tex HEADER6 := x -> concatenate (
    "\n\\par\\medskip\\noindent\\begingroup\\normal\\bf\n",
    apply(toList noopts x, tex), "\\endgroup\n\\par\\smallskip%\n")

tex String := texLiteral

tex COMMENT := x -> newline | concatenate apply(lines concatenate x,line -> "% " | line | newline)
tex IMG     :=
tex LITERAL := net

tex BR    := x -> "\n\\hfill\\break\n"
tex HR    := x -> "\n\\hfill\\break\\hrulefill\n"
tex PARA  := x -> concatenate("\n\\par", apply(x, tex))
tex UL    := x -> concatenate(
    ///\begin{itemize}///, newline,
    apply(x, x -> ( ///\item ///, tex x, newline)),
    ///\end{itemize}///, newline)

tex     TT := x -> concatenate ( VERBATIM, texExtraLiteral concatenate x, ENDVERBATIM )
texMath TT := x -> concatenate apply(x, texMath) -- can't use \begingroup and \parindent in math mode (at least not in mathjax)

tex     TABLE := x -> concatenate applyTable(x,tex)
texMath TABLE := x -> concatenate (
    "\n\\begin{matrix}\n",
    apply(x, row -> (apply(row,item -> (texMath item, "&")), "\\cr\n")),
    "\\end{matrix}\n")

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

maximumCodeWidth := 60 -- see also booktex.m2, an old file that sets the same variable

tex  PRE :=
tex CODE := x -> concatenate ( VERBATIM, "\n\\penalty-200\n", HALFLINE,
     shorten lines concatenate x
     / (line ->
	  if #line <= maximumCodeWidth then line
	  else concatenate(substring(0,maximumCodeWidth,line), " ..."))
     / texExtraLiteral
     / (line -> if line === "" then ///\penalty-170/// else line)
     / (line -> (line, ENDLINE)),
     ENDVERBATIM, HALFLINE, "\\penalty-200\\par{}\n")

texMath STRONG := tex STRONG := x -> concatenate("{\\bf ",apply(noopts x,tex),"}")
texMath ITALIC := tex ITALIC := x -> concatenate("{\\sl ",apply(noopts x,tex),"}")
texMath TEX := tex TEX := x -> concatenate toList x

texMath SUP := x -> concatenate( "^{", apply(noopts x, tex), "}" )
texMath SUB := x -> concatenate( "_{", apply(noopts x, tex), "}" )

texMath STYLE :=
tex     STYLE := x -> ""

-- this is wrong now
-- tex ANCHOR := x -> (
--      concatenate(
-- 	  ///\special{html:<a id="///, texLiteral x#0, ///">}///,
-- 	  tex x#-1,
-- 	  ///\special{html:</a>}///
-- 	  )
--      )

tex TO   := x -> tex TT format x#0
tex TO2  := x -> ( tag := x#0; text := x#1; tex TT text )
tex HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral toURL first x, "\">}", tex last x, "\\special{html:</a>}")

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
