--		Copyright 1993-2004 by Daniel R. Grayson
-----------------------------------------------------------------------------
-- tex and texMath output
-- See https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
-- TODO: eye-friendly block indentation
-----------------------------------------------------------------------------

needs "format.m2"
needs "html.m2"

newpara := "\n\\par "
maximumCodeWidth := 60 -- see also booktex.m2, an old file that sets the same variable

-----------------------------------------------------------------------------
-- Setup default rendering
-----------------------------------------------------------------------------

-- Rendering by concatenation of rendered inputs
scan({tex, texMath}, parser -> setupRenderer(parser, concatenate, Hypertext))

-----------------------------------------------------------------------------
-- Local utilities
-----------------------------------------------------------------------------

shorten := s -> (
     while #s > 0 and s#-1 == "" do s = drop(s,-1);
     while #s > 0 and s#0 == "" do s = drop(s,1);
     s)

-- TODO: remove as duplicate
noopts := x -> select(x,e -> class e =!= Option)

texLiteralTable := new MutableHashTable
scan(0 .. 255, c -> texLiteralTable#(ascii{c}) = concatenate(///{\char ///, toString c, "}"))
scan(characters ascii(32 .. 126), c -> texLiteralTable#c = c)
scan(characters "\\{}$&#^_%~|<>\"", c -> texLiteralTable#c = concatenate("{\\char ", toString (ascii c)#0, "}"))
texLiteralTable#"\n" = "\n"
texLiteralTable#"\r" = "\r"
texLiteralTable#"\t" = "\t"
texLiteralTable#"`"  = "{`}" -- break ligatures ?` and !` in font \tt. See page 381 of TeX Book.
texLiteral = s -> concatenate apply(characters s, c -> texLiteralTable#c)

HALFLINE    := "\\vskip 4.75pt\n"
ENDLINE     := "\\leavevmode\\hss\\endgraf\n"
VERBATIM    := "\\begingroup\\tt "
ENDVERBATIM := "\\endgroup{}"

texExtraLiteralTable := copy texLiteralTable
texExtraLiteralTable#" " = "\\ "
texExtraLiteral := s -> demark(ENDLINE, apply(lines s, l -> apply(characters l, c -> texExtraLiteralTable#c)))

--------------------------------------------
-- this loop depends on the feature of hash tables that when the keys
-- are consecutive integers starting at 0, the keys are scanned
-- in the natural order, which in turn depends on the hash number of
-- a small integer being the integer itself
levelLimit := 10;
sectionType = sectionNumber -> (
    level := # select(characters sectionNumber, i -> i === ".");
    if level > levelLimit then level = levelLimit;
    if level === 0 then "\\part" else
    if level === 1 then "\\chapter" else
    if level === 2 then "\\section" else
    if level === 3 then "\\subsection" else
    if level === 4 then "\\subsubsection" else
    if level === 5 then "\\paragraph" else
    if level === 6 then "\\subparagraph" else
    if level === 7 then "\\subsubparagraph" else
    if level === 8 then "\\subsubsubparagraph" else
    if level === 9 then "\\subsubsubsubparagraph" else
    "\\subsubsubsubsubparagraph");

-----------------------------------------------------------------------------

bbLetters := set characters "kABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- TODO: expand and document this behavior
suffixes := {"bar","tilde","hat","vec","dot","ddot","check","acute","grave","breve"};
suffixesRegExp := "\\w("|demark("|",suffixes)|")$";
texVariable := x -> (
    if x === "" then return "";
    xx := separate("\\$", x); if #xx > 1 then return demark("{\\char36}", texVariable \ xx); -- avoid the use of "$" in tex output
    if #x === 2 and x#0 === x#1 and bbLetters#?(x#0) then return "{\\mathbb " | x#0 | "}";
    if last x === "'" then return texVariable substring(x, 0, #x-1) | "'";
    if (r := regex(suffixesRegExp, x)) =!= null then return (
	r = r#1; "\\" | substring(r, x) | "{" | texVariable substring(x, 0, r#0) | "}");
    if #x === 1 or regex("[^[:alnum:]]", x) =!= null then x else "\\textit{" | x | "}")
texMath Symbol := x -> if keywordTexMath#?x then keywordTexMath#x else texVariable toString x

-----------------------------------------------------------------------------

tex     Nothing :=
texMath Nothing := x -> ""

tex     Thing := x -> concatenate("$", texMath x, "$")
texMath Thing := x -> texMath net x -- if we're desperate (in particular, for raw objects)

tex     String := texLiteral
texMath String := s -> "\\texttt{" | texLiteral s | "}"

texMath Net := n -> concatenate(
    "\\begin{array}{l}", demark("\\\\\n", apply(unstack n, texMath)), "\\end{array}")

texMath VerticalList := s -> concatenate(
    "\\left\\{\\begin{aligned}", demark("\\\\", apply(toList s, x -> "&" | texMath x)), "\\end{aligned}\\right\\}")

texMath NumberedVerticalList := s -> concatenate(
    "\\left\\{\\begin{aligned}", demark("\\\\", apply(#s, i -> i | ".\\quad&" | texMath s#i)), "\\end{aligned}\\right\\}")

texMathVisibleList := (op, L, delim, cl) -> concatenate("\\left", op, demark_delim apply(toList L, texMath), "\\right", cl)
texMath AngleBarList := L -> texMathVisibleList("<", L, ",\\,", ">")
texMath Array        := L -> texMathVisibleList("[", L, ",\\,", "]")
texMath Sequence     := L -> texMathVisibleList("(", L, ",\\,", ")")
texMath VisibleList  := L -> texMathVisibleList("\\{", L, ",\\,", "\\}")
texMath BasicList    := L -> concatenate(texMath class L, texMathVisibleList("\\{", L, ",\\,", "\\}"))
texMath MutableList  := L -> concatenate(texMath class L, "\\left\\{", if #L > 0 then "\\ldots "|#L|"\\ldots", "\\right\\}")

texMath HashTable := H -> if H.?texMath then H.texMath else (
    if hasAttribute(H, ReverseDictionary) then texMath toString getAttribute(H, ReverseDictionary) else
    if mutable H then      (lookup(texMath, MutableList)) H
    else texMath class H | (lookup(texMath, List)) apply(sortByName pairs H, (k, v) -> k => v))

texMath Function := f -> texMath toString f

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
    newpara, "\\medskip\\noindent\\begingroup\\Large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")
tex HEADER2 := x -> concatenate (
    newpara, "\\medskip\\noindent\\begingroup\\Large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")
tex HEADER3 := x -> concatenate (
    newpara, "\\medskip\\noindent\\begingroup\\large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")
tex HEADER4 := x -> concatenate (
    newpara, "\\medskip\\noindent\\begingroup\\large\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")
tex HEADER5 := x -> concatenate (
    newpara, "\\medskip\\noindent\\begingroup\\normal\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")
tex HEADER6 := x -> concatenate (
    newpara, "\\medskip\\noindent\\begingroup\\normal\\bf\n",
    apply(toList noopts x, tex), "\\endgroup", newpara, "\\smallskip%\n")

tex COMMENT := x -> newline | concatenate apply(lines concatenate x,line -> "% " | line | newline)
tex IMG     :=
tex LITERAL := net

tex BR    := x -> "\n\\hfill\\break\n"
tex HR    := x -> "\n\\hfill\\break\\hrulefill\n"
tex PARA  := x -> concatenate(newpara, apply(x, tex))

-- Lists
items := x -> apply(x, x -> ("\\item ", tex x, newline))
tex OL := x -> concatenate("\\begin{enumerate}", newline, items x, "\\end{enumerate}", newline)
tex UL := x -> concatenate("\\begin{itemize}",   newline, items x, "\\end{itemize}",   newline)

-- Description lists
tex DL := x -> concatenate(apply(noopts x, tex))
tex DT := x -> concatenate(newpara, apply(x, tex))
tex DD := x -> concatenate(newpara, apply(x, tex))

tex     TT := x -> concatenate ( VERBATIM, texExtraLiteral concatenate x, ENDVERBATIM )
texMath TT := x -> concatenate apply(x, texMath) -- can't use \begingroup and \parindent in math mode (at least not in mathjax)

tex     TABLE := x -> concatenate applyTable(noopts x, tex)

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
texMath TEX := tex TEX := x -> concatenate apply(x, y -> if instance(y, String) then y else tex y)

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

-- (tex, TOH) defined in format.m2
tex TO   := x -> tex TT format x#0
tex TO2  := x -> ( tag := x#0; text := x#1; tex TT text )
tex HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral toURL first x, "\">}", tex last x, "\\special{html:</a>}")

tex MENU := x -> tex drop(redoMENU x, 1)

-----------------------------------------------------------------------------
-- Viewing TeX
-----------------------------------------------------------------------------

-- TODO: incorporate this with packages/Style/M2book.tex.in
TeXclass := "\\documentclass{article}"
TeXpackages := {"amsmath", "amssymb"}
TeXtemplate := src -> concatenate( TeXclass,                newline,
    apply(TeXpackages, pkg -> "\\usepackage{" | pkg | "}"), newline,
    "\\begin{document}", newline, src, newline, "\\end{document}" )

showTex =
show TEX := x -> (
    fn := "show";
    makeDirectory(dir := temporaryFileName() | "/");
    dir | fn | ".tex" << TeXtemplate tex x << close;
    if 0 =!= chkrun concatenate("set -x ; cd ", dir, "; pdflatex -interaction=batchmode " , fn)
    then error("pdflatex failed on input file ", dir, fn, ".tex");
    show new URL from concatenate(rootURI, dir, fn, ".pdf"))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
