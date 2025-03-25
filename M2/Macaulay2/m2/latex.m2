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
noopts := x -> select(x,e -> class e =!= Option and class e =!= OptionTable)

texLiteralEncode := c -> concatenate apply(ascii c,i->("\\char",toString i))
texLiteralPairs := splice {
    apply(0..8|11..12|14..31|127..255|toSequence ascii "\\{}$&#^_%~|<>\"", c -> ascii c => texLiteralEncode ascii c),
    "`"  => "{`}" -- break ligatures ?` and !` in font \tt. See page 381 of TeX Book.
    }
texLiteralTable := hashTable texLiteralPairs
texLiteral1 := t -> s -> (
    flag:=false;
    concatenate apply(characters s,
	c -> if t#?c then (
	    s:=t#c;
	    flag=first s==="\\" and last s =!="}";
	    s
	    )
	else if flag then (flag=false; " "|c) else c
	) | if flag then " " else ""
    )
texLiteral := texLiteral1 texLiteralTable

HALFLINE    := "\\vskip 4.75pt\n"
ENDLINE     := "\\leavevmode\\hss\\endgraf\n"
VERBATIM    := "\\begingroup\\tt "
ENDVERBATIM := "\\endgroup{}"

texExtraLiteralTable := hashTable append(texLiteralPairs," " => "\\ ")
texExtraLiteral := s -> demark(ENDLINE, apply(lines s, l -> apply(l, c -> if texExtraLiteralTable#?c then texExtraLiteralTable#c else c)))

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
keywordTexMath = applyKeys(hashTable { -- both unary and binary keywords
	symbol |-   => "\\vdash ",
	symbol ..   => "\\,{.}{.}\\,",
	symbol ..<  => "\\,{.}{.}{<}\\,",
	symbol <=   => "\\le ",
	symbol >=   => "\\ge ",
	symbol =>   => "\\Rightarrow ",
	symbol ==>  => "\\Longrightarrow ",
	symbol <==  => "\\Longleftarrow ",
	symbol <==> => "\\Longleftrightarrow ",
	symbol _>   => "{}_>",
	symbol _<   => "{}_<",
	symbol ^>   => "{}^>",
	symbol ^<   => "{}^<",
	symbol **   => "\\otimes ",
	symbol ++   => "\\oplus ",
	symbol !=   => "\\ne ",
	symbol ->   => "\\rightarrow ",
	symbol <-   => "\\leftarrow ",
	symbol ===> => "{\\large\\Longrightarrow}",
	symbol <=== => "{\\large\\Longleftarrow}",
	symbol <<   => "\\ll ",
	symbol >>   => "\\gg ",
	symbol ^!   => "{}^!",
	symbol _!   => "{}_!",
	symbol  ~   => "\\sim ",
	symbol ^~   => "{}^\\sim",
	symbol _~   => "{}_\\sim",
	--symbol ^# => "{}^\\sharp",
	--symbol _# => "{}_\\sharp",
	symbol _   => "\\_",
	symbol {   => "\\{",
	symbol }   => "\\}",
	symbol \   => "\\backslash ",
	symbol \\  => "\\backslash\\backslash ",
	symbol #   => "\\#",
	symbol #?  => "\\#?",
	symbol %   => "\\%",
	symbol &   => "\\&",
	symbol ^   => "\\wedge ",
	symbol ^^  => "\\wedge\\wedge ",
	symbol <|  => "\\langle ",
	symbol |>  => "\\rangle ",
	symbol |   => "\\mid",
	symbol ||  => "\\mid\\mid",
	symbol ^** => "{}^{\\otimes}", -- temporary solution to KaTeX issue https://github.com/KaTeX/KaTeX/issues/3576
	symbol _*  => "{}_*", -- temporary solution to KaTeX issue https://github.com/KaTeX/KaTeX/issues/3576
	symbol ^*  => "{}^*", -- temporary solution to KaTeX issue https://github.com/KaTeX/KaTeX/issues/3576
	symbol ·   => "\\cdot",
	},symbolBody)

bbLetters := set characters "kABCDEFGHIJKLMNOPQRSTUVWXYZ"
-- greek letters below are only in math mode
texMathLiteralTable := merge(texLiteralTable,
    hashTable {
	"Α" => "\\Alpha", "Β" => "\\Beta", "Ε" => "\\Epsilon", "Ζ" => "\\Zeta", "Η" => "\\Eta", "Ι" => "\\Iota", "Κ" => "\\Kappa", "Μ" => "\\Mu", "Ν" => "\\Nu", "Ο" => "\\Omicron", "Ρ" => "\\Rho", "Τ" => "\\Tau", "Χ" => "\\Chi",
	"Γ" => "\\Gamma", "Δ" => "\\Delta", "Θ" => "\\Theta", "Λ" => "\\Lambda", "Ξ" => "\\Xi", "Π" => "\\Pi", "Σ" => "\\Sigma", "Υ" => "\\Upsilon", "Φ" => "\\Phi", "Ψ" => "\\Psi", "Ω" => "\\Omega",
	"ϱ" => "\\varrho", "ϵ" => "\\epsilon", "π" => "\\pi", "ρ" => "\\rho", "ς" => "\\varsigma", "σ" => "\\sigma", "τ" => "\\tau", "υ" => "\\upsilon", "φ" => "\\varphi", "χ" => "\\chi", "ψ" => "\\psi", "ω" => "\\omega", "ϑ" => "\\vartheta",
	"α" => "\\alpha", "β" => "\\beta", "γ" => "\\gamma", "ϕ" => "\\phi", "δ" => "\\delta", "ε" => "\\varepsilon", "ϖ" => "\\varpi", "ζ" => "\\zeta", "η" => "\\eta", "θ" => "\\theta", "ι" => "\\iota", "κ" => "\\kappa", "λ" => "\\lambda", "μ" => "\\mu", "ν" => "\\nu", "ξ" => "\\xi", "ο" => "\\omicron",
	"𝔞" => "\\mathfrak{a}","𝔟" => "\\mathfrak{b}","𝔠" => "\\mathfrak{c}","𝔡" => "\\mathfrak{d}","𝔢" => "\\mathfrak{e}","𝔣" => "\\mathfrak{f}","𝔤" => "\\mathfrak{g}","𝔥" => "\\mathfrak{h}","𝔦" => "\\mathfrak{i}","𝔧" => "\\mathfrak{j}","𝔨" => "\\mathfrak{k}",
	"𝔩" => "\\mathfrak{l}","𝔪" => "\\mathfrak{m}","𝔫" => "\\mathfrak{n}","𝔬" => "\\mathfrak{o}","𝔭" => "\\mathfrak{p}","𝔮" => "\\mathfrak{q}","𝔯" => "\\mathfrak{r}","𝔰" => "\\mathfrak{s}","𝔱" => "\\mathfrak{t}","𝔲" => "\\mathfrak{u}","𝔳" => "\\mathfrak{v}",
	"𝔴" => "\\mathfrak{w}","𝔵" => "\\mathfrak{x}","𝔶" => "\\mathfrak{y}","𝔷" => "\\mathfrak{z}","𝔄" => "\\mathfrak{A}","𝔅" => "\\mathfrak{B}","𝔆" => "\\mathfrak{C}","𝔇" => "\\mathfrak{D}","𝔈" => "\\mathfrak{E}","𝔉" => "\\mathfrak{F}","𝔊" => "\\mathfrak{G}",
	"𝔋" => "\\mathfrak{H}","𝔌" => "\\mathfrak{I}","𝔍" => "\\mathfrak{J}","𝔎" => "\\mathfrak{K}","𝔏" => "\\mathfrak{L}","𝔐" => "\\mathfrak{M}","𝔑" => "\\mathfrak{N}","𝔒" => "\\mathfrak{O}","𝔓" => "\\mathfrak{P}","𝔔" => "\\mathfrak{Q}","𝔕" => "\\mathfrak{R}",
	"𝔖" => "\\mathfrak{S}","𝔗" => "\\mathfrak{T}","𝔘" => "\\mathfrak{U}","𝔙" => "\\mathfrak{V}","𝔚" => "\\mathfrak{W}","𝔛" => "\\mathfrak{X}","𝔜" => "\\mathfrak{Y}","𝔝" => "\\mathfrak{Z}",
	"×" => "\\times", "÷" => "\\div", "±" => "\\pm",
	},last)
texMathLiteral = texLiteral1 texMathLiteralTable
-- TODO: expand and document this behavior
suffixes := {"bar","tilde","hat","vec","dot","ddot","check","acute","grave","breve"};
suffixesRegExp := "\\w("|demark("|",suffixes)|")$";
texVariable := x -> (
    if x === "" then return "";
    if #x === 2 and x#0 === x#1 and bbLetters#?(x#0) then return "{\\mathbb " | x#0 | "}";
    if last x === "'" then return texVariable substring(x, 0, #x-1) | "'";
    if (r := regex(suffixesRegExp, x)) =!= null then return (
	r = r#1; "\\" | substring(r, x) | "{" | texVariable substring(x, 0, r#0) | "}");
    if #x === 1 or regex("[^[:alnum:]]", x) =!= null then x else "\\mathit{" | x | "}")
texMathSymbol :=
texMath Symbol := texVariable @@ texMathLiteral @@ toString
texMath Keyword :=  texMath @@ symbolBody
texMath SymbolBody := s -> if keywordTexMath#?s then keywordTexMath#s else texMathSymbol s

-- add augmented operators
removeLast := s -> substring(s,0,#s-1)
keywordTexMath = merge(keywordTexMath, hashTable apply(toList augmentedAssignmentOperators, s -> symbolBody s => (texMath (getGlobalSymbol ( removeLast toString s ))) | "="), last)

-----------------------------------------------------------------------------

tex     Nothing := tex @@ toString
texMath Nothing := texMath @@ toString

tex     Thing := x -> concatenate("$", texMath x, "$")
texMath Thing := x -> texMath net x -- if we're desperate (in particular, for raw objects)

tex     String := texLiteral
texMath String := s -> "\\texttt{" | texLiteral s | "}"

tex Net := n -> concatenate(
    "\\begin{tabular}[t]{l}", demark("\\\\\n", apply(unstack n, tex)), "\\end{tabular}")
texMath Net := n -> concatenate(
    "\\begin{array}{l}", demark("\\\\\n", apply(unstack n, texMath)), "\\end{array}")

texMath VerticalList := s -> concatenate(
    "\\left\\{\\begin{aligned}", demark("\\\\", apply(toList s, x -> "&" | texMath x)), "\\end{aligned}\\right\\}")

texMath NumberedVerticalList := s -> concatenate(
    "\\left\\{\\begin{aligned}", demark("\\\\", apply(#s, i -> i | ".\\quad&" | texMath s#i)), "\\end{aligned}\\right\\}")

texMathVisibleList := (op, L, delim, cl) -> concatenate("\\left", op, if #L > 0 then demark_delim apply(toList L, texMath) else "\\,", "\\right", cl)
texMath AngleBarList := L -> texMathVisibleList("<", L, ",\\,", ">")
texMath Array        := L -> texMathVisibleList("[", L, ",\\,", "]")
texMath Sequence     := L -> texMathVisibleList("(", L, ",\\,", ")")
texMath VisibleList  := L -> texMathVisibleList("\\{", L, ",\\:", "\\}")
texMath BasicList    := L -> concatenate(texMath class L, texMathVisibleList("\\{", L, ",\\,", "\\}"))
texMathMutable :=
texMath MutableList  := L -> concatenate(texMath class L, "\\left\\{", if #L > 0 then "\\ldots "|#L|"\\ldots" else "\\,", "\\right\\}")

texMath HashTable := H -> if H.?texMath then H.texMath else (
    if hasAttribute(H, ReverseDictionary) then texMath toString getAttribute(H, ReverseDictionary)
    else if isMutable H then texMathMutable H
    else texMath class H | texMath apply(sortByName pairs H, (k, v) -> k => v))

texMath Function := f -> texMath toString f

texMath ZZ := n -> (
    s := simpleToString n;
    j := 1 - (#s-1) % 3;
    concatenate for i in s list (if j==2 then (j=0; "\\,",i) else (j += 1; i))
    )

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

tex    KBD := tex SAMP := tex VAR :=
tex     TT := x -> concatenate ( VERBATIM, texExtraLiteral concatenate x, ENDVERBATIM )

texMath KBD := texMath SAMP := texMath VAR :=
texMath TT := x -> concatenate apply(x, texMath) -- can't use \begingroup and \parindent in math mode (at least not in mathjax)

tex     TABLE := x -> concatenate applyTable(noopts x, tex)

tex  PRE :=
tex CODE := x -> concatenate ( VERBATIM, "\n\\penalty-200\n", HALFLINE,
     shorten lines concatenate apply(noopts x, y ->
	  if instance(y, Hypertext) then concatenate noopts y else y)
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
--tex HREF := x -> concatenate("\\special{html:<a href=\"", texLiteral toURL first x, "\">}", tex last x, "\\special{html:</a>}")
scan({texMath,tex}, f ->
    f HREF := x -> concatenate("\\href{", texLiteral toURL first x, "}{", f last x, "}")
    )

tex MENU := x -> tex drop(redoMENU x, 1)

-----------------------------------------------------------------------------
-- Viewing TeX
-----------------------------------------------------------------------------

-- TODO: incorporate this with packages/Style/M2book.tex.in
TeXclass := "\\documentclass{standalone}"
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
