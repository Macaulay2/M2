-- -*- coding: utf-8 -*-
--		Copyright 1993-2008 by Daniel R. Grayson

newPackage("Text",
     Keywords => {"Miscellaneous"},
     Headline => "documentation and hypertext"
     )

exportFrom_Core {
     "ANCHOR", "BLOCKQUOTE", "BODY", "BOLD", "BR", "CDATA", "CODE", "COMMENT", "DD", "DIV", "DL", "DT", "EM", "ExampleItem", "HEAD", "HEADER1",
     "HEADER2", "HEADER3", "HEADER4", "HEADER5", "HEADER6", "HR", "HREF", "HTML", "Hypertext", "HypertextContainer", "HypertextParagraph", "IMG", "ITALIC",
     "LABEL", "LATER", "LI", "LINK", "LITERAL", "MENU", "META", "PARA", "PRE", "SMALL", "SPAN", "STRONG", "STYLE", "SUB", "SUBSECTION", "SUP", "TABLE", "TD", "TH",
     "TEX", "TITLE", "TO", "TO2", "TOH", "TR", "TT", "UL", "OL", "validate",
     "MarkUpType", "IntermediateMarkUpType",
     "style"
     }

beginDocumentation()

document {
     Key => Text,
     Headline => "documentation and hypertext",
     PARA {
	  "This package is a repository for functions related to typesetting the documentation using $\\TeX$ and ",
	  TO "Macaulay2Doc :: hypertext list format", ", usable with either ", TO document, " or ", TO "SimpleDoc :: doc", ". ",
	  "In particular, see ", TO (html, TEX), ", ", TO (validate, Hypertext), ", and ", TO (show, Hypertext), "."
	  }
     }

-- hypertext.m2 documentation
-- Bart Snapp edited nearly all of these entries.
document {
     Key => Hypertext,
     Headline => "the class of markup lists used with hypertext",
     PARA "Intended for internal use only.",
     Subnodes => {TO (show, Hypertext), TO (style, Hypertext), TO validate} | TO \ select(sort currentPackage#"exported symbols", s -> parent value s === Hypertext)
     }
document {
     Key => HypertextContainer,
     Headline => "the class of markup lists that can contain paragraphs",
     PARA {
	 "Mark-up lists of this type get special handling when converted to nets by ",
	 TO "net", ", because the paragraphs must be collected and wrapped." },
     Subnodes => TO \ select(sort currentPackage#"exported symbols", s -> parent value s === HypertextContainer)
     }
document {
     Key => HypertextParagraph,
     Headline => "the class of markup lists that constitute separate paragraphs",
     PARA "Intended for internal use only.",
     Subnodes => TO \ select(sort currentPackage#"exported symbols", s -> parent value s === HypertextParagraph)
     }
document {
     Key => MarkUpType,
     Headline => "the class of markup types used with hypertext",
     "Some markup types allow options (attributes) to be inserted in their html tags.",
     EXAMPLE {
	 ///DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT "resolution", ":"},
	     "There are many ways to use ", TO "resolution", "."
	     )///,
	 "html oo"
	 },
     Subnodes => {TO (options, MarkUpType)} | TO \ select(sort currentPackage#"exported symbols", s -> parent value s === MarkUpType)
     }
document {
     Key => IntermediateMarkUpType,
     Headline => "the class of intermediate markup types",
     "An intermediate markup type is one that needs further processing to put it into final form.
     A good example of one is ", TO TOH, ", which represents a link to a documentation node,
     together with the headline of that node, which may not have been created yet at the time
     the ", TO TOH, " link is encountered.  Another example is ", TO HREF, ", which creates a
     link using the HTML ", TT "<a>", " element: when the link is created, the relative path to
     the target page depends on the path to the page incorporating the link.",
     Subnodes => TO \ select(sort currentPackage#"exported symbols", s -> parent value s === IntermediateMarkUpType)
     }

document {
     Key => DIV,
     Headline => "a hypertext division",
     Usage => "DIV x",
     "This corresponds directly to an HTML DIV element.  It is sort of a general-purpose container for top-level blocks, such as paragraphs, lists, and tables."
     }

document {
     Key => META,
     Headline => "a hypertext META element",
     Usage => "META x",
     "This corresponds directly to an html META element.",
     EXAMPLE ///html META { "name" => "description",  "content" => "Dan Grayson's home page." }///
     }

document {
     Key => LINK,
     Headline => "a hypertext LINK element",
     Usage => "LINK x",
     "This corresponds directly to an html LINK element.",
     EXAMPLE ///html LINK { "title" => "Macaulay2", "rel" => "Top", "href" => "index.html" }///
     }

document {
     Key => LATER,
     Headline => "a hypertext element for lazy evaluation",
     Usage => "LATER {f}",
     Inputs => { "f" => Function },
     Outputs => { LATER },
     "When the resulting hypertext element is processed later for conversion to html or for printing, the function f will be evaluated (with no
     arguments) and the hypertext it returns will be formatted.",
     EXAMPLE lines ///
     	  f = () -> DIV { "hi there" }
	  LATER {f}
	  peek oo
	  ///
     }

document {
     Key => BR,
     Headline => "line break",
     Usage => "BR{}",
     }

document {
     Key => PRE,
     Headline => "preformatted text",
	Usage => "PRE x",
	Inputs => {"x" => {}},
	Outputs => {PRE => {}},
     TT "PRE x", " encloses ", TT "x", " in a hypertext PRE item.",
     PARA{},
     "The argument ", TT "x", " should be a string, possibly containing newlines.",
     "Here is an example.",
     PRE "\n   1234   2345    4567    5678\n     34    345    3455       7\n",
     "If one wishes to use quotation marks in the preformatted text, then ", TO "Macaulay2Doc :: ///", " should be used instead of quotation marks as delimiters.",
     SeeAlso => "hypertext"
     }

document {
     Key => TITLE,
     Headline => "hypertext title",
	Usage => "TITLE x",
	Inputs => {"x" => {}},
	Outputs => {TITLE => {}},
     TT "TITLE x", " encloses ", TT "x", " in a hypertext TITLE item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => HEAD,
     Headline => "hypertext HEAD item",
	Usage => "HEAD x",
	Inputs => {"x" => {}},
	Outputs => {HEAD => {}},
     TT "HEAD x", " encloses ", TT "x", " in a hypertext HEAD item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => BODY,
     Headline => "hypertext BODY item",
	Usage => "BODY x",
	Inputs => {"x" => {}},
	Outputs => {BODY => {}},
     TT "BODY x", " encloses ", TT "x", " in a hypertext BODY item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => IMG,
     Headline => "hypertext IMG item",
	Usage => "IMG{x,y}",
	Inputs => {"x" => {}, "y" => {}},
	Outputs => {IMG => {}},
     TT ///IMG{"src" => x, "alt" => y}///, " creates a hypertext IMG item.",
     PARA{},
     "The argument ", TT "x", " should be a string containing the URL of the image, and
     ", TT "y", " should be a suitable string for the ALT attribute.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => SPAN,
     Headline => "hypertext span",
     Usage => "SPAN x",
     Inputs => {"x" => {}},
     Outputs => {SPAN => {}},
     TT "SPAN x", " encloses ", TT "x", " in a hypertext SPAN item.",
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  The result is that the entries in the
     list or sequence are displayed sequentially.",
     SeeAlso => "hypertext"
     }

document {
     Key => HTML,
     Headline => "hypertext item",
     Usage => "HTML x",
     Inputs => {"x"},
     Outputs => {HTML => { TT "x", " enclosed in an HTML item" }},
     EXAMPLE lines ///
	  HTML { HEAD { TITLE "foo" }, BODY { "Hi there" } }
	  html oo
     ///,
     SeeAlso => "hypertext"
     }

document {
     Key => HEADER1,
     Headline => "hypertext HEADER1 item",
	Usage => "HEADER1 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER1 => {}},
     TT "HEADER1 x", " encloses ", TT "x", " in a hypertext HEADER1 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER1 \"Interesting thing\"", " produces the following header.",
     HEADER1 "Interesting thing",
     SeeAlso => "hypertext"
     }

document {
     Key => HEADER2,
     Headline => "hypertext HEADER2 item",
	Usage => "HEADER2 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER2 => {}},
     TT "HEADER2 x", " encloses ", TT "x", " in a hypertext HEADER2 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER2 \"Interesting thing\"", " produces the following header.",
     HEADER2 "Interesting thing",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => "SUBSECTION",
     Headline => "hypertext SUBSECTION item",
	Usage => "SUBSECTION x", 
	Inputs => {"x" => {}},
	Outputs => {HEADER2 => {}},
     TT "SUBSECTION x", " encloses ", TT "x", " in a hypertext HEADER2 header item. It is often used in the Mathematical Overview section of the documentation.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "SUBSECTION \"Interesting thing\"", " produces the following header.",
     SUBSECTION "Interesting thing",
     PARA{},
     SeeAlso => {"HEADER2", "hypertext"}
     }

document {
     Key => HEADER3,
     Headline => "hypertext HEADER3 item",
	Usage => "HEADER3 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER3 => {}},
     TT "HEADER3 x", " encloses ", TT "x", " in a hypertext HEADER3 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER3 \"Interesting thing\"", " produces the following header.",
     HEADER3 "Interesting thing",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => HEADER4,
     Headline => "hypertext HEADER4 item",
	Usage => "HEADER4 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER4 => {}},
     TT "HEADER4 x", " encloses ", TT "x", " in a hypertext HEADER4 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER4 \"Interesting thing\"", " produces the following header.",
     HEADER4 "Interesting thing",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => HEADER5,
     Headline => "hypertext HEADER5 item",
	Usage => "HEADER5 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER5 =>{}},
     TT "HEADER5 x", " encloses ", TT "x", " in a hypertext HEADER5 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER5 \"Interesting thing\"", " produces the following header.",
     HEADER5 "Interesting thing",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => HEADER6,
     Headline => "hypertext HEADER6 item",
	Usage => "HEADER6 x",
	Inputs => {"x" => {}},
	Outputs => {HEADER6 => {}},
     TT "HEADER6 x", " encloses ", TT "x", " in a hypertext HEADER6 header item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.  The code ", PRE "HEADER6 \"Interesting thing\"", " produces the following header.",
     HEADER6 "Interesting thing",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => BLOCKQUOTE,
     Headline => "hypertext BLOCKQUOTE item",
	Usage => "BLOCKQUOTE x",
	Inputs => {"x" => {}},
	Outputs => {BLOCKQUOTE => {}},
     TT "BLOCKQUOTE x", " encloses ", TT "x", " in a hypertext BLOCKQUOTE item.",
     PARA{},
     "The argument ", TT "x", " should be a string.",
     PARA{},
     "Here is an example.",
     BLOCKQUOTE PARA "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  
   1234   2345    4567    5678
     34    345    3455       7
",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => TABLE,
     Headline => "hypertext table",
	Usage => "TABLE x",
	Inputs => {"x" => {}},
	Outputs => {TABLE => {}},
     TT "TABLE x", " produces a hypertext TABLE from a list of lists."
     }

document {
     Key => LITERAL,
     Headline => "hypertext literal text",
	Usage => "LITERAL x",
	Inputs => {"x" => {}},
	Outputs => {LITERAL => {}},
     TT "LITERAL x", " produces a special hypertext item which contains
     HTML text that should be left unchanged by ", TO "html", "."
     }

document {
     Key => STRONG,
     Headline => "hypertext STRONG item",
	Usage => "STRONG x",
	Inputs => {"x" => {}},
	Outputs => {STRONG => {}},
     TT "STRONG x", " encloses ", TT "x", " in a hypertext STRONG item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.",
     PARA{},
     STRONG "Here is strong text!",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => SMALL,
     Headline => "hypertext SMALL item",
	Usage => "SMALL x",
	Inputs => {"x" => {}},
	Outputs => {SMALL => {}},
     TT "SMALL x", " encloses ", TT "x", " in a hypertext SMALL item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.",
     PARA{},
     SMALL "Here is some small text.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => SUB,
     Headline => "hypertext subscript",
	Usage => "SUB x",
	Inputs => {"x" => {}},
	Outputs => {SUB => {}},
     TT "SUB x", " encloses ", TT "x", " in a hypertext ", TT "SUB", " item,
     thereby making it a subscript.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => SUP,
     Headline => "hypertext superscript",
	Usage => "SUP x",
	Inputs => {"x" =>{}},
	Outputs => {SUP => {}},
     TT "SUP x", " encloses ", TT "x", " in a hypertext ", TT "SUP", " item,
     thereby making it a superscript.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => LI,
     Headline => "hypertext list item",
     Usage => "LI x",
     Inputs => {"x"},
     Outputs => {LI},
     PARA {
	  "Entries in a ", TO "UL", " list should be of type ", TO "LI", ", but conversion to type ", TO "LI", " is done automatically for the user."
	  },
     SeeAlso => "hypertext"
     }

document {
     Key => LABEL,
     Headline => "hypertext label item",
     Usage => "LABEL x",
     Inputs => {"x"},
     Outputs => {LABEL},
     EXAMPLE lines ///
         html LABEL { "b" }
	 LABEL { "title" => "a", "b" }
     ///
     }

document { Key => MENU,
     Headline => "hypertext menu item",
     "This hyeprtext item is intended to be used at most once per page, at the end.  When converted by ", TO "info", ", it produces
     the Menu at the end of the page.",
     EXAMPLE {
	  ///DIV { PARA "Hi there.",
    MENU { "some topics", TO "topic a", TO "topic b", "more topics", TO "topic c" } }///,
	  "info oo",
	  "html ooo"
	  }
     }

document {
     Key => ITALIC,
     Headline => "hypertext italic font",
     Usage => "ITALIC x",
	Inputs => {"x" => {}},
	Outputs => {ITALIC => {}},
	TT "ITALIC x", " encloses ", TT "x", " in a hypertext ITALIC item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  The result corresponds to the
     html I command.",
     PARA{},
     "Here is an example.",
     PARA{},
     ITALIC "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => (html, TEX),
     Headline => "conversion of $\\TeX$ to html",
     Usage => "html t",
     Inputs => { "t" },
     Outputs => { {"a string containing the result of converting ", TT "t", " to html"} },
     PARA {
	 TEX "This method produces an HTML string, mainly converting several simple text formatting environments,
	 such as {\\bf bold face}, {\\it italics}, etc. Rendering mathematical characters and equations is done by ",
	 HREF{"https://katex.org/","$\\KaTeX$"}, ", a JavaScript math typesetting library for browsers. See the list of ",
	 HREF{"https://katex.org/docs/supported.html","supported functions and symbols"}, " for more information, or ",
	 HREF{"https://en.wikibooks.org/wiki/LaTeX/Mathematics","this page"}, " for an introduction to math mode in $\\LaTeX$." },
     PARA {
	 TEX "Equations in ", CODE "$..$", " or ", CODE "\\(...\\)", " appear in inline mode, such as $x^2-1$,
	 while those in ", CODE "$$..$$", " or ", CODE "\\[...\\]", " appear in display mode:",
	 TEX {"$$", texMath genericMatrix(QQ[x,y,z,w],2,2), ".$$"} },
     PARA {
	 "In addition, ", CODE "{\\bf ...}", ", ", CODE "{\\em ...}", ", ", CODE "{\\it ...}", ", ", CODE "{\\tt ...}",
	 ", and ", CODE "\\url{...}", " are converted to ", TO Hypertext, " objects:" },
     BLOCKQUOTE PARA TEX ///{\tt res(Module)} is the {\it method} for {\em making} {\bf resolutions} (see \url{https://macaulay2.com}).///,
     PARA {
	 "Here are some examples designed to illustrate various other features of this function when viewed in a browser:" },
     DIV { "style" => "display: flex",,
     TABLE flatten { "style" => "margin: 1.5em",
	 apply({
		 "\\Gamma\\Omega\\pi",
		 "\\partial\\ell\\infty",
		 "\\Re\\Im\\aleph\\beth",
		 "\\NN\\QQ\\RR\\CC\\ZZ\\PP",
		 "\\binom{n}{k}",
		 "\\sqrt[2]{\\frac{a}{b}}",
		 "\\sum\\prod\\coprod",
		 "\\bigoplus\\bigotimes",
		 "\\bigcup\\bigcap",
		 "\\bigvee\\bigwedge",
		 "\\int\\oint\\iint\\iiint",
		 "\\oint\\limits_{\\partial M}",
		 "\\lim\\limits_{x\\to0}",
		 "\\min\\limits_{x\\to\\infty}",
		 "\\det\\limits_{x\\to0}",
		 "\\Pr\\limits_{x\\in\\RR}",
		 "\\begin{pmatrix}\n a & b \\\\\n c & d\n\\end{pmatrix}",
		 "\\begin{vmatrix}\n a & b \\\\\n c & d\n\\end{vmatrix}"
		 }, s ->
	     TR {
		 TD { "style" => "padding-right:0.5em; border-right:1px black solid;", DIV PRE concatenate("$", s, "$") },
		 TD { "style" => "padding-left:0.5em;", PARA TEX concatenate("$", s, "$") }})
	 },
     TABLE flatten { "style" => "margin: 1.5em",
	 apply({
		 "mathnormal", "mathrm", "mathit", "mathbf", "mathsf", "mathtt", "mathfrak", "mathcal", "mathbb", "mathscr"
		 }, f ->
	     TR {
		 TD { "style" => "padding-right:0.5em; border-right:1px black solid;", DIV PRE concatenate("$\\", f, "{...}$") },
		 TD { "style" => "padding-left:0.5em;", PARA TEX concatenate("$\\", f, "{ABCD \\; abcd \\; 123}$") }})
	 },
     TABLE flatten { "style" => "margin: 1.5em",
	 apply({
		 "underline", "hat", "widehat", "tilde", "widetilde", "stackrel\\frown",
		 "check", "breve", "bar", "grave", "acute", "dot", "ddot", "not", "mathring",
		 "vec", "overrightarrow", "overleftarrow", "overline"}, f ->
	     TR {
		 TD { "style" => "padding-right:0.5em; border-right:1px black solid;", DIV PRE concatenate("$\\", f, "{a}$") },
		 TD { "style" => "padding-left:0.5em;", PARA TEX concatenate("$\\", f, "{a}$") }})
	 }
     },
     PARA {
	 "Lastly, new macros can be defined using script tags. For instance, inserting the following ",
	 TO LITERAL, " item in the documentation defines the structure sheaf:" },
     (
	 s := ///LITERAL ////<script type="text/javascript"> macros["\\OO"] = "\\mathcal{O}" </script>//// ///;
	 ( BLOCKQUOTE PRE s, value s )),
     PARA {
	 TEX ///The macro can be used at any point after:
	 $$ 0 \to 2\OO_{\\P^3}(-3) \to 3\OO_{\\P^3}(-2) \to \OO_{\\P^3} \to \OO_C \to 0 $$///},
     SeeAlso => {tex, texMath, (show, TEX)}
     }

document {
     Key => {TEX, (NewFromMethod, TEX, BasicList), (NewFromMethod, TEX, String)},
     Headline => "hypertext TEX item",
     Usage => "TEX x",
     Inputs => {"x" => ofClass{String, BasicList}},
     Outputs => {TEX},
     PARA {
	 "The constructor ", TT "TEX x", " returns an object which may contain one or more $\\LaTeX$
	 equations and matrices, several simple text formatting environments, as well as other ",
	 TO Hypertext, " items. It is useful to use strings delimited by ", TO "Macaulay2Doc :: ///",
	 " because in strings delimited by ", TO "Macaulay2Doc :: \"", " the backslashes often used
	 in $\\LaTeX$ must be doubled. For details on conversion to HTML see ", TO (html, TEX), "."},
     SeeAlso => {(show, TEX)},
     Subnodes => {TO (html, TEX)}
     }

document {
     Key => TT,
     Headline => "hypertext TT item",
	Usage => "TT x",
	Inputs => {"x" => {}},
	Outputs => {TT => {}}, 
     TT "TT x", " encloses ", TT "x", " in a hypertext ", TT "TT", " item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.",
     PARA{},
     TT "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => EM,
     Headline => "hypertext EM item",
	Usage => "EM x",
	Inputs => {"x" => {}},
	Outputs => {EM => {}},
     TT "EM x", " encloses ", TT "x", " in a hypertext EM item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.",
     PARA{},
     EM "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => BOLD,
     Headline => "hypertext BOLD item",
	Usage => "BOLD x",
	Inputs => {"x" => {}},
	Outputs => {BOLD => {}},
     TT "BOLD x", " encloses ", TT "x", " in a hypertext BOLD item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  This corresponds to B in html format.",
     PARA{},
     "Here is an example.",
     PARA{},
     BOLD "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => CODE,
     Headline => "hypertext CODE item",
	Usage => "CODE x",
	Inputs => {"x" => {}},
	Outputs => {CODE => {}},
     TT "CODE x", " encloses ", TT "x", " in a hypertext CODE item.",
     PARA{},
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA{},
     "Here is an example.",
     PARA{},
     CODE "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => HREF,
     Headline => "hypertext link",
	Usage => "HREF{u,p}\nHREF{u}",
	Inputs => {
		"u" => {"a url"},
		"p" => {"a phrase"}
		},
	Outputs => {HREF => {}},
     PARA {
	  TT "HREF{u,p}", " encloses the phrase ", TT "p", " in a hypertext HREF link
     	  pointing to the URL ", TT "u", ".  If ", TT "p", " is omitted, then ", TT "u", " is used as the phrase and as the URL."
	  },
     SeeAlso => "hypertext"
     }

document {
     Key => ANCHOR,
     Headline => "hypertext anchor",
     Usage => ///ANCHOR{"id"=>u,p}///,
     Inputs => {
	  "u" => {"the name of the anchor"},
	  "p" => {"some hypertext"}
	  },
     Consequences => { { "When converted to ", TO "html", ", the phrase ", TT "p", " will be enclosed in a hypertext anchor named ", TT "u", "." } },
     SeeAlso => "hypertext"
     }

document { Key => HR,
     Headline => "hypertext HR element (horizontal rule)", EXAMPLE lines ///
         DIV{"hi there", HR{}, "ho there" }
         html oo
     ///}
document { Key => TR,
     Headline => "hypertext TR element" }
document { Key => TD,
     Headline => "hypertext TD element", Subnodes => {TO TH} }
document { Key => TH,
     Headline => "hypertext TH element" }
document { Key => DL,
     Headline => "hypertext DL element" }
document { Key => DT,
     Headline => "hypertext DT element" }
document { Key => DD,
     Headline => "hypertext DD element" }
document { Key => STYLE,
     Headline => "hypertext STYLE element" }
document { Key => COMMENT,
     Headline => "hypertext COMMENT element", EXAMPLE lines ///
     html COMMENT "hi there"
///       }
document { Key => CDATA,
     Headline => "hypertext CDATA element", EXAMPLE lines ///
     html CDATA "hi there"
///       }

document {
     Key => UL,
     Headline => "hypertext UL item",
	Usage => "UL x",
	Inputs => {"x" => {}},
	Outputs => {UL => {}},
     TT "UL x", " encloses the list x as a hypertext UL itemized list.",
     PARA{},
     "The argument ", TT "x", " should be a list of strings or hypertext items.",
     PARA{},
     "Here is an example. The expression ",
     PRE "UL {\"first\",\"second\",\"third\"}",
     "produces",
     UL {"first","second","third"},
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => OL,
     Headline => "hypertext OL item",
	Usage => "OL x",
	Inputs => {"x" => {}},
	Outputs => {OL => {}},
     TT "OL x", " encloses the list x as a hypertext OL itemized list.",
     PARA{},
     "The argument ", TT "x", " should be a list of strings or hypertext items.",
     PARA{},
     "Here is an example. The expression ",
     PRE "OL {\"first\",\"second\",\"third\"}",
     "produces",
     OL {"first","second","third"},
     PARA{},
     SeeAlso => "hypertext"
     }

document {
     Key => {TO,(NewFromMethod,TO,Thing)},
     Headline => "hypertext documentation link",
	Usage => "TO x",
	Inputs => {"x" => {"a documentation key"}},
	Outputs => {TO => {}},
     TT "TO \"x\"", " produces a hypertext link to the documentation page labeled ", TT "x", ".",
     PARA{},
     "See also ", TO "hypertext", ".  The word ", ITALIC "hypertext", " in the previous
     sentence is an example of the use of ", TT "TO", ".",
     PARA{},
     "The special form ", TT "TO {\"x\", \"s\"}", " produces a hypertext link to
     the page labeled ", TT "x", ", but with the string \"s\" appended to the
     string \"x\" at the point where the reference occurs.  This form is needed
     because in some modes of output the link is indicated with a section number
     in brackets.",
     PARA{},
     "The special form ", TT "TO (f,X)", " produces a hypertext link to
     the page documenting the method used for applying the function ", TT "f", " to
     an argument of class ", TT "X", ".  For more arguments, use ", TT "TO (f,X,Y)", "
     or ", TT "TO (f,X,Y,Z)", ".",
     PARA{},
     "The special form ", TT "TO [f,X]", " produces a hypertext link to the
     page documenting the optional argument named ", TT "X", " for the 
     function ", TT "f", ".",
     PARA{},
     "If ", TT "TO x", " is an entry in a ", TO "UL", ", then it is treated
     as ", TT "TOH x", ", and headlines are added automatically.",
	SeeAlso => {Key, TO2, TOH}
     }

document {
     Key => TO2,
     Headline => "labeled hypertext documentation link",
	Usage => "TO2 {x,p}",
	Inputs => {
		"x" => {"a documentation key"},
		"p" => {"a phrase"}
		},
	Outputs => {TO2 => {}},
     TT "TO2 {x,\"p\"}", " produces a hypertext link labeled \"p\" to the documentation page labeled ", TT "x", "."
     }


document {
     Key => {TOH,(NewFromMethod,TOH,Thing)},
     Headline => "hypertext documentation link followed by a headline",
	Usage => "TOH x",
	Inputs => {"x" => {"a documentation key"}},
	Outputs => {TOH => {}},
     TT "TOH \"x\"", " produces a hypertext link to the documentation page labeled ", TT "x", " followed by the ", TO "Headline", " for ", TT "x", ".  If ", TT "TO x", " is an entry in a ", TO "UL", ", then it is treated
     as ", TT "TOH x", ", and headlines are added automatically."
 }

document {
     Key => ExampleItem,
     Headline => "a type of hypertext for holding example inputs awaiting outputs"
     }

document {
     Key => PARA,
     Headline => "hypertext paragraph container",
     Usage => "PARA x",
     Inputs => {
	  "x" => String => {", a ", TO2("Macaulay2Doc::hypertext list format", "hypertext list")}
	  },
     Outputs => { {" a ", TO "hypertext", " paragraph containing ", TT "x" }
     },
     "For an example, see ", TO "Macaulay2Doc::hypertext list format", "."
     }

document { Key => (options, MarkUpType),
     "Optional arguments of mark up types allow attributes to be added to html elements.",
     EXAMPLE lines ///
     	  DIV
     	  options DIV
	  d = DIV { "class" => "examples", "hi there" }
	  html d
	  net d
     ///
     }
document {
     Key => {(show, Hypertext), (show, TEX)},
     Usage => "show x",
     Inputs => { "x" => {ofClass{TEX,Hypertext}} },
     Consequences => {
	  { "displays x in the appropriate viewer" }
	  }
     }
document {
     Key => {validate, (validate, Hypertext)},
     Headline => "validate a hypertext object",
     Usage => "validate x",
     Inputs => { "x" => Hypertext => "a hypertext object to validate" },
     Outputs => { "x" => Hypertext => "the input hypertext object, possibly with errors fixed" },
     Consequences => { { "The hypertext is checked for validity, to ensure that the HTML code returned by ", TT "html validate x", " is valid." }},
     PARA {
	  "This function is somewhat provisional.  In particular, it is hard to check everything, because our hypertext format includes
	  some entities of class ", TO "IntermediateMarkUpType", " that don't correspond directly to HTML.  Either those will have to be
	  eliminated, or a more-final type of hypertext, convertible immediately to HTML, will have to be developed."
	  }
     }
document { Key => {(style, Hypertext),style},
    Usage => "style (x,opts)",
    Inputs => { "x" => { TO "hypertext" } },
    Consequences => { { "Returns a new Hypertext object with its style option updated." }},
    EXAMPLE lines ///
    d = DIV { "Hello" }
    d = style(d, "font-weight" => "bold")
    peek d
    ///
    }

isMissingDoc := value Core#"private dictionary"#"isMissingDoc";
isUndocumented := value Core#"private dictionary"#"isUndocumented";
scan({peek', show, validate, html, net, info, tex, texMath, mathML, NewFromMethod, toString, toExternalString, symbol?}, m ->
    undocumented select(toList \\ makeDocumentTag \ methods m, x ->
	    any(x.Key, s -> toString package s === "Text") and (isMissingDoc x or isUndocumented x)))

undocumented {
    (examples, Hypertext),
    (htmlWithTex, Hypertext),
    (hypertext, Hypertext)
    }
