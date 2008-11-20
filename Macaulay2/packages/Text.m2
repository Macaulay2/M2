-- -*- coding: utf-8 -*-
--		Copyright 1993-2008 by Daniel R. Grayson

newPackage "Text"
export \\ (s -> currentPackage#"private dictionary"#s = Core#"private dictionary"#s) \ {
     "ANCHOR", "BLOCKQUOTE", "BODY", "BOLD", "BR", "CDATA", "CODE", "COMMENT", "DD", "DIV", "DIV1", "DL", "DT", "EM", "ExampleItem", "HEAD", "HEADER1",
     "HEADER2", "HEADER3", "HEADER4", "HEADER5", "HEADER6", "HR", "HREF", "HTML", "Hypertext", "HypertextContainer", "HypertextParagraph", "IMG", "ITALIC",
     "LABEL", "LATER", "LI", "LINK", "LITERAL", "MENU", "META", "PARA", "PRE", "SMALL", "SPAN", "STRONG", "STYLE", "SUB", "SUBSECTION", "SUP", "TABLE", "TD",
     "TEX", "TITLE", "TO", "TO2", "TOH", "TR", "TT", "UL",
     "MarkUpType", "MarkUpTypeWithOptions", "IntermediateMarkUpType" }

-- html.m2 documentation
-- Bart Snapp edited nearly all of these entries.
document {
     Key => HypertextContainer,
     Headline => "the class of mark-up lists that can contain paragraphs",
     PARA {
	  "Mark-up lists of this time get special handling when converted to nets by ", TO "net", ", because
	  the paragraphs must be collected and wrapped."
	  }
     }
document {
     Key => Hypertext, 
     Headline => "the class of mark-up lists used with hypertext",
     PARA{},
     "Intended for internal use only."
     }
document {
     Key => HypertextParagraph,
     Headline => "the class of mark-up lists that constitute separate paragraphs",
     PARA "Intended for internal use only."
     }
document {
     Key => {MarkUpType,
	  (symbol SPACE, MarkUpType, String),
	  (symbol SPACE, MarkUpType, Net),
	  (symbol SPACE, MarkUpType, Hypertext)},
     Headline => "the class of mark-up types used with hypertext", 
     PARA "Intended for internal use only."
     }
document { Key => MarkUpTypeWithOptions,
     Headline => "the class of mark-up types used with hypertext, with option handling",
     "Some mark-up types allow options (attributes) to be inserted in their html tags.",
     EXAMPLE {
	  ///DIV ( "class" => "waystouse", SUBSECTION {"Ways to use ", TT "resolution", ":"},
    "There are many ways to use ", TO "resolution", "."
    )///,
     	  "html oo"
         }
    }

--document {
--     Key => PARA,
--     Headline => "paragraph separator",
--	Usage => "PARA x",
--     TT "PARA x", " makes a ", TO "hypertext", " double-spaced paragraph break."
--     }

document {
     Key => DIV1,
     Headline => "a single-spaced paragraph separator",
	Usage => "DIV1 x",
     TT "DIV1 x", " makes a ", TO "hypertext", " single-spaced paragraph break. This is mostly for the documentation formated in info mode.",
	SeeAlso => "PARA"
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
     PRE "
   1234   2345    4567    5678
     34    345    3455       7
",
	"If one wishes to use quotation marks in the preformatted text, then ", TO "///", " should be used instead of quotation marks as delimiters.",
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
     Key => (html,TEX),
     Headline => "conversion of TeX to html",
     Usage => "html t",
     Inputs => { "t" },
     Outputs => { {"a string containing the result of converting ", TT "t", " to html"} },
     "This method handles conversion to html, but only for a limited subset of TeX,
     but nevertheless, it can be useful in documentation nodes.",
     EXAMPLE {
	  "TEX ///A formula: $a \\times \\ {b\\over c^3}$///",
	  ///html oo///
	  }
     }

document {
     Key => TEX,
     Headline => "hypertext TEX item",
     Usage => "TEX x",
     Inputs => {"x" => {}},
     Outputs => {TEX => {}},
     TT "TEX s", " includes the string ", TT "s", ", presumably
     containing TeX commands, in the TeX version of the documentation
     containing this ", TO "hypertext", " item."
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
	Usage => "HREF{u,p}",
	Inputs => {
		"u" => {"a url"},
		"p" => {"a phrase"}
		},
	Outputs => {HREF => {}},
     TT "HREF{u,p}", " encloses the phrase ", TT "p", " in a hypertext HREF link
     pointing to the url ", TT "u", ".",
     PARA{},
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
     Headline => "hypertext TD element" }
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

document { Key => IntermediateMarkUpType,
     Headline => "the class of intermediate mark-up types",
     "An intermediate mark-up type is one that needs further processing to put it into final form.  A good example of one is ", TO "TOH", ", which
     represents a link to a documentation node, together with the headline of that node, which may not have been created yet at the time
     the ", TT "TOH", " link is encountered.  Another good example is ", TO "HREF", ", which creates a link using the HTML ", TT "A", " element:
     when the link is created, the relative path to the target page depends on the path to the page incorporating the link!"
     }

document {
     Key => PARA,
     Headline => "hypertext paragraph container",
     Usage => "PARA x",
     Inputs => {
	  "x" => String => {", a ", TO2("hypertext list format", "hypertext list")}
	  },
     Outputs => { {" a ", TO "hypertext", " paragraph containing ", TT "x" }
     },
     "For an example, see ", TO "hypertext list format", "."
     }