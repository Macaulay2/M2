--		Copyright 1993-2002 by Daniel R. Grayson

-- html.m2 documentation

document {
     Key => MarkUpList, 
     Headline => "the class of mark-up lists used with hypertext",
     PARA,
     "Intended for internal use only."
     }

document {
     Key => MarkUpType,
     Headline => "the class of mark-up types used with hypertext", 
     PARA,
     "Intended for internal use only.",
     SEEALSO "EmptyMarkUpType"
     }

document {
     Key => EmptyMarkUpType,
     Headline => "the class of empty mark-up types used with hypertext"
     }

document {
     Key => PARA,
     Headline => "paragraph separator",
     TT "PARA x", " -- a ", TO "hypertext", " paragraph.",
     }

document {
     Key => BR,
     Headline => "line break",
     TT "BR{}", " -- a ", TO "hypertext", " line break."
     }

document {
     Key => HR,
     Headline => "horizontal rule",
     TT "HR{}", " -- a ", TO "hypertext", " horiziontal rule."
     }

document {
     Key => html,
     Headline => "convert hypertext to html format",
     TT "html x", " -- convert ", TO "hypertext", " to html format",
     PARA,
     "The return value is a string which is suitable for use in an
     html file, readable by a world wide web client such as netscape.",
     SEEALSO "mathML"
     }

document {
     Key => PRE,
     Headline => "preformatted text",
     TT "PRE x", " -- encloses ", TT "x", " in a hypertext PRE item.",
     PARA,
     "The argument ", TT "x", " should be a string, possibly containing newlines.",
     PARA,
     "Here is an example.",
     PRE "
   1234   2345    4567    5678
     34    345    3455       7
",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => TITLE,
     Headline => "hypertext title",
     TT "TITLE x", " -- encloses ", TT "x", " in a hypertext TITLE item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEAD,
     Headline => "HTML HEAD item",
     TT "HEAD x", " -- encloses ", TT "x", " in a hypertext HEAD item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => BODY,
     Headline => "HTML BODY item",
     TT "BODY x", " -- encloses ", TT "x", " in a hypertext BODY item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => IMG,
     Headline => "HTML IMG item",
     TT "IMG x", " -- encloses ", TT "x", " in a hypertext IMG item.",
     PARA,
     "The argument ", TT "x", " should be a string containing the URL of the image.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => SEQ,
     Headline => "hypertext sequence",
     TT "SEQ x", " -- encloses ", TT "x", " in a hypertext SEQ item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  The result is that the entries in the
     list or sequence are displayed sequentially.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HTML,
     Headline => "HTML item",
     TT "HTML x", " -- encloses ", TT "x", " in a hypertext HTML item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER1,
     Headline => "HTML HEADER1 item",
     TT "HEADER1 x", " -- encloses ", TT "x", " in a hypertext HEADER1 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER1 \"Interesting thing\"",
     " produces ",
     HEADER1 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER2,
     Headline => "HTML HEADER2 item",
     TT "HEADER2 x", " -- encloses ", TT "x", " in a hypertext HEADER2 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER2 \"Interesting thing\"",
     " produces ",
     HEADER2 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER3,
     Headline => "HTML HEADER3 item",
     TT "HEADER3 x", " -- encloses ", TT "x", " in a hypertext HEADER3 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER3 \"Interesting thing\"",
     " produces ",
     HEADER3 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER4,
     Headline => "HTML HEADER4 item",
     TT "HEADER4 x", " -- encloses ", TT "x", " in a hypertext HEADER4 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER4 \"Interesting thing\"",
     " produces ",
     HEADER4 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER5,
     Headline => "HTML HEADER5 item",
     TT "HEADER5 x", " -- encloses ", TT "x", " in a hypertext HEADER5 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER5 \"Interesting thing\"",
     " produces ",
     HEADER5 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HEADER6,
     Headline => "HTML HEADER6 item",
     TT "HEADER6 x", " -- encloses ", TT "x", " in a hypertext HEADER6 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "HEADER6 \"Interesting thing\"",
     " produces ",
     HEADER6 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => LISTING,
     Headline => "HTML LISTING item",
     TT "LISTING x", " -- encloses ", TT "x", " in a hypertext LISTING item.",
     PARA,
     "The argument ", TT "x", " should be a string.",
     PARA,
     "Here is an example.",
     LISTING "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  
   1234   2345    4567    5678
     34    345    3455       7
",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => XMP,
     Headline => "HTML XMP item",
     TT "XMP x", " -- encloses ", TT "x", " in a hypertext XMP item.",
     PARA,
     "The argument ", TT "x", " should be a string.",
     PARA,
     "Here is an example.",
     XMP "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  
   1234   2345    4567    5678
     34    345    3455       7
",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => BLOCKQUOTE,
     Headline => "HTML BLOCKQUOTE item",
     TT "BLOCKQUOTE x", " -- encloses ", TT "x", " in a hypertext BLOCKQUOTE item.",
     PARA,
     "The argument ", TT "x", " should be a string.",
     PARA,
     "Here is an example.",
     BLOCKQUOTE "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  
   1234   2345    4567    5678
     34    345    3455       7
",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => EXAMPLE,
     Headline => "HTML EXAMPLE item",
     TT "EXAMPLE x", " -- evaluates the string or list of strings
     ", TT "x", " as Macaulay 2 code, inserting the results in
     hypertext preformatted ", TO "PRE", " items.",
     PARA,
     "The evaluation is done by the Makefile at a separate time, and the
     results are left where they can be found the next time the same
     EXAMPLE is encountered.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => TABLE,
     Headline => "hypertext table",
     TT "TABLE x", " -- produces a hypertext TABLE from a list of lists."
     }

document {
     Key => LITERAL,
     Headline => "hypertext literal text",
     TT "LITERAL x", " -- produces a special hypertext item which contains
     HTML text that should be left unchanged by ", TO "html", "."
     }

document {
     Key => ExampleTABLE,
     Headline => "hypertext list of examples",
     TT "ExampleTABLE x", " -- produces a hypertext display suitable for displaying
     a list of examples."
     }

document {
     Key => SECTION,
     Headline => "hypertext section",
     OldSynopsis => {
	  ///SECTION { "title of the section", ... contents of the section ... }///
	  },
     SEEALSO { "TOC" }
     }

document {
     Key => TOC,
     Headline => "table of contents for sections",
     OldSynopsis => {
	  ///TOC {
     SECTION { "section title 1", ... },
     SECTION { "section title 2", ... },
     SECTION { "section title 3", ... }
     }///
     	  },
     SEEALSO { "SECTION" }
     }

document {
     Key => VAR,
     Headline => "HTML VAR item",
     TT "VAR x", " -- encloses ", TT "x", " in a hypertext VAR item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     VAR "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => DFN,
     Headline => "HTML DFN item",
     TT "DFN x", " -- encloses ", TT "x", " in a hypertext DFN item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     DFN "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => STRONG,
     Headline => "HTML STRONG item",
     TT "STRONG x", " -- encloses ", TT "x", " in a hypertext STRONG item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     STRONG "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => BIG,
     Headline => "HTML BIG item",
     TT "BIG x", " -- encloses ", TT "x", " in a hypertext BIG item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     BIG "Here is some big text.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => SMALL,
     Headline => "HTML SMALL item",
     TT "SMALL x", " -- encloses ", TT "x", " in a hypertext SMALL item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     SMALL "Here is some small text.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => SUB,
     Headline => "HTML subscript",
     TT "SUB x", " -- encloses ", TT "x", " in a hypertext ", TT "SUB", " item,
     thereby making it a subscript.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => SUP,
     Headline => "HTML superscript",
     TT "SUP x", " -- encloses ", TT "x", " in a hypertext ", TT "SUP", " item,
     thereby making it a superscript.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => SAMP,
     Headline => "HTML SAMP item",
     TT "SAMP x", " -- encloses ", TT "x", " in a hypertext SAMP item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     SAMP "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => KBD,
     Headline => "HTML KBD item",
     TT "KBD x", " -- encloses ", TT "x", " in a hypertext KBD item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     KBD "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => ITALIC,
     Headline => "HTML italic font",
     TT "ITALIC x", " -- encloses ", TT "x", " in a hypertext ITALIC item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  The result corresponds to the
     html I command.",
     PARA,
     "Here is an example.",
     PARA,
     ITALIC "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => UNDERLINE,
     Headline => "HTML underlining",
     TT "UNDERLINE x", " -- encloses ", TT "x", " in a hypertext UNDERLINE item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     UNDERLINE "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => TEX,
     Headline => "hypertext TEX item",
     TT "TEX s", " -- includes the string ", TT "s", ", presumably
     containing TeX commands, in the TeX version of the documentation
     containing this ", TO "hypertext", " item.",
     PARA,
     "Invisible in the text and html versions."
     }

document {
     Key => TT,
     Headline => "HTML TT item",
     TT "TT x", " -- encloses ", TT "x", " in a hypertext ", TT "TT", " item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     TT "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => EM,
     Headline => "HTML EM item",
     TT "EM x", " -- encloses ", TT "x", " in a hypertext EM item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     EM "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => CITE,
     Headline => "HTML CITE item",
     TT "CITE x", " -- encloses ", TT "x", " in a hypertext CITE item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     CITE "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => BOLD,
     Headline => "HTML BOLD item",
     TT "BOLD x", " -- encloses ", TT "x", " in a hypertext BOLD item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  This corresponds to B in html format.",
     PARA,
     "Here is an example.",
     PARA,
     BOLD "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => CODE,
     Headline => "HTML CODE item",
     TT "CODE x", " -- encloses ", TT "x", " in a hypertext CODE item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     PARA,
     CODE "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => HREF,
     Headline => "HTML link",
     TT "HREF{u,p}", " -- encloses the phrase ", TT "p", " in a hypertext HREF link
     pointing to the url ", TT "u", ".",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => ANCHOR,
     Headline => "HTML anchor",
     TT "ANCHOR{u,p}", " -- encloses the phrase ", TT "p", " in a hypertext A anchor
     named ", TT "u", ".",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => UL,
     Headline => "HTML UL item",
     TT "UL x", " -- encloses the list x as a hypertext UL itemized list.",
     PARA,
     "The argument ", TT "x", " should be a list of strings or hypertext items.",
     PARA,
     "Here is an example. The expression ",
     PRE "UL {\"first\",\"second\",\"third\"}",
     "produces",
     UL {"first","second","third"},
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => DL,
     Headline => "HTML DL item",
     TT "DL x", " -- encloses the list x as a hypertext DL itemized list.",
     PARA,
     "The argument ", TT "x", " should be a list, each member of which should be
     a list of length two.",
     PARA,
     "Here is an example. The expression ",
     PRE "DL {
     {\"first heading\",\"first item's text\"},
     {\"second heading\",\"second item's text\"},
     {\"third heading\",\"third item's text\"}}",
     "produces",
     DL {
     	  {"first heading","first item's text"},
	  {"second heading","second item's text"},
	  {"third heading","third item's text"}
	  },
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => OL,
     Headline => "HTML OL item",
     TT "OL x", " -- encloses the list x as a hypertext OL itemized list.",
     PARA,
     "The argument ", TT "x", " should be a list of strings or hypertext items.
     The items are numbered in the display.",
     PARA,
     "Here is an example. The expression ",
     PRE "OL {\"first\",\"second\",\"third\"}",
     "produces",
     OL {"first","second","third"},
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => NL,
     Headline => "HTML NL item",
     TT "NL x", " -- encloses the list x as a hypertext NL itemized list.",
     PARA,
     "The argument ", TT "x", " should be a list of strings or hypertext items.",
     PARA,
     "Here is an example. The expression ",
     PRE "NL {\"first\",\"second\",\"third\"}",
     "produces",
     NL {"first","second","third"},
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => NOINDENT,
     Headline => "cancel hypertext indentation",
     TT "NOINDENT{}", " -- specifies that no indentation should be used
     for following hypertext items.",
     PARA,
     "This item is mainly of interest for hypertext that ultimately will
     be viewed with TeX, for html viewers don't indent paragraphs.",
     PARA,
     SEEALSO "hypertext"
     }

document {
     Key => TO,
     Headline => "hypertext documentation link",
     TT "TO \"x\"", " -- produces a hypertext link to the documentation page labelled ", TT "x", ".",
     PARA,
     "See also ", TO "hypertext", ".  The word ", ITALIC "hypertext", " in the previous
     sentence is an example of the use of ", TT "TO", ".",
     PARA,
     "The special form ", TT "TO {\"x\", \"s\"}", " produces a hypertext link to
     the page labelled ", TT "x", ", but with the string \"s\" appended to the
     string \"x\" at the point where the reference occurs.  This form is needed
     because in some modes of output the link is indicated with a section number
     in brackets.",
     PARA,
     "The special form ", TT "TO (f,X)", " produces a hypertext link to
     the page documenting the method used for applying the function ", TT "f", " to
     an argument of class ", TT "X", ".  For more arguments, use ", TT "TO (f,X,Y)", "
     or ", TT "TO (f,X,Y,Z)", ".",
     PARA,
     "The special form ", TT "TO (f,X)", " produces a hypertext link to the
     page documenting the optional argument named ", TT "X", " for the 
     function ", TT "f", ".",
     PARA,
     "If ", TT "TO x", " is an entry in a ", TO "UL", ", then it is treated
     as ", TT "TOH x", ", and headlines are added automatically.  See also ", TO "TOH", "."
     }

document {
     Key => Command,
     Headline => "the class of all commands",
     OldSynopsis => {
	  "f = Command g",
	  "g" => "a function or a string",
	  "f" => {
	       "a new command that will evaluate ", TT "g()", " if ", TT "g", " is a function, and
	       will evaluate ", TT "run g", " if ", TT "g", " is a string" }
	  },
     "A command behaves as a function does if it is followed by an adjacent
     expression which can serve as its argument or argument list.  In addition,
     if it appears as the value of an expression typed by the user at top
     level (i.e., not in a file), then it gets executed with empty argument list.",
     PARA,
     EXAMPLE {
	  "(f = Command ( () -> 2^30 );)",
	  "f",
	  "(c = Command \"date\";)",
	  "c"
	  },
     SEEALSO {"run", "AfterEval"}
     }


document {
     Key => monomialCurveIdeal, 
     Headline => "make the ideal of a monomial curve",
     TT "monomialCurveIdeal(R,a)", " -- yields the defining ideal of the projective
     curve given parametrically on an affine piece by 
     t |---> (t^a1, ..., t^an).",
     PARA,
     "The ideal is defined in the polynomial ring R,
     which must have at least n+1 variables, preferably all of equal 
     degree.  The first n+1 variables in the ring are used",
     "For example, the following defines a plane quintic curve of genus 6.",
     EXAMPLE {
	  "R = ZZ/101[a..f]",
	  "monomialCurveIdeal(R,{3,5})",
	  },
     "Here is a genus 2 curve with one singular point.",
     EXAMPLE "monomialCurveIdeal(R,{3,4,5})",
     "Here is one with two singular points, genus 7.",
     EXAMPLE "monomialCurveIdeal(R,{6,7,8,9,11})",
     "Finally, here is the smooth rational quartic in P^3.",
     EXAMPLE "monomialCurveIdeal(R,{1,3,4})"
     }

TEST ///
    R := ZZ/101[a..f];
    -- plane quintic, genus=6
    I1 := monomialCurveIdeal(R,{3,5});
    assert(I1 == image matrix{{b^5-a^2*c^3}});

    -- one singular point, g=2
    I2 := monomialCurveIdeal(R,{3,4,5});
    assert(I2 == image matrix {{c^2-b*d, b^2*c-a*d^2, b^3-a*c*d}});

    -- two singular points, g=7
    I3 := monomialCurveIdeal(R,{6,7,8,9,11});
    assert(I3 == image matrix {{
               d*e-b*f, e^2-c*f, c*d-b*e, d^2-c*e, 
               c^2-b*d, b*c*e-a*f^2, b^2*d-a*e*f, b^2*c-a*d*f, b^3-a*c*f}});

    -- smooth rational quartic in P^3
    I4 := monomialCurveIdeal(R,{1,3,4});
    assert(I4 == image matrix {{b*c-a*d, c^3-b*d^2, a*c^2-b^2*d, b^3-a^2*c}});
///

document {
     Key => Fano, 
     Headline => "Fano scheme",
     TT "Fano(k,I,GR) or  Fano(k,I)", " -- computes 
     the ideal of a Fano scheme in the Grassmannian.",
     PARA,
     "Given an ideal ", TT "I", " representing a projective variety ", TT "X", "
     in ", TT "P^r", ", a positive integer k<r, and optionally a 
     ring ", TT "GR", " with (exactly) ", TT "r+1", " choose ", TT "k+1", " variables, 
     representing the ambient space of the Grassmannian of 
     k-planes in ", TT "P^r", ", this routine returns the ideal in
     ", TT "GR", " of the Fano scheme that parametrizes the k-planes 
     lying on ", TT "X", ".  If the optional third argument is not 
     present, the routine fabricates its own ring, 
     and returns an ideal in it."
     }

document {
     Key => Grassmannian,
     Headline => "find the ideal of a Grassmannian",
     TT "Grassmannian(k,r)", " -- Grassmannian of k-planes in P^r",BR,NOINDENT,
     TT "Grassmanian(k,r,R)", 
     PARA,
     "Given natural numbers k <= r, and optionally a ring ", TT "R", " with
     at least ", TT "binomial(r+1,k+1)", " variables, the routine finds the
     ideal of the Grassmannian of projective k-planes in P^r, using the
     first ", TT "binomial(r+1,k+1)", " variables of ", TT "R", ".  If ", TT "R", "
     is not given, the routine makes and uses ", TT "ZZ/31991[vars(0..binomial(r+1,k+1)-1].",
     PARA,
     "For example, the Grassmannian of projective lines in P^3:",
     EXAMPLE "J = Grassmannian(1,3)",
     EXAMPLE "R = QQ[a..f];",
     EXAMPLE "J = Grassmannian(1,3,R)",
     CAVEAT "Currently, this ideal is constructed using relations on minors
     of a generic matrix.  It should really use the Pluecker equations."
     }

document {
     Key => code,
     Headline => "display source code",
     TT "code f", " -- prints out the source code of the function ", TT "f", ".",
     BR, NOINDENT,
     TT "code(f,X)", " -- prints out the source code of the particular 
     method that would be applied if ", TT "f", " were applied to an argument of 
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "code(f,X,Y)", " -- prints out the source code of the particular 
     method that would be applied if ", TT "f", " were applied to arguments of
     classes ", TT "X", " and ", TT "Y", ".",
     BR, NOINDENT,
     TT "code(f,X,Y,Z)", " -- prints out the source code of the 
     particular method that would be applied if ", TT "f", " were applied to 
     arguments of classes ", TT "X", ", ", TT "Y", ", and ", TT "Z", ".",
     BR, NOINDENT,
     TT "code {v,w,...}", " -- prints out the source code for each
     of the items listed.",
     PARA,
     EXAMPLE "code methods use",
     SEEALSO "methods"
     }

document {
     Key => edit,
     Headline => "edit source code",
     TT "edit", " -- a command which starts the text editor",
     BR,NOINDENT,
     TT "edit f", " -- starts the text editor at the source code of the
     function ", TT "f", ".",
     BR,NOINDENT,
     TT "edit(f,X)", " -- edits the source code of the particular 
     method that would be applied if ", TT "f", " were applied to an argument of 
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "edit(f,X,Y)", " -- edits the source code of the particular 
     method that would be applied if ", TT "f", " were applied to arguments of
     classes ", TT "X", " and ", TT "Y", ".",
     BR, NOINDENT,
     TT "edit(f,X,Y,Z)", " -- edits the source code of the 
     particular method that would be applied if ", TT "f", " were applied to 
     arguments of classes ", TT "X", ", ", TT "Y", ", and ", TT "Z", ".",
     PARA,
     "The name of the user's preferred editor is take from the environment 
     variable ", TT "EDITOR", ".  If X is running and the editor is not
     emacs, then the editor is started in a new ", TT "xterm", " window.",
     PARA,
     "For an interactive example, try ", TT "edit(dim,Module)", ".",
     PARA,
     "The value returned is the exit code returned by the editor, as with
     ", TO "run", ", usually zero."
     }

document {
     Key => methods,
     Headline => "list methods",
     TT "methods F", " -- produces a list of those methods associated with the
     function or type F.",
     BR, NOINDENT,
     TT "methods symbol **", " -- produces a list of the methods 
     usable with the operator ", TT "**", ".",
     BR, NOINDENT,
     TT "methods (symbol **, X)", " -- produces a list of the 
     methods usable with the operator ", TT "**", " and a thing of
     class ", TT "X", ".",
     BR, NOINDENT,
     TT "methods (X, Y)", " -- produces a list of the 
     methods usable with a thing of class ", TT "X", " and a thing of class
     ", TT "Y", ".",
     PARA,
     "This function operates by examining those types which are values of
     global symbols for keys which appear to be storing references to
     methods.  Types which don't appear as values of global variables will
     not be examined, so perhaps not all methods will be found.",
     PARA,
     EXAMPLE "methods drop"
     }

document {
     Key => isTable,
     Headline => "whether something is a rectangular list of lists",
     PARA,
     "Warning: it is intrinsically impossible to represent a ", TT "0", " 
     by ", TT "k", " matrix as a list of lists.",
     EXAMPLE {
	  "isTable {{1,2,3},{4,5}}",
	  "isTable {{1,2,3},{4,5,6}}",
	  }
     }

document {
     Key => Monoid,
     Headline => "the class of all monoids",
     "A monoid is a set with a multiplicative operation on
     it and an identity element.  A typical monoid is the set
     of monomials in a polynomial ring, which we consider to be
     created before the polynomial ring is created."
     }

document {
     Key => (symbol _, ZZ, Monoid),
     Headline => "get unit element",
     TT "1_M", " -- provides the unit element of a monoid ", TT "M", "."
     }


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
