--		Copyright 1994 by Daniel R. Grayson

document { "getting started",
     "The best way to run Macaulay 2 is with emacs - for details on getting
     that set up, see ", TO "emacs", ".  Learning emacs is worth the effort!
     Alternatively, you may start Macaulay 2 with the command ", TT "M2", "
     in a shell window.  On most Unix systems Macaulay 2 will start very
     quickly, but other parts of the program may have to be loaded from the
     disk later, causing a slight delay.",
     PARA,
     "Your first input prompt will be ", TT "i1 = ", " in response to the prompt,
     type ", TT "2+2", " and press return.  The expression you entered will be
     evaluated -- no punctuation is required at the end of the line.",
     EXAMPLE "2+2",
     "The answer is displayed to the right of the output label ", TT "o1 =", ".",
     PARA,
     "Here is some arithmetic with fractions.",
     EXAMPLE "3/5 + 7/11",
     "Notice the additional line of output labelled with ", TT "o2 :", ".  Output 
     lines labelled with colons provide information about the type of output.  In 
     this case, the symbol ", TO "QQ", " is our notation for the class of all 
     rational numbers, and indicates that the answer on the previous line is a 
     rational number.",
     PARA,
     "Multiplication is indicated with *.",
     EXAMPLE "1*2*3*4",
     "Powers are obtained with ", TO "^", ".",
     EXAMPLE "2^200",
     "Factorials are obtained with ", TO "!", ".",
     EXAMPLE "40!",			  -- this is o4 and is retrieved below
     "Because some answers can be very long, it is a good idea to run the
     program in a window which does not wrap output lines, and allows the
     user to scroll left horizontally to see the rest of the output.  (See
     ", TO "emacs", ".)",
     EXAMPLE "100!",
     "Multiple expressions may be separated by semicolons.",
     EXAMPLE "1;2;3*4",
     "A semicolon at the end of the line suppresses the printing of the value.",
     EXAMPLE "4*5;",
     "The output from the previous line can be obtained with ", TO "oo", ", even if 
     a semicolon prevented it from being printed.",
     EXAMPLE "oo",
     "Lines before that can be obtained with ", TO "ooo", " and ", TO "oooo", ".  
     Alternatively, the symbol labeling an output line
     can be used to retrieve the value, as in the following example.",
     EXAMPLE "o5 + 1",
     "To enter a string, use quotation marks.",
     EXAMPLE "\"hi there\"",
     "A value can be assigned to a variable with ", TO "=", ".",
     EXAMPLE "s = \"hi there\"",
     "Strings may be concatenated horizontally with ", TT "|", ", (see 
     ", TO (quote |, String, String), ").",
     EXAMPLE "s | \" - \" | s",
     "or vertically with ", TT "||", ", (see ", TO (quote ||, Net, Net), ").",
     EXAMPLE "s || \" - \" || s",
     "A list of expressions can be formed with braces.
     (See ", TO "lists, arrays, and sequences", ".)",
     EXAMPLE "{1, 2, s}",
     "Lists behave like vectors.",
     EXAMPLE "10*{1,2,3} + {1,1,1}",
     "A function can be created with the arrow operator, ", TO "->", " .",
     EXAMPLE "f = i -> i^3",
     "To evaluate a function, place its argument to the right of the
     function.",
     EXAMPLE "f 5",
     "Functions of more than one variable take a parenthesized sequence of
     arguments.",
     EXAMPLE {
	  "g = (x,y) -> x * y",
      	  "g(6,9)",
	  },
     "The function ", TO "apply", " can be used to apply a function to each 
     element of a list.",
     EXAMPLE {
	  "apply({1,2,3,4}, i -> i^2)",
      	  "apply({1,2,3,4}, f)",
	  },
     "The operator ", TO "..", " may be used to generate sequences of
     consecutive numbers.",
     EXAMPLE "apply(1 .. 4, f)",
     "If the first argument to ", TT "apply", " is an integer ", TT "n", " then
     it stands for the list ", TT "{0, 1, ..., n-1}", ".",
     EXAMPLE "apply(5, f)",
     "The function ", TO "scan", " is analogous to ", TO "apply", " except
     that no value is returned.  It may be used to implement loops in
     programs.",
     EXAMPLE "scan(5, i -> print (i, i^3))",
     EXAMPLE "j=1; scan(10, i -> j = 2*j); j",
     "Most computations with polynomials take place in rings that may be
     specified in usual mathematical notation.",
     EXAMPLE "R = ZZ/5[x,y,z];",
     "(We reserve single letter symbols such as ", TT "Z", " for use as variables in rings,
     hence we must use something like ", TT "ZZ", " to stand for the ring of integers.
     It may remind you of the \"blackboard bold\" font of AMSTeX.  If you prefer
     ", TT "Z", " to ", TT "ZZ", ", you may put ", TT "Z=ZZ", " in your ", TO "initialization file", ".)",
     EXAMPLE "(x+y)^5",
     "Rings and certain other types of things acquire the name of the global
     variable they are assigned to.",
     EXAMPLE "R",
     "To see the description of a ring, use ", TO "describe", ".",
     EXAMPLE "describe R",
     "A free module can be created as follows.",
     EXAMPLE "F = R^3",
     "The i-th basis element of ", TT "F", " can be obtained as ", TT "F_i", ".  In
     this example, the valid values for ", TT "i", " are 0, 1, and 2.",
     EXAMPLE "F_1",
     "Using a list of indices instead will produce the homomorphism corresponding
     to the basis vectors indicated.",
     EXAMPLE "F_{1,2}",
     "Repetitions are allowed.",
     EXAMPLE "F_{2,1,1}",
     "We can create a homomorphism between free modules with ", TO "matrix", "
     by providing the list of rows of the matrix, each of which is in turn
     a list of ring elements.",
     EXAMPLE "f = matrix {{x,y,z}}",
     "Use ", TO "image", " to get the image of f.",
     EXAMPLE "image f",
     "We may use ", TO "ideal", " to produce the corresponding ideal.",
     EXAMPLE "ideal (x,y,z)",
     "We may use ", TO "kernel", " to compute the kernel of f.",
     EXAMPLE "kernel f",
     "The answer comes out as a module which is expressed as the image of
     a homomorphism whose matrix is displayed.  In case the matrix itself
     is desired, it can be obtained with ", TO "generators", ".",
     EXAMPLE "generators oo",
     "We may use ", TO "poincare", " to compute the Poincare polynomial.",
     EXAMPLE "poincare kernel f",
     "We may use ", TO "rank", " to compute the rank.",
     EXAMPLE "rank kernel f",
     "A presentation for the kernel can be obtained with ", TO "presentation", ".",
     EXAMPLE "presentation kernel f",
     "We can produce the cokernel with ", TO "cokernel", "; no computation is
     performed.",
     EXAMPLE "cokernel f",
     "The direct sum is formed with ", TO (quote ++,Module,Module), ".",
     EXAMPLE "N = kernel f ++ cokernel f",
     "The answer is expressed in terms of the ", TO "subquotient", " function, which
     produces subquotient modules.  Each subquotient module is accompanied
     by its matrix of generators and its matrix of relations.  These matrices
     can be recovered with ", TO "generators", " and ", TO "relations", ".",
     EXAMPLE {
	  "generators N",
      	  "relations N",
	  },
     "The function ", TO "prune", " can be used to convert a subquotient
     module to a quotient module.",
     EXAMPLE "prune N",
     "We can use ", TO "resolution", " to compute a projective resolution of the 
     cokernel of ", TT "f", ".",
     EXAMPLE "C = resolution cokernel f",
     "To see the differentials we examine 'C.dd'.",
     EXAMPLE "C.dd",
     "We can verify that ", TT "C", " is a complex by squaring the differential map.",
     EXAMPLE "C.dd^2 == 0",
     "We can use ", TO "betti", " to see the degrees of the components of C.",
     EXAMPLE "betti C",
     "Let's try a harder example.  We can use ", TO "vars", " to create a sequence
     of variables.",
     EXAMPLE "R = ZZ/101[a .. r];",
     "We use ", TO "genericMatrix", " to make a 3 by 6 generic matrix whose
     entries are drawn from the variables of the ring ", TT "R", ".",
     EXAMPLE "g = genericMatrix(R,a,3,6)",
     "Then we construct its cokernel with ", TO "cokernel", ".",
     EXAMPLE "M = cokernel g",
     "We may use ", TO "resolution", " to produce a projective resolution of it, and
     ", TO "time", " to report the time required.",
     EXAMPLE "time C = resolution M",
     "As before, we may examine the degrees of its components, or display it.",
     EXAMPLE "betti C",
     "We can make a polynomial ring with 18 ", TO "IndexedVariable", "s.",
     EXAMPLE "S = ZZ/101[t_1 .. t_9, u_1 .. u_9];",
     "We can use ", TO "genericMatrix", " to pack the variables into 
     3-by-3 matrices.",
     EXAMPLE {
	  "m = genericMatrix(S, t_1, 3, 3)",
      	  "n = genericMatrix(S, u_1, 3, 3)",
	  },
     "We may look at the matrix product.",
     EXAMPLE "m*n",
     "Let's produce the equations generated by the equations which assert
     that m and n commute with each other.  (See ", TO "flatten", ".)",
     EXAMPLE "j = flatten(m*n - n*m)",
     "Let's compute a Groebner basis for the image of ", TT "j", " with ", TO "gb", ".",
     EXAMPLE "gb j",
     "The resulting Groebner basis contains a lot of information.
     We can get the generators of the basis, and even though we call upon
     ", TO "gb", " again, the computation will not be repeated.",
     EXAMPLE "generators gb j;",
     "The semicolon prevents the matrix of generators from appearing on the 
     screen, but the class of the matrix appears -- we see that there are 26
     generators.",
     PARA,
     "We can use ", TO "betti", " to see the degrees involved in the Groebner
     basis.",
     EXAMPLE "betti gb j"
     }

document { "executing other programs",
     "The ", TO "run", " command can be used to execute another program,
     after which control will return to Macaulay 2.",
     PARA,
     "To pass the contents of a string as input to a program or
     command, open an output file with ", TO "openOut", " whose name is
     the character '!' followed by the command, write the data to the 
     resulting file, and then close the file.  The output is displayed 
     on the screen.",
     PARA,
     "If you want to collect the output from a command in a string, use 
     ", TO "get", " with a file name consisting of the character '!' 
     followed by the command."
     }

-- html.m2 documentation

document {
     "hypertext",
     "All the online documentation for Macaulay 2 is maintained in
     hypertext form in a special internal format which can be easily
     manipulated or examined.  The function ", TO "html", " can be used to 
     convert it to standard world-wide web format, suitable for use with
     a world-wide web server such as netscape.  The function ", TO "text", "
     can be used to convert it to straight ascii text, suitable for
     viewing on an ascii terminal.",
     PARA,
     "Here are the functions for creating hypertext.",
     MENU {
	  TO "BIG",
	  TO "BLOCKQUOTE",
	  TO "BODY",
	  TO "BOLD",
	  TO "BR",
	  TO "CENTER",
	  TO "CITE",
	  TO "CODE",
	  TO "DFN",
	  TO "DL",
	  TO "EM",
	  TO "EXAMPLE",
	  TO "ExampleTABLE",
	  TO "H1",
	  TO "H2",
	  TO "H3",
	  TO "H4",
	  TO "H5",
	  TO "H6",
	  TO "HEAD",
	  TO "HR",
	  TO "HREF",
	  TO "HTML",
	  TO "IMG",
	  TO "ITALIC",
	  TO "KBD",
	  TO "LISTING",
	  TO "MENU",
	  TO "NL",
	  TO "NOINDENT",
	  TO "OL",
	  TO "PARA",
	  TO "PRE",
	  TO "SAMP",
	  TO "SEEALSO",
	  TO "SEQ",
	  TO "SHIELD",
	  TO "SMALL",
	  TO "STRONG",
	  TO "SUB",
	  TO "SUP",
	  TO "TABLE",
	  TO "TEST",
	  TO "TEX",
	  TO "TITLE",
	  TO "TO",
	  TO "TT",
	  TO "UL",
	  TO "UNDERLINE",
	  TO "VAR",
	  TO "XMP"
	  },
     SEEALSO {"MarkUpList", "MarkUpType", "Entity"}
     }

document { quote MarkUpList,
     "MarkUpList", " -- the class of lists used with ", TO "hypertext", ".",
     PARA,
     "Intended for internal use only."
     }

document { quote MarkUpType,
     "MarkUpType", " -- the class of types used with ", TO "hypertext", ".",
     PARA,
     SEEALSO "EmptyMarkUpType"
     }

document { quote EmptyMarkUpType,
     "EmptyMarkUpType", " -- a type of ", TO "MarkUpType", " used with ", TO "hypertext", ",
     which can't accept any content."
     }

document { quote SHIELD,
     TT "SHIELD v", " -- indicates that the ", TO "hypertext", " links in v
     will not lead immediately to subsections of the book, even though
     the expression is in a ", TO "MENU", "."
     }

document { quote PARA,
     TT "PARA x", " -- a ", TO "hypertext", " paragraph.",
     }

document { quote BR,
     TT "BR{}", " -- a ", TO "hypertext", " line break."
     }

document { quote HR,
     TT "HR{}", " -- a ", TO "hypertext", " horiziontal rule."
     }

document {
     quote text,
     "text x -- convert ", TO "hypertext", " to text format",
     PARA,
     "The return value is a string which is suitable display on an
     ascii terminal."
     }

document {
     quote html,
     "html x -- convert ", TO "hypertext", " to html format",
     PARA,
     "The return value is a string which is suitable for use in an
     html file, readable by a world wide web client such as netscape.",
     PARA,
     "The function ", TO "linkFilename", " is used internally by the
     code that generates html files from hypertext.",
     SEEALSO "mathML"
     }

document { quote PRE,
     TT "PRE x", " -- encloses x in a hypertext PRE item.",
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

document { quote TITLE,
     TT "TITLE x", " -- encloses x in a hypertext TITLE item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote HEAD,
     TT "HEAD x", " -- encloses x in a hypertext HEAD item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote BODY,
     TT "BODY x", " -- encloses x in a hypertext BODY item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote IMG,
     TT "IMG x", " -- encloses x in a hypertext IMG item.",
     PARA,
     "The argument ", TT "x", " should be a string containing the URL of the image.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote SEQ,
     TT "SEQ x", " -- encloses x in a hypertext SEQ item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.  The result is that the entries in the
     list or sequence are displayed sequentially.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote HTML,
     TT "HTML x", " -- encloses x in a hypertext HTML item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H1,
     TT "H1 x", " -- encloses x in a hypertext H1 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H1 \"Interesting thing\"",
     " produces ",
     H1 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H2,
     TT "H2 x", " -- encloses x in a hypertext H2 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H2 \"Interesting thing\"",
     " produces ",
     H2 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H3,
     TT "H3 x", " -- encloses x in a hypertext H3 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H3 \"Interesting thing\"",
     " produces ",
     H3 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H4,
     TT "H4 x", " -- encloses x in a hypertext H4 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H4 \"Interesting thing\"",
     " produces ",
     H4 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H5,
     TT "H5 x", " -- encloses x in a hypertext H5 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H5 \"Interesting thing\"",
     " produces ",
     H5 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote H6,
     TT "H6 x", " -- encloses x in a hypertext H6 header item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.  The code ",
     PRE "H6 \"Interesting thing\"",
     " produces ",
     H6 "Interesting thing",
     PARA,
     SEEALSO "hypertext"
     }

document { quote LISTING,
     TT "LISTING x", " -- encloses x in a hypertext LISTING item.",
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

document { quote XMP,
     TT "XMP x", " -- encloses x in a hypertext XMP item.",
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

document { quote BLOCKQUOTE,
     TT "BLOCKQUOTE x", " -- encloses x in a hypertext BLOCKQUOTE item.",
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

document { quote EXAMPLE,
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

document { quote TABLE,
     TT "TABLE x", " -- produces a hypertext TABLE from a list of lists."
     }

document { quote ExampleTABLE,
     TT "ExampleTABLE x", " -- produces a hypertext display suitable for displaying
     a list of examples."
     }

document { quote VAR,
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

document { quote DFN,
     TT "DFN x", " -- encloses x in a hypertext DFN item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     "Here is an example.",
     DFN "Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.  Here is a long line.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote STRONG,
     TT "STRONG x", " -- encloses x in a hypertext STRONG item.",
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

document { quote CENTER,
     TT "CENTER x", " -- encloses x in a hypertext CENTER item.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     SEEALSO "hypertext"
     }

document { quote BIG,
     TT "BIG x", " -- encloses x in a hypertext BIG item.",
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

document { quote SMALL,
     TT "SMALL x", " -- encloses x in a hypertext SMALL item.",
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

document { quote SUB,
     TT "SUB x", " -- encloses ", TT "x", " in a hypertext ", TT "SUB", " item,
     thereby making it a subscript.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote SUP,
     TT "SUP x", " -- encloses ", TT "x", " in a hypertext ", TT "SUP", " item,
     thereby making it a superscript.",
     PARA,
     "The argument ", TT "x", " should be a string, or a list or sequence of
     strings or hypertext items.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote SAMP,
     TT "SAMP x", " -- encloses x in a hypertext SAMP item.",
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

document { quote KBD,
     TT "KBD x", " -- encloses x in a hypertext KBD item.",
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

document { quote ITALIC,
     TT "ITALIC x", " -- encloses x in a hypertext ITALIC item.",
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

document { quote UNDERLINE,
     TT "UNDERLINE x", " -- encloses x in a hypertext UNDERLINE item.",
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

document { quote TEX,
     TT "TEX s", " -- includes the string ", TT "s", ", presumably
     containing TeX commands, in the TeX version of the documentation
     containing this ", TO "hypertext", " item.",
     PARA,
     "Invisible in the text and html versions."
     }

document { quote TT,
     TT "TT x", " -- encloses x in a hypertext TT item.",
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

document { quote EM,
     TT "EM x", " -- encloses x in a hypertext EM item.",
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

document { quote CITE,
     TT "CITE x", " -- encloses x in a hypertext CITE item.",
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

document { quote BOLD,
     TT "BOLD x", " -- encloses x in a hypertext BOLD item.",
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

document { quote CODE,
     TT "CODE x", " -- encloses x in a hypertext CODE item.",
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

document { quote HREF,
     TT "HREF{u,p}", " -- encloses the phrase ", TT "p", " in a hypertext HREF link
     pointing to the url ", TT "u", ".",
     PARA,
     SEEALSO "hypertext"
     }

document { quote MENU,
     TT "MENU x", " -- encloses the list x as a hypertext MENU.",
     PARA,
     "The argument ", TT "x", " should be a list of hypertext items or strings.",
     PARA,
     "Here is an example. The expression ",
     PRE "MENU {\"first\",\"second\",\"third\"}",
     "produces",
     MENU {"first","second","third"},
     PARA,
     SEEALSO "hypertext"
     }

document { quote UL,
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

document { quote DL,
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

document { quote OL,
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

document { quote NL,
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

document { quote NOINDENT,
     TT "NOINDENT{}", " -- specifies that no indentation should be used
     for following hypertext items.",
     PARA,
     "This item is mainly of interest for hypertext that ultimately will
     be viewed with TeX, for html viewers don't indent paragraphs.",
     PARA,
     SEEALSO "hypertext"
     }

document { quote TO,
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
     "The special form ", TT "TO (f => X)", " produces a hypertext link to the
     page documenting the optional argument named ", TT "X", " for the 
     function ", TT "f", "."
     }

document { quote Command,
     TT "Command", " -- denotes the class of all commands.",
     PARA,
     "A command behaves as a function does if it is followed by an adjacent
     expression which can serve as its argument or argument list.  In addition,
     if it appears as the value of an expression typed by the user at top
     level, then it gets executed with empty argument list.  This is
     accomplished by installing a method for ", TO "AfterEval", ".",
     PARA,
     "Create a new command ", TT "f", " from a function ", TT "g", " with ", 
     TT "f = new Command from g", ".",
     PARA,
     "Create a new command ", TT "f", " from a shell command named g with
     ", TT "f = new Command from \"g\"", ".  Arguments to f will be provided to g."
     }
