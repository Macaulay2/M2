--		Copyright 1993-1999 by Daniel R. Grayson

document { "Invoking the program",
     "On systems with a command line interface, the following commands
     can be used to start the program.  When the program starts up,
     the ", TO "initialization file", ", ", TT "init.m2", ", will be loaded.",
     PARA, NOINDENT,
     TT "M2", " -- starts the program.",
     PARA, NOINDENT,
     TT "M2 file1 file2 ... ", " -- starts the program, reading and 
     executing the specified files.",
     PARA,
     "These are the options that can also be provided on the command
     line.",
     MENU {
	  {TT "--", "         -- ignore previous arguments after reloading data"},
	  {TT "-e x", "       -- evaluates the expression x"},
	  {TT "-h", "         -- displays the usage message"},
	  {TT "-mpwprompt", " -- MPW-style imput prompts"},
	  {TT "-n", "         -- print no input prompts"},
	  {TT "-q", "         -- suppresses loading of init file 'init.m2'"},
	  {TT "-s", "         -- stops execution if an error occurs"},
	  {TT "-silent", "    -- don't print the startup banner"},
	  {TT "-tty", "       -- assume stdin is a tty"},
	  {TT "-x", "         -- special mode for running examples"},
	  },
     "The file ", TT "M2", " is actually a shell script which calls the executable file
     with appropriate arguments for loading the Macaulay 2 code previously
     compiled.",
     PARA,
     "To terminate the program, one may type ", TO "exit", ", ", TO "quit", ",
     ", TO "end", ", or the end of file character."
     }

document { "Macaulay2/COPYING",
     "This is the text of the license agreement under which Macaulay 2 is distributed.",
     PARA,
     PRE get (currentFileDirectory | "../COPYING")
     }

document { "Copyright and license",
     "Macaulay 2, its object code and source code, and its documentation,
     are copyright by Daniel R. Grayson and Michael E. Stillman.  We permit 
     you to use it under the terms of the GNU General Public License, version
     2, as published by the Free Software Foundation; see the file
     ", TO "Macaulay2/COPYING", ".",
     PARA,
     "Various free libraries have been compiled into Macaulay 2.",
     SHIELD MENU {
	  TO "Singular-Factory",
	  TO "Singular-Libfac",
	  TO "GNU MP",
	  TO "GC garbage collector"
	  }
     }

document { "GC garbage collector",
     "Macaulay 2 uses the excellent garbage collector GC, version ", version#"gc version", ",
     written by Hans-J. Boehm and Alan J. Demers and generously licensed
     to the public.  It is available at
     ", HREF "http://www.hpl.hp.com/personal/Hans_Boehm/gc/", ".",
     SEEALSO {"collectGarbage" 
	  -- , "gcDump"
	  }
     }

document { "Singular-Factory",
     "With the kind permission of the authors of Singular, 
     G.-M. Greuel, R. Stobbe, G. Pfister, H. Schoenemann, and J. Schmidt,
     University of Kaiserslautern, Macaulay 2 incorporates ", TT "Singular-Factory", ",
     version ", version#"factory version", ", a free library of polynomial routines
     which provides for factorization of polynomials.  It is distributed under the
     terms of the GNU General Public License and is available at 
     ", HREF "ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Factory",".",
     SEEALSO {"factor", "gcd"}
     }

document { "Singular-Libfac",
     "With the kind permission of the author, Michael Messollen, Macaulay 2
     incorporates ", TT "Singular-Libfac", ", version ", version#"libfac version", ",
     a free library of routines which provides factorization of multivariate
     polynomials over finite fields and computation of the minimal associated primes 
     of ideals via characteristic sets.  It is distributed under the terms of the
     GNU General Public License, and is available at 
     ", HREF "ftp://www.mathematik.uni-kl.de/pub/Math/Singular/Libfac",".",
     SEEALSO {"factor", "gcd", "decompose", "irreducibleCharacteristicSeries"}
     }

document { "GNU MP",
     "The GNU MP library, gmp, version ", version#"gmp version", " provides
     routines for arbitrary precision integer and floating point arithmetic,
     and is distributed under the terms of the GNU Lesser General Public License.
     It's available at ", HREF "ftp://ftp.gnu.org/gnu/gmp/", " and 
     ", HREF "http://www.swox.com/gmp/", ".",
     SEEALSO "How to get this program"
     }

document { "operators",
     Headline => "an overview",
     "Here is a list of unary and binary operators in the language.  Many
     of them can have methods installed for handling arguments of specific
     types.",
     MENU {
          (TO " ", " -- function application"),
          (TO ",", " -- separates elements of lists or sequences"),
          (TO ";", " -- statement separator"),
          (TO "=", " -- assignment"),
          (TO "<-", " -- assignment with left hand side evaluated"),
	  TO "->",
          (TO ":=", " -- assignment of method or new local variable"),
          (TO "==", " -- equal"),
          (TO "!=", " -- not equal"),
          (TO "===", " -- strictly equal"),
          (TO "=!=", " -- strictly not equal"),
          (TO "<", " -- less than"),
          (TO "<=", " -- less than or equal"),
          (TO "=>", " -- option"),
          (TO ">", " -- greater than"),
          (TO ">=", " -- greater than or equal"),
          (TO "?", " -- comparison"),
	  (TO "or", " -- or"),
	  (TO "and", " -- and"),
          (TO "not", " -- negation"),
          (TO "..", " -- sequence builder"),
          (TO "+", " -- addition"),
          (TO "-", " -- subtraction"),
          (TO "*", " -- multiplication"),
          (TO "/", " -- division, or applying a function to elements of a list"),
          (TO "//", " -- quotient"),
          (TO "\\\\", " -- left quotient"),
          (TO "%", " -- remainder"),
          (TO "^", " -- power"),
          (TO "^**", " -- tensor power"),
          (TO "/^", " -- divided power"),
          (TO "!", " -- factorial"),
          (TO "++", " -- direct sum"),
          (TO "**", " -- tensor product"),
          (TO "<<", " -- file output, bit shifting"),
          (TO ">>", " -- bit shifting"),
          (TO "_", " -- subscripting"),
          (TO ".", " -- hash table access or assignment"),
          (TO ".?", " -- test for hash table access"),
          (TO "#", " -- hash table access; length of a list, sequence or hash table"),
          (TO "#?", " -- test for hash table access"),
          (TO "|", " -- horizontal concatenation of strings or matrices"),
          (TO "||", " -- vertical concatentation of strings or matrices"),
          (TO "&", " -- bit-wise and"),
          (TO ":", " -- ideal quotient, repetitions"),
          (TO "\\", " -- applying a function to elements of a list"),
          (TO "==>", " -- attaching options to a function"),
          (TO "@@", " -- composing functions"),
          TO "@",
          TO "&&",
          TO "^^",
          (TO "~", " -- making a coherent sheaf")
     	  }
     }

document { "Acknowledgements",
     "We thank the National Science Foundation for generous funding since
     1993 for this project, Gert-Martin Greuel and Ruediger Stobbe for the
     incorporation of their library ", TO "Singular-Factory", ", Michael Messollen for
     the incorporation of his library ", TO "Singular-Libfac", ",
     and David Eisenbud, Wolfram Decker and Sorin Popescu for
     early support, encouragement and suggestions.  We also acknowledge an
     intellectual debt to David Bayer, who, with Michael Stillman,
     wrote Macaulay, a specialized computer algebra system for algebraic
     geometry and the predecessor of this program."
     }

load "tutorials.m2"

document { "Miscellaneous Topics",
     "Some of these topics will have to be merged with sections above.",
     MENU {
	  TO "help functions",
	  TO "replacements for commands and scripts from Macaulay",
	  TO "obsolete functions and symbols",
     	  }
     }

document { "internals",
     "Here are some functions and classes that are intended for internal use 
     by the developers only.",
     MENU {
	  TO "formatDocumentTag",
	  TO "setSpin",
	  }
     }

document { "Reading the Documentation",
     "The documentation is divided into the main parts, the ", TO "User's Guide", "
     and the ", TO "Reference Manual", ".  The reference manual is organized mainly
     according to the types of things, and reflects the particular way we've
     programmed the system.  For example, if you want to find out about 
     matrices, then you might get there by clicking first on ", TO "Thing", ",
     then click on ", TO "Type", ", and then on ", TO "Matrix", ".  The user's
     guide is organized more according to mathematical topics that might occur to
     an initial user, and might be a good place to look first.",
     PARA,
     "The documentation for Macaulay 2 is available in several formats.
     The directory ", TT ("Macaulay2-" | version#"VERSION" | "/html"), " 
     contains the documentation in html form, suitable for viewing with a web 
     browser such as lynx or Netscape, and this is the best way to view 
     it.  The command ", TT "M2-help", " will start your favorite web browser
     and point direct it to the web pages there.  Each documentation page has a text
     box for entering a search string.  This will work if you view the documentation
     at our web site; it will work on your local machine only if you or your
     system administrator has already installed ", TT "htdig", ", which is a 
     free indexing package available at ", TT "http://www.htdig.org/", ".",
     PARA,
     "Finally, all the documentation can be viewed within the program in
     text form using ", TO "help", "."
     }

document { "Preface",
     MENU {
	  TO "How to get this program",
	  TO "Resources required",
	  TO "Invoking the program",
	  TO "Copyright and license",
	  TO "Acknowledgements",
	  TO "The authors",
	  }
     }

document { "User's Guide",
     "Here are the basic concepts needed to use Macaulay 2 effectively.",
     MENU {
	  TO "Preface",
	  TO "Getting started",
	  TO "Reading the Documentation",
	  TO "Mathematical Overview",
	  TO "Language and Programming Overview",
	  TO "Miscellaneous Topics",
	  }
     }

document { "Mathematical Vignettes",
     "In this section we present some tutorials which aim to introduce
     the user to some mathematical ways of using Macaulay 2.  The tutorials
     are relatively independent of each other, and each one introduces the use
     of some features of Macaulay 2 in a slow and leisurely way, assuming the
     reader is already familiar with the mathematical concepts involved.  
     ", TO "David Eisenbud", " joins us as a co-author of these tutorials.",
     MENU {
	  TO "Elementary uses of Groebner bases",
	  TO "Canonical Embeddings of Plane Curves and Gonality",
	  TO "Fano varieties",
	  TO "Divisors",
	  }
     }

document { "Reference Manual",
     "This section is intended to offer detailed documentation on
     every aspect of the system of interest to users.  We organize the
     software and therefore the documentation primarily according to
     the types of objects, and secondarily according to the functions
     available for dealing with objects of a given type.  For example,
     if you want to know how to get a projective resolution of an ideal,
     you should first find either the section dealing with ideals or 
     the section dealing with chain complexes.  More briefly, look first
     for the noun and then for the verb.",
     PARA,
     "In line with that advice, we list here some types of things
     with particular mathematical interest.",
     SHIELD MENU {
	  TO "Ring",
	  TO "Ideal",
	  TO "Matrix",
	  TO "Module",
	  TO "RingMap",
	  TO "GroebnerBasis",
	  TO "ChainComplex",
	  TO "Variety",
	  },
     "Some types of things of interest to the programmer and user:",
     SHIELD MENU {
	  TO "Function",
	  TO "VisibleList",
	  TO "File"
	  },
     "A list of the operators:",
     MENU {
	  SHIELD TO "operators",
	  },
     "We've mentioned that our software is organized according to the
     types of things there are.  The types form a hierarchy; for example,
     a polynomial ring is a type of ring, and an affine variety is a
     type of variety.  At the top of the hierarchy sits ", TO "Thing", ";
     everything is a thing, and every type is a type of thing.  The
     documentation is organized here as a view of the hierarchy from the
     top down, so you can find everything (eventually) in the following
     section, its subsections, or the sections it refers to.",
     MENU {
     	  TO "Thing"
	  }
     }

document {  "Developer's Corner",
     MENU {
	  TO "engine",
	  TO "internals",
	  }
     }

document { "Macaulay 2",
     FileName => "index",
     IMG "../html/9planets.gif", PARA,
     "Macaulay 2 is a software system devoted to supporting research in 
     algebraic geometry and commutative algebra, developed with funding
     from the National Science Foundation.  The current version is 
     ", version#"VERSION", ".  The program is still under development, but
     the main features are working.  We are eager to help new users
     get started with it.",
     MENU {
	  (TO "User's Guide", " -- This is the place to get started"),
 	  (TO "Mathematical Vignettes", " -- Several extended examples"),
	  (TO "Reference Manual", " -- Complete documentation for every type and function in Macaulay 2"),
	  (TO "Developer's Corner", " -- For experts only")
	  }
     }

document { "specifying typical values",
     "For the purpose of construction good documentation automatically, it
     is useful to specify the type of value typically returned by a function
     or method.  For example, the function ", TO "isModule", " returns a boolean
     value, and this is specified when creating the method function with the
     option ", TO "TypicalValue", " as follows.",
     PRE ///isModule = method(TypicalValue => Boolean)///,
     PARA,
     "Other functions, such as ", TO "prune", ", return values of various types,
     depending on the type of the arguments provided.  To install a
     function ", TT "f", " as the handler for ", TT "prune", " applied to a matrix,
     we would normally use the following statement.",
     PRE ///prune Matrix := f///,
     "To specify that the value typically returned is a matrix (of class ", TT "Matrix", "),
     we replace ", TT "f", " by ", TT "Matrix => f", ", as follows.",
     PRE ///prune Matrix := Matrix => f///,
     "Here is the way our code looks.",
     EXAMPLE "code(prune, Matrix)",
     "The information is stored in the hash table ", TO "typicalValues", ", and can
     be recovered like this.",
     EXAMPLE "typicalValues#(prune,Matrix)",
     PARA,
     "Warning: don't imagine that a definition of the form ",
     PRE "f = t -> (...)",
     "can be replaced with a declaration of the following form.",
     PRE "f = X => t -> (...)",
     "The difference here is that here we are using simple assignment, rather than
     installing a method.  To document the return type is ", TT "X", " in this case, 
     make an entry in ", TT "typicalValues", " directly.",
     PRE "f = t -> (...)\ntypicalValues#f = X"
     }


document { "The authors",
     "The authors of Macaulay 2 and the bulk of this manual:",
     MENU {
	  TO "Daniel R. Grayson",
	  TO "Michael E. Stillman"
	  },
     "Our co-author for the tutorials:",
     MENU {
	  TO "David Eisenbud",
	  },
     "To communicate with us about the program, use this email address:
     ", HREF {"mailto:Macaulay2@math.uiuc.edu", "<Macaulay2@math.uiuc.edu>"}, "."
     
     }

document { "David Eisenbud",
     HREF {"http://www.msri.org/people/staff/de/", "David Eisenbud "}, " ",
     HREF {"mailto:de@msri.org", "<de@msri.org>"}, ".",
     PARA,
     "In this spot will go a brief biography of David Eisenbud."
     }

document { "Daniel R. Grayson",
     HREF {"http://www.math.uiuc.edu/~dan/", "Daniel R. Grayson"}, " ",
     HREF {"mailto:dan@math.uiuc.edu", "<dan@math.uiuc.edu>"}, ".",
     PARA,
     "Daniel Grayson received his PhD in Mathematics from MIT in 1976, taught
     at Columbia from 1976 to 1981, and came to the University of Illinois at
     Urbana-Champaign in 1981, where he is a Professor.  His mathematical
     research concerns algebraic K-theory, but he has always been intrigued
     by computers.  In 1986 he joined with Stephen Wolfram and six other
     co-authors to write ", ITALIC "Mathematica", " which in the years since
     its introduction in 1988 has become the pre-eminent system for
     mathematics on the computer.",
     PARA,
     IMG "../html/Grayson2.jpg"
     }

document { "Michael E. Stillman",
     HREF { "http://www.math.cornell.edu/~mike/", "Michael E. Stillman"}, " ",
     HREF {"mailto:mike@math.cornell.edu", "<mike@math.cornell.edu>"},
     PARA,
     "Michael E. Stillman received his PhD in Mathematics from Harvard in 1983,
     taught at University of Chicago 1983-85, was at Brandeis and then MIT 1985-87,
     and then came to Cornell University.  His mathematical research concerns
     computational algebraic geometry and algebraic geometry.  He started writing
     syzygy programs as an undergraduate at the University of Illinois, and from
     1983 to 1992 with David Bayer he wrote Macaulay, a specialized computer
     algebra system for algebraic geometry and the predecessor of this program."
     }

document { "Resources required",
     "You will need about 12 megabytes of disk space to install Macaulay 2, though
     this may vary.  It will need about 12 megabytes of RAM to run modest size problems,
     and can benefit from any additional memory."
     }

document { "How to get this program",
     "The program is available over the web at the Macaulay 2 home page",
     PARA, 
     CENTER HREF {"http://www.math.uiuc.edu/Macaulay2/"}, 
     PARA,
     NOINDENT,
     "or by ftp to the host ", TT "ftp.math.uiuc.edu", " with user name ", TT "Macaulay2", " 
     and password ", TT "Macaulay2", ".  There you will find the documentation, both in
     readable form and available for downloading, the source code, ready for compiling
     on the machine of your choice, and various precompiled versions, ready to run."
     }

document { "syntax",
     Headline => "an overview",
     "A newline ends a statement if it can, otherwise it acts like any
     white space.",
     EXAMPLE "2+\n3+\n4",
     PARA,
     "Parsing is determined by a triple of numbers attached to each token.
     The following table (produced by the command ", TO "seeParsing", "), 
     displays each of these numbers.",
     EXAMPLE "seeParsing()",
     "Here is the way these numbers work.  The parser maintains a number
     which we will call the current parsing level, or simply, the level.
     The parser builds up an expression until it encounters an input token
     whose parsing precedence is less than or equal to the current level.
     The tokens preceding the offending token are bundled into an expression
     appropriately and incorporated into the containing expression.",
     PARA,
     "When an operator or token is encountered, its binding strength serves
     as the level for parsing the subsequent expression, unless the current level
     is higher, in which case it is used.",
     PARA,
     "Consider a binary operator such as ", TT "*", ".  The relationship between
     its binary binding strength and its parsing precedence turns out to determine
     whether ", TT "a*b*c", " is parsed as ", TT "(a*b)*c", " or as ", TT "a*(b*c)", ".
     When the parser encounters the second ", TT "*", ", the current parsing level 
     is equal to the binding strength of the first ", TT "*", ".  If the binding 
     strength is less than the precedence, then the second ", TT "*", " becomes
     part of the right hand operand of the first ", TT "*", ", and the
     expression is parsed as ", TT "a*(b*c)", ".  Otherwise, the expression is
     parsed as ", TT "(a*b)*c", ".",
     PARA,
     "For unary operators, the unary binding strength is used instead of the binary
     binding strength to reset the current level.  The reason for having both numbers 
     is that some operators can be either unary or binary, depending on the context.
     A good example is ", TO "#", " which binds as tightly as ", TO ".", "
     when used as an infix operator, and binds as loosely as adjacency or
     function application when used as a prefix operator.",
     PARA,
     "To handle expressions like ", TT "b c d", ", where there are no tokens present
     which can serve as a binary multiplication operator, after parsing ", TT "b", ",
     the level will be set to 1 less than the precedence of an identifier,
     so that ", TT "b c d", " will be parsed as ", TT "b (c d)", ".",
     PARA,
     "The comma and semicolon get special treatment; the empty expression can
     occur to the right of the comma or semicolon or to the left of the comma."
     }

document { "debugging",
     Headline => "an overview",
     "Here are some debugging tools.",
     SHIELD MENU {
	  TO "assert",
	  TO "backtrace",
	  TO "benchmark",
	  TO "browse",
	  TO "code",
	  TO "currentFileName",
	  TO "edit",
	  TO "error",
	  TO "errorDepth",
	  TO "examine",
	  TO "flag",
	  TO "frame",
	  TO "listUserSymbols",
     	  TO "locate",
	  TO "methods",
	  TO "on",
	  TO "peek",
	  TO "peek2",
	  TO "profile",
	  TO "shield",
	  TO "showStructure",
	  TO "showUserStructure",
	  TO "try",
	  TO "userSymbols"
	  },
     "These functions are for debugging the kernel interpreter itself, and
     are not intended for users.",
     MENU {
	  TO "buckets",
	  TO "seeParsing"
	  }
     }

document { "system",
     Headline => "an overview",
     "Loading files:",
     SHIELD MENU {
	  TO "autoload",
	  TO "initialization file",
	  TO "input",
	  TO "load",
	  TO "needs"
	  },
     "Echoing characters:",
     SHIELD MENU {
	  TO "clearEcho",
	  TO "setEcho"
	  },
     "Dumping and restoring the state of the system:",
     SHIELD MENU {
	  TO "dumpdata",
	  TO "loaddata",
	  TO "reloaded",
	  TO "restart",
	  TO "addStartFunction",
	  TO "addEndFunction"
	  },
     "Interface to the operating system:",
     SHIELD MENU{
	  TO "top level loop",
	  TO "alarm",
	  TO "currentDirectory",
	  TO "exec",
	  TO "exit",
	  TO "fork",
	  TO "getenv",
	  TO "processID",
	  TO "path",
	  TO "pathSeparator",
	  TO "quit",
	  TO "run",
	  TO "sleep",
	  TO "time",
	  TO "timing",
	  TO "temporaryFileName",
	  TO "wait"
	  },
     "Variables with information about the state of the current process:",
     SHIELD MENU {
	  TO "commandLine",
	  TO "environment",
	  TO "version"
	  },
     "Miscellaneous commands:",
     SHIELD MENU {
	  TO "getWWW"
	  },
     "Dealing with the garbage collector:",
     SHIELD MENU {
	  TO "collectGarbage",
	  -- TO "gcDump"
	  }
     }

document { "initialization file",
     "The file ", TT "init.m2", " is loaded automatically when the
     program is started.",
     PARA,
     "The file is sought in each of the directories of the ", TO "path", ",
     and also in the home directory of the user.  At most one file is loaded.",
     SEEALSO "load"
     }

document { "combinatorial functions",
     Headline => "an overview",     
     MENU {
	  TO "random",
	  TO "binomial",
	  TO "subsets",
	  TO "tally",
	  TO "partitions"
	  }
     }

document { "top level loop",
     "The top level evaluation loop of the interpreter contains hooks so the user can
     control how printing of the results of evaluation is done.  If the result is 
     ", TO "null", " then nothing is printed.  Otherwise, the appropriate method
     associated with the symbol ", TO "Print", " is applied to perform the printing,
     unless the printing is to be suppressed, as indicated by a semicolon at the end
     of the statement, in which case the ", TO "NoPrint", " method is applied.",
     MENU {
	  TO "AfterEval",
	  TO "AfterPrint",
	  TO "AfterNoPrint",
	  TO "NoPrint",
	  TO "Print",
	  }
     }

document { "help functions",
     "Online Macaulay 2 documentation is stored in ", TO "hypertext", "
     form.",
     PARA,
     NOINDENT,
     "Functions for accessing the documentation:",
     SHIELD MENU {
	  TO "apropos",
	  TO "briefDocumentation",
	  TO "documentation",
	  TO "examples",
	  TO "help", 
	  TO "topicList", 
	  TO "topics"
	  },
     "How to write documentation yourself:",
     SHIELD MENU {
	  TO "document",
	  TO "hypertext",
	  },
     "Output formatting routines:",
     SHIELD MENU {
	  TO "html",
	  TO "mathML",
	  TO "tex",
	  TO "text",
	  },
     "Some internals:",
     SHIELD MENU {
	  TO "Documentation",
	  TO "phase",
	  },
     SEEALSO "Reading the Documentation"
     }

document { "Getting started",
     "The best way to run Macaulay 2 is with emacs - for details on getting
     that set up, see ", TO "emacs", ".  (On MacOS systems, we include a shareware
     editor called ", TT "alpha", " for this purpose.)  Learning emacs is worth 
     the effort! Alternatively, you may start Macaulay 2 with the command ", TT "M2", "
     in a shell window.  On most systems Macaulay 2 will start very
     quickly, but other parts of the program may have to be loaded from the
     disk later, causing a slight delay.",
     PARA,
     "Your first input prompt will be ", TT "i1 : ", ".  In response to the prompt,
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
     ", TO (symbol |, String, String), ").",
     EXAMPLE "s | \" - \" | s",
     "or vertically with ", TT "||", ", (see ", TO (symbol ||, Net, Net), ").",
     EXAMPLE "s || \" - \" || s",
     "A list of expressions can be formed with braces.",
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
     ", TT "Z", " to ", TT "ZZ", ", you may put ", TT "Z=ZZ", " in your
     ", TO "initialization file", ".)",
     EXAMPLE "(x+y)^5",
     "Rings and certain other types of things acquire the name of the global
     variable they are assigned to.",
     EXAMPLE "R",
     "To see the original description of a ring, use ", TO "describe", ".",
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
     "The direct sum is formed with ", TO (symbol ++,Module,Module), ".",
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
     Headline => "an overview",
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
