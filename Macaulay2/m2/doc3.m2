--		Copyright 1993-1998 by Daniel R. Grayson

document { "the authors",
     "The authors of Macaulay 2 and the bulk of this manual:",
     MENU {
	  TO "Daniel R. Grayson",
	  TO "Michael E. Stillman"
	  },
     "Our co-author for the tutorials:",
     MENU {
	  TO "David Eisenbud",
	  }
     }

document { "David Eisenbud",
     HREF {"http://www.msri.org/people/staff/de/", "David Eisenbud "}, " ",
     HREF {"mailto:de@msri.org", "<de@msri.org>"}, ".",
     PARA,
     "In this spot will go a brief biography of David Eisenbud."
     }

document { "Daniel R. Grayson",
     HREF {"http://www.math.uiuc.edu/~dan", "Daniel R. Grayson"}, " ",
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
     IMG "Grayson2.jpg"
     }

document { "Michael E. Stillman",
     HREF { "http://www.math.cornell.edu/~mike", "Michael E. Stillman"}, " ",
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

document { "resources required",
     "You will need about 12 megabytes of disk space to install Macaulay 2, though
     this may vary.  It will need about 12 megabytes of RAM to run modest size problems,
     and can benefit from any additional memory."
     }

document { "how to get this program",
     "The program is available over the web at the Macaulay 2 home page",
     PARA, 
     HREF {"http://www.math.uiuc.edu/Macaulay2"}, 
     PARA, 
     "or by ftp to the host ", TT "ftp.math.uiuc.edu", " with user name ", TT "Macaulay2", " 
     and password ", TT "Macaulay2", ".  There you will find the documentation, both in
     readable form and available for downloading, the source code, ready for compiling
     on the machine of your choice, and various precompiled versions, ready to run."
     }

document { "syntax",
     "A newline ends a statement if it can, otherwise it acts like any
     white space.",
     EXAMPLE "2+\n3+\n4",
     PARA,
     "Parsing is determined by a triple of numbers attached to each token.
     The following table (produced by ", TO "seeParsing", "), displays each
     of these numbers.",
     EXAMPLE "seeParsing()",
     "Here is the way these numbers work.  The parser maintains a number
     which we will call the current parsing level, or simply, the level.
     The parser builds up an expression until it encounters an input token
     whose precedence is less than or equal to the current level.  The
     tokens preceding the offending token are bundled into an expression
     appropriately and incorporated into the containing expression.",
     PARA,
     "When an operator or token is encountered, its scope serves as the
     level for parsing the subsequent expression, unless the current level
     is higher, in which case it is used.",
     PARA,
     "Consider a binary operator such as ", TT "*", ".  The relationship between
     its scope and its precedence turns out to determine whether ", TT "a*b*c", "
     is parsed as ", TT "(a*b)*c", " or as ", TT "a*(b*c)", ".  When the parser encounters
     the second ", TT "*", ", the current parsing level is equal to the scope of
     the first ", TT "*", ".  If the scope is less than the precedence, then
     the second ", TT "*", " becomes part of the right hand operand of the
     first ", TT "*", ", and the expression is parsed as ", TT "a*(b*c)", ".  Otherwise, the
     expression is parsed as ", TT "(a*b)*c", ".",
     PARA,
     "For unary operators, the strength is used instead of the scope to reset
     the current level.  The reason for having both numbers is that some
     operators can be either unary or binary, depending on the context.
     A good example is ", TO "#", " which binds as tightly as ", TO ".", "
     when used as an infix operator, and binds as loosely as adjacency or
     function application when used as a prefix operator.",
     PARA,
     "To handle expressions like ", TT "b c d", ", where there are no tokens present
     which can serve as a binary multiplication operator, after parsing b,
     the level will be set to 1 less than the precedence of an identifier,
     so that ", TT "b c d", " will be parsed as ", TT "b (c d)", ".",
     PARA,
     "The exclamation point is allowed as a unary operator either to the
     right or to the left of its operand.  The other unary operators occur
     to the left of their operands.",
     PARA,
     "Three operators are treated specially, in that the empty expression
     is allowed to the right of them.  These are newline, comma, and semicolon."
     }

document { "programming",
     "Here are some useful programming constructs for controlling the flow 
     of execution.",
     MENU {
	  (TO "apply", "  -- loop over a list, keeping results"),
	  (TO "if", "     -- condition testing"),
	  (TO "scan", "   -- loop over a list"),
	  (TO "while", "  -- loop control"),
	  (TO ";", "      -- statement separator")
	  },
     "Controlling the scope of variables:",
     MENU {
	  (TO ":=", "        -- assignment to and declaring a new local variable"),
	  (TO "global", "    -- using global symbols which have values"),
	  (TO "local", "     -- declaring new local symbols"),
	  (TO "quote", "     -- using symbols which have values")
	  },
     "Miscellaneous items:",
     MENU {
	  (TO "--", "                -- introducing comments"),
	  (TO "addEndFunction", "    -- do something upon exiting"),
	  (TO "addStartFunction", "  -- do something after loading dumped data"),
	  (TO "clearOutput", "       -- release some memory"),
	  (TO "clearAll", "          -- release more memory"),
	  (TO "Command", "           -- top level commands"),
     	  (TO "erase", "             -- remove a symbol"),
	  (TO "value", "             -- evaluate a string, symbol, or expression"),
	  (TO "memoize", "           -- memoizing functions"),
	  (TO "using methods", "     -- using methods"),
	  (TO "notImplemented", "    -- 'not implemented yet' error message"),
	  (TO "processArgs", "       -- process optional arguments to functions"),
	  (TO "protect", "           -- protecting the value of a symbol"),
	  (TO "runEndFunctions", "   -- run the ending functions"),
	  (TO "runStartFunctions", " -- run the starting functions"),
	  (TO "setrecursionlimit", " -- limits on recursion depth"),
	  SHIELD {
	       (TO "syntax", "            -- the syntax of the language")
	       },
	  (TO "value", "             -- getting values of symbols")
	  },
     "For internal use only:",
     MENU {
	  (TO "lineNumber", "        -- the current line number"),
	  (TO "lookupCount", "       -- how many times a symbol has been seen"),
	  (TO "phase", "             -- internal variable for compilation"),
--	  (TO "readExamples", "      -- used to control compilation"),
	  (TO "runStartFunctions", " -- run the start functions")
--	  (TO "writeExamples", "     -- used to control compilation")
	  }
     }

-- document { quote readExamples,
--      TT "readExamples", " -- a variable used to control the compilation
--      of the interpreted code of the system.",
--      PARA,
--      SEEALSO "writeExamples"
--      }

-- document { quote writeExamples,
--      TT "writeExamples", " -- a variable used to control the compilation
--      of the interpreted code of the system.",
--      PARA,
--      SEEALSO "readExamples"
--      }

document { quote "shield",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document { quote phase,
     TT "phase", " -- an internal variable indicating which phase of compilation we
     are in.",
     PARA,
     "The value 0 indicates that we are running as an interpreter, as usual.
     The value 1 indicates that we are loading setup.m2 and will dump data
     afterward.  The value 2 indicates that we are loading setup.m2, creating a
     preliminary version of the help file whose name is
     Macaulay2-pre, and creating example input files.  The value 3 indicates 
     that we are running an example input file, and referring to Macaulay2.pre.
     The value 4 indicates that we are loading setup.m2, reading the
     example output files, and creating the final version of the help file,
     called Macaulay2-doc.  The value 5 indicates that we are running the
     interpreter as usual, but reading the example output files when
     ", TO "document", " is used."
     }

document { quote lineNumber,
     TT "lineNumber()", " -- returns the current line number."
     }

document { quote backtrace,
     TT "backtrace()", " -- after an error, returns a list representing the
     steps in the computation that led to the error.",
     PARA,
     "The elements in the list are expressions that can be examined, or
     reevaluated with ", TO "value", ", or are references to positions in the 
     source code.",
     PARA,
     "Bug: some of the expressions are reconstructed from the local variables
     of the function returning an error, so the parameters passed to the
     routine may have been replaced by new values.",
     SEEALSO {"Expression", "Position"}
     }

document { quote Position,
     TT "Position", " -- a type of list designed to represent a position
     in a file.",
     PARA,
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document { "debugging",
     "Here are some debugging tools.",
     MENU {
	  (TO "assert", "           -- insist on something"),
	  (TO "backtrace", "        -- trace the evaluation chain after an error"),
	  (TO "benchmark", "        -- benchmark some code"),
	  (TO "browse", "           -- examine a list or hash table"),
	  (TO "code", "             -- display source code for a function"),
	  (TO "currentFile", "      -- name of current source file"),
	  (TO "edit", "             -- edit source code for a function"),
	  (TO "error", "            -- signalling an error"),
	  (TO "errorDepth", "       -- set the error depth"),
	  (TO "examine", "          -- examine socpes bound in a closure"),
	  (TO "flag", "             -- flag each use of a symbol"),
	  (TO "frame", "            -- get frame for a function closure"),
	  (TO "listUserSymbols", "  -- display global variables defined by user"),
     	  (TO "locate", "           -- locate the source code of a function"),
	  (TO "methods", "          -- find methods installed for a function"),
	  (TO "on", "               -- trace entry into a function"),
	  (TO "peek", "             -- print contents of something"),
	  (TO "peek2", "            -- print contents of something to a certain depth"),
	  (TO "profile", "          -- record run times for functions"),
	  (TO "shield", "           -- shield interpreted code from interrupts"),
	  (TO "try", "              -- catching errors"),
	  (TO "userSymbols", "      -- list global variables defined by user")
	  },
     "These functions are for debugging the kernel interpreter itself, and
     are not intended for users.",
     MENU {
	  (TO "buckets", "   -- display contents of buckets in a hash table"),
	  TO "seeParsing"
	  }
     }

document { quote currentFile,
     TT "currentFile", " -- a variable whose value is the name of the current
     source file."
     }

document { quote flag,
     TT "flag x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
     }

document { quote frame,
     TT "frame f", " -- provides the frame of values for local variables
     bound up in a function closure.",
     PARA,
     "This routine is provisional."
     }

document { quote examine,
     TT "examine ()", " -- list the sequence numbers for the scopes corresponding
     to the frames currently in use.", BR,NOINDENT, 
     TT "examine f", " -- display internal information about an interpreted 
     function ", TT "f", ".",BR,NOINDENT, 
     TT "examine x", " -- display internal information about a symbol ", TT "x", ".",
     PARA,
     "This function is intended for debugging the interpreter itself.",
     SEEALSO "debugging"
     }

document { quote seeParsing,
     TT "seeParsing()", " -- print the syntax table which governs parsing
     precedence."
     }

document { "subclass",
     "We say that a class X is a subclass of a class P if P is X, or
     P is the ", TO "parent", " of X, or P is the parent of the parent
     of X, and so on.  See also ", TO "classes", "."
     }

document { "classes",
     "Every thing ", TT "x", " belongs to a ", ITALIC "class", " ", TT "X", " -- a
     hash table that indicates in a weak sort of way what type of thing ", TT "x", "
     is.  We may also say that ", TT "x", " is an ", TO "instance", " 
     of ", TT "X", ".  The mathematical notion of a set ", TT "X", " and an
     element ", TT "x", " of ", TT "X", " can be 
     modeled this way.  The class of ", TT "x", " can be obtained with the function
     ", TO "class", ".",
     PARA,
     "Every thing ", TT "X", " also has a ", ITALIC "parent", " ", TT "P", ", which 
     indicates a larger class to which every instance ", TT "x", " of ", TT "X", " belongs.  We 
     also say that
     ", TT "X", " is a ", TO "subclass", " of P.  For example, the mathematical
     notion of a module P and a submodule ", TT "X", " may be modelled this way.
     The parent of ", TT "x", " can be obtained with the function ", TO "parent", ".",
     EXAMPLE {
	  "parent 2",
      	  "parent parent 2",
      	  "class 2",
      	  "parent class 2",
      	  "class class 2",
      	  "parent class class 2",
	  },
     PARA,
     "The classes and parents provide a uniform way for operations on
     things to locate the appropriate functions needed to perform them.
     Please see ", TO "using methods", " and ", TO "binary method", " now for a 
     brief discussion.",
     PARA,
     "For more details, see one of the topics below.",
     MENU {
	  TO "newClass",
	  TO "new",
	  TO "ancestor",
	  TO "instance"
	  },
     "For related topics, see one of the following.",
     MENU {
	  TO "uniform",
	  TO "Thing",
	  TO "Nothing",
	  TO "Type",
	  TO "MutableList",
	  TO "MutableHashTable",
	  TO "MutableHashTable",
	  TO "SelfInitializingType"
	  }
     }

document { quote instance,
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA,
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA,
     SEEALSO { "classes", "class", "parent" }
     }

document { "mathematics",
     "Here we document the mathematical objects which form the heart of 
     the system.",
     MENU{
	  TO "combinatorial functions",
     	  TO "Set",
	  TO "Monoid",
	  TO "Ring",
	  TO "Ideal",
	  TO "Module",
	  TO "ModuleMap",
	  TO "Matrix",
	  TO "GradedModule",
	  TO "ChainComplex",
	  TO "GroebnerBasis",
	  TO "MonomialIdeal",
	  TO "Variety",
	  TO "CoherentSheaf",
	  }
     }

document { "system",
     "Loading files:",
     MENU {
	  TO "autoload",
	  TO "initialization file",
	  TO "input",
	  TO "load",
	  TO "needs"
	  },
     "Dumping and restoring the state of the system:",
     MENU {
	  TO "dumpdata",
	  TO "loaddata",
	  TO "reloaded",
	  TO "restart",
	  TO "addStartFunction"
	  },
     "Interface to the operating system:",
     MENU{
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
	  TO "tmpname",
	  TO "wait"
	  },
     "Variables with information about the state of the current process:",
     MENU {
	  TO "commandLine",
	  TO "environment",
	  TO "version"
	  },
     "Miscellaneous commands:",
     MENU {
	  TO "getWWW"
	  },
     "Dealing with the garbage collector:",
     MENU {
	  TO "collectGarbage",
	  TO "gcDump"
	  }
     }

document { quote pathSeparator,
     TT "pathSeparator", " -- the character used under the current operating
     system to separate the component directory names in a file path.",
     PARA,
     "Under unix it is ", TT ///"/"///, ", and on a Macintosh it is
     ", TT ///":"///, "."
     }

document { quote alarm,
     TT "alarm n", " -- arrange for an interrupt to occur in ", TT "n", "
     seconds, cancelling any previously set alarm.",
     PARA,
     "If ", TT "n", " is zero, then no alarm is scheduled, and any
     previously scheduled alarm is cancelled.",
     PARA,
     "The value returned is the number of seconds  remaining  until  any
     previously  scheduled  alarm  was  due to be delivered, or
     zero if there was no previously scheduled alarm.",
     PARA,
     "This command could be used in concert with ", TO "try", " in order
     to abandon a computation that is taking too long.",
     PARA,
     "This command may interfere with ", TO "time", " on some systems,
     causing it to provide incorrect answers."
     }

document { "initialization file",
     "The file ", TT "init.m2", " is loaded automatically when the
     program is started.",
     PARA,
     "The file is sought in each of the directories of the ", TO "path", ",
     and also in the home directory of the user.  At most one file is loaded.",
     SEEALSO "load"
     }

document { quote Field,
     TT "Field", " -- the class of all fields.",
     PARA,
     "Some fields:",
     MENU {
	  TO "QQ",
	  TO "RR",
	  TO "CC"
	  },
     "Functions for creating fields:",
     MENU {
	  TO "frac",
	  TO "GF"
	  },
     "Functions that can be applied to fields and rings:",
     MENU {
	  TO "char",
	  TO "isField"
	  },
     EXAMPLE "isField (ZZ/101)",
     SEEALSO "coefficientRing"
     }

document { quote char,
     TT "char F", " -- returns the characteristic of the ring ", TT "F", ".",
     PARA,
     "The key ", TO "char", " is used to store the characteristic
     in F after it has been computed."
     }

document { quote basictype,
     TT "basictype x", " -- yields a symbol representing the basic type of ", TT "x", ".",
     PARA,
     "Every thing has basic type which tells what sort of thing it
     really is, internally.  It is not possible for the user to create 
     new basic types.",
     PARA,
     "The parent of a basic type is ", TO "Thing", ", and this property
     characterizes the basic types.",
     PARA,
     EXAMPLE "select(values symbolTable(), i -> parent value i === Thing)",
     SEEALSO "Thing"
     }

document { quote ++,
     TT "M ++ N", " -- direct sum for modules, matrices, or chain complexes and
     disjoint union for sets.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator with code
     such as ",
     PRE "         X ++ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     PARA,
     MENU {
	  TO (quote ++,ChainComplex,ChainComplex),
	  TO (quote ++,Module,Module),
	  TO (quote ++, Set, Set)
	  },
     SEEALSO {"classes", "directSum"}
     }

document { quote @@,
     TT "f @@ g", " -- a binary operator used for composition of functions.",
     PARA,
     "If ", TT "f", " and ", TT "g", " are homomorphisms of modules, then ", TT "f @@ g", " yields their
     composite as a homomorphism.",
     PARA,
     "The user may install other ", TO {"binary method", "s"}, " for this 
     operator with code such as ",
     PRE "         X @@ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { quote @,
     TT "x @ y", " -- a binary operator.",
     PARA,
     "This operator is right associative.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE "         X @ Y := (x,y) -> ...",
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", ".",
     SEEALSO {
	  (quote @, List, Function),
	  (quote @, OptionTable, Function),
	  }
     }

document { quote \,
     TT ///x \ y///, " -- a binary operator used for function application.",
     PARA,
     "This operator is right associative.",
     PARA,
     "The user may install ", TO {"binary method", "s"}, " for this operator 
     with code such as ",
     PRE ///         X \ Y := (x,y) -> ...///,
     "where ", TT "X", " is the class of ", TT "x", " and ", TT "Y", " is the
     class of ", TT "y", "."
     }

document { (quote /, List, Function),
     TT "w / f", " -- apply the function ", TT "f", " to each member of the 
     list or sequence ", TT "w"," returning a list or sequence containing the 
     results.  The same as ", TT "apply(w,f)", ".",
     PARA,
     "This operator is left associative, which means that ", TT "w / f / g", "
     is interpreted as meaning ", TT "(w / f) / g", ".",
     EXAMPLE "{1,2,3} / (i -> i+1) / (j -> j^2)",
     SEEALSO {"apply", (quote \,Function, List)}
     }

document { (quote \,Function, List),
     TT ///f \ w///, " -- apply the function ", TT "f", " to each member of the 
     list or sequence ", TT "w"," returning a list or sequence containing the 
     results.  The same as ", TT "apply(w,f)", ".",
     PARA,
     "This operator is right associative, which means that ", TT ///g \ f \ w///, "
     is interpreted as meaning ", TT ///g \ (f \ w)///, ".",
     EXAMPLE ///(j -> j^2) \ (i -> i+1) \ {1,2,3}///,
     "The precendence is lower than that of ", TT "@@", ".  Hence, the following 
     two examples yield the same result.",
     EXAMPLE {
	  ///sin \ sin \ {1,2,3}///,
      	  ///sin @@ sin \ {1,2,3}///,
	  },
     SEEALSO {"apply", "@@", (quote /,List, Function)}
     }

document { quote String,
     TT "String", " -- the class of all strings.",
     PARA,
     "A string is thing which contains a sequence of characters (bytes).
     A string is normally entered as a sequence of characters surrounded 
     by quotation marks.",
     PARA,
     EXAMPLE "\"abcd\"",
     PARA,
     "For an alternate method of entering strings which does not involve
     any escape sequences, see ", TO "///", ".",
     PARA,
     "A net is a two-dimensional array of characters, and strings are regarded
     as a type of net.  See ", TO "Net", ".",
     PARA,
     "Operations on strings:",
     MENU {
	  (TO "String # ZZ", " -- getting a character from a string"),
	  (TO quote #, " -- length of a string"),
 	  (TO (quote |, String, String), "        -- concatenation"),
 	  (TO "ascii", " -- ASCII conversion"),
 	  (TO "substring", " -- substring extraction"),
 	  (TO "concatenate", " -- concatenation"),
 	  (TO "characters", " -- extraction of characters"),
	  (TO "unhex", "      -- translate key strings from web browsers"),
 	  (TO "transnet", "   -- convert integers into network order"),
	  (TO "match", "      -- match patterns")
 	  }
     }

document { quote unhex,
     TT "unhex s", " -- translates a string provided as the trailing part
     of a URL by a web browser.",
     PARA,
     "A web browser sometimes has to put special characters into a URL.  So
     such characters do not interfere with other things, they are translated
     into a special form.  For example, each space is replaced by ", TT "+", "
     and various other characters (including ", TT "/", " and ", TT "%", ")
     are replaced by ", TT "%", " followed by two hex digits giving the numerical
     code of the character in hexadecimal notation.",
     PARA,
     "The purpose of ", TO "unhex", " is to undo such translations, providing
     the original string.",
     EXAMPLE {
	  ///unhex "abcd+efgh%2B"///
	  }
     }

document { "///",
     TT "/// a string ///", " -- a string.",
     PARA,
     "This method for entering a string involves no escape characters, so
     it can be used for easily inserting large chunks of text into a string
     without treating the characters ", TT "\\", " and ", TT "\"", " specially.",
     EXAMPLE {
	  "/// \\ \" ///",
      	  "ascii oo",
	  },
     SEEALSO "String"
     }

document { quote Net,
     TT "Net", " -- the class of all nets.",
     PARA,
     "A net is a generalization of a string which is designed to facilitate
     two-dimensional printing on ascii terminals.  It consists of a rectangular
     array of characters subdivided horizontally by an imaginary baseline.",
     PARA,
     "Operations on nets also accept strings by interpreting a string as a rectangle
     of height one with the baseline just below it.  In fact, the parent of
     ", TO "String", " is ", TO "Net", ".",
     PARA,
     "Multiple nets per line can be sent to an output file with ", TO "<<", "
     but care must be taken to use ", TO "endl", " to end lines, for nets with
     new line characters embedded in them will be displayed in an unexpected way.",
     PARA,
     "Warning: if so many characters are written to a file that an internal buffer
     is filled before the line ends or first net is seen, then the buffer will be 
     flushed, and writing a net subsequently will produce an unexpected result.",
     PARA,
     "Operations on nets:",
     MENU {
	  TO (quote |, String, String),
	  TO (quote ||, Net, Net),
	  TO (quote ^,Net, ZZ),
	  TO "depth",
	  TO "height",
	  TO "horizontalJoin",
	  TO "netRows",
	  TO "stack",
	  TO "width",
	  },
     "Formatting expressions:",
     MENU {
	  TO "net"
	  },
     SEEALSO "String"
     }

document { quote net,
     TT "net x", " -- format ", TT "x", " for printing.",
     PARA,
     "This function is the primary function called upon by ", TO "<<", " to
     format expressions for printing.  The default method provided by the
     system is to convert ", TT "x", " to an ", TO "Expression", " with 
     ", TO "expression", " and then to convert that to a net.",
     PARA,
     "A new method for formatting expressions of class ", TT "X", " may be
     installed by the user with code of the form ", TT "net X := x -> ...", ".
     The function provided by the user should return a net or a string.",
     PARA,
     "A string is formatted by wrapping it in quotation marks and converting
     nonprinting characters to escape sequences.  A net is formatted for printing
     by enclosing it in a box.",
     EXAMPLE {
	  "\"a string\"",
      	  "net \"a string\"",
	  },
     EXAMPLE {
	  "ZZ[x];",
      	  "x^2",
      	  "net x^2",
	  },
     EXAMPLE "code(net,List)",
     PARA,
     SEEALSO {"Net", "expression", "Expression", "Net"}
     }

document { quote horizontalJoin,
     TT "horizontalJoin(m,n,...)", " -- joins nets or strings by concatenating
     them horizontally.  The baselines in each of the nets are aligned
     appropriately.",
     PARA,
     "Nested sequences among the arguments are first spliced together.",
     PARA,
     "If there are no arguments, then the net returned has zero height and
     zero depth.  This might be unexpected.",
     SEEALSO {"Net", (quote |, String, String)}
     }

document { quote stack,
     TT "stack(m,n,...)", " -- joins nets or strings by concatenating
     them vertically.  The baseline of the result is the baseline of the
     first argument.",
     PARA,
     "Nested sequences among the arguments are first spliced together.",
     PARA,
     "If there are no arguments, then the net returned has zero height and
     zero depth.  This might be unexpected.",
     PARA,
     "Tab characters in any of the strings are first expanded into spaces,
     assuming tab stops at every eighth column.",
     SEEALSO {"Net", (quote ||, Net, Net)}
     }

document { (quote ^, Net, ZZ),
     TT "n^i", " -- elevates a net or string ", TT "n", " by raising its
     characters by ", TT "i", " rows.",
     PARA,
     "The number ", TT "i", " may be negative, in which case the net is
     lowered.",
     PARA,
     "If ", TT "n", " is a string, then ", TT "n^0", " is an easy way to convert
     it to a net."
     }

document { quote width,
     TT "width f", " -- determines the width of the terminal associated to an
     output file ", TT "f", ", if any.", BR,NOINDENT, 
     TT "width n", " -- the width of a net ", TT "n", ".",
     SEEALSO {"Net", "File"}
     }

document { quote height,
     TT "height n", " -- the height of a net ", TT "n", ".",
     PARA,
     "The height of a net is the number of rows of characters it has above
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "depth"}
     }

document { quote depth,
     TT "depth n", " -- the depth of a net ", TT "n", ".",
     PARA,
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "height"}
     }

document { "String # ZZ",
     TT "s#i", " -- produce the ", TT "i", "-th character from a string ", TT "s", ".",
     PARA,
     "If ", TT "i", " is negative and the length of the string is ", TT "n", ", then
     the (n-i)-th character is provided.",
     PARA,
     SEEALSO "String"
     }

document { quote class,
     TT "class x", " -- yields the class of x.",
     PARA,
     SEEALSO "classes"
     }

document { "assignment",
     MENU {
     	  (TO "=", "   -- assignment"),
     	  (TO ":=", "  -- assignment to a new local variable, or 
	       installation of a method"),
     	  (TO "<-", "  -- assignment via overlay")
	  }
     }

document { "comparison",
     MENU {
	  (TO "===", "        -- strict equality"),
	  (TO "=!=", "        -- strict inequality"),
	  (TO "==", "         -- equality"),
	  (TO "!=", "         -- inequality"),
	  (TO "<", "          -- less than"),
	  (TO "<=", "         -- less than or equal"),
	  (TO ">", "          -- greater than"),
	  (TO ">=", "         -- greater than or equal"),
	  (TO "?", "          -- comparison")
	  }
     }

document { "combinatorial functions",
     MENU {
	  (TO "random", "               -- random real number or integer"),
	  (TO "binomial", "             -- binomial coefficients"),
	  (TO "subsets", "              -- subsets"),
	  (TO "tally", "                -- tally occurrences in a list"),
	  (TO "partitions", "           -- partitions of an integer")
	  }
     }

document { quote hash,
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA,
     "The hash code of ", TT "x", " is an integer produced in a deterministic way
     from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
     See ", TO "hashing", " for a discussion of the requirements that
     the hash codes used here are designed to satisfy."
     }

document { quote remove,
     TT "remove(x,k)", " -- removes the entry stored in the hash table ", TT "x", "
     under the key ", TT "k", ".",
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
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
	  TO "Print",
	  TO "NoPrint"
	  }
     }

document { quote BasicList,
     TT "BasicList", " -- the class of all things represented internally as a
     list.  A list is a sequence of expressions indexed by integers
     0, 1, ..., N-1, where N is the length of the sequence.",
     PARA,
     "The reason for distinguishing ", TO "List", " from ", TO "BasicList", "
     is so lists can be treated as vectors, without everything else
     implemented as a basic list inheriting that behavior.",
     PARA,
     "Other types of list:",
     MENU {
	  TO "Array",
	  TO "List",
	  TO "MutableList"
	  },
     }

document { quote toSequence,
     TT "toSequence x", " -- yields the elements of a list ", TT "x", " as a sequence.",
     PARA,
     "If ", TT "x", " is a sequence, then ", TT "x", " is returned.",
     PARA,
     EXAMPLE {
	  "toSequence {1,2,3}",
      	  "toSequence (1,2,3)"
	  },
     }

document { quote Boolean,
     TT "Boolean", " -- the class whose two members are ", TO "true", " and
     ", TO "false", ".",
     PARA,
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA,
     EXAMPLE "3 == 4",
     PARA,
     "Boolean constants:",
     MENU {
	  TO "false",
	  TO "true"
	  },
     PARA,
     "Functions dealing with truth values.",
     MENU {
	  (TO "not", "     -- negation"),
	  (TO "and", "     -- conjunction"),
	  (TO "or", "      -- disjunction"),
	  (TO "if", "      -- condition testing"),
	  (TO "select", "  -- selection of elements"),
	  (TO "while", "   -- loop control")
	  }
     }

document { "numbers",
     "There are four types of numbers:",
     MENU {
	  (TO "CC", " -- the class of complex numbers."),
	  (TO "QQ", " -- the class of rational numbers."),
	  (TO "RR", " -- the class of real numbers."),
	  (TO "ZZ", " -- the class of integers.")
	  },
     "Operations on numbers:",
     MENU {
	  TO "arithmetic functions",
	  TO "integrate",
	  TO "transcendental functions"
	  },
     "Standard predefined numbers:",
     MENU {
          (TO "pi", " -- pi."),
          (TO "ii", " -- the square root of -1.")
	  },
     "Some other quantities which are not quite numbers:",
     MENU {
	  TO "infinity",
	  TO "-infinity",
	  TO "indeterminate"
	  }
     }

document { quote Symbol,
     TT "Symbol", " -- denotes the class of all symbols.",
     PARA,
     "Symbols are entered as an alphabetic character followed by a
     sequence of alphanumeric characters; case is significant.
     The single quote character ' is regarded as alphabetic, so that
     symbols such as ", TT "x'", " may be used.",
     PARA,
     "Symbols are used as names for values to be preserved, as indeterminates
     in polynomial rings, and as keys in hash tables.  They may have
     global scope, meaning they are visible from every line of code,
     or local scope, with visibility restricted to a single file or
     function body.",
     PARA,
     EXAMPLE "ab12345cde",
     PARA,
     SEEALSO {"symbolTable", "local", "global", "quote", ":="}
     }

document { quote File,
     TT "File", " -- the class of all files.",
     PARA,
     "Files may be input files, output files, pipes, or sockets.
     The class of all files is ", TO "File", ".",
     PARA,
     "Some standard files, already open:",
     MENU {
          (TO "stdio", "    -- standard input output file"),
          (TO "stderr", "   -- standard error output file")
	  },
     "Ways to create new files:",
     MENU {
          (TO "openIn", "        -- open an input file"),
          (TO "openOut", "       -- open an output file"),
          (TO "openInOut", "     -- open an input output file"),
          (TO "openListener", "  -- open a listener"),
	  },
     "Ways to change the state of files:",
     MENU {
	  (TO "echoOff", " -- set echoing off"),
	  (TO "echoOn", "  -- set echoing on"),
	  },
     "Input operations:",
     MENU {
          (TO "getc", "        -- get one character from a file"),
          (TO "get", "         -- get contents of a file"),
	  (TO "read", "        -- get some bytes from a file"),
	  (TO "atEndOfFile", " -- whether the file is at the end"),
	  (TO "isReady", "     -- whether data is available from a file, or it's at the end"),
	  },
     "Further processing for data obtained from a file:",
     MENU {
          (TO "lines", "    -- split a string into lines")
	  },
     "Output operations:",
     MENU {
          (TO "<<", "         -- print to file"),
	  (TO "endl", "       -- end a line"),
          (TO "flush", "      -- flush a file"),
	  -- (TO "netscape", "   -- call netscape to display an expression"),
	  (TO "printString", "  -- print a generalized string"),
          (TO "print", "      -- print an expression on a line"),
	  (TO "TeX", "        -- call TeX to display an expression")
	  },
     "Preparing expressions for output:",
     MENU {
          TO "columnate",
	  TO "expression",
	  TO "format",
          TO "name",
          {TO "null", "     -- a symbol which doesn't print"},
          TO "pad",
          TO "string",
	  TO "tex",
	  TO "toString"
	  },
     "Destroying files:",
     MENU {
          {TO "close", "    -- close a file"},
          {TO "closeIn", "  -- close a file for input"},
          {TO "closeOut", " -- close a file for output"},
	  {TO "kill", "     -- kill a process associated with a file"},
	  },
     "Information about files",
     MENU { 
	  {TO "width", "        -- width of a terminal"},
          {TO "openFiles", "    -- list open files"},
	  {TO "isOpenFile", "   -- whether it's an open file"},
	  {TO "isInputFile", "  -- whether it's an input file"},
	  {TO "isOutputFile", " -- whether it's an output file"},
	  {TO "isListener", "   -- whether it's a listener"},
	  },
     }

ccc := echoOn
erase quote echoOn
echoOn = new Command from ccc

ccc = echoOff
erase quote echoOff
echoOff = new Command from ccc

document { quote echoOn,
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document { quote echoOff,
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document { quote printString,
     TT "printString(o,s)", " -- send the string ", TT "s", " to the output file ", TT "o", ".",
     PARA,
     "The argument ", TT "s", " may also be a sequence or list, in which case
     its elements are printed.  If an integer is encountered, then
     it specifies a number of spaces to be printed.  If a symbol
     or indeterminate is encountered, its name is printed.  If ", TT "null", "
     is encountered, nothing is printed.",
     PARA,
     EXAMPLE ///printString(stdio, (a,10,"b",20,c))///
     }

document { "help functions",
     "Online Macaulay 2 documentation is stored in ", TO "hypertext", "
     form.",
     PARA,
     NOINDENT,
     "Functions for accessing the documentation:",
     MENU {
	  TO "apropos",
	  TO "doc",
	  TO "examples",
	  TO "help", 
	  TO "topicList", 
	  TO "topics"
	  },
     "How to write documentation yourself:",
     MENU {
	  TO "document",
	  TO "hypertext",
	  },
     "Output formatting routines:",
     MENU {
	  TO "html",
	  TO "mathML",
	  TO "tex",
	  TO "text",
	  },
     "Some internals:",
     MENU {
	  TO "Documentation",
	  TO "phase",
	  },
     SEEALSO "reading the documentation"
     }

document { "arithmetic functions",
     "These arithmetic functions act on numbers, but some of them
     are also act on more abstract entities, such as polynomials.",
     MENU {
	  (TO "+", "          -- addition"),
	  (TO "plus", "       -- addition"),
	  (TO "-", "          -- subtraction and minus"),
	  (TO "minus", "      -- minus"),
	  (TO "difference", " -- subtraction"),
          (TO "*", "          -- multiplication"),
          (TO "times", "      -- multiplication"),
          (TO "/", "          -- division"),
          (TO "//", "         -- quotient"),
          (TO "\\\\", "         -- left quotient"),
          (TO "%", "          -- remainder"),
	  (TO "mod", "        -- reduction modulo n"),
          (TO "^", "          -- power"),
          (TO "power", "      -- power"),
          (TO "!", "          -- factorial"),
          (TO "xor", "        -- bitwise EXCLUSIVE OR of two integers"),
          (TO "&", "          -- bitwise AND of two integers"),
          (TO "|", "          -- bitwise OR of two integers"),
          (TO "<<", "         -- shift bits"),
          (TO ">>", "         -- shift bits"),
          (TO "gcd", "        -- greatest common divisor"),
          (TO "odd", "        -- predicate for odd integers"),
          (TO "even", "       -- predicate for even integers"),
          (TO "ceiling", "    -- ceiling"),
          (TO "floor", "      -- floor"),
	  (TO "isPrime", "    -- primality test"),
	  (TO "factor", "     -- factor"),
          (TO "Numeric", "    -- numeric conversion")
	  }
     }

document { "transcendental functions",
     MENU {
	  (TO "abs", "   -- absolute value"),
	  (TO "sin", "   -- sine"),
	  (TO "cos", "   -- cosine"),
	  (TO "tan", "   -- tangent"),
	  (TO "asin", "  -- arcsine"),
	  (TO "acos", "  -- arccosine"),
	  (TO "atan", "  -- arctangent"),
	  (TO "sinh", "  -- hyperbolic sine"),
	  (TO "cosh", "  -- hyperbolic cosine"),
	  (TO "tanh", "  -- hyperbolic tangent"),
	  (TO "exp", "   -- exponential"),
	  (TO "log", "   -- logarithm"),
	  (TO "sqrt", "  -- square root")
	  }
     }

document { quote mutable,
     TT "mutable x", " -- returns true or false, depending on whether x is mutable.",
     PARA,
     "If ", TT "x", " is a hash table, list, or database, then it's mutable if its contents
     can be destructively altered.",
     PARA,
     "If ", TT "x", " is a symbol, then it's mutable if a value can be assigned to
     it. (See ", TO "protect", ".)",
     PARA,
     "If ", TT "x", " is anything else, then it isn't mutable.",
     PARA,
     "The contents of a mutable hash table do not participate in strong comparison
     with ", TO "===", " or in ", TO "hashing", ".",
     SEEALSO {"MutableList", "MutableHashTable"}
     }
