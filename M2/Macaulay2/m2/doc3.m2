--		Copyright 1993-1998 by Daniel R. Grayson

document { "shield",
     HEADLINE "shield evaluation from interrupts",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document { symbol phase,
     HEADLINE "compilation phase",
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

document { lineNumber,
     HEADLINE "current line number",
     TT "lineNumber()", " -- returns the current line number.",
     BR, NOINDENT,
     TT "lineNumber n", " -- sets the line number to ", TO "n", " and
     returns the old line number.",
     }

document { backtrace,
     HEADLINE "trace back through evaluations after an error",
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

document { Position,
     HEADLINE "the class of all file positions",
     TT "Position", " -- a type of list designed to represent a position
     in a file.",
     PARA,
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document { symbol "currentFile",
     HEADLINE "the current source file",
     TT "currentFile", " -- a variable whose value is the name of the current
     source file."
     }

document { flag,
     HEADLINE "flag a symbol",
     TT "flag x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
     }

document { frame,
     HEADLINE "the frame of a function",
     TT "frame f", " -- provides the frame of values for local variables
     bound up in a function closure.",
     PARA,
     "It also works when ", TT "f", " is a symbol, and returns the frame for the
     scope it was defined in, even if the function that created it has
     returned.",
     PARA,
     "The return value is a mutable list, so it's possible to modify the values
     of the local variables, but it's probably not a good idea.",
     PARA,
     "Exception: if the scope of ", TT "f", " is the global scope, then an
     empty list is returned instead, since some of the global variables are protected.",
     PARA,
     "This function is provisional, and is to be used mainly for debugging."
     }

document { examine,
     HEADLINE "examine internal information about functions or symbols",
     TT "examine ()", " -- list the sequence numbers for the scopes corresponding
     to the frames currently in use.", BR,NOINDENT, 
     TT "examine f", " -- display internal information about an interpreted 
     function ", TT "f", ".",BR,NOINDENT, 
     TT "examine x", " -- display internal information about a symbol ", TT "x", ".",
     PARA,
     "This function is intended for debugging the interpreter itself.",
     SEEALSO "debugging"
     }

document { seeParsing,
     HEADLINE "print syntax table",
     TT "seeParsing()", " -- print the syntax table which governs parsing
     precedence."
     }

document { instance,
     HEADLINE "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA,
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA,
     SEEALSO { "classes", "class", "parent" }
     }

document { symbol "pathSeparator",
     HEADLINE "path separator for filenames",
     TT "pathSeparator", " -- the character used under the current operating
     system to separate the component directory names in a file path.",
     PARA,
     "Under unix it is ", TT ///"/"///, ", and on a Macintosh it is
     ", TT ///":"///, "."
     }

document { alarm,
     HEADLINE "set an alarm",
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

document { Field,
     HEADLINE "the class of all fields",
     "Use ", TO "isField", " to determine whether a given ring is a field,
     since some rings that are fields, for implementation reasons, cannot
     be instances of ", TO "Field", ".",
     EXAMPLE "isField (ZZ/101)"
     }

document { char,
     HEADLINE "the characteristic of a field or ring",
     TT "char F", " -- returns the characteristic of the ring ", TT "F", ".",
     PARA,
     "The key ", TO "char", " is used to store the characteristic
     in F after it has been computed."
     }

document { basictype,
     HEADLINE "the basic type",
     TT "basictype x", " -- yields the class representing the basic type of ", TT "x", ".",
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

document { symbol "++",
     HEADLINE "a binary operator"
     }

document { symbol "@@",
     HEADLINE "a binary operator"
     }

document { symbol "==>",
     HEADLINE "a binary operator",
     "This operator is right associative.",
     PARA,
     "New methods must be installed with ", TO "installMethod", ", because the
     parsing precedence is so low!  This will be fixed eventually."
     }

document { symbol "@",
     HEADLINE "a binary operator",
     "This operator is right associative."
     }

document { symbol "\\",
     HEADLINE "a binary operator"
     }

document { (symbol /, VisibleList, Function),
     HEADLINE "apply a function to each member of a list",
     TT "w / f", " -- apply the function ", TT "f", " to each member of the 
     list or sequence ", TT "w"," returning a list or sequence containing the 
     results.  The same as ", TT "apply(w,f)", ".",
     PARA,
     "This operator is left associative, which means that ", TT "w / f / g", "
     is interpreted as meaning ", TT "(w / f) / g", ".",
     EXAMPLE "{1,2,3} / (i -> i+1) / (j -> j^2)",
     SEEALSO {"apply", (symbol \,Function, List)}
     }

document { (symbol \,Function, VisibleList),
     HEADLINE "apply a function to each member of a list",
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
     SEEALSO {"apply", "@@", (symbol /,List, Function)}
     }

document { String,
     HEADLINE "the class of all strings",
     "A string is thing which contains a sequence of characters (bytes).
     A string is normally entered as a sequence of characters surrounded 
     by quotation marks.",
     PARA,
     EXAMPLE "\"abcd\"",
     PARA,
     "For an alternate method of entering strings which does not involve
     any escape sequences, see ", TO "///", ".",	    -- ///
     PARA,
     "A net is a two-dimensional array of characters, and strings are regarded
     as a type of net.  See ", TO "Net", ".",
     }

document { unhex,
     HEADLINE "translate a URL",
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
     HEADLINE "delineate a string",
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

document { Net,
     HEADLINE "the class of all nets and strings",
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
     flushed, and writing a net subsequently will produce an unexpected result."
     }

document { net, 
     HEADLINE "convert to net",
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
	  }
     }

document { horizontalJoin,
     HEADLINE "join nets or strings horizontally",
     TT "horizontalJoin(m,n,...)", " -- joins nets or strings by concatenating
     them horizontally.  The baselines in each of the nets are aligned
     appropriately.",
     PARA,
     "Nested sequences among the arguments are first spliced together.",
     PARA,
     "If there are no arguments, then the net returned has zero height and
     zero depth.  This might be unexpected.",
     PARA,
     "Null arguments are allowed and ignored.",
     SEEALSO {"Net", (symbol |, String, String)}
     }

document { stack,
     HEADLINE "join nets or string vertically",
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
     PARA,
     "Null arguments are allowed and ignored.",
     SEEALSO { (symbol ||, Net, Net)}
     }

document { (symbol ^, Net, ZZ),
     HEADLINE "raise a net",
     TT "n^i", " -- elevates a net or string ", TT "n", " by raising its
     characters by ", TT "i", " rows.",
     PARA,
     "The number ", TT "i", " may be negative, in which case the net is
     lowered.",
     PARA,
     "If ", TT "n", " is a string, then ", TT "n^0", " is an easy way to convert
     it to a net."
     }

document { width,
     HEADLINE "width of a file or net",
     TT "width f", " -- determines the width of the terminal associated to an
     output file ", TT "f", ", if any.", BR,NOINDENT, 
     TT "width n", " -- the width of a net ", TT "n", ".",
     SEEALSO {"Net", "File"}
     }

document { height,
     HEADLINE "height of a net",
     TT "height n", " -- the height of a net ", TT "n", ".",
     PARA,
     "The height of a net is the number of rows of characters it has above
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "depth"}
     }

document { depth,
     HEADLINE "depth of a net",
     TT "depth n", " -- the depth of a net ", TT "n", ".",
     PARA,
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "height"}
     }

document { class,
     HEADLINE "class of an object",
     TT "class x", " -- yields the class of ", TT "x", ".",
     PARA,
     SEEALSO "classes"
     }

document { hash,
     HEADLINE "hash code of an object",
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA,
     "The hash code of ", TT "x", " is an integer produced in a deterministic way
     from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
     See ", TO "hashing", " for a discussion of the requirements that
     the hash codes used here are designed to satisfy."
     }

document { remove,
     HEADLINE "remove an entry from a hash table",
     TT "remove(x,k)", " -- removes the entry stored in the hash table ", TT "x", "
     under the key ", TT "k", ".",
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
	  }
     }

document { BasicList,
     HEADLINE "the class of all basic lists",
     "A list is a sequence of expressions indexed by integers
     ", TT "0", ", ", TT "1", ", ..., ", TT "N-1", ", where ", TT "N", " is the length of the sequence.",
     PARA,
     "The reason for distinguishing ", TO "List", " from ", TT "BasicList", "
     is so lists can be treated as vectors, without everything else
     implemented as a basic list inheriting that behavior.",
     }

document { toSequence,
     HEADLINE "convert to sequence",
     TT "toSequence x", " -- yields the elements of a list ", TT "x", " as a sequence.",
     PARA,
     "If ", TT "x", " is a sequence, then ", TT "x", " is returned.",
     PARA,
     EXAMPLE {
	  "toSequence {1,2,3}",
      	  "toSequence (1,2,3)"
	  },
     }

document { Boolean,
     HEADLINE "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA,
     "Special operators dealing with truth values.",
     SHIELD MENU {
	  TO "not",
	  TO "and",
	  TO "or",
	  TO "if",
	  TO "while"
	  }
     }

document { Symbol,
     HEADLINE "the class of all symbols",
     "Symbols are entered as an alphabetic character followed by a
     sequence of alphanumeric characters; case is significant.
     The single symbol character ' is regarded as alphabetic, so that
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
     SEEALSO {"symbolTable", "local", "global", "symbol", ":="}
     }

document { File,
     HEADLINE "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", "."
     }

document { connectionCount,
     HEADLINE "the number of connections",
     TT "connectionCount f", " -- returns the number of connections accepted by 
     the listener ", TT "f", " so far."
     }

ccc := echoOn
erase symbol echoOn
echoOn = Command ccc

ccc = echoOff
erase symbol echoOff
echoOff = Command ccc

document { echoOn,
     HEADLINE "turn on echoing",
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document { echoOff,
     HEADLINE "turn off echoing",
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document { printString,
     HEADLINE "lowlevel function to print a string",
     TT "printString(o,s)", " -- send the string ", TT "s", " to the output file ", TT "o", ".",
     PARA,
     "This function is intended for internal use only.",
     PARA,
     "The argument ", TT "s", " may also be a sequence or list, in which case
     its elements are printed.  If an integer is encountered, then
     it specifies a number of spaces to be printed.  If a symbol
     or indeterminate is encountered, its name is printed.  If ", TT "null", "
     is encountered, nothing is printed.",
     PARA,
     EXAMPLE ///printString(stdio, (a,10,"b",20,c))///
     }

document { mutable,
     HEADLINE "whether something may be modified",
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

document { setEcho,
     HEADLINE "turn on echoing",
     TT "setEcho stdio", " -- turn on echoing of characters typed to the standard
     input."
     }

document { clearEcho,
     HEADLINE "turn off echoing",
     TT "clearEcho stdio", " -- turn off echoing of characters typed to the standard
     input."
     }
