--		Copyright 1993-1998 by Daniel R. Grayson

document { "shield",
     Headline => "shield evaluation from interrupts",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document { symbol phase,
     Headline => "compilation phase",
     TT "phase", " -- an internal variable indicating which phase of compilation we
     are in.",
     PARA,
     "The value 0 indicates that we are running as an interpreter, as usual.
     The value 1 indicates that we are loading setup.m2 and will dump data
     afterward.  The value 2 indicates that we are loading setup.m2, creating a
     preliminary version of the help file (whose name is
     ", TT "Macaulay2-pre", "), and creating example input files.  The value 3 indicates 
     that we are running an example input file, and referring to Macaulay2.pre.
     The value 4 indicates that we are loading setup.m2, printing warning
     messages if the example output files are missing, and creating the final
     version of the help file, called Macaulay2-doc.  The value 5 indicates
     that we are reading creating the html files."
     }

document { lineNumber,
     Headline => "current line number",
     TT "lineNumber()", " -- returns the current line number.",
     BR, NOINDENT,
     TT "lineNumber n", " -- sets the line number to ", TO "n", " and
     returns the old line number.",
     }

document { backtrace,
     Headline => "trace back through evaluations after an error",
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
     Headline => "the class of all file positions",
     TT "Position", " -- a type of list designed to represent a position
     in a file.",
     PARA,
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document { symbol "currentFileName",
     Headline => "the current source file",
     TT "currentFileName", " -- a variable whose value is the name of the current
     source file."
     }

document { flag,
     Headline => "flag a symbol",
     TT "flag x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
     }

document { frame,
     Headline => "the frame of a function",
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
     Headline => "examine internal information about functions or symbols",
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
     Headline => "print syntax table",
     TT "seeParsing()", " -- print the syntax table which governs parsing
     precedence."
     }

document { instance,
     Headline => "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA,
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA,
     SEEALSO { "classes and types", "class", "parent" }
     }

document { symbol "pathSeparator",
     Headline => "path separator for filenames",
     TT "pathSeparator", " -- the character used under the current operating
     system to separate the component directory names in a file path.",
     PARA,
     "Under unix it is ", TT ///"/"///, ", and on a Macintosh it is
     ", TT ///":"///, "."
     }

document { alarm,
     Headline => "set an alarm",
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

document { char,
     Headline => "the characteristic of a field or ring",
     TT "char F", " -- returns the characteristic of the ring ", TT "F", ".",
     PARA,
     "The key ", TO "char", " is used to store the characteristic
     in F after it has been computed."
     }

document { basictype,
     Headline => "the basic type of an object",
     Synopsis => {
	  "T = basictype x",
	  "x" => "anything",
	  "T" => { "class representing the basic type of ", TT "x" }
	  },
     "Every thing has basic type which tells what sort of thing it
     really is, internally.",
     PARA,
     "The parent of a basic type is ", TO "Thing", ", and this property
     characterizes the basic types.  The basic type of an object ", TT "x", "
     is defined to be the ancestor of the class of ", TT "x", " that is a
     basic type.",
     PARA,
     "Let's compute a list of all the basic types:",
     EXAMPLE ///stack sort (toString \
     select(values symbolTable(), i -> parent value i === Thing)
     )///,
     SEEALSO "basic types"
     }

document { symbol "++",
     Headline => "a binary operator, usually used for direct sum"
     }

document { symbol "@@",
     Headline => "a binary operator"
     }

document { symbol "==>",
     Headline => "a binary operator",
     "This operator is right associative.",
     PARA,
     "New methods must be installed with ", TO "installMethod", ", because the
     parsing precedence is so low!  This will be fixed eventually."
     }

document { symbol "@",
     Headline => "a binary operator",
     "This operator is right associative."
     }

document { symbol "\\",
     Headline => "a binary operator"
     }

document { (symbol /, VisibleList, Function),
     Headline => "apply a function to each member of a list",
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
     Headline => "apply a function to each member of a list",
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
     Headline => "the class of all strings",
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
     Headline => "translate a URL",
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

document { "///",					    -- ///
     Headline => "delineate a string",
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
     Headline => "the class of all nets and strings",
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
     Headline => "convert to net",
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
     Headline => "join nets or strings horizontally",
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
     Headline => "join nets or string vertically",
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
     Headline => "raise a net",
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
     Headline => "width of a file or net",
     TT "width f", " -- determines the width of the terminal associated to an
     output file ", TT "f", ", if any.", BR,NOINDENT, 
     TT "width n", " -- the width of a net ", TT "n", ".",
     SEEALSO {"Net", "File"}
     }

document { height,
     Headline => "height of a net",
     TT "height n", " -- the height of a net ", TT "n", ".",
     PARA,
     "The height of a net is the number of rows of characters it has above
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "depth"}
     }

document { depth,
     Headline => "depth of a net",
     TT "depth n", " -- the depth of a net ", TT "n", ".",
     PARA,
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SEEALSO {"Net", "height"}
     }

document { class,
     Headline => "class of an object",
     TT "class x", " -- yields the class of ", TT "x", ".",
     PARA,
     SEEALSO "classes and types"
     }

document { hash,
     Headline => "hash code of an object",
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA,
     "The hash code of ", TT "x", " is an integer produced in a deterministic way
     from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
     See ", TO "hashing", " for a discussion of the requirements that
     the hash codes used here are designed to satisfy."
     }

document { remove,
     Headline => "remove an entry from a hash table",
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
     Headline => "the class of all basic lists",
     "A list is a sequence of expressions indexed by integers
     ", TT "0", ", ", TT "1", ", ..., ", TT "N-1", ", where ", TT "N", " is the length of the sequence.",
     PARA,
     "The reason for distinguishing ", TO "List", " from ", TT "BasicList", "
     is so lists can be treated as vectors, without everything else
     implemented as a basic list inheriting that behavior.",
     }

document { toSequence,
     Headline => "convert to sequence",
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
     Headline => "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA,
     "Special operators dealing with truth values.",
     SHIELD MENU {
	  TO "not",
	  TO "and",
	  TO "or",
	  TO "if"
	  }
     }

document { Symbol,
     Headline => "the class of all symbols",
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
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", "."
     }

document { connectionCount,
     Headline => "the number of connections",
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
     Headline => "turn on echoing",
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document { echoOff,
     Headline => "turn off echoing",
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document { printString,
     Headline => "lowlevel function to print a string",
     TT "printString(o,s)", " -- send the string ", TT "s", " to the output file ", TT "o", ".",
     PARA,
     "This function is intended for internal use only.",
     PARA,
     "The argument ", TT "s", " may also be a sequence or list, in which case
     its elements are printed.  If an integer is encountered, then
     it specifies a number of spaces to be printed.  If a symbol
     or indeterminate is encountered, its name is printed.  If ", TO "null", "
     is encountered, nothing is printed.",
     PARA,
     EXAMPLE ///printString(stdio, (a,10,"b",20,c))///
     }

document { mutable,
     Headline => "whether something may be modified",
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
     Headline => "turn on echoing",
     TT "setEcho stdio", " -- turn on echoing of characters typed to the standard
     input."
     }

document { clearEcho,
     Headline => "turn off echoing",
     TT "clearEcho stdio", " -- turn off echoing of characters typed to the standard
     input."
     }

document { CacheTable,
     Headline => "hash tables for caching",
     "A type of mutable hash table designed for caching computed values that
     could always be recomputed.  Cache tables are designed so their contents
     will not participate in any comparisons by the strict comparison
     operator ", TT "===", ".  To that end, any two cache tables with the same
     class and parent are considered equal to each other and have hash code equal to 0."
     }


document { cacheFileName,
     Headline => "produce the name of a cache file",
     "Macaulay 2 needs to remember some bits of data from one invocation to the next,
     so it stores this data in cache files.  The name and directory of the cache file
     depend on the data being stored.  For example, the input code for examples associated
     with one of the documentation node will be stored in a file whose name depends on
     the name of the node."
     }

document { (cacheFileName, String, String),
     Synopsis => {
	  ///fn = cacheFileName(prefix,key)///,
	  "prefix" => "the prefix from which to construct the file name",
	  "key" => "a key, which can be anything"
	  },
     "A file name is returned that depends only on the prefix and the key.  The
     prefix should be the path to a directory, together with a terminating path component
     separator.  When the program terminates, a file called
     ", TT "Macaulay2-index-cache", " will be created or updated, if
     necessary, in which to store the table of correspondences between keys
     and filenames.",
     EXAMPLE {
	  ///cacheFileName("./tmp/","algebra")///,
	  ///cacheFileName("./tmp/","14")///,
	  ///cacheFileName("./tmp/","14")///
	  }
     }

document { (cacheFileName, String, String, String),
     Synopsis => {
	  ///fn = cacheFileName(prefix,key,base)///,
	  "prefix" => "the prefix from which to construct the file name",
	  "key" => "a key, which can be anything",
	  "base" => "the base part of the file name",
	  "fn" => "a new file name"
	  },
     "A file name ", TT "fn", " is constructed by concatenating ", TT "prefix", " and
     ", TT "base", " and associate with the key for future retrieval with
     ", TT "cacheFileName", ".  The prefix should be the path to a directory,
     together with a terminating path component
     separator.  When the program terminates, a file called
     ", TT "Macaulay2-index-cache", " will be created or updated, if
     necessary, in which to store the table of correspondences between keys
     and filenames.",
     EXAMPLE {
	  ///cacheFileName("./tmp/","K-theory","motives")///,
	  ///cacheFileName("./tmp/","K-theory")///,
	  }
     }

document { (cacheFileName, List, String),
     Synopsis => {
	  ///fn = cacheFileName(path,key)///,
	  "path" => "a search path (list) of prefixes from which to construct the
	        file name",
	  "key" => "a key",
	  "fn" => "a new file name"
	  },
     "The path should be a list of prefixes which correspond to existing
     directories.  A list of those file names for the given key that have already been
     assigned (see ", TO (cacheFileName, String, String), ") in one of
     the directories on the path will be returned.  In case no previous assignments
     to this key have occurred yet, one will be made using the first element of the
     search path.",
     EXAMPLE {
	  ///documentationPath///,
	  ///cacheFileName ( documentationPath, "sin" )///,
	  ///cacheFileName ( documentationPath, "xxxxx" )///
	  },
     SEEALSO { "documentationPath" }
     }

document { symbol documentationPath,
     Headline => "search path for documentation",
     Synopsis => {
	  ///documentationPath = x///,
	  "x" => "a list of paths to directories where documentation is stored"
	  },
     EXAMPLE {
	  ///documentationPath = unique append (documentationPath, "tmp/cache/doc/")///
	  },
     SEEALSO {cacheFileName}
     }

