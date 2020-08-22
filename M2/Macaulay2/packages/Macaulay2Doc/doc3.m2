--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => "shield",
     Headline => "shield evaluation from interrupts",
     TT "shield x", " -- executes the expression ", TT "x", ", temporarily
     ignoring interrupts."
     }

document {
     Key => symbol lineNumber,
     Headline => "current line number",
     TT "lineNumber", " -- the current line number.",
     BR{},
     TT "lineNumber = n", " -- sets the line number to ", TT "n", ".",
     }

document {
     Key => FilePosition,
     Headline => "the class of all file positions",
     TT "FilePosition", " -- a type of list designed to represent a position
     in a file.",
     PARA{},
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document {
     Key => flagLookup,
     Headline => "flag a symbol",
     Usage => "flagLookup x",
     Inputs => { "x" => Symbol },
     Outputs => {
	  Boolean => {"whether each subsequent reference to the symbol ", TT "x", " will result in an error message"}
	  },
     Consequences => {{ 
	       "The first use arranges for each subsequent reference to the symbol ", TT "x", " to result in an error message.
	       The second use cancels the arrangement.  Each subsequent use toggles the state."
	       }},
     PARA {
	  "To get access to a flagged symbol without signalling an error, use ", TO "getGlobalSymbol", "."
	  }
     }

document {
     Key => {instance,(instance, Thing, Type)},
     Headline => "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA{},
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA{},
     SeeAlso => { "class", "parent" }
     }

document {
     Key => alarm,
     Headline => "set an alarm",
     Usage => "alarm n",
     Inputs => {
	  "n" => ZZ,
	  },
     Consequences => {
	  { "the alarm will be sounded after ", TT "n", " seconds; it can be intercepted with ", TO "try" }
	  },
     "If ", TT "n", " is zero, then no alarm is scheduled, and any previously scheduled alarm is cancelled.
     Any pending alarm will be cancelled when any other error occurs, or when the top level loop offers an input prompt to the user.",
     PARA{},
     "The value returned is the number of seconds remaining until any previously scheduled alarm was due to be delivered, or
     zero if there was no previously scheduled alarm.",
     PARA{},
     "This command may interfere with ", TO "time", " or ", TO "sleep", " on some systems."
     }

document {
     Key => symbol ++,
     Headline => "a binary operator, usually used for direct sum"
     }

document {
     Key => symbol (*),
     Headline => "a unary postfix operator, used for indicating a graded object"
     }

document {
     Key => symbol ^*,
     Headline => "a unary postfix operator, used for indicating pullback maps"
     }

document {
     Key => symbol _*,
     Headline => "a unary postfix operator, used for indicating pushforward maps"
     }

document {
     Key => symbol <==>,
     Headline => "a binary operator"
     }

document {
     Key => symbol ,,
     Headline => "the comma, used for separating entries in a list or sequence"
     }

document {
     Key => symbol ==>,
     Headline => "a binary operator"
     }

document {
     Key => symbol |-,
     Headline => "a binary operator"
     }

document {
     Key => symbol ===>,
     Headline => "a binary operator"
     }

document {
     Key => symbol <===,
     Headline => "a unary and binary operator"
     }

document {
     Key => symbol <==,
     Headline => "a unary and binary operator"
     }

document {
     Key => symbol @@,
     Headline => "a binary operator"
     }

document {
     Key => (symbol @@, Function, Function),
     Headline => "composition of functions",
     Usage => "f @@ g",
     Inputs => { "f", "g" },
     Outputs => {{ "the composite function of ", TT "f", " and ", TT "g", "." }},
     EXAMPLE {
	  "f = i -> i+1",
	  "g = i -> i^2",
	  "apply(0 .. 10, f @@ g)",
	  "apply(0 .. 10, g @@ f)"
	  }
     }

document {
     Key => symbol @,
     Headline => "a binary operator",
     "This operator is right associative."
     }

document {
     Key => Number,
     Headline => "the class of all numbers"
     }

document {
     Key => {(symbol /,List,Thing)},
     Headline => "vector division",
     Usage => "v/c",
     Inputs => {"v" => "to be treated as a vector", "c" => "a number or scalar ring element"},
     Outputs => {{ "the quotient vector; every element of ", TT "v", " is divided by ", TT "c" }},
     EXAMPLE "{1,2,3,4} / 3"     
     }

document {
     Key => { (symbol /,VisibleList,Function),
	  (symbol /,List,Function),
	  (symbol \,Function,VisibleList),
	  (symbol \,Function,VirtualTally),
	  (symbol \,SelfInitializingType,VisibleList),
	  (symbol \,Command,VisibleList),
	  (symbol \,RingMap,List),
	  (symbol \,Command,VirtualTally),
	  (symbol /,VisibleList,SelfInitializingType),
	  (symbol /,List,Command),
	  (symbol /,VirtualTally,Command),
	  (symbol /,VirtualTally,Function),
	  (symbol /,List,RingMap),
	  (symbol /,VisibleList,Command)
	  },
     Headline => "apply a function to elements of a list",
     Usage => "x/f\nf\\x",
     Inputs => { "x" => Nothing => {ofClass{VisibleList,List,Sequence,Array,Tally,Set}}, "f" => Nothing => {ofClass{Function,Command,SelfInitializingType,RingMap}} },
     Outputs => {{ "the list, tally, or set obtained by applying ", TT "f", " to each element of ", TT "x", "; it has the same type as ", TT "x", " has" }},
     PARA {
	  "The function ", TO "apply", " does the same thing."
	  },
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  f = x -> x+1
	  g = x -> 2*x
     	  g \ (1 .. 10)
     	  (1 .. 10) / g
     	  f \ g \ (1 .. 10)
     	  f @@ g \ (1 .. 10)
	  set (1 .. 10)
	  g \ oo
	  R = QQ[x];
	  f = map(R,R,{x^2})
	  f \ {x,x^2,x^3,x^4}
     ///,
     SourceCode => {(symbol /,VisibleList,Function)},
     }

document {
     Key => { (symbol /,Ideal,Function),
	  (symbol \,Function,Ideal)},
     Headline => "apply a function to generators of an ideal",
     Usage => "I/f\nf\\I",
     Inputs => { "I","f"},
     Outputs => {List => { "obtained by applying the function ", TT "f", " to each generator of ", TT "I"}},
     PARA {
     	  "The operator ", TO "/", " is left associative, which means that ", TT "w / f / g", " is interpreted as ", TT "(w / f) / g", ".
     	  The operator ", TO "\\", " is right associative, so ", TT ///g \ f \ w///, " is interpreted as ", TT ///g \ (f \ w)///, ".
	  Both operators have parsing precedence lower than that of ", TO "@@", ", which means that the previous two expressions are 
	  equivalent to ", TT "w / g @@ f", "
	  and ", TT "g @@ f \\ w", ", respectively. See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  R = ZZ[a..d];
	  I = ideal"abc-d3,ab-d-1,a2+b2+c3-14d-3"
     	  I/size
	  (f->f+a*b-1)\I
	  I/leadTerm/support/set//sum
     ///,
     }

document {
     Key => {(symbol //,Thing,Function),(symbol \\,Function,Thing),
	  (symbol //,Thing,Command),(symbol \\,Command,Thing),
	  (symbol //,Thing,SelfInitializingType),(symbol \\,SelfInitializingType,Thing)
	  },
     Headline => "apply a function",
     Usage => "x // f\nf \\\\ x",
     Inputs => { "x", "f" => Nothing => {ofClass{Function,Command,SelfInitializingType}}},
     Outputs => {{ "the result of applying ", TT "f", " to ", TT "x", ", i.e., ", TT "f x" }},
     SeeAlso => {(symbol /,VisibleList,Function)},
     PARA {
	  "The parsing precedence of the operators ", TT "//", " and ", TT "\\\\", " is rather low, which makes
	  them useful for avoiding parentheses.  See ", TO "precedence of operators", "."
	  },
     EXAMPLE lines ///
     	  toList \\ sin \ ( 1 .. 5 )
     	  ( 1 .. 5 ) / sin // toList
	  (x -> (x,x)) \ (a,b,c,d)
	  splice \\ (x -> (x,x)) \ (a,b,c,d)
     ///
     }


document {
     Key => Boolean,
     Headline => "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA{},
     "Special operators dealing with truth values.",
     UL {
	  TO "not",
	  TO "and",
	  TO "or",
	  TO "if"
	  }
     }

document {
     Key => Symbol,
     Headline => "the class of all symbols",
     "Symbols are entered as an alphabetic character followed by a
     sequence of alphanumeric characters; case is significant.
     The single symbol character ' is regarded as alphabetic, so that
     symbols such as ", TT "x'", " may be used.",
     PARA{},
     "Symbols are used as names for values to be preserved, as indeterminates
     in polynomial rings, and as keys in hash tables.  They may have
     global scope, meaning they are visible from every line of code,
     or local scope, with visibility restricted to a single file or
     function body.",
     EXAMPLE {
	  "x",
	  "ab12"
	  },
     SeeAlso => {":="}
     }

document {
     Key => Keyword,
     Headline => "the class of all keywords",
     PARA {
     	  "Keywords are symbols that are treated specially by the system while parsing user input.  Some of them,
     	  such as ", TO "and", ", consist of alphanumeric characters and look just like
     	  ordinary symbols.  Others, such as ", TO "==>", ", consist of special characters
     	  and are called operators."
	  },
     SeeAlso => {"precedence of operators"}
     }

document {
     Key => File,
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", ".",
     DIV {
	  "class" => "waystouse",
	  SUBSECTION {"Functions operating on path names:"},
	  UL {
	       TO minimizeFilename,
	       TO "path",
	       TO relativizeFilename,
	       TO searchPath,
	       TO temporaryFileName,
	       TO toAbsolutePath
	       }
	  }
     }

document {
     Key => connectionCount,
     Headline => "the number of connections",
     TT "connectionCount f", " -- returns the number of connections accepted by 
     the listener ", TT "f", " so far."
     }

document {
     Key => {echoOn,(echoOn, File)},
     Headline => "turn on echoing",
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document {
     Key => {echoOff,(echoOff, File)},
     Headline => "turn off echoing",
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document {
     Key => printString,
     Headline => "lowlevel function to print a string, net, or symbol",
     TT "printString(o,s)", " -- send the string ", TT "s", " to the output file ", TT "o", ".",
     PARA{},
     "This function is intended for internal use only.",
     PARA{},
     "The argument ", TT "s", " may also be a sequence or list, in which case
     its elements are printed.  If an integer is encountered, then
     it specifies a number of spaces to be printed.  If a symbol
     or indeterminate is encountered, its name is printed.  If ", TO "null", "
     is encountered, nothing is printed.",
     PARA{},
     EXAMPLE ///printString(stdio, (a,10,"b",20,c))///
     }

document {
     Key => serialNumber,
     Headline => "serial number of a dictionary, task, symbol, mutable hash table, or mutable list, ",
     Usage => "serialNumber x",
     Inputs => {"x"},
     Outputs => { ZZ => { "the serial number of ", TT "x" } },
     EXAMPLE lines ///
     	  serialNumber asdf
	  serialNumber foo
	  serialNumber ZZ
     ///
     }     

document {
     Key => setEcho,
     Headline => "turn on echoing",
     TT "setEcho stdio", " -- turn on echoing of characters typed to the standard
     input."
     }

document {
     Key => clearEcho,
     Headline => "turn off echoing",
     TT "clearEcho stdio", " -- turn off echoing of characters typed to the standard
     input."
     }

document {
     Key => CacheTable,
     Headline => "hash tables for caching",
     "A type of mutable hash table designed for caching computed values that
     could always be recomputed.  Cache tables are designed so their contents
     will not participate in any comparisons by the strict comparison
     operator ", TT "===", ".  To that end, any two cache tables with the same
     class and parent are considered equal to each other and have hash code equal to 0."
     }

document {
     Key => "using functions with optional inputs",
     "Some functions accept optional inputs in addition to their required inputs.  In the documentation,
     such an optional input is indicated by writing ", TT "NAME => ...", ", where ", TT "NAME", " is the
     name of the optional input, and the dots indicate the place where the user will provide the
     value of the optional input.",
     PARA{},
     "Optional inputs can be provided between parentheses along with the
     other inputs (or arguments) of the function.  For example, if the function is normally used with two
     required inputs, then instead of typing ", TT "f(x,y)", ", you may type 
     ", TT "f(x,y,FOO => t, BAR => u)", ", where ", TT "t", " is the value to be provided to ", TT "f", " as
     the value of the optional input named ", TT "FOO", " and ", TT "u", " is the value to be
     provided to ", TT "f", " as the value of the optional input named ", TT "BAR", ".",
     PARA{},
     "The optional inputs can be inserted
     in any order, and may even occur before the required inputs.  If more than one optional input with the same
     option name are given, then the value accompanying the right-most one is the value provided to the function.",
     PARA{},
     "Use ", TO2{ (options,Function), "options" }, " to discover the optional arguments accepted by a function.",
     EXAMPLE {
     	  "R = ZZ/101[x,y,z,w];",
     	  "gb ideal(x*y-z^2,y^2-w^2)",
	  "gens oo"
	  },
     "One of the optional arguments for ", TO "gb", "
     is named ", TO "DegreeLimit", "; we may use ", TO2{ (options,Function), "options" }, " to discover that,
     as follows.",
     EXAMPLE {
	  "options gb"
	  },
     "The optional input named ", TO "DegreeLimit", " can be used to specify that the computation should stop after a certain degree has been reached.",
     EXAMPLE {
	  "gb(ideal(x*y-z^2,y^2-w^2), DegreeLimit => 2)",
	  "gens oo",
	  },
     SUBSECTION "Programming hint",
     PARA {
     	  "The value returned by ", TO2{ (options,Function), "options" }, " is a type of hash table that can be used to obtain default values."
	  },
     PARA {
	  "See also ", TO "making new functions with optional arguments", "."
	  },
     EXAMPLE {
	  "(options gb).Syzygies"
	  }
     }

document {
     Key => {"operatorAttributes", Flexible, Binary, Prefix, Postfix},
     Usage => "operatorAttributes",
     Outputs => {{ "an experimental hash table that give information about ", TO "operators", " in the Macaulay2 language" }},
     "Meanings of the symbols used:",
     UL {
	  LI { TO "Flexible", " -- user defined methods may be installed" },
	  LI { TO "Binary", " -- it's a binary operator" },
	  LI { TO "Prefix", " -- it's a prefix unary operator" },
	  LI { TO "Postfix", " -- it's a postfix unary operator" }
	  },
     "We intend to add parsing precedences to this table and eliminate ", TO "seeParsing", "."
     }


document{
     Headline => "a type of method function",
     Key => MethodFunctionBinary,
     PARA {
	  "The type of all method functions created with the option ", TO "Binary", " set to ", TO "true", ", such as ", TO "gcd", "."
	  },
     SeeAlso => { "method" }
     }

document{
     Headline => "a type of method function",
     Key => MethodFunctionSingle,
     PARA {
	  "The type of all method functions created with the option ", TO "Dispatch", " set to ", TO "Thing", ", such as ", TO "code", "."
	  },
     SeeAlso => { "method" }
     }

document { Key => MethodFunction,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
document { Key => MethodFunctionWithOptions,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
undocumented (methodOptions, MethodFunctionWithOptions)
undocumented (methodOptions, MethodFunction)
undocumented (methodOptions, Symbol)
document { Key => {(methodOptions, Function),(methodOptions, Command),(methodOptions, ScriptedFunctor),methodOptions},
     Headline => "recover the options used when a method function was created",
     Usage => "methodOptions f",
     Inputs => { "f" },
     Outputs => {{ "the options used when ", TT "f", " was created by ", TO "method", "" }},
     EXAMPLE lines ///
     	  methodOptions source
     	  methodOptions res
     ///
     }

document { Key => {(numRows, Matrix),(numRows, MutableMatrix),numRows},
     Headline => "number of rows in a matrix or mutable matrix",
     Usage => "numRows m", Inputs => { "m" }, Outputs => {{ "the number of rows in ", TT "m" }}}
document { Key => {(numColumns, Matrix),(numColumns, MutableMatrix),numColumns},
     Headline => "number of columns in a matrix or mutable matrix",
     Usage => "numColumns m", Inputs => { "m" }, Outputs => {{ "the number of columns in ", TT "m" }}}
document { Key => {mutableMatrix,
	  (mutableMatrix, MutableMatrix),
	  (mutableMatrix, Matrix),
	  (mutableMatrix, List),
	  [mutableMatrix, Dense]},
     Headline => "make a mutable matrix",
     Usage => "mutableMatrix m",
     Inputs => { "m" => {ofClass{Matrix, MutableMatrix, List}},
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {{ "a new mutable matrix whose entries are obtained from ", TT "m", ".  If ", TT "m", " is a list, it should
	       be a doubly nested list (table) of ring elements, all from the same ring." }},
     EXAMPLE lines ///
     	  f = mutableMatrix {{1,2,3,4}}
	  f_(0,2)
	  f_(0,2) = 33
	  f
	  R = QQ[a..z]
	  mutableMatrix genericMatrix(R,3,3)
     ///
     }
document { Key => {(randomMutableMatrix, ZZ, ZZ, RR, ZZ),
	  [randomMutableMatrix,Dense],
	  randomMutableMatrix},
     Headline => "a random mutable matrix of integers",
     Usage => "randomMutableMatrix(nrows,ncols,zerof,max)",
     Inputs => {
	  "nrows",
	  "ncols",
	  "zerof" => { "between 0 and 1" },
	  "max",
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {
	  {"a random mutable ", TT "nrows", " by ", TT "ncols", " matrix of integers.  
	       The absolute value of the 
	       entries is less than ", TT "max", ", and
	       the frequency of entries equal to zero is given by ", TT "zerof", "." }
	  },
     "This function has been superceded by ", TO fillMatrix, ", which works over 
     more rings, is much faster for large matrices, and is more flexible.",
     EXAMPLE lines ///
          randomMutableMatrix(10,15,.9,100)
     ///,
     SeeAlso => {mutableMatrix, fillMatrix, setRandomSeed, random}
     }
document { Key => {(mutableMatrix, Ring, ZZ, ZZ),(mutableMatrix, RingFamily, ZZ, ZZ) },
     Headline => "make a mutable matrix filled with zeroes",
     Usage => "mutableMatrix(R,nrows,ncols)",
     Inputs => { "R",
	          "nrows",
		  "ncols",
	  	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
		  },
     Outputs => {{"an ", TT "nrows", " by ", TT "ncols", " mutable matrix filled with zeroes from the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableMatrix(QQ,10,20)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableIdentity, mutableMatrix}
     }
document { Key => {(mutableIdentity, Ring, ZZ),(mutableIdentity, RingFamily, ZZ),
	  [mutableIdentity,Dense],
	  mutableIdentity},
     Headline => "make a mutable identity matrix",
     Usage => "mutableIdentity(R,nrows)",
     Inputs => { "R",
	  "nrows",
	  Dense => {"whether the encoding of the matrix should be dense or not: see ", TO MutableMatrix}
	  },
     Outputs => {
	  MutableMatrix => {"an ", TT "nrows", " by ", TT "nrows", " mutable identity matrix filled with elements of the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableIdentity(QQ,10)
	 m_(5,5) = 11/13
	 m
     ///,
     SeeAlso => {mutableMatrix}
     }

undocumented (pretty, Thing)
document { Key => pretty,
     Headline => "a pretty printer", "This function is experimental and under development." }
document { Key => {(symlinkDirectory, String, String),symlinkDirectory,[symlinkDirectory,Undo],[symlinkDirectory, Exclude],
	  [symlinkDirectory, FollowLinks],[symlinkDirectory, Verbose]}, 
     Headline => "make symbolic links for all files in a directory tree",
     Usage => "symlinkDirectory(src,dst)",
     Inputs => {
	  "src" => "the path to an existing directory, the root of the source directory tree",
	  "dst" => "a path to the root of the destination directory tree, which may not exist yet",
	  Exclude => {"a string containing a regular expression, or a list of such strings.  If the base part of the name of a file in the source tree
	       matches one of the regular expressions, then no link to it is created"
	       },
	  Undo => Boolean => {"whether to undo the symbolic links created in a previous application of this function.  The directories in the destination
	       directory tree will remain."
	       },
	  FollowLinks => Boolean => {"whether to follow symbolic links in the source tree to directories"},
	  Verbose => Boolean => {"whether to report the creation or deletion of each symbolic link"}
	  },
     Consequences => {
	  {"The directory tree rooted at ", TT "src", " is duplicated by a directory tree rooted at ", TT "dst", ".  The files in the source tree are represented by
	       relative symbolic links in the destination tree to the original files in the source tree."
	       }
	  },
     EXAMPLE lines ///
     	  src = temporaryFileName() | "/"
	  dst = temporaryFileName() | "/"
	  makeDirectory (src|"a/")
	  makeDirectory (src|"b/")
	  makeDirectory (src|"b/c/")
	  src|"a/f" << "hi there" << close
	  src|"a/g" << "hi there" << close
	  src|"b/c/g" << "ho there" << close
	  symlinkDirectory(src,dst,Verbose=>true)
	  get (dst|"b/c/g")
	  symlinkDirectory(src,dst,Verbose=>true,Undo=>true)
     ///,
     "Now we remove the files and directories we created.",
     EXAMPLE lines ///
	  rm = d -> if isDirectory d then removeDirectory d else removeFile d
	  scan(reverse findFiles src, rm)
	  scan(reverse findFiles dst, rm)
     ///,
     SeeAlso => { symlinkFile, copyDirectory }
     }

document { Key => symlinkFile,
     Headline => "make a symbolic link to a file",
     Usage => "symlinkFile(src,dst)",
     Inputs => {
	  "src" => String,
	  "dst" => String
	  },
     Consequences => {
	  {"a symbolic link at the location in the directory tree specified by ", TT "dst", " is created, pointing to ", TT "src"}
	  },
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  symlinkFile("qwert", fn)
	  fileExists fn
	  readlink fn
	  removeFile fn
     ///,
     SeeAlso => { symlinkDirectory }
     }

document { Key => {(copyDirectory, String, String),copyDirectory,[copyDirectory, Exclude],[copyDirectory, UpdateOnly],[copyDirectory, FollowLinks],[copyDirectory, Verbose]},
     Usage => "copyDirectory(src,dst)",
     Inputs => {
	  "src" => String,
	  "dst" => String,
	  Exclude => {"a string containing a regular expression, or a list of such strings.  If the base part of the name of a file in the source tree
	       matches one of the regular expressions, then no link to it is created"
	       },
	  UpdateOnly => Boolean => {"whether to copy files only if the target file does not exist or is older than the source file"},
	  FollowLinks => Boolean => {"whether to follow symbolic links in the source tree to directories"},
	  Verbose => Boolean => {"whether to report individual file operations"}
	  },
     Consequences => {
	  {"a copy of the directory tree rooted at ", TT "src", " is created, rooted at ", TT "dst"}
	  },
     EXAMPLE lines ///
     	  src = temporaryFileName() | "/"
	  dst = temporaryFileName() | "/"
	  makeDirectory (src|"a/")
	  makeDirectory (src|"b/")
	  makeDirectory (src|"b/c/")
	  src|"a/f" << "hi there" << close
	  src|"a/g" << "hi there" << close
	  src|"b/c/g" << "ho there" << close
	  stack findFiles src
	  copyDirectory(src,dst,Verbose=>true)
	  copyDirectory(src,dst,Verbose=>true,UpdateOnly => true)
	  stack findFiles dst
	  get (dst|"b/c/g")
     ///,
     "Now we remove the files and directories we created.",
     EXAMPLE lines ///
	  rm = d -> if isDirectory d then removeDirectory d else removeFile d
	  scan(reverse findFiles src, rm)
	  scan(reverse findFiles dst, rm)
     ///,
     SeeAlso => { symlinkDirectory }
     }
document { Key => {(copyFile, String, String),copyFile,[copyFile, UpdateOnly],[copyFile, Verbose]},
     Usage => "copyFile(src,dst)",
     Inputs => {
	  "src" => "the filename or path to an existing regular file",
	  "dst" => "the filename or path to the copy to be made",
	  UpdateOnly => Boolean => {"whether to copy file only if the destination file does not exist or is older than the source file"},
	  Verbose => Boolean => {"whether to report individual file operations"}
	  },
     Consequences => {
	  "the file may be copied"
	  },     
     EXAMPLE lines ///
     	  src = temporaryFileName()
	  dst = temporaryFileName()
	  src << "hi there" << close
	  copyFile(src,dst,Verbose=>true)
	  get dst
	  copyFile(src,dst,Verbose=>true,UpdateOnly => true)
	  src << "ho there" << close
	  copyFile(src,dst,Verbose=>true,UpdateOnly => true)
	  get dst
	  removeFile src
	  removeFile dst
     ///,
     SeeAlso => { copyDirectory, symlinkDirectory }
     }
document { Key => {(moveFile, String, String),(moveFile, String),moveFile,[moveFile,Verbose]},
     Usage => "moveFile(src,dst)",
     Inputs => {
	  "src" => "the filename or path to an existing file",
	  "dst" => "the new filename or path to a location (on the same file system).
	            Omit this argument and an appropriately numbered backup filename will be invented.",
	  Verbose => Boolean => {"whether to report individual file operations"}
	  },
     Outputs => {
	  {"the name of the backup file if one was created, or ", TO "null"}
	  },
     Consequences => {
	  "the file will be moved by creating a new link to the file and removing the old one"
	  },     
     EXAMPLE lines ///
     	  src = temporaryFileName()
	  dst = temporaryFileName()
	  src << "hi there" << close
	  moveFile(src,dst,Verbose=>true)
	  get dst
	  bak = moveFile(dst,Verbose=>true)
	  removeFile bak
     ///,
     SeeAlso => { copyFile }
     }
document { Key => mkdir,
     Usage => "mkdir p",
     Inputs => {
	  "p" => String => "a path to a directory to be made"
	  },
     Consequences => {{"a directory will be created at the path ", TT "p"}},
     PARA {"Only one directory will be made, so the components of the path p other than the last must already exist."},
     EXAMPLE lines ///
     	  p = temporaryFileName() | "/"
	  mkdir p
	  isDirectory p
	  (fn = p | "foo") << "hi there" << close
	  get fn
	  removeFile fn
	  removeDirectory p
     ///,
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the home directory."},
     SeeAlso => {makeDirectory}
     }

document { 
     Key => {(makeDirectory,String),makeDirectory},
     Headline => "make a directory",
     Usage => "makeDirectory dir",
     Inputs => { "dir" => String => "a path to the desired directory" },
     Consequences => { { "the directory is made, with as many new path components as needed" } },     
     EXAMPLE lines ///
     	  dir = temporaryFileName()
	  makeDirectory (dir|"/a/b/c")
	  removeDirectory (dir|"/a/b/c")
	  removeDirectory (dir|"/a/b")
	  removeDirectory (dir|"/a")
     ///,
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the home directory."},
     SeeAlso => {mkdir}
     }

document { Key => Bareiss,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }
document { Key => Cofactor,
     "This symbol is used as one of the permissible values for the strategy option in function dealing with determinants.",
     SeeAlso => {[exteriorPower,Strategy], [minors,Strategy], [det,Strategy]}
     }

document { Key => GlobalDictionary,
     Headline => "the class of all global dictionaries",
     SeeAlso => { "dictionaryPath", LocalDictionary }
     }
document { Key => LocalDictionary,
     Headline => "the class of all local dictionaries",
     "A local dictionary is one used in connection with a local scope, such as one that is bounded by the body of a function closure.
     A local dictionary is created on the fly by the interpreter, and after the scope has been closed, the dictionary can be enlarged no further.
     Accessing local dictionaries can be a useful debugging tool.  The local dictionaries accessible to the user come with frames, so their symbols
     have values; thus they may be referred to as dictionary closures.",
     SeeAlso => { localDictionaries, GlobalDictionary }
     }
document { Key => {localDictionaries,(localDictionaries, Symbol), (localDictionaries, Pseudocode), (localDictionaries, Dictionary), (localDictionaries, Function)},
     Headline => "get local dictionaries",
     Usage => "localDictionaries f",
     Inputs => {
	  "f" => {"() or ", ofClass{Function,Symbol,Pseudocode,Dictionary}}
	  },
     Outputs => {
	  List => {"a list of the local dictionaries associated with the lexical scopes containing ", TT "f"}
	  },
     EXAMPLE lines ///
     	  f := x -> y -> z -> 11;
	  d := localDictionaries ((f 22) 33)
	  peek d
	  d#0#"y"
	  value d#0#"y"
	  peek localDictionaries()
     ///
     }

document { Key => DocumentTag,
     Headline => "the class of all document tags", "This class is mainly for internal use, in constructing documentation." }
document { Key => CompiledFunctionBody,
     Headline => "the class of all compiled function bodies",
     "A compiled function body is the body of a compiled function closure.  It is not a function.",
     EXAMPLE lines ///
     	  source
	  functionBody source
     ///
     }

document { Key => ImmutableType,
     Headline => "the class of immutable types",
     "All types are implemented as hash tables.  Most types are mutable, so that additional methods for handling their instances can be added
     at any time.  However, if a type has an ancestor where the methods can be stored, then mutability is not needed.",
     PARA{},
     "When a type is used to represent a mathematical object, then immutability is desirable, in order to make the strict equality operator work on it.  For example, a
     module ", TT "M", " is a type, with its elements are its instances, but we would like to be able to compare two modules quickly, and form sets of modules.  This
     is possible, because we have implemented modules as immutable types, and we have put the methods for adding and subtracting elements of ", TT "M", " into the
     class ", TO "Vector", ".",
     EXAMPLE lines ///
     	  F = ZZ^3
	  class F
	  parent class F
	  showStructure class F
	  showStructure F
     	  v = F_0 + 3*F_2
	  F === ZZ^3
	  set (ZZ^3, ZZ^2, ZZ^3)
	  peek F
     ///,
     "Another advantage of immutability of modules is that there is no particular reason, aside from efficiency, to avoid creating a given module multiple times, as
     one copy of the module is as good as another.",
     EXAMPLE lines ///
     	  ZZ^3_0 + ZZ^3_2
     ///,
     SeeAlso => {showStructure,parent,class}
     }

document { Key => SumOfTwists,
     Headline => "the class of all sums of twists",
     "This class is used internally as an abstract representation of a graded module as an infinite direct sum of twists of a coherent sheaf.",
     EXAMPLE lines ///
     	  R = QQ[x,y,z]
     	  X = Proj R
	  OO_X(*)
	  peek oo
	  OO_X(>=2)
	  peek oo
	  Ext^0(OO_X^1, OO_X^1)
	  Ext^0(OO_X^1, OO_X^1(*))
     ///
     }

document { Key => {(symbol (*),CoherentSheaf),
	  (symbol (*),SheafOfRings)},
     Headline => "sum of twists",
     Usage => "F(*)",
     Inputs => {"F" => {" or a ", ofClass SheafOfRings}},
     Outputs => {{"a symbolic representation of the graded object consisting of the twists ", TT "F(n)", ", for all integers ", TT "n"}},
     EXAMPLE lines ///
     	  R = QQ[x,y,z];
     	  X = Proj R
	  Ext^0(OO_X^1, OO_X^1)
	  Ext^0(OO_X^1, OO_X^1(*))
	  Ext^0(OO_X^1, OO_X(*))	  
     ///}

document { Key => Wrap,
     Headline => "a key for methods for wrapping printed output",
     "The default method for printing results of computations (stored in ", TT "Thing#{Standard,Print}", ") searches for a method for wrapping the output by search in the class of 
     the result (and its ancestors) for a function stored under the key ", TT "Wrap", ".  The preinstalled wrapping methods use ", TO "wrap", ", they differ in their choice of
     separator between wrapped lines, or in whether to wrap at all.",
     EXAMPLE lines ///
     	  QQ[x_0 .. x_40 ]
	  concatenate(50:"abcd ")
     ///
     }

document { Key => {BettiTally,
	  (symbol ++,BettiTally,BettiTally), (symbol SPACE,BettiTally,ZZ),
	  (symbol **,BettiTally,BettiTally), (symbol *,QQ,BettiTally),(symbol *,ZZ,BettiTally),(lift, BettiTally, ZZ),
	  (symbol SPACE,BettiTally,Array), 
	  (dual,BettiTally),(degree, BettiTally),(codim, BettiTally),(regularity,BettiTally),(hilbertPolynomial, ZZ, BettiTally),
	  (hilbertSeries, ZZ, BettiTally),(pdim, BettiTally),(poincare, BettiTally)
	  },
     Headline => "the class of all Betti tallies",
     "A Betti tally is a special type of ", TO "Tally", " that is printed as a display of 
     graded Betti numbers.  The class was created
     so the function ", TO "betti", " could return something that both prints nicely and 
     from which information can be extracted.  The keys
     are triples ", TT "(i,d,h)", ", where ", TT "i", " is the homological degree, ", 
     TT "d", " is a list of integers giving a multidegree,
     and ", TT "h", " is the result of applying a weight covector to ", TT "d", ".
     Only ", TT "i", " and ", TT "h", " are used in printing.",
     EXAMPLE lines ///
          t = new BettiTally from { (0,{0},0) => 1, (1,{1},1) => 2, (2,{3},3) => 3, (2,{4},4) => 4 }
	  peek oo
     ///,
     "For convenience, the operations of direct sum (", TO "++", "), tensor product (", TO "**", "), ", 
     TO codim, ", ",
     TO degree, ", ",
     TO dual, ", ",
     TO hilbertPolynomial, ", ",
     TO hilbertSeries, ", ",
     TO pdim, ", ",
     TO poincare, ", ",
     TO regularity, ", and degree shifting (numbers in brackets or parentheses), have
     been implemented for Betti tallies.  These operations mimic the corresponding operations on chain complexes.",
     EXAMPLE lines ///
     	  t(5)
     	  t[-5]
     	  t ++ oo
	  t ** t
	  dual t
	  regularity t
     ///,
     "A Betti tally can be multiplied by an integer or by a rational number, and the values can be lifted
     to integers, when possible.",
     EXAMPLE lines ///
     	  (1/2) * t
	  2 * oo
	  lift(oo,ZZ)
     ///,     
     "Various combinations of the degree vectors can be displayed by using ", TO (betti,BettiTally), "."
     }

document { Key => {(betti,BettiTally)},
     Headline => "view and set the weights of a betti display",
     Usage => "betti(t, Weights => w)",
     Inputs => { 
	  "t",
	  Weights => List => { "a list of integers, ", TT "w", ", with the same degree length as that of ", TT "t"}
	  },
     Outputs => {
	  BettiTally => {"different from the input only in its degree weights.  If ", TT "w", " is
	       non-null, then a new betti tally with new weight values is returned"}
	       },
     EXAMPLE lines ///
     	  R = ZZ/101[a..d,Degrees=>{2:{1,0},2:{0,1}}];
	  I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
	  t = betti res I
	  peek t
	  ///,
     "The following three displays display the first degree, the second degree, and the total
     degree, respectively.",
     EXAMPLE lines ///
	  betti(t,Weights=>{1,0})
	  betti(t,Weights=>{0,1})
	  t1 = betti(t,Weights=>{1,1})
	  peek t1
     ///
     }

document { Key => {MultigradedBettiTally,
	(symbol SPACE,MultigradedBettiTally,List)},
    Headline => "the class of all multigraded Betti tallies",
    "A multigraded Betti tally is a special type of ", TO "BettiTally", " that is
     printed as a display of the multigraded Betti numbers.  The class was
     created so that the function ", TO "multigraded", " could return something that
     both prints nicely and from which information could be extracted.  The keys
     are triples ", TT "(i,d,h)", " where ", TT "i", " is the homological
     degree, ", TT "d", " is a list of integers giving a multidegree, and ",
     TT "h", " is the result of applying a weight covector to ", TT "d", ".",
    PARA{},
    "By default the data is presented as a table of polynomials where each column
     corresponds to a given homological degree appearing as the top entry and each
     monomial in the other entries represents the multidegree of a given generator.",
    PARA{},
    "When ", TT "compactMatrixForm", " is set to true, the entries in a
     column correspond to a fixed multidegree, ordered by the ", TT "h",
     ".  The number of summand correspond to a given multidegree appears to
     the left of the multidegree.",
    EXAMPLE lines ///
      B = new MultigradedBettiTally from {(0, {0, 0}, 0) => 1, (1, {0, 2}, 2) => 1, (1, {1, 1}, 2) => 2, (1, {2, 0}, 2) => 1, (2, {1, 2}, 3) => 2, (2, {2, 1}, 3) => 2, (3, {2, 2}, 4) => 1}
      peek oo
    ///,
    "For convenience, most operations on", TT "BettiTally", " such as direct sum
     (", TO "++", "), tensor product (", TO "**", "), ", TO "pdim", " and degree
     shifting (numbers in brackets or lists in parentheses) are automatically
     extended to work with multigraded Betti tables.  These operations mimic the
     corresponding operations on chain complexes.",
    EXAMPLE lines ///
      B({-1,-1})
      B[1]
      B[1] ++ B
      B ** B
      pdim B
      compactMatrixForm = false
      dual B
    ///,
    "A multigraded Betti tally also can multiplied by an integer or by a rational number.",
    EXAMPLE lines ///
      (1/2) * B
      2 * oo
      lift(oo,ZZ)
    ///,
    "This feature was implemented by Mahrud Sayrafi based on earlier work by Gregory G. Smith.",
    SeeAlso => { BettiTally }
    }

document { Key => { (multigraded, BettiTally), multigraded },
    Headline => "convert a Betti tally into a multigraded Betti tally",
    Usage => "multigraded t",
    Inputs => { "t" => BettiTally },
    Outputs => { MultigradedBettiTally => { "different from the input only in the ordering of each column"} },
    "A multigraded Betti tally is a special type of ", TO "BettiTally", " that both
     prints nicely and from which multigraded Betti numbers could be easily extracted.",
    EXAMPLE lines ///
      R = ZZ/101[a..d, Degrees => {2:{1,0},2:{0,1}}];
      I = ideal random(R^1, R^{2:{-2,-2},2:{-3,-3}});
      t = betti res I
      peek t
      B = multigraded t
      peek B
    ///,
    "By changing the weights, we can reorder the columns of the diagram. The following three
     displays display the first degree, the second degree, and the total degree, respectively.",
    EXAMPLE lines ///
      betti(B, Weights => {1,0})
      betti(B, Weights => {0,1})
      B' = betti(B, Weights => {1,1})
    ///,
    SeeAlso => {
	MultigradedBettiTally,
	(betti, BettiTally)
	}
    }

document { Key => {(netList, VisibleList),
	  netList,
	  [netList, Boxes],
	  [netList, BaseRow],
	  [netList, HorizontalSpace],
	  [netList, VerticalSpace],
	  [netList, Alignment]},
     Headline => "a table of boxes",
     Usage => "netList v",
     Inputs => { 
	  "v" => {"a list of lists of things to be converted to nets and displayed as a table in a net"},
	  Boxes => Boolean => {"whether to draw boxes around the individual nets"},
	  BaseRow => ZZ => {"the index of the base row, for the purpose of setting the baseline of the net produced.  The value
	       is allowed to be as large as the length of ", TT "v", ", larger by 1 than one might expect."},
	  HorizontalSpace => ZZ => {"the amount of space horizontally between entries or between entries and their enclosing boxes"},
	  VerticalSpace => ZZ => "the amount of space vertically between entries or between entries and their enclosing boxes",
	  Alignment => {TT "Center", ", ", TT "Left", ", ", TT "Right", ", or a list of those symbols indicating horizontal adjustment; if it's a list, the ", TT "i", "-th
	       entry specifies the adjustment in the ", TT "i", "-th column; if not, the symbol applies to all columns."}
	  },
     Outputs => {{"a net obtained by converting the elements of each list in the list of lists ", TT "v", " to nets and arranging them in a table, as specified by the
	       options"}},
     EXAMPLE lines ///
	  f = {{"hi there","foo"},{-3, 2^40}}
	  netList f
	  netList(f,Boxes=>false)
	  netList(f,Boxes=>true,HorizontalSpace=>1,VerticalSpace=>1)
	  netList(f,Boxes=>true,Alignment=>Center)
	  netList(f,Boxes=>true,BaseRow=>1)
	  netList apply(5,i->apply(i+1,j->(i,j)))
     ///}

document { Key => cache,
     Headline => "a key under which to store cache tables",
     SeeAlso => {CacheTable},
     EXAMPLE lines ///
     	  F = ZZ^3
     	  peek F
	  F.cache#Foo = Bar
	  peek F
	  peek F.cache
	  F === ZZ^3
     ///}
document { Key => {(capture, String),capture},
     Headline => "evaluate Macaulay2 code and capture the output (under development)" }
document { Key => "catch",
     Headline => "catch a thrown exception", SeeAlso => {"throw"},
     Usage => "catch c",
     Outputs => {{"the value obtained by evaluating the code ", TT "c", ", or, if a ", TO "throw", " was executed during the evaluation of ", TT "c", ",
	       the argument given to ", TO "throw", "."}},
     EXAMPLE lines ///
          catch scan(0..10, i -> if i == 5 then throw 18 else print i)
     ///}
document { Key => "throw",
     Headline => "throw an exception", SeeAlso => {"catch"},
     Usage => "throw x", 
     Consequences => {{"the flow of control is passed to the surrounding ", TO "catch", ", and ", TT "x", " is returned as its value"}},
     EXAMPLE lines ///
          catch scan(0..10, i -> if i == 5 then throw 18 else print i)
     ///}
document { Key => centerString,
     Headline => "center a string or net",
     Usage => "centerString(wid,s)",
     Inputs => { "wid" => ZZ, "s" => Net },
     Outputs => {{"a net with spaces added, as needed, to center ", TT "s", " in a net of width ", TT "wid" }},
     EXAMPLE lines ///
         centerString(18,"asdf"||"qwer")
     ///}
document { Key => commandInterpreter,
     Headline => "the top level command interpreter",
     Usage => "commandInterpreter f",
     Inputs => { 
	  "f" => { TT "()", " or ", ofClass{Dictionary, Symbol, Pseudocode, Function} }
	  },
     Consequences => {{"the top level command interpreter will be called with the symbols in the lexical scope of ", TT "f", " visible to the user."}},
     "One more ", TT "i", " will be added to prompt each time the command interpreter is entered.  To leave it, type ", TT "end", " or the end of file character.",
     EXAMPLE lines ///
     	  f = (x -> y -> 11) 13
     	  commandInterpreter f
	  x
	  end
	  x
     ///,
     "This facility is useful as a debugging tool, and is used by the standard debugger."
     }

document { Key => "continue",
     Headline => "continue with the next iteration of a loop",
     Usage => "continue x",
     Inputs => {"x"},
     Consequences => {
	  {"the currently executing ", TT "list", "-clause of a ", TO "for", "-loop or ", TO "while", "-loop is 
	       finished, and iteration continues with the ", TO "do", "-clause or the next iteration of
	       the loop, if any.  The value ", TT "x", " is added to the list being accumulated.
	       If ", TT "x", " is omitted, then no value is added to the list, and the statement may be used in a ", TT "do", "-clause."
	       },
	  {"Alternatively, as a debugger command, causes execution to be resumed, starting with the current expression."}
	  },
     EXAMPLE lines ///
          for i from 1 to 4 list (continue 4; print ho) do print hi
          for i from 1 to 4 list (continue ; 14) do print hi
          for i from 1 to 4 list 14 do print hi
	  i = 0 ; while i < 10 do ( i = i+1; if i == 5 then continue ; print i )
     ///,
     PARA {
	  "Here is an example of the use of ", TO "continue", " in the debugger after altering a value so continuation will not cause the
	  error to recur."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code f
     x
     x = 11
     continue
     ///
     }
document { Key => "copyright",
     Headline => "a string containing the copyright notice for Macaulay2",
     EXAMPLE "copyright" }
document { Key => {listSymbols,(listSymbols, Dictionary), (listSymbols, List)},
     Headline => "compact display of symbols and their values",
     Usage => "listSymbols v",
     Inputs => { "v" => {ofClass{List,Dictionary}, "; if it's a list, it should be a list of symbols"}},
     Outputs => { Net => {"a compact display of the symbols in ", TT "v", " and their values"}},
     EXAMPLE lines ///
     	  x:=3; y:="hi there"; z:=2^30; f = x->x;
	  listSymbols { symbol x, symbol y }
	  listSymbols first localDictionaries()
     ///}

document { Key => listLocalSymbols,
     Headline => "display of local symbols and their values",
     SYNOPSIS (
	  Usage => "listLocalSymbols f",
	  Inputs => { "f" => {ofClass{Pseudocode,Symbol,Dictionary,Function}}},
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to the closure ", TT "f", ", and their values"}},
	  EXAMPLE lines ///
	       x:=3; y:="hi there"; z:=2^30; f = x->x;
	       listLocalSymbols f
	       listLocalSymbols symbol x
	  ///
	  ),
     SYNOPSIS (
	  Usage => "listLocalSymbols",
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to ", TO "current"}},
	  PARA {
	       "This usage works only in the debugger, where ", TO "current", " has a non-null value."
	       },
	  EXAMPLE lines ///
	  load "Macaulay2Doc/demo1.m2"
	  g 2
	  listLocalSymbols
	  ///,
	  ),
     SYNOPSIS (
     	  Usage => "listLocalSymbols(X,f)",
     	  Inputs => { "X" => Type, "f" => {ofClass{Pseudocode,Symbol,Dictionary,Function}}},
     	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to the closure ", TT "f", ", and their values, provided their
		    values are instances of the type ", TT "X"}},
	  EXAMPLE lines ///
	  h := x -> y -> y+1;
	  listLocalSymbols(ZZ,h 11)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "listLocalSymbols X",
	  Outputs => { Net => {"a compact display of the symbols in the local dictionaries attached to ", TO "current", " whose values
		    have type ", TT "X", "."}},
	  PARA {
	       "This usage works only in the debugger, where ", TO "current", " has a non-null value."
	       },
	  EXAMPLE lines ///
	  load "Macaulay2Doc/demo1.m2"
	  g 2
	  listLocalSymbols ZZ
	  ///
	  )
     }

document { Key => {makeDocumentTag,(makeDocumentTag, Thing), (makeDocumentTag, DocumentTag), (makeDocumentTag, String)},
     Headline => "convert a documentation key to a documentation tag",
     EXAMPLE lines ///
     	  makeDocumentTag (res,Module)
	  peek oo
     	  makeDocumentTag (koszul,ZZ,Matrix)
	  peek oo
	  makeDocumentTag [res,PairLimit]
	  peek oo
     ///,
     SeeAlso => {"documentation keys"}
     }

document { Key => (module, Ring),
     Usage => "module R",
     Inputs => {"R"},
     Outputs => {{"the free module of rank 1 over the ring R"}},
     EXAMPLE lines ///
     	  ZZ
	  module ZZ
     ///}

document { Key => (module, Vector),
     Headline => "the module of a vector",
     Usage => "module v",
     Inputs => {"v"},
     Outputs => {{"the module that contains the vector ", TT "v"}},
     "The class of ", TT "v", " is also equal to the module of ", TT "v", ".",
     EXAMPLE lines ///
     	  F = ZZ^4
	  v = F_2
	  module v
	  class v
     ///}

document { Key => {package,(package, Dictionary), (package, Thing),
	  (package, HashTable), (package, Function), (package, Symbol),
	  (package, Sequence)
	  },
     Headline => "get containing package",
     Usage => "package x",
     Inputs => {"x"},
     Outputs => {{"the package in which the documentation key ", TT "x", " was defined"}},
     EXAMPLE lines ///
     	  package sin
	  package poly
     ///}
document { Key => {(rotate, ZZ, VisibleList),rotate},
     Headline => "rotate a list",
     Usage => "rotate(n,v)",
     Inputs => {"n","v"},
     Outputs => {{"the list obtained by rotating the list ", TT "v", " leftward ", TT "n", " places"}},
     EXAMPLE lines ///
     	 p = 0 .. 20
	 rotate(3,p)
	 rotate(-3,p)
     ///}
document { Key => "fileDictionaries",
     Headline => "local dictionaries for loaded files",
     Usage => "fileDictionaries#fn",
     Inputs => { "fn" => String },
     Outputs => {{"the local dictionary in effect for the scope of the file loaded from the path ", TT "fn"}},
     }
document { Key => fileMode,
     Headline => "set or get file mode"
     }
document { Key => (fileMode,String),
     Headline => "get file mode",
     Usage => "fileMode fn",
     Inputs => {"fn"},
     Outputs => {{"the mode of the file located at the filename or path ", TT "fn"}},
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  fn << "hi there" << close
	  fileMode fn
	  removeFile fn
     ///	       
     }
document { Key => (fileMode,File),
     Headline => "get file mode",
     Usage => "fileMode f",
     Inputs => {"f"},
     Outputs => {{"the mode of the open file ", TT "f"}},
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  f = fn << "hi there"
	  fileMode f
	  close f
	  removeFile fn
     ///	       
     }
document { Key => (fileMode,ZZ,String),
     Headline => "set file mode",
     Usage => "fileMode(mo,fn)",
     Inputs => {"mo","fn"},
     Consequences => {{"the mode of the file located at the filename or path ", TT "fn", " is set to ", TT "mo"}},
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  fn << "hi there" << close
	  m = fileMode fn
	  fileMode(m|7,fn)
	  fileMode fn
	  removeFile fn
     ///	       
     }

document { Key => (fileMode,ZZ,File),
     Headline => "set file mode",
     Usage => "fileMode(mo,f)",
     Inputs => {"mo","f"},
     Consequences => {{"the mode of the open file ", TT "f", " is set to ", TT "mo"}},
     EXAMPLE lines ///
     	  fn = temporaryFileName()
	  f = fn << "hi there"
	  m = 7 + 7*8 + 7*64
	  fileMode(m,f)
	  fileMode f
	  close f
	  fileMode fn
	  removeFile fn
     ///	       
     }

document { Key => {frames,(frames, Symbol), (frames, Sequence), (frames, Pseudocode), (frames, Function)},
     Headline => "get the frames associated to a closure",
     Usage => "frames f",
     Inputs => { "f" => {"() or ", ofClass{Symbol,Function,Pseudocode}}},
     Outputs => {{"a list of mutable lists, the frames attached to the closure ", TT "f", " or, if ", TT "f", " is ", TT "()", ", then
	       the frames attached to the current lexical scope" 
	       }},
     "This function is occasionally useful as a debugging tool.",
     EXAMPLE lines ///
     	  f = (x,y,z) -> t -> t
	  g = f(111,222,"hi there")
	  frames g
	  peek first oo
     ///}

document {
     Key => globalAssignFunction,
     Headline => "the standard method for the global assignment hook",
     TT "globalAssignFunction", " -- the standard function that can be used
     as a method for ", TO GlobalAssignHook, " so that certain types of
     things, when assigned to a global variable, will acquire
     the name of the global variable as their name.  The companion function
     ", TO "globalReleaseFunction", " is used to release the name when the
     global variable gets reassigned.",
     PARA{},
     "Another thing done by this function is to apply ", TO use, " to the thing.
     This is used for polynomial rings to assign values to the symbols representing
     the variables (indeterminates) in the ring.",
     PARA{},
     EXAMPLE {
	  "X = new Type of MutableHashTable",
      	  "x = new X",
      	  "X.GlobalAssignHook = globalAssignFunction",
      	  "X.GlobalReleaseHook = globalReleaseFunction",
      	  "x' = new X",
      	  "t = {x,x'}",
      	  "x = x' = 44",
      	  "t",
      	  "code globalAssignFunction",
	  },
     SeeAlso => { "symbol", "SelfInitializingType" }
     }

document {
     Key => globalReleaseFunction,
     Headline => "the standard method for the global variable release hook",
     TT "globalReleaseFunction", " -- the standard function that can be used as
     a method for ", TO GlobalReleaseHook, " so that certain types of things, which
     have acquired as their name the name of a global variable to which they have
     been assigned, will lose that name when a different value is assigned to
     the variable.",
     PARA{},
     SeeAlso => "globalAssignFunction"
     }

document { Key => globalAssignment,
     Headline => "install standard global assignment method",
     Usage => "globalAssignment X",
     Inputs => { "X" => Type },
     Consequences => {{"the functions ", TO "globalAssignFunction", " and ", TO "globalReleaseFunction", " are installed in the type ", TT "X", " 
	       under ", TO "GlobalAssignHook", " and ", TO "GlobalReleaseHook", ", respectively.  The effect is that when an object of type ", TT "X", " is
	       assigned to a global variable, the function ", TO "use", " is called on it, and thereafter that object will print out as the name of the variable."
	       }},
     "One type for which this has been done is ", TO "Ring", ", as illustrated in the following example.",
     EXAMPLE lines ///
     	  S := QQ[x]
	  S
	  S^3
	  R = S
	  S
	  S^3
     ///}

document { Key => toAbsolutePath,
     Headline => "the absolute path version of a file name",
     Usage => "toAbsolutePath filename",
     Inputs => { "filename" => String },
     Outputs => {
	  String => {"the absolute (real) path name of ", TT "filename"}
	  },
     EXAMPLE lines ///
         toAbsolutePath "a/b.m2"
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,minimizeFilename, relativizeFilename, baseFilename, "path", "rootPath", "rootURI"}
     }
document { Key => baseFilename,
     Headline => "the base part of a filename or path",
     Usage => "baseFilename fn",
     Inputs => { "fn" => String => "a filename or path" },
     Outputs => { "the last component of the path" },
     EXAMPLE lines ///
     	  baseFilename "/a/b/dir/"
     	  baseFilename "/a/b/file"
     ///,
     SeeAlso => {File,minimizeFilename, relativizeFilename, toAbsolutePath, searchPath, "path"}
     }
document { Key => {(searchPath, List, String), (searchPath, String), searchPath},
     Headline => "search a path for a file",
     Usage => "searchPath(pa,fn)\nsearchPath fn",
     Inputs => { "pa" => {"a list of strings giving paths to directories.  Each one ends with a slash.
	  If omitted, then ", TO "path", " is used"}, "fn" },
     Outputs => {{"a list of those directories in ", TT "pa", " containing files named ", TT "fn" }},
     SeeAlso => {File,minimizeFilename, relativizeFilename, baseFilename, toAbsolutePath, "path"}     
     }
document { Key => minimizeFilename,
     Headline => "minimize a file name",
     Usage => "minimizeFilename fn",
     Inputs => { "fn" => "a path to a file" },
     Outputs => {{"a minimized path, equivalent to ", TT "fn"}},
     EXAMPLE lines ///
         minimizeFilename "a/b/c/../d"
	 minimizeFilename "../../../../../../"
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,relativizeFilename, baseFilename, toAbsolutePath, searchPath, "path"}
     }
document { Key => relativizeFilename,
     Headline => "relativize a file name",
     Usage => "relativizeFilename(dir,fn)",
     Inputs => { "dir" => "a path to a directory", "fn" => "a path to a file" },
     Outputs => {{"a relativized path, equivalent to ", TT "fn", " when starting from ", TT "dir"}},
     EXAMPLE lines ///
         relativizeFilename("a/b/","a/b/c/d")
         relativizeFilename("a/b/c/d","a/b/")
         relativizeFilename("a/b/c/d","a/b/e/f")
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,minimizeFilename, baseFilename, toAbsolutePath, searchPath, "path"}
     }
document {
     Key => temporaryFileName,
     Headline => "make a temporary file name",
     Usage => "temporaryFileName()",
     Outputs => { "a unique temporary file name." },
     "The file name is so unique that even with various suffixes
     appended, no collision with existing files will occur.  The files will be removed
     when the program terminates, unless it terminates as the result of an error.",
     EXAMPLE {
	  ///temporaryFileName () | ".tex"///,
     	  ///temporaryFileName () | ".html"///,
	  },
     PARA{
     	  "This function will work under Unix, and also under Windows
     	  if you have a directory on the same drive called ", TT "/tmp", "."
	  },
     PARA {
	  "If the name of the temporary file will be given to an external program, it may be necessary to
	  concatenate it with ", TO "rootPath", " or ", TO "rootURI", " to enable the external program to find the file."
	  },
     PARA {
	  "The temporary file name is derived from the value of the environment variable ", TT "TMPDIR", ", if it has one."
	  },
     PARA {
	  "If ", TO "fork", " is used, then the parent and child Macaulay2 processes will each remove their
	  own temporary files upon termination, with the parent removing any files created before ", TO "fork",
	  " was called.",
	  },
     SeeAlso => {File, "rootPath", "rootURI"}
     }

document { Key => info,
     Headline => "convert hypertext to info format",
     "This function is used internally when preparing documentation."
     }

document { Key => "nullaryMethods",
     "This experimental hash table is to contain methods for handling the case where a method function, ", TT "f", ", say, is called with
     0 arguments, i.e., as ", TT "f()", "."
     }

document { Key => pager,
     Headline => "display with paging",
     Usage => "pager x",
     Inputs => {"x"},
     Consequences => {{TT "x", " is converted to a net and displayed through the pager specified by the environment variable PAGER, if set,
	       else through the program ", TT "more", "."
	       }}}

document { Key => {precision,
	  (precision, QuotientRing), (precision, Ring),(precision,Number),
	  (precision, MutableMatrix),(precision, RingElement),(precision, PolynomialRing),
	  (precision, InexactNumber),(precision, InexactField),(precision, Matrix)
	  },
     Usage => "precision x",
     Inputs => { "x" => {ofClass{Ring,Matrix,RingElement,Number}}},
     Outputs => { ZZ => {"the precision to which ", TT "x", " or its instances are stored"}},
     EXAMPLE lines ///
     	  precision 3p111
	  precision (RR[x])
	  precision 3
     ///
     }

document { Key => "printingTimeLimit",
     "This variable specifies the number of seconds to allow for printing an output line"
     }

document { Key => "stopIfError",
     Headline => "whether to stop the program when an error occurs"
     }
document { Key => "interpreterDepth",
     Headline => "nesting depth of the interpreter",
     SeeAlso => {commandInterpreter}}
document { Key => "printingPrecision",
     Headline => "current precision for printing numbers",
     Usage => "printingPrecision = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits of precision.
	       Meaningless digits will not be displayed.
	       The special case where ", TT "n=0", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line to display
	       all the meaningful digits."}
	  },
     EXAMPLE lines ///
     	  1/3p100
     	  {1/3p100}
	  printingPrecision
	  printingPrecision = 16
     	  {1/3p100}
	  printingPrecision = 0
     	  {1/3p100}
     ///,
     PARA {
	  "For complex numbers, if ", TO "printingAccuracy", " is set to its default value of ", TT "-1", ",
	  the two parts of the number are treated together (although a digit further further to the right of
	  the point may sometimes be displayed in the smaller part)."
	  },
     EXAMPLE lines ///
     printingAccuracy
     printingPrecision = 16
     {1p100e12/3+1p100/3*ii}
     printingAccuracy = 10
     {1p100e12/3+1p100/3*ii}
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingAccuracy",
     Headline => "current accuracy for printing numbers",
     Usage => "printingAccuracy = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Henceforth, inexact numbers are printed with at most ", TT "n", " digits to the right of
	       the decimal point displayed.
	       The special case where ", TT "n=-1", " is
	       interpreted as meaning ", TT "n=infinity", ", and this case is
	       used when a number appears alone on an output line."}
	  },
     EXAMPLE lines ///
	  printingPrecision,printingAccuracy
     	  1p100e-5/3
     	  x = {1p100e-5/3,1p100e-4/3,1p100e-3/3,1p100e-2/3}
	  printingAccuracy = 8
     	  x
	  printingAccuracy = 4
     	  x
     ///,
     SeeAlso => {"printingAccuracy", "printingLeadLimit", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingLeadLimit",
     Headline => "maximum number of leading zeroes to use when printing real numbers",
     Usage => "printingLeadLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " leading zeroes."}
	  },
     EXAMPLE lines ///
     	  1/30000000000.
	  printingLeadLimit
	  printingLeadLimit = 20
     	  1/30000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingTrailLimit", "printingSeparator", format}
     }
document { Key => "printingTrailLimit",
     Headline => "maximum number of additional trailing digits to use when printing real numbers",
     Usage => "printingTrailLimit = n",
     Inputs => {
	  "n" => ZZ
	  },
     Consequences => {
	  {"Real numbers are printed with at most ", TT "n", " additional trailing digist, in addition to those specified by ", TT "printingPrecision", "."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingTrailLimit
	  printingTrailLimit = 20
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingSeparator", format}
     }
document { Key => "printingSeparator",
     Headline => "string used to separate mantissa from exponent when printing real numbers",
     Usage => "printingSeparator = s",
     Inputs => {
	  "s" => String
	  },
     Consequences => {
	  {"The string ", TT "s", " will be used to separate mantissa and exponent when printing real numbers."}
	  },
     EXAMPLE lines ///
     	  3000000000000.
	  printingSeparator
	  printingSeparator = "E"
     	  3000000000000.
     ///,
     SeeAlso => {"printingPrecision", "printingAccuracy", "printingLeadLimit", "printingTrailLimit", format}
     }

document { Key => "notify",
     Headline => "whether to notify the user when a file is loaded",
     Usage => "notify = true\nnotify = false",
     Consequences => {
	  {"If ", TO "notify", " is set to ", TO "true", ", then each time a file or a package is loaded, a message will be displayed."}
	  },
     EXAMPLE lines ///
     notify = true
     loadPackage "FirstPackage"
     ///,
     SeeAlso => {load, needs, loadPackage, needsPackage}
     }

document { Key => "fileExitHooks",
     Headline => "a list of hooks (functions) to execute when the current file has been loaded"
     }
document { Key => "engineDebugLevel",
     Headline => "current engine debugging level",
     "This variable is in place, but the corresponding code is not implemented yet."
     }
document { Key => "debuggingMode",
     Headline => "whether to enter the debugger when an error occurs",
     Usage => "debuggingMode = true",
     Consequences => {{"the debugger will be entered when an error occurs"}}}
document { Key => "debugLevel",
     Headline => "current level debugging",
     Usage => "debugLevel = n",
     Inputs => {"n" => ZZ },
     "Some M2 routines will display debugging information if ", TO "debugLevel", " is set to a value greater than 0."}
document { Key => "debugError",
     Headline => "a function to debug",
     Usage => "debugError()",
     "In certain situations, after an error occurs, the offending code, in the form of a function, will be stored in the
     variable ", TO "debugError", ", so the user can debug it by running it."
     }
document { Key => "currentPackage",
     Headline => "the current package",
     EXAMPLE lines ///
     	  newPackage "Foo"
	  currentPackage
	  endPackage "Foo"
     ///}

document { Key => {quotientRemainder,(quotientRemainder, Matrix, GroebnerBasis), (quotientRemainder, Matrix, Matrix)},
     Headline => "matrix quotient and remainder",
     Usage => "(q,r) = quotientRemainder(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon division by ", TT "g"},
	  "r" => {"the remainder of ", TT "f", " upon division by ", TT "g"}
	  },
     "The equation ", TT "g*q+r == f", " will hold.  The source of ", TT "f", " should be a free module.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  (q,r) = quotientRemainder(f,g)
	  g*q+r == f
	  f = f + map(target f, source f, id_(R^2))
	  (q,r) = quotientRemainder(f,g)
	  g*q+r == f
     ///,
     SeeAlso => {quotientRemainder'}
     }

document { Key => {quotientRemainder',(quotientRemainder', Matrix, Matrix)},
     Headline => "matrix quotient and remainder (opposite)",
     Usage => "(q,r) = quotientRemainder'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon (opposite) division by ", TT "g"},
	  "r" => {"the remainder of ", TT "f", " upon (opposite) division by ", TT "g"}
	  },
     "The equation ", TT "q*g+r == f", " will hold.  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "quotientRemainder", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  (q,r) = quotientRemainder'(f,g)
	  q*g+r == f
	  f = f + map(target f, source f, id_(R^2))
	  (q,r) = quotientRemainder'(f,g)
	  q*g+r == f
     ///,
     SeeAlso => {quotientRemainder},
     SourceCode => {quotientRemainder'}
     }
     
document { Key => {(quotient, Matrix, GroebnerBasis), (quotient, Matrix, Matrix)},
     Headline => "matrix quotient",
     Usage => "q = quotient(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon division by ", TT "g"}
	  },
     "The equation ", TT "g*q+r == f", " will hold, where ", TT "r", " is the map provided by ", TO "remainder", ".  The source of ", TT "f", " should be a free module.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  quotient(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  quotient(f,g)
     ///,
     SeeAlso => {quotientRemainder,quotient'}
     }

document { Key => {quotient',(quotient', Matrix, Matrix)},
     Headline => "matrix quotient (opposite)",
     Usage => "q = quotient'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon (opposite) division by ", TT "g"},
	  },
     "The equation ", TT "q*g+r == f", " will hold, where ", TT "r", " is the map provided by ", TO "remainder'", " .  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "quotient", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  quotient'(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  quotient'(f,g)
     ///,
     SeeAlso => {quotientRemainder,quotient},
     SourceCode => {quotient'}
     }

document { Key => {remainder,(remainder, Matrix, GroebnerBasis), (remainder, Matrix, Matrix)},
     Headline => "matrix remainder",
     Usage => "r = remainder(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "r" => {"the remainder of ", TT "f", " upon division by ", TT "g"}
	  },
     PARA{"This operation is the same as ", TO (symbol %, Matrix, GroebnerBasis), "."},
     PARA{"The equation ", TT "g*q+r == f", " will hold, where ", TT "q", " is the map provided by ", TO "quotient", ".  The source of ", TT "f", " should be a free module."},
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  remainder(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  remainder(f,g)
     ///,
     SeeAlso => {quotientRemainder,remainder'}
     }

document { Key => {remainder',(remainder', Matrix, Matrix)},
     Headline => "matrix quotient and remainder (opposite)",
     Usage => "r = remainder'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "r" => {"the remainder of ", TT "f", " upon (opposite) division by ", TT "g"}
	  },
     "The equation ", TT "q*g+r == f", " will hold, where ", TT "q", " is the map provided by ", TO "quotient'", ".  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "remainder", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  remainder'(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  remainder'(f,g)
     ///,
     SeeAlso => {quotientRemainder,remainder},
     SourceCode => {remainder'}
     }

document { Key => toLower,
     Headline => "convert to lower case",
     Usage => "toLower s",
     Inputs => {"s"=>String},
     Outputs => {String => {"the string produced from ", TT "s", " by converting its characters to lower case"}},
     EXAMPLE lines ///
     	  toLower "A b C d E f"
     ///}

document { Key => toUpper,
     Headline => "convert to upper case",
     Usage => "toUpper s",
     Inputs => {"s"=>String},
     Outputs => {String => {"the string produced from ", TT "s", " by converting its characters to lower case"}},
     EXAMPLE lines ///
     	  toUpper "A b C d E f"
     ///}

document { Key => "synonym",
     Headline => "synonym for members of a class",
     Usage => "synonym X",
     Inputs => { "X" => Type },
     Outputs => { String => {"a synonym for members of the class ", TT "X" }},
     "A synonym can be installed with the assignment statement ", TT "X.synonym=t", ".  The synonym is used by ", TO "ofClass", ".",
     EXAMPLE lines ///
     	  synonym ZZ
	  Stack = new Type of HashTable
	  synonym Stack
	  Stack.synonym = "Deligne-Mumford stack"
	  ofClass Stack
     ///}

document { Key => {
	  (sheafHom, CoherentSheaf, CoherentSheaf),
	  sheafHom,
	  (sheafHom, SheafOfRings, CoherentSheaf),
	  (sheafHom, CoherentSheaf, SheafOfRings),
	  (sheafHom, SheafOfRings, SheafOfRings)
	  },
     Headline => "sheaf Hom",
     Usage => "sheafHom(M,N)",
     Inputs => {"M","N"},
     Outputs => {{"the coherent sheaf of homomorphisms from ", TT "M", " to ", TT "N", ""}},
     "If ", TT "M", " or ", TT "N", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "M", " and ", TT "N", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
     PARA{},
     "The result is the sheaf associated to the graded module Hom(module M, module N).",
     EXAMPLE lines ///
     	  X = Proj(QQ[x,y])
	  sheafHom(OO_X^1(2),OO_X(11)^1)
     ///,
     SeeAlso => {OO, sheafExt, Hom, Ext, HH, (Hom, CoherentSheaf, CoherentSheaf)}
     }

document { Key => "globalAssignmentHooks",
     Headline => "assignment hooks for global symbols",
     Usage => "globalAssignmentHooks#s = f",
     Inputs => {
	  "s" => Symbol => "a global symbol",
	  "f" => Function => {"a function of two arguments, ", TT "f = (sym,val) -> ...", "; the argument ", TT "sym", " is the symbol whose
	       value is about to be assigned to, and ", TT "val", " is the value about be assigned to it"
	       }
	  },
     Consequences => {
	  {"whenever an assignment statement of the form ", TT "s=e", " is done, the expression ", TT "f(s,e)", " is first evaluated"}
	  },
     SeeAlso => {globalAssignFunction}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
