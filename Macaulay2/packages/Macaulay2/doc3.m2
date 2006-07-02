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
     TT "flagLookup x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
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
	  (symbol \,Function,Tally),
	  (symbol \,SelfInitializingType,VisibleList),
	  (symbol \,Command,VisibleList),
	  (symbol \,RingMap,List),
	  (symbol \,Command,Tally),
	  (symbol /,VisibleList,SelfInitializingType),
	  (symbol /,List,Command),
	  (symbol /,Tally,Command),
	  (symbol /,Tally,Function),
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
     Key => hash,
     Headline => "hash code of an object",
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA{},
     "The hash code of ", TT "x", " is an integer produced in a deterministic way
     from ", TT "x", ", and perhaps from the hash codes of the contents of ", TT "x", ".
     See ", TO "hashing", " for a discussion of the requirements that
     the hash codes used here are designed to satisfy."
     }

document {
     Key => remove,
     Headline => "remove an entry from a hash table",
     TT "remove(x,k)", " -- removes the entry stored in the hash table ", TT "x", "
     under the key ", TT "k", ".",
     PARA{},
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
	  }
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
     "Keywords are symbols that are treated specially by the system while parsing user input.  Some of them,
     such as ", TO "and", ", consist of alphanumeric characters and look just like
     ordinary symbols.  Others, such as ", TO "==>", ", consist of special characters
     and are called operators."
     }

document {
     Key => File,
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", "."
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
     Key => mutable,
     Headline => "whether something may be modified",
     TT "mutable x", " -- returns true or false, depending on whether x is mutable.",
     PARA{},
     "If ", TT "x", " is a hash table, list, or database, then it's mutable if its contents
     can be destructively altered.",
     PARA{},
     "If ", TT "x", " is a symbol, then it's mutable if a value can be assigned to
     it. (See ", TO "protect", ".)",
     PARA{},
     "If ", TT "x", " is anything else, then it isn't mutable.",
     PARA{},
     "The contents of a mutable hash table do not participate in strong comparison
     with ", TO "===", " or in ", TO "hashing", ".",
     SeeAlso => {"MutableList", "MutableHashTable"}
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
     SUBSECTION "Examples",
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
     "The value returned by ", TO2{ (options,Function), "options" }, " is a type of hash table that can be used to obtain default values.",
     EXAMPLE {
	  "(options gb).Syzygies"
	  }
     }

document {
     Key => {operatorAttributes, Flexible, Binary, Prefix, Postfix},
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

undocumented lift'
undocumented promote'
undocumented liftable'

undocumented methods lift'
undocumented methods promote'
undocumented methods liftable'

document { Key => MethodFunction,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
document { Key => MethodFunctionWithOptions,
     Headline => "a type of method function", "Functions of this type are created by ", TO "method", "." }
undocumented (methodOptions, MethodFunctionWithOptions)
undocumented (methodOptions, MethodFunction)
document { Key => {(methodOptions, Function),methodOptions},
     Headline => "recover the options used when a method function was created",
     Usage => "methodOptions f",
     Inputs => { "f" },
     Outputs => {{ "the options used when ", TT "f", " was created by ", TO "method", "" }},
     EXAMPLE lines ///
     	  methodOptions source
     	  methodOptions res
     ///
     }

document { Key => ExampleItem,
     Headline => "a type of hypertext for holding example inputs awaiting outputs" }
document { Key => {(numRows, MutableMatrix),numRows},
     Headline => "number of rows in a mutable matrix",
     Usage => "numRows m", Inputs => { "m" }, Outputs => {{ "the number of rows in ", TT "m" }}}
document { Key => {(numColumns, MutableMatrix),numColumns},
     Headline => "number of columns in a mutable matrix",
     Usage => "numColumns m", Inputs => { "m" }, Outputs => {{ "the number of columns in ", TT "m" }}}
document { Key => {mutableMatrix,(mutableMatrix, MutableMatrix),(mutableMatrix, Matrix),(mutableMatrix, List)},
     Headline => "make a mutable matrix",
     Usage => "mutableMatrix m",
     Inputs => { "m" => {ofClass{Matrix, MutableMatrix, List}}},
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
document { Key => {(randomMutableMatrix, ZZ, ZZ, RR, ZZ),randomMutableMatrix},
     Headline => "a random mutable matrix of integers",
     Usage => "randomMutableMatrix(nrows,ncols,zerof,max)",
     Inputs => {
	  "nrows",
	  "ncols",
	  "zerof" => { "between 0 and 1" },
	  "max"
	  },
     Outputs => {
	  {"a random mutable ", TT "nrows", " by ", TT "ncols", " matrix of integers.  The absolute value of the entries is bounded by ", TT "max", ", and
	       the frequency of entries equal to zero is given by ", TT "zerof", "." }
	  },
     EXAMPLE lines ///
          randomMutableMatrix(10,15,.9,100)
     ///
     }

document { Key => {(mutableZero, Ring, ZZ, ZZ),mutableZero},
     Headline => "make a mutable matrix filled with zeroes",
     Usage => "mutableZero(R,nrows,ncols)",
     Inputs => { "R","nrows","ncols" },
     Outputs => {{"an ", TT "nrows", " by ", TT "ncols", " mutable matrix filled with zeroes from the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableZero(QQ,10,20)
	 m_(5,5) = 11/13
	 m
     ///
     }
document { Key => {(mutableIdentity, Ring, ZZ),mutableIdentity},
     Headline => "make a mutable identity matrix",
     Usage => "mutableIdentity(R,nrows)",
     Inputs => { "R","nrows" },
     Outputs => {{"an ", TT "nrows", " by ", TT "nrows", " mutable identity matrix filled elements of the ring ", TT "R" }},
     EXAMPLE lines ///
         m = mutableIdentity(QQ,10)
	 m_(5,5) = 11/13
	 m
     ///
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

document { Key => {(copyDirectory, String, String),copyDirectory,[copyDirectory, Exclude],[copyDirectory, FollowLinks],[copyDirectory, Verbose]},
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
document { Key => {(moveFile, String, String),moveFile},
     Usage => "moveFile(src,dst)",
     Inputs => {
	  "src" => "the filename or path to an existing file",
	  "dst" => "the new filename or path to a location (on the same file system)",
	  Verbose => Boolean => {"whether to report individual file operations"}
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
	  removeFile dst
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

document { Key => CCC,
     Headline => "high-precision complex numbers", "This class is experimental." }
document { Key => RRR,
     Headline => "high-precision real numbers", "This class is experimental." }
document { Key => GlobalDictionary,
     Headline => "the class of all global dictionaries",
     SeeAlso => { "dictionaryPath", LocalDictionary }
     }
document { Key => LocalDictionary,
     Headline => "the class of all local dictionaries",
     "A local dictionary is one used in connection with a local scope, such as one which is bounded by the body of a function closure.
     A local dictionary is created on the fly by the interpreter, and after the scope has been closed, the dictionary can be enlarged no further.
     Accessing local dictionaries can be a useful debugging tool.  The local dictionaries accessible to the user come with frames, so their symbols
     have values; thus they may be referred to as dictionary closures.",
     SeeAlso => { localDictionaries, GlobalDictionary }
     }
document { Key => localDictionaries,
     Headline => "get local dictionaries",
     Usage => "localDictionaries f",
     Inputs => {
	  "f" => {"() or ", ofClass{Function,Symbol,Pseudocode,LocalDictionary,GlobalDictionary}}
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

document { Key => Wrap,
     Headline => "a key for methods for wrapping printed output",
     "The default method for printing results of computations (stored in ", TT "Thing.Print", ") searches for a method for wrapping the output by search in the class of 
     the result (and its ancestors) for a function stored under the key ", TT "Wrap", ".  The preinstalled wrapping methods use ", TO "wrap", ", they differ in their choice of
     separator between wrapped lines, or in whether to wrap at all.",
     EXAMPLE lines ///
     	  QQ[x_0 .. x_40 ]
	  concatenate(50:"abcd ")
     ///
     }

document { Key => baseFilename,
     Headline => "the base part of a filename or path",
     Usage => "baseFilename fn",
     Inputs => { "fn" => String => "a filename or path" },
     Outputs => { "the last component of the path" },
     EXAMPLE lines ///
     	  baseFilename "/a/b/dir/"
     	  baseFilename "/a/b/file"
     ///
     }
document { Key => BettiTally,
     Headline => "the class of all Betti tallies",
     "A Betti tally is a special type of ", TO "Tally", " that is printed as a display of graded Betti numbers.  The class was created
     so the function ", TO "betti", " could return something that both prints nicely and from which information can be extracted.  The keys
     are pairs ", TT "(i,d)", ", where ", TT "i", " is the homological degree, and ", TT "d", " is a list of integers giving a multidegree.
     Only the first component of ", TT "d", " is used in printing.",
     EXAMPLE lines ///
          t = new BettiTally from { (0,{0}) => 1, (1,{1}) => 2, (2,{3}) => 3, (2,{4}) => 4 }
	  peek oo
	  t#(2,{4})
     ///
     }

document { Key => {(searchPath, List, String), searchPath},
     Headline => "search a path for a file",
     Usage => "searchPath(pa,fn)",
     Inputs => { "pa" => {"a list of strings giving paths to directories.  Each one ends with a slash."}, "fn" },
     Outputs => {{"a list of those directories in ", TT "pa", " containing files named ", TT "fn" }}}

document { Key => {(boxList, List),boxList,(boxList, Sequence)},
     Headline => "ascii art: a vertical list of boxes",
     Usage => "boxList v",
     Inputs => { "v" },
     Outputs => {{"a net obtained by converting the elements of the list ", TT "v", " to nets and stacking them vertically, with boxes drawn around them.  Its baseline
	       is the baseline of first net."}},
     EXAMPLE lines ///
         boxList {12345,"b","asdsf"}
     ///}

document { Key => {(boxTable, List),boxTable,(boxTable, Sequence)},
     Headline => "ascii art: a table of boxes",
     Usage => "boxTable v",
     Inputs => { "v" },
     Outputs => {{"a net obtained by converting the elements of each list in the list of lists ", TT "v", " to nets and arranging them
	       in a table, with boxes drawn around them.  Its baseline is the baseline of nets in the top row."}},
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  random(R^2,R^{2:-2})
	  boxTable entries oo
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
     Headline => "evaluate Macaulay 2 code and capture the output (under development)" }
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
     Consequences => {{"the currently executing ", TT "list", "-clause of a ", TO "for", "-loop or ", TO "while", "-loop is finished, and iteration continues with the
	       ", TO "do", "-clause or the next iteration of the loop, if any.  The value ", TT "x", " is
	       added to the list being accumulated.  If ", TT "x", " is omitted, then no value is added to the list, and the statement may be used in a ", TT "do", "-clause."
	       }},
     EXAMPLE lines ///
          for i from 1 to 4 list (continue 4; print ho) do print hi
          for i from 1 to 4 list (continue ; 14) do print hi
          for i from 1 to 4 list 14 do print hi
	  i = 0 ; while i < 10 do ( i = i+1; if i == 5 then continue ; print i )
     ///}
document { Key => "copyright",
     Headline => "a string containing the copyright notice for Macaulay 2",
     EXAMPLE "copyright" }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
