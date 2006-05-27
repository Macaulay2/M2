--		Copyright 2006 by Daniel R. Grayson

document {
     Key => cacheValue,
     Headline => "cache values of functions in their arguments",
     Usage => "((cacheValue KEY) f) x",
     Inputs => {
	  "KEY" => null,
	  "f" => Function => null,
	  "x" => {"an argument for ", TT "f", " that has ", ofClass CacheTable, " stored in it under ", TT "x.cache"}
	  },
     Outputs => {
	  { TT "f x", " is returned, but the value is saved in ", TT "x.cache#KEY", " and not recomputed later" }
	  },
     EXAMPLE {
	  "x = new HashTable from { val => 1000, cache => new CacheTable }",
	  ///g = (t -> (print "hi there"; t.val^4))///,
	  ///f = (cacheValue VALUE) g///,
	  "f x",
	  "f x",
	  "peek'_2 x"
	  },
     SourceCode => { cacheValue },
     SeeAlso => { stashValue }
     }

document {
     Key => stashValue,
     Headline => "stash values of functions in their arguments",
     Usage => "((stashValue KEY) f) x",
     Inputs => {
	  "KEY" => null,
	  "f" => Function => null,
	  "x" => MutableHashTable => { "an argument for ", TT "f" }
	  },
     Outputs => {
	  { TT "f x", " is returned, but the value is saved in ", TT "x#KEY", " and not recomputed later" }
	  },
     EXAMPLE {
	  "x = new MutableHashTable from { val => 1000 }",
	  ///g = (t -> (print "hi there"; t.val^4))///,
	  ///f = (stashValue VALUE) g///,
	  "f x",
	  "f x",
	  "peek x"
	  },
     SourceCode => { stashValue },
     SeeAlso => { cacheValue }
     }

undocumented (addHook,MutableHashTable,Thing,Function)
document {
     Key => { (addHook,HashTable,Thing,Function), addHook },
     Headline => "add a hook function to an object for later processing",
     Usage => "addHook(obj,key,hook)",
     Inputs => { "obj" => null, "key" => null, "hook" => null },
     Consequences => {
	  { "the function ", TT "hook", " is added to the list (possibly absent) of hooks stored in ", TT "obj#key", " or ", TT "obj.cache#key" }
	  },
     SourceCode => {(addHook,HashTable,Thing,Function), (addHook,MutableHashTable,Thing,Function)},
     SeeAlso => { runHooks, removeHook }
     }
undocumented (removeHook,MutableHashTable,Thing,Function)
document {
     Key => { (removeHook,HashTable,Thing,Function), removeHook },
     Headline => "remove a hook function from an object",
     Usage => "removeHook(obj,key,hook)",
     Inputs => { "obj" => null, "key" => null, "hook" => null },
     Consequences => {
	  { "the function ", TT "hook", " is removed from the list of hooks stored in ", TT "obj#key", " or ", TT "obj.cache#key" }
	  },
     SourceCode => {(removeHook,HashTable,Thing,Function), (removeHook,MutableHashTable,Thing,Function)},
     SeeAlso => { runHooks, removeHook }
     }
undocumented (runHooks,MutableHashTable,Thing,Thing)
document {
     Key => { (runHooks,HashTable,Thing,Thing), runHooks },
     Headline => "run the hook functions stored in an object",
     Usage => "runHooks(obj,key,arg)",
     Inputs => { "obj" => null, "key" => null, "arg" => null },
     Consequences => {
	  { "each function ", TT "hook", " in list of hooks stored in ", TT "obj#key", " or ", TT "obj.cache#key", " is
	       called with ", TT "arg", " as its argument or sequence of arguments" }
	  },
     SourceCode => { (runHooks,HashTable,Thing,Thing), (runHooks,MutableHashTable,Thing,Thing) },
     SeeAlso => { addHook, removeHook }
     }
undocumented (generateAssertions, List)
document { Key => {generateAssertions,(generateAssertions, String)},
     Headline => "generate assert statements from experimental input",
     Usage => "generateAssertions x",
     Inputs => { "x" => { "a string whose non-comment non-blank lines are Macaulay 2 expressions to be evaluated" } },
     Outputs => { { "a net whose lines are assert statements that assert that the expressions evaluate to the expected value, just computed" }},
     EXAMPLE {
	  "generateAssertions ///
2+2
2^20
///",
     	  ///value \ unstack oo///
	  }
     }
document { Key => unSingleton,
     Headline => "extract the single element from a sequence of length 1",
     Usage => "unSingleton x",
     Inputs => { "x" => Thing => null },
     Outputs => { { TT "x#0", ", if ", TT "x", " is a sequence of length 1, otherwise ", TT "x", "" } },
     EXAMPLE { "unSingleton (2:a)", "unSingleton (1:a)", "unSingleton (0:a)" },
     SeeAlso => sequence}

document { Key => {permutations, (permutations, ZZ), (permutations, VisibleList)},
     Headline => "produce all permutations of a list",
     Usage => "permutations x",
     Inputs => { "x" => { ofClass VisibleList, " or ", ofClass ZZ } },
     Outputs => { { "a list of all the permutations of the visible list ", TT "x", ", or, if ", TT "x", " is an integer, of the list of
	       integers from 0 through ", TT "n-1" 
	       } },
     EXAMPLE {
	  "permutations {a,b,c,d}",
	  "permutations 3"
	  }
     }

document { Key => separateRegexp, Headline => "separate a string into pieces, with separators determined by a regular expression" }
document { Key => (separateRegexp, String, String),
     Usage => "separateRegexp(sep,str)",
     Inputs => { "sep" => "a regular expression" , "str" => "a string to be separated" },
     Outputs => { { "a list of substrings consecutively extracted from ", TT "str", ", with separators recognized by ", TT "sep" } },
     EXAMPLE { ///separateRegexp("-", "asdf-qwer-dfadf")/// }}
document { Key => (separateRegexp, String, ZZ, String),
     Usage => "separateRegexp(sep,n,str)",
     Inputs => { "sep" => "a regular expression" , "n" => "", "str" => "a string to be separated" },
     Outputs => { { "a list of substrings consecutively extracted from ", TT "str", ", with separators recognized by the ", TT "n", "-th parenthesized subexpression of", TT "sep" } },
     EXAMPLE { ///separateRegexp("f(-)", 1, "asdf-qwer-dfadf")/// }}
document { Key => tutorial, Headline => "convert documentation from tutorial format",
     Usage => "tutorial x",
     Inputs => { "x" => String => "documentation in tutorial format" },
     Outputs => {{ "documentation in hypertext format" }},
     PARA { "Some of the Macaulay2 documentation is written in this format." },
     EXAMPLE {
	  "///-- We can compute $(x+y)^3$ as follows.
R = QQ[x,y]
(x+y)^3
///",
     	  "tutorial oo",
	  "peek oo"
	  }}
document { Key => {preimage,(preimage, RingMap, Ideal)},
     Headline => "preimage of an ideal under a ring map",   -- hopefully more general later
     Usage => "preimage(f,I)",
     Inputs => { "I" => { "an ideal in the target ring of ", TT "f" }, "f" => "" },
     Outputs => { { "the preimage of ", TT "I", " under the map ", TT "f" } },
     EXAMPLE lines /// 
	  R = QQ[x,y,z]
	  S = QQ[t,u]
	  f = map(R,S,{x*y,y*z})
	  preimage_f ideal(x^2,y^2)
     	  ///
     }
document { Key => {[installPackage, UserMode], UserMode},
     Headline => "user mode option for installPackage",
     Usage => "installPackage(..., UserMode => false)",
     Consequences => { { "the installation will ignore packages installed in the user's application directory
	       (see ", TO "applicationDirectory", ") and will ignore the user's init.m2 files when running
	       examples" }}}
document { Key => symbol applicationDirectorySuffix,
     Headline => "suffix that determines the user's application directory",
     Usage => "applicationDirectorySuffix = s",
     Inputs => { "s" => String => { "a relative path, which will be appended to the user's home directory to determine the user's application directory" } },
     SeeAlso => applicationDirectory,
     PARA {
	  "The value of ", TT "applicationDirectorySuffix", " may also be a function of no arguments, in which case its value is used as the path.
	  The initial value of ", TT "applicationDirectorySuffix", " is a function whose value depends on the operating system and its conventions."
	  },
     EXAMPLE lines ///
     	  applicationDirectorySuffix()
	  applicationDirectory()
	  applicationDirectorySuffix = "local/Mac2"
	  applicationDirectory()
     	  ///,	  
     SourceCode => applicationDirectorySuffix,
     Consequences => { { "the value of the function ", TT "applicationDirectory", " will use the new value of ", TT "applicationDirectorySuffix" }}}
document { Key => applicationDirectory,
     Headline => "the path to the user's application directory",
     Usage => "applicationDirectory()",
     Outputs => { String => "the path to the user's application directory" },
     SourceCode => applicationDirectory,
     PARA { "The function ", TO "installPackage", ", by default, installs packages under the application directory.  At program startup,
	  unless the ", TT "-q", " option is provided on the command line, an entry will be added to the ", TO "packagePath", " so
	  packages can be loaded from there by ", TO "loadPackage", " and ", TO "needsPackage", ".  An entry will also be added to the ", TO "path", "
	  so ", TO "load", " can find files there.  Moreover, the ", TO "initialization file", ", if found there, will be run."
	  },
     PARA { "The function ", TO "applicationDirectorySuffix", " determines the value of ", TT "applicationDirectory", ", and can be modified by the user." },
     EXAMPLE "applicationDirectory()",
     SeeAlso => applicationDirectorySuffix}
document { Key => round,
     Headline => "round a number to the nearest integer",
     Usage => "round x",
     Inputs => { "x" => "a number" },
     Outputs => {{ "the integer nearest to ", TT "x" }},
     SourceCode => round,
     EXAMPLE lines ///
     	  round(-2.3)
	  round(2/3)
	  ///,
     SeeAlso => { floor, ceiling }
     }
document { Key => symbol currentLineNumber,
     Headline => "current line number of the current input file",
     Usage => "currentLineNumber()",
     Outputs => { ZZ => "the current line number of the current input file" },
     EXAMPLE "currentLineNumber()",
     SeeAlso => "currentFileName" }
document { Key => symbol currentFileDirectory,
     Headline => "the directory containing the current input file",
     Usage => "currentFileDirectory",
     Outputs => { String => "the directory containing the current input file" },
     EXAMPLE "currentFileDirectory",
     SeeAlso => "currentFileName" }
document { Key => symbol currentFileName,
     Headline => "the current source file",
     Usage => "currentFileName",
     Outputs => { String => "the name of the current source file" },
     EXAMPLE "currentFileName",
     SeeAlso => "currentLineNumber" }
document { Key => {URL,(NewFromMethod, URL, String)},
     Headline => "a type representing a URL",
     Usage => "URL h",
     Inputs => { "h" => String => "a URL" },
     Outputs => {{ "an object of type ", TT "URL" }},
     PARA {
	  "The function ", TO "show", " knows how display entities of various types, including URLs."
	  }}
document { Key => {show, (show, TEX), (show, URL)},
     Headline => "display various TeX, hypertext, or a URL, in an external application",
     Usage => "show x",
     Inputs => { "x" => { "an object of type ", TT "TEX", ", or ", TT "URL" }},
     Consequences => {{ "an external viewer, such as a web browser, is started to view the object ", TT "x" }},
     SeeAlso => { showTex, showHtml }}
document { Key => {(irreducibleDecomposition,MonomialIdeal),irreducibleDecomposition},
     Headline => "express a monomial ideal as an intersection of irreducible monomial ideals",
     Usage => "irreducibleDecomposition I",
     Inputs => { "I" => "" },
     EXAMPLE lines ///
        QQ[x..z];
        I = monomialIdeal (x*y^3, x*y^2*z)
	w = irreducibleDecomposition I
	assert( I == intersect w )
     ///,
     Outputs => {{ "a list of irreducible monomial ideals whose intersection is ", TT "I" }}}
undocumented {(isConstant, ZZ),(isConstant, QQ),(isConstant, RRR),(isConstant, RR),(isConstant, CC)}
document { Key => {isConstant,(isConstant, RingElement)},
     Headline => "whether a ring element is constant",
     Usage => "isConstant f",
     Inputs => { "f" },
     Outputs => { { "whether f is constant, i.e., is in the coefficient ring" } },
     EXAMPLE lines ///
     	  isConstant 3
	  QQ[a,b][x,y];
	  isConstant (x+a-x)
	  isConstant x
	  ///,
     SeeAlso => coefficientRing,
     SourceCode => (isConstant,RingElement)
     }
document { Key => {[installPackage,RerunExamples],RerunExamples},
     Headline => "when installing a package, rerun all the examples",
     Usage => "installPackage(..., RerunExamples => true)",
     Consequences => {{ "all the examples are rerun" }},
     SeeAlso => installPackage }
document { Key => {[newPackage,Version],Version},
     Headline => "specify the version number when creating a new package",
     Usage => "newPackage(..., Version => num)",
     Inputs => { "num" => String => "the version number of the new package" },
     Consequences => {{ "the version number will be stored under the key ", TO "Version", " in the resulting package" }},
     EXAMPLE "(options Macaulay2Core).Version"}
document { Key => currentTime,
     Headline => "get the current time",
     Usage => "currentTime()",
     Outputs => {ZZ => "the current time, in seconds since 00:00:00 1970-01-01 UTC, the beginning of the epoch" },
     EXAMPLE "currentTime()",
     PARA { "We can compute, roughly, how many years ago the epoch began as follows." },
     EXAMPLE "currentTime() /( (365 + 97./400) * 24 * 60 * 60 )",
     PARA { "We can also compute how many months account for the fractional part of that number." },
     EXAMPLE "12 * (oo - floor oo)",
     PARA { "Compare that to the current date, available from a standard Unix command." },
     EXAMPLE ///run "date"///
     }
document { Key => Partition, 
     Headline => "a type of list representing a partition of a natural number",
     SeeAlso => { partitions, (conjugate,Partition) } }
document { Key => partitions, Headline => "list the partitions of an integer" }
document { Key => (partitions, ZZ, ZZ),
     Usage => "partitions(n,k)",
     Inputs => { "n" => "", "k" => "" },
     Outputs => {{"a list of the partitions of the integer ", TT "n", " as a sum of terms each of which does not exceed ", TT "k"}},
     PARA { "Each partition is a basic list of type ", TO "Partition", "." },
     SeeAlso => {Partition, (partitions, ZZ)},
     EXAMPLE "partitions(4,2)"}
document { Key => (partitions, ZZ),
     Usage => "partitions n",
     Inputs => { "n" => "" },
     Outputs => {{"a list of the partitions of the integer ", TT "n"}},
     PARA { "Each partition is a basic list of type ", TO "Partition", "." },
     SeeAlso => {Partition,(partitions, ZZ, ZZ)},
     EXAMPLE "partitions 4"}
document { Key => UpdateOnly, Headline => "only copies of newer files should replace files" }
document { Key => [copyDirectory, UpdateOnly],
     Usage => "copyDirectory(..., UpdateOnly => true)",
     Consequences => {{ "during the indicated copy operation, newer files will not be replaced by copies of older ones" }}}
document { Key => [copyFile, UpdateOnly],
     Usage => "copyFile(..., UpdateOnly => true)",
     Consequences => {{ "during the indicated copy operation, newer files will not be replaced by copies of older ones" }}}
document { Key => Verbose, Headline => "request verbose feedback" }
document { Key => [symlinkDirectory, Verbose],
     Usage => "symlinkDirectory(..., Verbose => ...)",
     Consequences => {{ "during the file operation, details of the operations performed will be displayed" }}}
document { Key => [copyDirectory, Verbose],
     Usage => "copyDirectory(..., Verbose => ...)",
     Consequences => {{ "during the file operation, details of the operations performed will be displayed" }}}
document { Key => [copyFile, Verbose],
     Usage => "copyFile(..., Verbose => ...)",
     Consequences => {{ "during the file operation, details of the operations performed will be displayed" }}}
document { Key => [moveFile, Verbose],
     Usage => "moveFile(..., Verbose => ...)",
     Consequences => {{ "during the file operation, details of the operations performed will be displayed" }}}
document { Key => PrimaryTag, Headline => "for internal use only: a symbol used in storing documentation" }
document { Key => LoadDocumentation, Headline => "when loading a package, load the documentation, too" }
document { Key => [loadPackage, LoadDocumentation],
     Usage => "loadPackage(..., LoadDocumentation => ...)",
     SeeAlso => beginDocumentation,
     Consequences => {{ "when the package is loaded, the documentation is loaded, too" }}}
document { Key => [needsPackage, LoadDocumentation],
     Usage => "needsPackage(..., LoadDocumentation => ...)",
     SeeAlso => beginDocumentation,
     Consequences => {{ "when the package is loaded, the documentation is loaded, too" }}}
document { Key => ofClass, 
     Headline => "English phrases for types",
     Usage => "ofClass T",
     Inputs => { "T" => Type => "" },
     Outputs => {{ "an English phrase, using a synonym for an instance of the type, together with an appropriate indefinite article" }},
     PARA { "When viewed in html, the phrase is a hot link to the documentation node for the class." },
     EXAMPLE lines ///
     	  ofClass class 3
	  peek oo
     	  ofClass Ring
     	  ofClass HashTable
     	  ofClass ProjectiveVariety
	  document { Key => foo, "We may need ", ofClass ZZ, " and ", ofClass HashTable, "." }
	  help foo
     ///}
document { Key => inverse, Headline => "compute the inverse" }
document { Key => (inverse, Matrix),
     Usage => "inverse f",
     Inputs => { "f" => "" },
     Outputs => {{ "the inverse of ", TT "f" }},
     SourceCode => (inverse, Matrix)}
document { Key => functionBody,
     Headline => "get the body of a function",
     Usage => "functionBody f",
     Inputs => { "f" => Function => "" },
     Outputs => { FunctionBody => { "the body of the function ", TT "f" }},
     PARA { "The body of ", TT "f", " is essentially just the source code of ", TT "f", ", with no frames providing bindings for
	  the local variables in scopes enclosing the scope of ", TT "f", ".  Function bodies cannot act as functions, but they can be tested for
	  equality (", TO "===", "), and they can be used as keys in hash tables."
	  },
     EXAMPLE lines ///
     	  f = a -> b -> a+b+a*b
	  functionBody f 1
	  f 1 === f 2
	  functionBody f 1 === functionBody f 2
     ///,
     SeeAlso => FunctionBody }
document { Key => FunctionBody,
     Headline => "the class of function bodies",
     SeeAlso => functionBody }
document { Key => {[newPackage, Authors], Authors},
     Usage => "newPackage(..., Authors => au)",
     Headline => "provide contact information for the authors of a package",
     Inputs => { "au" => List => { "a list of lists, each of which describes one of the authors" } },
     Consequences => { { "the authors will be stored in the newly created package" } },
     PARA { "Each elemnt of ", TT "au", " should be a list of options of the form ", TT "key => val", ",
	  where ", TT "key", " is ", TT "Name", ", ", TT "Email", ", or ", TT "HomePage", ", and
	  ", TT "val", " is a string containing the corresponding information."
	  },
     EXAMPLE "Macaulay2Core.Options.Authors"
     }
document { Key => Name,
     "This symbol is used as a key when providing information about the authors of a package to the ", TO "newPackage", " command.
     See ", TO [newPackage, Authors], "." }
document { Key => Email,
     "This symbol is used as a key when providing information about the authors of a package to the ", TO "newPackage", " command.
     See ", TO [newPackage, Authors], "." }
document { Key => HomePage,
     "This symbol is used as a key when providing information about the authors of a package to the ", TO "newPackage", " command.
     See ", TO [newPackage, Authors], "." }
document { Key => fileLength,
     Headline => "the length of a file",
     Usage => "fileLength f",
     Inputs => { "f" => { ofClass String, " or ", ofClass File }},
     Outputs => { ZZ => { "the length of the file ", TT "f", " or the file whose name is ", TT "f" }},
     PARA { "The length of an open output file is determined from the internal count of the number of bytes written so far." },
     SeeAlso => {fileTime},
     EXAMPLE lines ///
     	  f = temporaryFileName() << "hi there"
	  fileLength f
	  close f
	  filename = toString f
	  fileLength filename
	  get filename
	  length oo
     ///
     }
document { Key => "loadedFiles",
     SeeAlso => {"load", "filesLoaded"},
     PARA { "After each source file is successfully loaded, the full path to the file is stored in the hash table ", TO "loadedFiles", ".  It is stored as the
	  value, with the corresponding key being a small integer, consecutively assigned, starting at 0."
	  },
     EXAMPLE "peek loadedFiles"}


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
