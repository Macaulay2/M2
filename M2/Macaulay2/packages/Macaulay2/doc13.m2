--		Copyright 2006 by Daniel R. Grayson

document {
     Key => cacheValue,
     Headline => "cache values of functions in their arguments",
     Usage => "((cacheValue KEY) f) x",
     Inputs => {
	  "KEY" => null,
	  "f" => Function => null,
	  "x" => {"an argument for ", TT "f", " that has ", OFCLASS CacheTable, " stored in it under ", TT "x.cache"}
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
     Inputs => { "x" => { OFCLASS VisibleList, " or ", OFCLASS ZZ } },
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
document { Key => {show,(show, Hypertext), (show, TEX), (show, URL)},
     Headline => "display various TeX, hypertext, or a URL, in an external application",
     Usage => "show x",
     Inputs => { "x" => { "an object of type ", TT "Hypertext", ", ", TT "TEX", ", or ", TT "URL" }},
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
     Outputs => {{ "a list of irreducible monomial ideals whos intersection is ", TT "I" }}}
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
     Outputs => {ZZ => "the current time, in seconds from the beginning of the epoch" },
     EXAMPLE "currentTime()" }
document { Key => {[installPackage, MakeDocumentation],MakeDocumentation},
     Headline => "specify whether the package documentation should be made",
     Usage => "installPackage(..., MakeDocumentation => false)",
     Consequences => {{ "the documentation will not be made" }},
     PARA { "Normally, when installing a package, the package's documentation is made.  This involves running the
	  example inputs through Macaulay 2, checking for errors, and producing the documentation in various forms,
	  such as html and for the info reader.  This option specifies whether to do that, and the default is ", TT "true", "."
	  }}
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
