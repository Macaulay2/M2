--		Copyright 2006 by Daniel R. Grayson

document {
     Key => cacheValue,
     Headline => "cache values of functions in their arguments",
     Usage => "((cacheValue KEY) f) x",
     Inputs => {
	  "KEY",
	  "f" => Function,
	  "x" => {"an argument for ", TT "f", " that has ", ofClass CacheTable, " stored in it under ", TT "x.cache"}
	  },
     Outputs => {
	  { TT "f x", " is returned, but the value is saved in ", TT "x.cache#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x.cache#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x.cache#KEY x", " is returned instead, after
	       first removing ", TT "x.cache#KEY", " from ", TT "x.cache", "." }
	  },
     EXAMPLE {
	  "x = new HashTable from { val => 1000, cache => new CacheTable }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (cacheValue VALUE) f///,
	  "h x",
	  "h x",
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
	  "KEY",
	  "f" => Function,
	  "x" => MutableHashTable => { "an argument for ", TT "f" }
	  },
     Outputs => {
	  { "The value of ", TT "f x", " is returned, but the value is saved in ", TT "x#KEY", " and not recomputed later.
	       However, if the value found in ", TT "x#KEY", " is ", ofClass CacheFunction, ", such as is
	       returned by ", TT "(stashValue KEY) f", ", then the value of ", TT "x#KEY x", " is returned instead, after
	       first removing ", TT "x#KEY", " from ", TT "x", "."
	       }
	  },
     EXAMPLE {
	  "x = new MutableHashTable from { val => 1000 }",
	  ///f = (t -> (print "hi there"; t.val^4))///,
	  ///h = (stashValue VALUE) f///,
	  "h x",
	  "h x",
	  "peek x"
	  },
     SourceCode => { stashValue },
     SeeAlso => { cacheValue }
     }

document {
     Key => CacheFunction,
     Headline => "the class of cache functions",
     "Functions of class ", TO "CacheFunction", " are created and used by ", TO "cacheValue", " and by ", TO "stashValue", "."
     }

undocumented {(generateAssertions, List)}
document { Key => {generateAssertions,(generateAssertions, String)},
     Headline => "generate assert statements from experimental input",
     Usage => "generateAssertions x",
     Inputs => { "x" => { "a string whose non-comment non-blank lines are Macaulay2 expressions to be evaluated" } },
     Outputs => { { "a net whose lines are assert statements that assert that the expressions evaluate to the expected value, just computed" }},
     EXAMPLE {
	  "generateAssertions ///
2+2
2^20
///",
     	  ///value \ unstack oo///
	  }
     }
document { Key => unsequence,
     Headline => "extract the single element from a sequence of length 1",
     Usage => "unsequence x",
     Inputs => { "x" => Thing },
     Outputs => { { TT "x#0", ", if ", TT "x", " is a sequence of length 1, otherwise ", TT "x", "" } },
     EXAMPLE { "unsequence (2:a)", "unsequence (1:a)", "unsequence (0:a)" },
     SeeAlso => sequence}

document { Key => {permutations, (permutations, ZZ), (permutations, VisibleList)},
     Headline => "produce all permutations of a list",
     Usage => "permutations x",
     Inputs => { "x" => { ofClass {VisibleList, ZZ} } },
     Outputs => { { "a list of all the permutations of the visible list ", TT "x", ", or, if ", TT "x", " is an integer, of the list of
	       integers from 0 through ", TT "n-1" 
	       } },
     EXAMPLE {
	  "permutations {a,b,c,d}",
	  "permutations 3"
	  }
     }

document { Key => tutorial,
     Headline => "convert documentation from tutorial format",
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
     Inputs => { "I" => { "an ideal in the target ring of ", TT "f" }, "f" },
     Outputs => { { "the preimage of ", TT "I", " under the map ", TT "f" } },
     EXAMPLE lines /// 
	  R = QQ[x,y,z]
	  S = QQ[t,u]
	  f = map(R,S,{x*y,y*z})
	  preimage_f ideal(x^2,y^2)
     	  ///
     }
document { Key => symbol applicationDirectorySuffix,
     Headline => "suffix that determines the user's application directory",
     Usage => "applicationDirectorySuffix = s",
     Inputs => { "s" => String => { "a relative path, which will be appended to the user's home directory to determine the user's application directory" } },
     SeeAlso => applicationDirectory,
     PARA {
	  "The value of ", TT "applicationDirectorySuffix", " may also be a function of no arguments, in which case its value is used as the path.
	  The initial value of ", TT "applicationDirectorySuffix", " is a string whose value depends on the operating system and its conventions."
	  },
     EXAMPLE lines ///
     	  applicationDirectorySuffix
	  applicationDirectory()
	  applicationDirectorySuffix = "local/Mac2"
	  applicationDirectory()
     	  ///,	  
     Consequences => { { "the value of the function ", TT "applicationDirectory", " will use the new value of ", TT "applicationDirectorySuffix" }}}
document { Key => {applicationDirectory, "application directory"},
     Headline => "the path to the user's application directory",
     Usage => "applicationDirectory()",
     Outputs => { String => "the path to the user's application directory" },
     SourceCode => applicationDirectory,
     PARA { "The function ", TO "installPackage", ", by default, installs packages under the application directory.  At program startup,
	  unless the ", TT "-q", " option is provided on the command line, an entry will be added to the ", TO "path", " so
	  packages can be loaded from there by ", TO "loadPackage", " and ", TO "needsPackage", ".  Moreover, the ", TO "initialization file", ", if found there, will be run."
	  },
     PARA { "The function ", TO "applicationDirectorySuffix", " determines the value of ", TT "applicationDirectory", ", and can be modified by the user." },
     EXAMPLE "applicationDirectory()",
     SeeAlso => "applicationDirectorySuffix"}

document { Key => {round,(round,QQ),(round,RR),(round,ZZ,RR),(round,ZZ),(round,CC)},
     Headline => "round a number",
     SYNOPSIS (
	  Usage => "round x",
	  Inputs => { "x" => "a number" },
	  Outputs => {{ "the integer nearest to ", TT "x" }},
	  EXAMPLE lines ///
	  round(-2.3)
	  round(-2.3+5*ii)
	  round(2/3)
	  ///
	  ),
     SYNOPSIS (
	  Usage => "round(n,x)",
	  Inputs => { "n" => ZZ, "x" => RR },
	  Outputs => {{ "the real number with just n decimal digits to the right of the decimal point nearest to ", TT "x" }},
	  EXAMPLE lines ///
	  round(2,1234.5678)
	  round(-2,1234.5678)
	  ///
	  ),
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
document { Key => {show, (show, URL)},
     Headline => "display various TeX, hypertext, or a URL, in an external application",
     Usage => "show x",
     Inputs => { "x" => { ofClass{ Hypertext, TEX, URL } }},
     Consequences => {{ "an external viewer, such as a web browser, is started to view the object ", TT "x" }},
     SeeAlso => { showTex, showHtml }}
document { Key => showHtml,
     Headline => "convert hypertext to html and display with a browser",
     Usage => "showHtml x",
     Inputs => { "x" => Hypertext },
     Consequences => {{ "an external viewer, such as a web browser, is started to view the object ", TT "x" }},
     "The result is the same as ", TT "show x", ".",
     Caveat => "This function may be phased out."
     }

undocumented {(isConstant, Number)}
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
document { Key => (conjugate,Partition),
     Headline => "conjugate a partition",
     Usage => "conjugate p", Inputs => {"p"}, Outputs => {{"the conjugate of ", TT "p" }},
     EXAMPLE lines ///
     	  partitions 4
	  conjugate \ oo
     ///}
document { Key => UpdateOnly,
     Headline => "only copies of newer files should replace files" }
document { Key => Verbose,
     Headline => "request verbose feedback" }
document { Key => {ofClass,(ofClass, Type),(ofClass, ImmutableType),(ofClass, List)}, 
     Headline => "English phrases for types",
     Usage => "ofClass T",
     Inputs => { "T" => Nothing => {ofClass{Type,ImmutableType,List}, " of types"} },
     Outputs => { Sequence => { "an English phrase in hypertext, using a synonym for each type, together with appropriate indefinite articles, and, if
	       a list is presented, the word ", EM "or", " as a conjunction at the end" }},
     PARA { "When viewed in html, words in the phrase hot link(s) to the documentation node(s) for the class(es)." },
     EXAMPLE lines ///
     	  ofClass class 3
	  peek oo
     	  ofClass Ring
	  needsPackage "Text"
     	  SPAN ofClass {HashTable, ProjectiveVariety}
	  document { Key => foo, "We may need ", ofClass ZZ, " and ", ofClass HashTable, "." }
	  help foo
     ///}
document { Key => inverse,
     Headline => "compute the inverse" }
document { Key => (inverse, Matrix),
     Usage => "inverse f",
     Inputs => { "f" },
     Outputs => {{ "the inverse of ", TT "f" }},
     SourceCode => (inverse, Matrix)}
document { Key => functionBody,
     Headline => "get the body of a function",
     Usage => "functionBody f",
     Inputs => { "f" => Function },
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
document { Key => fileLength,
     Headline => "the length of a file",
     Usage => "fileLength f",
     Inputs => { "f" => { ofClass {String, File} }},
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
	  removeFile filename
     ///
     }
document { Key => "loadedFiles",
     SeeAlso => {"load"},
     PARA { "After each source file is successfully loaded, the full path to the file is stored in the hash table ", TO "loadedFiles", ".  It is stored as the
	  value, with the corresponding key being a small integer, consecutively assigned, starting at 0."
	  },
     EXAMPLE "peek loadedFiles"}

document { Key => VerticalList,
     Headline => "a type of visible self-initializing list that prints vertically",
     Usage => "VerticalList x",
     Inputs => { "x" => List },
     Outputs => { VerticalList },
     "All operations on lists apply to vertical lists, since they inherit from the type ", TO VisibleList, ".  The
     only difference is the way that a vertical list is displayed vertically.",
     EXAMPLE lines ///
     	 a .. e
	 v = VerticalList oo
     	 v_1
	 length v
	 ///,
     "One may get a normal list back from a vertical list as follows.",
     EXAMPLE lines ///
     	 toList v
         ///,
     SeeAlso => { NumberedVerticalList }
     }

document { Key => NumberedVerticalList,
     Headline => "a type of visible self-initializing list that prints vertically",
     Usage => "NumberedVerticalList x",
     Inputs => { "x" => List },
     Outputs => { NumberedVerticalList },
     "All operations on lists apply to numbered vertical lists, since they inherit from the type ", TO VisibleList, ".  The
     only difference is the way that a numbered vertical list is displayed vertically, with index numbers labelling the entries.",
     EXAMPLE lines ///
     	 a .. e
	 v = NumberedVerticalList oo
     	 v_1
	 length v
	 ///,
     "One may get a normal list back from a vertical list as follows.",
     EXAMPLE lines ///
     	 toList v
         ///,
     SeeAlso => { VerticalList }
     }

document { Key => FunctionClosure,
     Headline => "the class of all function closures",
     "Functions created by the operator ", TO "->", " are function closures.",
     EXAMPLE "class (x->x)"
     }
document { Key => CompiledFunction,
     Headline => "the class of all compiled functions",
     "Compiled functions in Macaulay2 are written in a special purpose language, translated to C during compilation and not available to general users.",
     EXAMPLE "class sin"
     }
document { Key => CompiledFunctionClosure,
     Headline => "the class of all compiled function closures",
     "Some compiled functions return compiled function closures as values.",
     EXAMPLE lines ///
	  class depth
	  f = method()
	  class f
     ///
     }
TEST ///
assert ( class (x->x) === FunctionClosure )
assert ( class sin === CompiledFunction )
assert ( class depth === MethodFunction )
///


document { Key => {LowerBound,(symbol SPACE,CoherentSheaf,LowerBound),(symbol SPACE,SheafOfRings,LowerBound)},
     Headline => "the class of lower bound objects",
     "This is a type of list that represents a lower bound.  The single element of the list is an integer, and the object represents the condititon
     that some other integer, such as the index in a direct sum, should be at least as large.",
     EXAMPLE {
	  "LowerBound {4}",
	  ">= 4",
	  "> 4"
	  }}

document { Key => {NetFile,(symbol <<, NetFile, String),(symbol <<, NetFile, Net),(symbol SPACE,Manipulator,NetFile),(symbol <<,NetFile,Manipulator)},
     Headline => "the class of all net files",
     "This class is experimental.  Net files are intended to supplant output files eventually.  Whereas a file is a stream of bytes,
     or in some non-unix operating systems, a sequence of lines each of which is a sequence of bytes, a net file is a sequence of lines, each of which is
     a net.  Each output line is assembled by joining nets one by one.",
     EXAMPLE lines ///
     	  f = newNetFile()
     	  f << "aabbcc" << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
	  f << "aa" << "bb"^1 << "cc"^-1 << endl
     	  getNetFile f
	  peek oo
     	  class \ ooo
     ///
     }
document { Key => getNetFile,
     Headline => "get the sequence of completed lines (nets) from a net file",
     Usage => "getNetFile n",
     Inputs => { "n" => NetFile },
     "This function is experimental."
     }
document { Key => newNetFile,
     Headline => "create a new net file",
     Usage => "newNetFile()",
     Outputs => { NetFile },
     "This function is experimental."
     }
document { Key => OutputDictionary,
     Headline => "the dictionary for output values",
     "The symbols ", TT "o1", ", ", TT "o2", ", ", TT "o3", ", etc., are used to store the output values arising from interaction with the user,
     one line at a time.  The dictionary ", TT "OutputDictionary", " is the dictionary in which those symbols reside.",
     EXAMPLE lines ///
     	  2+2
	  "asdf" | "qwer"
	  value \ values OutputDictionary
	  dictionaryPath
	  peek OutputDictionary
     ///,
     SeeAlso => { "dictionaryPath" }
     }
document { Key => Pseudocode,
     Headline => "the class of pseudocodes",
     "The Macaulay2 interpreter compiles its language into pseudocode, which is evaluated later, step by step.  At each
     step, the evaluator is considering a pseudocode item.  These pseudocode items are normally not available to the user, but
     the interanl function ", TO "disassemble", " can display their contents, the function ", TO "pseudocode", " can convert
     a function closure to pseudocode, the function ", TO "value", " can evaluate it (bindings of values to local symbols
     are enclosed with the pseudocode), the operator ", TO "===", " can be used for equality testing, 
     and when the debugger is activated after an error, the variable ", TO "current", " contains the pseudcode step whose execution produced the error.",
     }
document { Key => pseudocode,
     Headline => "produce the pseudocode for a function",
     Usage => "pseudocode f",
     Inputs => { "f" => FunctionClosure },
     Outputs => { Pseudocode => { "the pseudocode of the function ", TT "f"} },
     SeeAlso => { disassemble },
     EXAMPLE lines ///
     	  pseudocode resolution
          disassemble oo
     ///
     }
document { Key => disassemble,
     Headline => "disassemble pseudocode or a function",
     Usage => "disassemble c",
     Inputs => { "c" => Pseudocode },
     Outputs => { String => {"the disassembled form of ", TT "c"} },
     SeeAlso => { pseudocode },
     EXAMPLE lines ///
     disassemble res
     ///,
     PARA {
	  "It may be useful to disassemble code during debugging, as in the following demonstration."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code current
     disassemble current
     ///
     }
document { Key => "current",
     Headline => "the pseudocode that produced an error",
     Usage => "current",
     Outputs => { Pseudocode => { "the pseudocode that produced an error, or ", TO "null", ", if none" } },
     "Use ", TO "value", " to evaluate the code again, for debugging purposes.",
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code current
     disassemble current
     value current
     x = 11
     value current
     continue
     ///
     }
document { Key => (value, Pseudocode),
     Headline => "execute pseudocode",
     Usage => "value p",
     Inputs => { "p" },
     Outputs => {{ "the value returned by evaluation of ", TT "p" }},
     PARA {
	  "Here is an example of its use in the debugger, to see whether modifying the value of a local
	  variable fixed the problem in the code, by executing just the offending line."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     g 2
     value current
     x = 11
     value current
     ///,
     SeeAlso => { "current", pseudocode }
     }
document { Key => SheafOfRings,
     SeeAlso => { Variety, OO },
     Headline => "the class of sheaves of rings",
     EXAMPLE lines ///
     	  X = Proj(QQ[x..z])
	  OO_X
	  OO_X^6
     ///
     }
document { Key => (module, SheafOfRings),
     SeeAlso => { Variety, OO },
     Usage => "module F",
     Inputs => { "F" },
     Outputs => { { "the module corresponding to ", TT "F" }},
     EXAMPLE lines ///
     	  R = QQ[x..z]
     	  X = Proj R
	  OO_X^6
	  module oo
     ///
     }
document { Key => "when",
     Headline => "a keyword",
     "A keyword used in ", TO "for", " loops."
     }
document { Key => zero,
     Headline => "whether something is zero",
     SourceCode => zero,
     Usage => "zero x",
     Inputs => { "x" },
     Outputs => { { "whether ", TT "x", " is equal to 0" }}}
document { Key => "homeDirectory",
     Headline => "the home directory of the user",
     Usage => "homeDirectory",
     Outputs => { String => "the home directory of the user" },
     PARA {"In file operations, file names beginning with ", TT "~/", " will have it replaced
	  with the home directory."
	  },
     EXAMPLE "homeDirectory"
     }
document { Key => "backtrace",
     Headline => "whether a backtrace is displayed following an error message",
     Usage => "backtrace = false",
     Consequences => { "a backtrace will not displayed following an error message" }
     }
document { Key => "prefixDirectory",
     Headline => "the prefix directory",
     PARA {
	 "When Macaulay2 is successfully installed, its files are installed in a directory tree whose layout, relative to the root, is determined
	 by the hash table ", TO "Layout", ".  When M2 starts up, it detects whether it is running in such a layout, and sets ", TO "prefixDirectory", "
	 to the root of that directory tree.  The way it does that is that it locates the running M2 executable and determines whether it is
	 located in a directory whose name is ", TT "bin", " that has a sibling directory called ", TT "share", " that leads to a directory
	 called ", TT "share/Macaulay2/Core/", "."
	 },
     PARA {
	  "The prefix directory can be set by the user at an early stage when ", TT "M2", " starts up with the ", TT "--prefix", " command 
	  line option.  This will affect the value of ", TO "path", " and thus the locations of the files loaded initially.  Use the
	  ", TT "--notify", " command line option to display the locations of files as they are loaded."
	  },
     SeeAlso => { "prefixPath", "Invoking the program" }
     }
document { Key => getGlobalSymbol,
     Headline => "create a global symbol in a global dictionary",
     Usage => "getGlobalSymbol(dict,nam)\ngetGlobalSymbol nam",
     Inputs => { 
	  "dict" => GlobalDictionary,
	  "nam" => String
	  },
     Outputs => {
	  { "a global symbol in the dictionary ", TT "dict", " whose name is the string ", TT "nam", ", which will be created, if necessary" }
	  },
     Consequences => {
	  { "if a new symbol is created, it is stored under the name ", TT "nam", " in the dictionary ", TT "dict" }
	  },
     PARA {
	  "If ", TT "dict", " is omitted, then the first symbol found in the dictionaries listed in ", TO "dictionaryPath", " will be returned.
	  If none is found, one will be created in the first dictionary listed in ", TO "dictionaryPath", ", unless it is not mutable, in 
	  which case an error will be signalled; perhaps that behavior should be changed."
	  },
     EXAMPLE lines ///
     	  d = new Dictionary
	  sym = getGlobalSymbol(d,"foo")
	  d
	  peek d
	  d#"foo" === sym
	  d#"asfd" = sym
	  peek d
     ///
     }     
document { Key => {Bag,(symbol ?,Bag,Bag)},
     Headline => "the class of all bags",
     PARA "A bag can be used for enclosing something in a container to prevent it from being printed, in normal circumstances.
     Any mutable list can be used for this purpose, but bags are designed for this purpose.  In comparison and sorting, they
     are declared to be incomparable.",
     SeeAlso => {unbag}
     }
document { Key => {(unbag, Bag), unbag},
     Usage => "unbag y",
     Inputs => { "y" },
     Outputs => { { "the contents of ", TT "y" }},
     EXAMPLE lines ///
     	  x = 100!
	  y = Bag {x}
	  unbag y
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
