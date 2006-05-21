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
     BR, NOINDENT,
     TT "lineNumber = n", " -- sets the line number to ", TT "n", ".",
     }

document {
     Key => FilePosition,
     Headline => "the class of all file positions",
     TT "FilePosition", " -- a type of list designed to represent a position
     in a file.",
     PARA,
     "It's implemented as a list whose three elements are the file name,
     the line number, and the column number."
     }

document {
     Key => symbol "currentFileName",
     Headline => "the current source file",
     TT "currentFileName", " -- a variable whose value is the name of the current
     source file."
     }

document {
     Key => flagLookup,
     Headline => "flag a symbol",
     TT "flagLookup x", " -- arranges for each subsequent reference to a
     symbol x to be flagged with a warning message."
     }

document {
     Key => instance,
     Headline => "whether something has a certain type",
     TT "instance(x,X)", " -- tells whether ", TT "x", " is an instance
     of the type ", TT "X", ".",
     PARA,
     "We say that x is an instance of X if X is the class of x, or a parent
     of the class of x, or a grandparent, and so on.",
     PARA,
     SeeAlso => { "classes and types", "class", "parent" }
     }

document {
     Key => alarm,
     Headline => "set an alarm",
     Usage => "alarm n",
     Inputs => {
	  "n" => ZZ => "",
	  },
     Consequences => {
	  { "the alarm will be sounded after ", TT "n", " seconds; it can be intercepted with ", TO "try" }
	  },
     "If ", TT "n", " is zero, then no alarm is scheduled, and any previously scheduled alarm is cancelled.
     Any pending alarm will be cancelled when any other error occurs, or when the top level loop offers an input prompt to the user.",
     PARA,
     "The value returned is the number of seconds remaining until any previously scheduled alarm was due to be delivered, or
     zero if there was no previously scheduled alarm.",
     PARA,
     "This command may interfere with ", TO "time", " or ", TO "sleep", " on some systems."
     }


document {
     Key => basictype,
     Headline => "the basic type of an object",
     Usage => "T = basictype x",
     Inputs => { "x" => "anything" },
     Outputs => { { "class representing the basic type of ", TT "x" } },
     "Every thing has basic type which tells what sort of thing it
     really is, internally.",
     PARA,
     "The parent of a basic type is ", TO "Thing", ", and this property
     characterizes the basic types.  The basic type of an object ", TT "x", "
     is defined to be the ancestor of the class of ", TT "x", " that is a
     basic type.",
     PARA,
     "Let's compute a list of all the basic types:",
     EXAMPLE ///stack sort unique (toString \
     select(values Macaulay2Core.Dictionary, i -> parent value i === Thing)
     )///,
     SeeAlso => "basic types"
     }

document {
     Key => symbol "++",
     Headline => "a binary operator, usually used for direct sum"
     }

document {
     Key => symbol "(*)",
     Headline => ""
     }

document {
     Key => symbol "<==>",
     Headline => "a postfix operator, used for indicating a graded object"
     }

document {
     Key => symbol ",",
     Headline => "the comma, used for separating entries in a list or sequence"
     }

document {
     Key => symbol "==>",
     Headline => "a binary operator, available to the user as implication in logical expressions"
     }

document {
     Key => symbol "|-",
     Headline => "a binary operator, available to the user in logical expressions"
     }

document {
     Key => symbol "===>",
     Headline => "a binary operator, available to the user"
     }

document {
     Key => symbol "@@",
     Headline => "a binary operator"
     }

document {
     Key => (symbol @@, Function, Function),
     Headline => "composition of functions",
     Usage => "f @@ g",
     Inputs => { "f" =>  null, "g" => null },
     Outputs => {{ "the composite function of ", TT "f", " and ", TT "g", "." }},
     EXAMPLE {
	  "f = i -> i+1",
	  "g = i -> i^2",
	  "apply(0 .. 10, f @@ g)",
	  "apply(0 .. 10, g @@ f)"
	  }
     }

document {
     Key => symbol "@",
     Headline => "a binary operator",
     "This operator is right associative."
     }

document {
     Key => symbol "\\",
     Headline => "a binary operator"
     }

document {
     Key => (symbol /, VisibleList, Function),
     Headline => "apply a function to each member of a list",
     TT "w / f", " -- apply the function ", TT "f", " to each member of the 
     list or sequence ", TT "w"," returning a list or sequence containing the 
     results.  The same as ", TT "apply(w,f)", ".",
     PARA,
     "This operator is left associative, which means that ", TT "w / f / g", "
     is interpreted as meaning ", TT "(w / f) / g", ".",
     EXAMPLE "{1,2,3} / (i -> i+1) / (j -> j^2)",
     SeeAlso => {"apply", (symbol \,Function, VisibleList)}
     }

document {
     Key => (symbol \,Function, VisibleList),
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
     SeeAlso => {"apply", "@@", (symbol /,List, Function)}
     }

document {
     Key => String,
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

document {
     Key => "///",					    -- ///
     Headline => "delineate a string",
     "This method for entering a string involves no escape characters, so
     it can be used for easily inserting large chunks of text into a string
     without treating the characters ", TT "\\", " and ", TT "\"", " specially.",
     EXAMPLE {
	  "/// \\ \" ///",
      	  "ascii oo",
	  },
     SeeAlso => "String"
     }

document {
     Key => Net,
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

document {
     Key => horizontalJoin,
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
     SeeAlso => {"Net", (symbol |, String, String)}
     }

document {
     Key => stack,
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
     SeeAlso => { (symbol ||, Net, Net)}
     }

document {
     Key => (symbol ^, Net, ZZ),
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

document {
     Key => width,
     Headline => "width of a file or net",
     TT "width f", " -- determines the width of the terminal associated to an
     output file ", TT "f", ", if any.", BR,NOINDENT, 
     TT "width n", " -- the width of a net ", TT "n", ".",
     SeeAlso => {"Net", "File"}
     }

document {
     Key => height,
     Headline => "height of a net",
     TT "height n", " -- the height of a net ", TT "n", ".",
     PARA,
     "The height of a net is the number of rows of characters it has above
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SeeAlso => {"Net", "depth"}
     }

document {
     Key => depth,
     Headline => "depth of a net",
     TT "depth n", " -- the depth of a net ", TT "n", ".",
     PARA,
     "The depth of a net is the number of rows of characters it has below
     the baseline.  It may be a negative number, but the depth plus the 
     height is always the total number of rows, which is not negative.",
     SeeAlso => {"Net", "height"}
     }

document {
     Key => hash,
     Headline => "hash code of an object",
     TT "hash x", " -- returns the hash code of ", TT "x", ".",
     PARA,
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
     PARA,
     EXAMPLE {
	  "x = new MutableHashTable from {a => 1, b => 2}",
	  "remove(x,a)",
	  "x"
	  }
     }

document {
     Key => BasicList,
     Headline => "the class of all basic lists",
     "A basic list is a sequence of expressions indexed by a seequence of consecutive integers of the form
     ", TT "0", ", ", TT "1", ", ..., ", TT "N-1", ".  The number ", TT "N", " is called the length of the list.",
     PARA,
     "There are various types of basic lists, depending on the application, and they are displayed in different ways.
     The types first encountered are those of type ", TO "VisibleList", ", but new types are easy to introduce.
     In the following example we introduce a new type of basic list called ", TT "L", ".",
     EXAMPLE {
	  "L = new Type of BasicList",
	  "x = new L from {a,b,c,d}",
	  "join(x,x)"
	  }
     }

document {
     Key => toSequence,
     Headline => "convert to sequence",
     TT "toSequence x", " -- yields the elements of a list ", TT "x", " as a sequence.",
     PARA,
     "If ", TT "x", " is a sequence, then ", TT "x", " is returned.",
     PARA,
     EXAMPLE {
	  "toSequence {1,2,3}"
	  },
     }

document {
     Key => Boolean,
     Headline => "the class of Boolean values",
     "Predicate functions return these as values, and the logical connectives 
     expect to receive them as arguments.",
     PARA,
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
     PARA,
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
     Key => echoOn,
     Headline => "turn on echoing",
     TT "echoOn f", " -- turns on echoing for the file ", TT "f", "."
     }

document {
     Key => echoOff,
     Headline => "turn off echoing",
     TT "echoOff f", " -- turns off echoing for the file ", TT "f", "."
     }

document {
     Key => printString,
     Headline => "lowlevel function to print a string, net, or symbol",
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

document {
     Key => mutable,
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
     PARA,
     "Optional inputs can be provided between parentheses along with the
     other inputs (or arguments) of the function.  For example, if the function is normally used with two
     required inputs, then instead of typing ", TT "f(x,y)", ", you may type 
     ", TT "f(x,y,FOO => t, BAR => u)", ", where ", TT "t", " is the value to be provided to ", TT "f", " as
     the value of the optional input named ", TT "FOO", " and ", TT "u", " is the value to be
     provided to ", TT "f", " as the value of the optional input named ", TT "BAR", ".",
     PARA,
     "The optional inputs can be inserted
     in any order, and may even occur before the required inputs.  If more than one optional input with the same
     option name are given, then the value accompanying the right-most one is the value provided to the function.",
     PARA,
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
     EXAMPLE { "unSingleton (2:a)", "unSingleton (1:a)", "unSingleton (0:a)" }}

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
	  "x = ///-- Given a variety $X$ in projective
-- $r$-space ${\\bf P}^r$, the Fano scheme
-- $Fano_k(X)$ is the natural parameter 
-- space for the linear $k$-planes
-- lying on $X$.

-- First make the homogeneous coordinate 
-- ring of the ambient projective $3$-space
R = ZZ/32003[a,b,c,d]

-- and the ideal of a nonsingular cubic
X = ideal(a^3+b^3+c^3+d^3)
///",
     	  "tutorial x"}}

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
