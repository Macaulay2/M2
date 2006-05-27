--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => exit,
     Headline => "exit the program",
     TT "exit n", " -- terminates the program and returns ", TT "n", " as return code.",
     BR{},
     NOINDENT{}, 
     TT "exit", " -- terminates the program and returns 0 as return code.",
     PARA{},
     "Files are flushed and closed.  Functions registered with ", TO "addStartFunction", "
     are called, unless a nonzero return value has been provided.  Another
     way to exit is to type the end of file character, which is typically
     set to Control-D in unix systems, and is Control-Z under Windows.",
     SeeAlso => {"quit"}
     }

document {
     Key => quit,
     Headline => "quit the program",
     TT "quit", " -- terminates the program and returns 0 as return code.",
     PARA{},
     "Files are flushed and closed.  Another way to exit is to type the end of
     file character, which is typically set to Control-D in unix systems, and is
     Control-Z under MS-DOS.",
     SeeAlso => "exit"
     }

document {
     Key => fork,
     Headline => "fork the process",
     TT "fork()", " -- forks the process, returning the process id of the child
     in the parent, and returning 0 in the child."
     }

document {
     Key => sleep,
     Headline => "sleep for a while",
     TT "sleep n", " -- sleeps for ", TT "n", " seconds."
     }

document {
     Key => processID,
     Headline => "the process identifier",
     TT "processID()", " -- returns the process identifier of the current 
     Macaulay 2 process."
     }

document {
     Key => dumpdata,
     Headline => "dump state of the system to a file",
     TT "dumpdata s", " -- dump all data segments for the current process to 
     the file whose name is stored in the string ", TT "s", ".",
     PARA{},
     "This effectively saves the entire state of the system, except that the
     input buffer for the file ", TO "stdio", " appears to have been emptied,
     and care is taken so that the environment and the command line arguments
     maintain their new values when the data is reloaded later with 
     ", TO "loaddata", "."
     }

document {
     Key => loaddata,
     Headline => "load state of the system from a file",
     TT "loaddata s", " -- load all data segments for the current process from 
     the file whose name is stored in the string ", TT "s", ".  The file must have been
     created with ", TO "dumpdata", " and the same version of Macaulay 2.",
     PARA{},
     "The file should have been created with ", TO "dumpdata", ".  Everything will
     be returned to its former state except:",
     UL {
	  TO "environment",
	  TO "commandLine",
	  "whether the standard input is echoed and prompts to the 
	  standard output are properly flushed, which depends on whether 
	  the standard input is a terminal."
	  }
     }

document {
     Key => buckets,
     Headline => "list the buckets in a hash table",
     TT "buckets x", " -- returns a list of the buckets used internally in an 
     hash table ", TT "x", ".",
     PARA{},
     "Each bucket is represented as a list of key/value pairs."
     }

document {
     Key => identity,
     Headline => "the identity function",
     TT "identity x", " -- returns x.",
     PARA{},
     "This is the identity function."
     }

document {
     Key => generatorSymbols,
     Headline => "store the symbols for the generators",
     TT "generatorSymbols", " -- a key used in a ", TO "Monoid", " under
     which is stored a list of the symbols used as generators for the monoid."
     }

document {
     Key => generatorExpressions,
     Headline => "store the generators",
     TT "generatorExpressions", " -- a key used in a ", TO "Monoid", " under which 
     is stored a list of the generators for the monoid."
     }

document {
     Key => pairs,
     Headline => "list the pairs in a hash table",
     TT "pairs x", " -- makes a list of all key/value pairs ", TT "(k,v)", " in
     a hash table ", TT "x", ".",
     PARA{},
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2, c => 3}",
	  "pairs x",
	  }
     }

document {
     Key => sequence,
     Headline => "make a sequence",
     Usage => "sequence v",
     Inputs => { "v" => Thing => "" },
     Outputs => { Sequence => {TT "v", " if ", TT "v", " is a sequence, otherwise a sequence of length 1 containing ", TT "v"}},
     PARA { "Such a function is needed occasionally to restore uniformity, because a nonempty parenthesized expression with no commas is not parsed as a sequence." },
     EXAMPLE {
	  "sequence()",
	  "sequence(4)",
      	  "sequence(4,5)",
	  "identity()",
	  "identity(4)",
      	  "identity(4,5)",
	  },
     SeeAlso => { unsequence }
     }

document {
     Key => xor,
     Headline => "logical exclusive-or",
     TT "xor(i,j)", " -- produces the bitwise logical exclusive-or of
     the integers ", TT "i", " and ", TT "j", ".",
     PARA{},
     EXAMPLE "xor(10,12)"
     }

document {
     Key => mingle,
     Headline => "mingle elements of several lists",
     TT "mingle {v,w,...}", " -- produces a new list from the lists or
     sequences v,w,... by taking the first element from each, then the second, 
     and so on.",
     BR{}, NOINDENT{},
     TT "mingle (v,w,...)", " -- does the same.",
     PARA{},
     "After one of the lists is exhausted, it is silently ignored.",
     EXAMPLE {
	  "mingle({1,2,3,4},{a},{F,F,F,F,F,F,F,F,F,F})",
      	  ///concatenate mingle( {"a","b","c"} , {",",","} )///,
	  },
     "It is easy to transpose a nested list (thinking of it as a matrix)
     using ", TO "mingle", " and ", TO "pack", ".",
     EXAMPLE {
      	  "pack(2, mingle {{1,2,3,4},{5,6,7,8}})"
	  }
     }

document {
     Key => SelfInitializingType,
     Headline => "the class of all self initializing types",
     "A self initializing type ", TT "X", " will produce an instance of X from
     initial data ", TT "v", " with the expression ", TT "X v", ".",
     PARA{},
     EXAMPLE {
	  "X = new SelfInitializingType of BasicList",
      	  "x = X {1,2,3}",
      	  "class x",
	  },
     PARA{},
     TO "Command", " is an example of a self initializing type.",
     SeeAlso => {"HeaderType", "WrapperType"}
     }

document {
     Key => Manipulator,
     Headline => "the class of all file manipulators",
     "A file manipulator is a type of list which, when put out to
     a file with ", TO "<<", " causes a chosen function to be applied
     to the file.",
     }

document {
     Key => close,
     Headline => "close a file",
     TT "f << close", " -- closes the file ", TT "f", ".",
     BR{}, NOINDENT{},
     TT "close f", " -- closes the file ", TT "f", ".",
     PARA{},
     "In the case of an output file, any buffered output is first
     written to the file, and the return value is an integer,
     normally 0, or -1 on error, or the return status of the child
     process in case the the file was a pipe.",
     PARA{},
     "If the file was open for both input and output, both directions
     are closed.",
     PARA{},
     "If the file is a pipe to another process, i.e., the filename
     began with the character ", TT "!", ", we will wait for the
     process to terminate.  If you don't want to wait for the process
     to terminate, open the file with ", TO "openInOut", ", and if
     necessary, use ", TO "closeIn", " to close it, to indicate that
     it has received all its input.",
     PARA{},
     "If the file is ", TT "stdio", " then it is left open, and
     no error is signaled."
     }

document {
     Key => kill,
     Headline => "kill a process",
     TT "kill f", " -- kill the process associated with the file ", TT "f", "."
     }

document {
     Key => closeIn,
     Headline => "close an input file",
     TT "f << closeIn", " -- closes the input file ", TT "f", ".",
     BR{}, NOINDENT{},
     TT "closeIn f", " -- closes the input file ", TT "f", ".",
     PARA{},
     "If the file was open only for input, then ", TO "close", " is
     easier to use and has the same effect.",
     PARA{},
     "If the file was open for both input and output, it remains
     open for output."
     }

document {
     Key => closeOut,
     Headline => "close an output file",
     TT "f << closeOut", " -- closes the output file ", TT "f", ".",
     BR{}, NOINDENT{},
     TT "closeOut f", " -- closes the output file ", TT "f", ".",
     PARA{},
     "Any buffered output is first written to the file,
     and the return value is an integer, normally 0, or -1
     on error, or the return status of the child process
     in case the the file was a pipe.",
     PARA{},
     "If the file was open only for output, then ", TO "close", " is
     easier to use and has the same effect.",
     PARA{},
     "If the file was open for both input and output, it remains
     open for input."
     }

document {
     Key => flush,
     Headline => "flush output to file",
     TT "f << flush", " -- writes out any buffered output for the output file ", TT "f", ".",
     }

document {
     Key => endl,
     Headline => "end an output line",
     TT "f << endl", " -- ends the line currently being put out to the
     file ", TT "f", ".",
     PARA{},
     "It is an essential portable programming practice to use ", TT "endl", "
     always, for writing newline characters (see ", TO "newline", ") to a
     file will not terminate a line containing nets properly,
     and it will not flush the output buffer."
     }

document {
     Key => symbol "newline",
     Headline => "the new line character sequence",
     TT "newline", " -- a string containing the character or sequence of
     characters which represents the end of a line.  To end an output line,
     you should use ", TO "endl", " instead, because there is more to 
     ending an output line than emitting the characters in ", TT "newline", ",
     especially when nets are being used.",
     PARA{},
     "This string depends on what your operating system is: on Unix systems
     it is the ascii character 10; on Macintoshes it is the ascii character
     13, and under MS-DOS and Windows 95 it is a string of length 2 containing
     ascii characters 13 and 10.",
     PARA{},
     "Try to avoid confusing the newline string described here with the
     ASCII character called ", TT "newline", ".  That character can be
     incorporated into a string with the escape sequence ", TT "\\n", ",
     and it always has ASCII code 10.",
     EXAMPLE ///ascii "\n"///,
     SeeAlso => "Net"
     }

document {
     Key => collectGarbage,
     Headline => "collect the garbage in memory",
     TT "collectGarbage()", " -- attempt a garbage collection.",
     PARA{},
     SeeAlso => "GC garbage collector"
     }

--document { gcDump,
--     Headline => "the status of the memory allocator",
--     TT "gcDump()", " -- produces a dump of the status of the garbage collector.",
--     PARA{},
--     "Users will normally not want to use this function.  It calls the 
--     function ", TT "GC_dump", " in the garbage collector, and the output can
--     be used to debug problems with memory allocation.",
--     PARA{},
--     SeeAlso => "GC garbage collector"
--     }

document {
     Key => lookupCount,
     Headline => "reference count for a symbol",
     TT "lookupCount s", " -- the number of times the symbol ", TT "s", " has been
     encountered in source code presented to the interpreter."
     }

document {
     Key => "version",
     Headline => "information about this version of the program",
     Usage => "version",
     Consequences => {
	  {ofClass HashTable, " describing this version of the program"}
	  },
     "The values stored in this hash table depend on the source code version
     the architecture for which the program was compiled, and the libraries
     (both static and dynamic) against which the program is linked.",
     EXAMPLE "version"
     }

document {
     Key => Database,
     Headline => "the class of all database files",
     "A database file is just like a hash table, except both the keys and
     values have to be strings.  In this example we create a database file, store
     a few entries, remove one by assigning ", TO "null", " to it, close the file, 
     and then remove the file.",
     EXAMPLE {
	  ///filename = temporaryFileName () | ".dbm"///,
      	  ///x = openDatabaseOut filename///,
      	  ///x#"first" = "hi there"///,
      	  ///x#"first"///,
      	  ///x#"second" = "ho there"///,
      	  ///scanKeys(x,print)///,
      	  ///x#"second" = null///,
      	  ///scanKeys(x,print)///,
      	  ///close x///,
      	  ///removeFile filename///,
	  },
     SeeAlso => {"HashTable", "String", "removeFile"}
     }

document {
     Key => reorganize,
     Headline => "reorganize a database file",
     TT "reorganize x", " -- reorganize the database ", TT "file", " x, compactifying it.",
     PARA{},
     SeeAlso => "Database"
     }

document {
     Key => openDatabase,
     Headline => "open a database file",
     TT "openDatabase \"filename\"", " -- open a database file with the given
     file name."
     }

document {
     Key => openDatabaseOut,
     Headline => "open a database file for writing",
     TT "openDatabaseOut \"filename\"", " -- open a database file with the given
     file name, and allow changes to be made to it."
     }

document {
     Key => firstkey,
     Headline => "get the first key",
     TT "firstkey f", " -- return the first key available in the database
     file ", TT "f", ".",
     PARA{},
     "Returns ", TO "null", " if none.",
     PARA{},
     SeeAlso => "Database"
     }

document {
     Key => nextkey,
     Headline => "the next key in a database",
     TT "nextkey f", " -- return the next key available in the database
     file ", TT "f", ".",
     PARA{},
     "Returns ", TO "null", " if none.",
     PARA{},
     SeeAlso => "Database"
     }

document {
     Key => symbol "oo",
     Headline => "the last output value",
     TT "oo", " -- denotes the value of the expression on the previous output
     line.",
     SeeAlso => { "oo", "ooo", "oooo" }
     }

document {
     Key => symbol "ooo",
     Headline => "the next to the last output value",
     TT "ooo", " -- denotes the value of the expression on the output line
     two lines above.",
     SeeAlso => { "oo", "oooo" }
     }

document {
     Key => symbol "oooo",
     Headline => "the third to the last output value",
     TT "oooo", " -- denotes the value of the expression on the output line
     three lines above.",
     SeeAlso => { "oo", "ooo" }
     }

document {
     Key => InverseMethod,
     Headline => "compute reciprocals",
     TT "InverseMethod", " -- a key used under which is stored a method
     for computing multiplicative inverses.",
     PARA{},
     "Internal routines for computing powers call upon that method when
     the exponent is negative."
     }

document {
     Key => "or",
     Headline => "disjunction",
     TT "t or u", " -- returns true if ", TT "t", " is true or ", TT "u", "
     is true.",
     PARA{},
     "If ", TT "t", " is true, then the code in ", TT "u", " is not evaluated.",
     SeeAlso =>{ "and", "not" }
     }

document {
     Key => "and",
     Headline => "conjunction",
     TT "t and u", " -- returns true if ", TT "t", " is true and ", TT "u", "
     is true.",
     PARA{},
     "If ", TT "t", " is false, then the code in ", TT "u", " is not evaluated.",
     SeeAlso =>{ "or", "not" }
     }

document {
     Key => locate,
     Headline => "locate source code",
     TT "locate f", " -- for a symbol interpreted function ", TT "f", " 
     returns a sequence ", TT "(n,i,c,j,d)", " describing the location of
     the definition in the source code.  The name of the source file 
     is ", TT "n", " and the code is occupies line ", TT "i", " column ", TT "c", " 
     through line ", TT "j", " column ", TT "d", ". If the ", TT "f", " is compiled, 
     then the location is not available, and ", TO "null", " is returned.",
     PARA{},
     "If ", TT "f", " is a sequence, then ", TO "lookup", " is applied
     first, and then the location of the resulting function is provided.",
     PARA{},
     "If ", TT "f", " is ", TO "null", ", then ", TO "null", " is returned."
     }

document {
     Key => MutableHashTable,
     Headline => "the class of all mutable hash tables",
     PARA{},
     "A mutable hash table is a type of hash table whose entries can be changed.",
     PARA{},
     "Normally the entries in a mutable hash table are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     EXAMPLE {
	  "x = new MutableHashTable",
      	  "scan(0 .. 30, i -> x#i = i^2)",
      	  "x # 20",
      	  "x #? 40",
	  },
     SeeAlso => "HashTable"
     }

-- document {
--      Key => precedence,
--      Headline => "parsing precedence",
--      TT "precedence x", " -- returns the parsing precedence of ", TT "x", " for use in
--      the printing routines.",
--      PARA{},
--      SeeAlso => {"Expression", "net", "toString"}
--      }

document {
     Key => hashTable,
     Headline => "make a hash table",
     TT "hashTable v", " -- produce a hash table from a list ", TT "v", " of key-value
     pairs.",
     PARA{},
     "The pairs may be of the form ", TT "a=>b", ", ", TT "{a,b}", ",
     or ", TT "(a,b)", ".",
     PARA{},
     "Missing entries in the list, represented by ", TO "null", ", will be silently
     ignored.",
     PARA{},
     EXAMPLE {
	  "x = hashTable {a=>b, c=>d, }",
      	  "x#a"
	  },
     }

document {
     Key => (toList, BasicList),
     Headline => "list of elements",
     TT "toList x", " -- provides a list of elements in the basic list ", TT "x", ".",
     PARA{},
     "This is a good way to convert a list of some other type to a list of type
     ", TO "List", ".",
     EXAMPLE {
	  "toList [a,b,c]"
	  } 
     }

document {
     Key => (toList, Set),
     Headline => "list of elements",
     TT "toList x", " -- provides a list of element in the set ", TT "x", ".",
     EXAMPLE {
	  "x = set {a,b,c}",
	  "toList x"
	  }
     }

document {
     Key => toList,
     Headline => "list of elements"
     }

document {
     Key => saturate,
     Headline => "saturation of ideal or submodule",
     TT "saturate(I,J,options)", " -- computes the saturation ", TT "(I : J^*)", " 
     of I with respect to ", TT "J", ".  If ", TT "J", " is not given, the 
     ideal ", TT "J", " is taken to be the ideal generated by the variables of 
     the ring ", TT "R", " of ", TT "I", ".",
     PARA{},
     "If I is either an ideal or a submodule of a module M,
     the saturation (I : J^*) is defined to be the set of elements
     f in the ring (first case) or in M (second case) such that
     J^N * f is contained in I, for some N large enough.",
     PARA{},
     "For example, one way to homogenize an ideal is to
     homogenize the generators and then saturate with respect to
     the homogenizing variable.",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
	  "I = ideal(a^3-b, a^4-c)",
	  "Ih = homogenize(I,d)",
	  "saturate(Ih,d)",
	  },
     "We can use this command to remove graded submodules of 
     finite length.",
     EXAMPLE {
	  "m = ideal vars R",
	  "M = R^1 / (a * m^2)",
	  "M / saturate 0_M",
	  },
     PARA{},
     "The computation is currently not stored anywhere: this means
     that the computation cannot be continued after an interrupt.
     This will be changed in a later version."
     }

document {
     Key => [saturate,Strategy],
     "There are four strategy values:",
     SUBSECTION "Iterate",
         TT "saturate(I,J,Strategy => Iterate)", " -- indicates that successive ideal
	 or module quotients should be used.",
	 PARA{},
	 "This value is the default.",
     SUBSECTION "Linear",
         TT "saturate(I,J,Strategy => Linear)", 
	 TT "Strategy => Linear", " -- indicates that the reverse lex order should
	 be used to compute the saturation.",
	 PARA{},
	 "This presumes that ", TT "J", " is a single, linear polynomial, and that ", TT "I", " 
	 is homogeneous.",
	 PARA{},
	 "This is also an option value for ", TO "pushForward1", ".",
     SUBSECTION "Bayer",
     	 TT "saturate(I,f,Strategy => Bayer)", " -- indicates that the method of Bayer's 
	 thesis should be used.",
	 PARA{},
	 "The method is to compute ", TT "(I:f)", " for ", TT "I", " and ", TT "f", " homogeneous,
	 add a new variable ", TT "z", ", compute a Groebner basis of ", TT "(I,f-z)", " in reverse 
	 lex order, divide by ", TT "z", ", and finally replace ", TT "z", " by ", TT "f", ".",
     SUBSECTION "Eliminate",
	 TT "saturate(I,f,Strategy => Eliminate)", " -- indicates that the
	 saturation ", TT "(I:f)", " should be computed by eliminating
	 f", TT "z", " from ", TT "(I,f*z-1)", ", where ", TT "z", " is a new variable."
     }

document {
     Key => [saturate,DegreeLimit],
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "saturate", " which specifies that the computation should halt after dealing 
     with degree n."
     }

document {
     Key => profile,
     Headline => "profile a function",
     TT "f = profile f", " -- replace a global function ", TT "f", " by a profiled version.",
     PARA{},
     "The new function is the same as the old one, except that when
     the new function is run, it will record the number of times it
     is called and the total execution time.  Use ", TO "profileSummary", "
     to display the data recorded so far."
     }

document {
     Key => profileSummary,
     Headline => "display profiling data",
     TT "profileSummary", " -- a command which will display the data
     accumulated by running functions produced with ", TO "profile", "."
     }

document {
     Key => globalAssignFunction,
     Headline => "the standard method for the global assignment hook",
     TT "globalAssignFunction", " -- the standard function which can be used
     as a method for ", TO GlobalAssignHook, " so that certain types of
     mutable hash tables ", TT "X", ", when assigned to a global variable, will acquire
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
     SeeAlso => { "name", "symbol", "SelfInitializingType" }
     }

document {
     Key => globalReleaseFunction,
     Headline => "the standard method for the global variable release hook",
     TT "globalReleaseFunction", " -- the standard function which can be used as
     a method for ", TO GlobalReleaseHook, " so that certain types of things, which
     have acquired as their name the name of a global variable to which they have
     been assigned, will lose that name when a different value is assigned to
     the variable.",
     PARA{},
     SeeAlso => "globalAssignFunction"
     }

document {
     Key => unstack,
     Headline => "list the rows of a net",
     TT "unstack x", " -- produces a list of strings, each containing the
     characters in one row of the ", TT "Net", " ", TT "x", ".",
     PARA{},
     "The orginal net, adjusted so its height is 1, may be recovered
     with ", TO "stack", ". The individual strings will have 
     all trailing spaces removed, unless this would make all of them 
     narrower than the original net, in which case the first string
     retains its trailing spaces."
     }

document {
     Key => uncurry,
     Headline => "uncurry a function",
     TT "uncurry(f, (a,b))", "     -- computes ", TT "((f a) b)", ".",
     BR{}, NOINDENT{},
     TT "uncurry(f, (a,b,c))", "   -- computes ", TT "(((f a) b) c)", ".",
     BR{}, NOINDENT{},
     TT "uncurry(f, (a,b,c,d))", " -- computes ", TT "((((f a) b) c) d)", ".",
     BR{}, NOINDENT{},
     "... and so on.",
     EXAMPLE {
	  "f = a -> b -> c -> [a,b,c]",
	  "uncurry(f, (1,2,3))"
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
