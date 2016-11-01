-- -*- coding: utf-8 -*-
--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => exit,
     Headline => "exit the program",
     TT "exit n", " -- terminates the program and returns ", TT "n", " as return code.",
     BR{},
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
     Usage => "fork()",
     Outputs => {
         "When successful, it returns the process id of the child in the parent, and returns 0 in
         the child.  When unsuccessful, it returns -1."
         },
     PARA{
         "Platforms that do not have a built-in ", TT "fork()", " function will always return -1."
         },
     PARA{
         "Warning: in multithreaded programs like Macaulay2, very few operations can be safely
         done in the child.  This is especially true when the user has been ",
         TO "parallel programming with threads and tasks", ".
         Even allocating memory in the child may hang the process."
         }
     }

document {
     Key => sleep,
     Headline => "sleep for a while",
     TT "sleep n", " -- sleeps for ", TT "n", " seconds."
     }

document {
     Key => processID,
     Headline => "the process identifier",
     Usage => "processID()",
     Outputs => {
	  ZZ => "the process identifier of the current Macaulay2 process"
	  },
     EXAMPLE "processID()",
     SeeAlso => {groupID, setGroupID}
     }

document {
     Key => groupID,
     Headline => "the process group identifier",
     Usage => "groupID()",
     Outputs => {
	  ZZ => "the process group identifier of the current Macaulay2 process"
	  },
     EXAMPLE "groupID()",
     SeeAlso => {processID, setGroupID}
     }

document {
     Key => setGroupID,
     Headline => "set the process group identifier",
     Usage => "setGroupID(pid,pgid)",
     Inputs => {
	  "pid" => ZZ,
	  "pgid" => ZZ
	  },
     Consequences => {
	  {
	       "The process group id of the process with process id ", TT "pid", " is
	       set to ", TT "pgid", ".  If ", TT "pid", " is 0, the current process
	       is affected.  If ", TT "pgid", " is 0, the new process group id is
	       equal to the process id."
	       }
	  },
     SeeAlso => {processID, groupID}
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
     created with ", TO "dumpdata", " and the same version of Macaulay2.",
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
     Key => identity,
     Headline => "the identity function",
     TT "identity x", " -- returns x.",
     PARA{},
     "This is the identity function."
     }

document {
     Key => {pairs},
     Headline => "list the pairs in a hash table, dictionary, or basic list",
     }

document {
     Key => {(pairs, HashTable)},
     Headline => "list the pairs in a hash table",
     Usage => "pairs x",
     Inputs => { "x" },
     Outputs => {{ "a list of all pairs ", TT "(k,x#k)" }},
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2, c => 3}",
	  "pairs x",
	  }
     }

document {
     Key => {(pairs, Dictionary)},
     Headline => "list the pairs in a dictionary",
     Usage => "pairs d",
     Inputs => { "d" },
     Outputs => {{ "a list of all pairs ", TT "(k,d#k)" }},
     EXAMPLE lines ///
     	  d = new Dictionary
	  getGlobalSymbol (d,"foo")
	  getGlobalSymbol (d,"bar")
	  pairs d
	  first oo
	  class \ oo
	  ///
     }

document {
     Key => {(pairs, BasicList)},
     Headline => "list the pairs in a sequence or list",
     Usage => "pairs L",
     Inputs => { "L" },
     Outputs => {{ "a list of pairs ", TT "(i,L#i)" }},
     EXAMPLE {
	  "L = (a,b,c)",
	  "pairs L",
	  "pairs {x,y,z}",
	  }
     }

document {
     Key => sequence,
     Headline => "make a sequence",
     Usage => "sequence v",
     Inputs => { "v" => Thing },
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
     Key => {xor,
	  (xor,ZZ,ZZ)},
     Headline => "logical exclusive-or",
     Usage => "xor(m,n)",
     Inputs => { "m", "n"},
     Outputs => { 
	  ZZ => {"the bitwise logical exclusive-or of
     	       the integers ", TT "m", " and ", TT "n"}
	  },
     EXAMPLE "xor(10,12)",
     SeeAlso => { (symbol|,ZZ,ZZ), (symbol&,ZZ,ZZ) }
     }

document {
     Key => {mingle,(mingle, BasicList)},
     Headline => "mingle elements of several lists",
     TT "mingle {v,w,...}", " -- produces a new list from the lists or
     sequences v,w,... by taking the first element from each, then the second, 
     and so on.",
     BR{},
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
     Key => {SelfInitializingType,
	  (symbol SPACE, SelfInitializingType, Thing)},
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
     Key => {
	  Manipulator,
	  (symbol SPACE, Manipulator, Database),
	  (symbol SPACE, Manipulator, File),
	  (symbol SPACE, Manipulator, Nothing),
	  (NewFromMethod, Manipulator, Function)
	  },
     Headline => "the class of all file manipulators",
     "A file manipulator is a type of list that, when put out to
     a file with ", TO "<<", " causes a chosen function to be applied
     to the file.  Alternatively, a manipulator can be used the way a function is used.",
     EXAMPLE lines ///
     	  f = new Manipulator from identity
	  stdio << f
	  f stdio
     ///
     }

document {
     Key => close,
     Headline => "close a file",
     TT "f << close", " -- closes the file ", TT "f", ".",
     BR{},
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
     Key => {kill,(kill, File)},
     Headline => "kill a process",
     TT "kill f", " -- kill the process associated with the file ", TT "f", "."
     }

multidoc ///
Node
 Key
  (kill,ZZ)
 Usage
  kill n
 Inputs
  n:
 Consequences
  Item
   the process with id number {\tt n} is killed
///

document {
     Key => closeIn,
     Headline => "close an input file",
     TT "f << closeIn", " -- closes the input file ", TT "f", ".",
     BR{},
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
     BR{},
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
     Key => symbol newline,
     Headline => "the new line character sequence",
     TT "newline", " -- a string containing the character or sequence of
     characters that represents the end of a line.  To end an output line,
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
     a few entries, remove an entry with ", TO "remove", ", close the file, 
     and then remove the file.",
     EXAMPLE {
	  ///filename = temporaryFileName () | ".dbm"///,
      	  ///x = openDatabaseOut filename///,
      	  ///x#"first" = "hi there"///,
      	  ///x#"first"///,
      	  ///x#"second" = "ho there"///,
      	  ///scanKeys(x,print)///,
      	  ///remove(x,"second")///,
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
     Key => {openDatabase,(openDatabase, String)},
     Headline => "open a database file",
     TT "openDatabase \"filename\"", " -- open a database file with the given
     file name."
     }

document {
     Key => {openDatabaseOut,(openDatabaseOut, String)},
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
     Key => symbol oo,
     Headline => "the last output value",
     TT "oo", " -- denotes the value of the expression on the previous output
     line.",
     SeeAlso => { "oo", "ooo", "oooo" }
     }

document {
     Key => symbol ooo,
     Headline => "the next to the last output value",
     TT "ooo", " -- denotes the value of the expression on the output line
     two lines above.",
     SeeAlso => { "oo", "oooo" }
     }

document {
     Key => symbol oooo,
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
     Key => {locate,(locate, Pseudocode), (locate, Function), (locate, Sequence), (locate, Symbol), (locate, Nothing)},
     Headline => "locate source code",
     TT "locate f", " -- for a symbol interpreted function ", TT "f", " 
     returns a sequence ", TT "(n,i,c,j,d,k,e)", " describing the location of
     the definition in the source code.  The name of the source file 
     is ", TT "n", " and the code is occupies line ", TT "i", " column ", TT "c", " 
     through line ", TT "j", " column ", TT "d", ", with the central point of interest
     located at line ", TT "k", " column ", TT "e", ".  If the function ", TT "f", " is compiled, 
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
     Key => {hashTable,(hashTable, List)},
     Headline => "make a hash table",
     TT "hashTable(h,v)", " -- produce a hash table from a list ", TT "v", " of key-value pairs, with an optional collision handler function ", TT "h", ".",
     PARA{},
     "The pairs may be of the form ", TT "a=>b", ", ", TT "{a,b}", ",
     or ", TT "(a,b)", ".",
     PARA{},
     "Missing entries in the list, represented by ", TO "null", ", will be silently
     ignored.",
     PARA{},
     EXAMPLE {
	  "x = hashTable {a=>b, c=>d, }",
      	  "x#a",
     	  "hashTable(plus, {(a,3),(b,4),(a,10)})"
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
     Key => {profile,(profile, Function),(profile, String, Function)},
     Headline => "profile a function",
     TT "f = profile f", " -- replace a global function ", TT "f", " by a profiled version.",
     PARA{},
     "The new function is the same as the old one, except that when the new function is run, it will record the number of times it
     is called and the total execution time.  Use the command ", TO "profileSummary", " to display the data recorded so far.",
     EXAMPLE lines ///
     	  R = ZZ/31[x]
	  f = (x^110+1)*(x^13+1)
	  time factor f
     	  g = () -> factor f
	  g = profile g
	  h = profile("h", () -> factor f)
	  for i to 10 do (g();h();h())
     	  profileSummary
     ///}

document {
     Key => profileSummary,
     Headline => "display profiling data",
     TT "profileSummary", " -- a command that will display the data
     accumulated by running functions produced with ", TO "profile", "."
     }

document {
     Key => {unstack,(unstack, Net)},
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
     BR{},
     TT "uncurry(f, (a,b,c))", "   -- computes ", TT "(((f a) b) c)", ".",
     BR{},
     TT "uncurry(f, (a,b,c,d))", " -- computes ", TT "((((f a) b) c) d)", ".",
     BR{},
     "... and so on.",
     EXAMPLE {
	  "f = a -> b -> c -> [a,b,c]",
	  "uncurry(f, (1,2,3))"
	  }
     }

document {
     Key => {(smithNormalForm,Matrix), 
	  smithNormalForm,
	  [smithNormalForm,ChangeMatrix],
	  [smithNormalForm,KeepZeroes]
	  },
     Headline => "smith normal form for a matrix over ZZ or a PID",
     Usage => "(D,P,Q) = smithNormalForm M\n(D,P) = smithNormalForm(M,ChangeMatrix=>{true,false})\n(D,Q) = smithNormalForm(M,ChangeMatrix=>{false,true})\nD = smithNormalForm(M,ChangeMatrix=>{false,false})\n",
     Inputs => {
	  "M",
	  ChangeMatrix => List => {"of two Boolean elements.
	  This determines whether the change of basis matrices ", TT "P", " and/or ", TT "Q", " are computed"},
	  KeepZeroes => Boolean => "whether to keep rows and columns that are completely zero"
	  },
     Outputs => {
	  "D" => Matrix => {"The Smith normal form of ", TT "M"},
	  "P" => Matrix => "invertible (left) change of basis matrix",
	  "Q" => Matrix => "invertible (right) change of basis matrix"
	  },
     "This function produces a diagonal matrix ", TT "D", ", and invertible matrices ", TT "P", " and ", TT "Q", " such that
     ", TT "D = PMQ", ".  Warning: even though this function is called the Smith normal form, it doesn't necessarily satisfy the
     more stringent condition that the diagonal entries ", TT "d1, d2, ..., dn", " of ", TT "D", " satisfy: ", TT "d1|d2|...|dn.", ".",
     EXAMPLE lines ///
         M = matrix{{1,2,3},{1,34,45},{2213,1123,6543},{0,0,0}}
	 (D,P,Q) = smithNormalForm M
     	 D == P * M * Q
	 (D,P) = smithNormalForm(M, ChangeMatrix=>{true,false})
	 D = smithNormalForm(M, ChangeMatrix=>{false,false}, KeepZeroes=>true)
     ///,
     PARA{
	  "This function is the underlying routine used by ", TO minimalPresentation, 
	  " in the case when the ring is ", TO ZZ, ", or a polynomial ring in one variable over a field."},
     EXAMPLE lines ///
         prune coker M
     ///,
     "In the following example, we test the result be checking that the entries of ", TT "D1, P1 M Q1", " are the same. 
     The degrees associated to these matrices do not match up, so a simple test of equality would return false.",
     EXAMPLE lines ///
	  S = ZZ/101[t]
	  D = diagonalMatrix{t^2+1, (t^2+1)^2, (t^2+1)^3, (t^2+1)^5}
	  P = random(S^4, S^4)
	  Q = random(S^4, S^4)
	  M = P*D*Q
	  (D1,P1,Q1) = smithNormalForm M;
	  D1 - P1*M*Q1 == 0
	  prune coker M
     ///,
     "This routine is under development.  The main idea is to compute a Gröbner basis, transpose the generators, and repeat, until
     we encounter a matrix whose transpose is already a Gröbner basis.  This may depend heavily on the monomial order.",
     Caveat => "The Smith normal form itself is NOT returned! This function is under development, 
     and its performance might need to be improved.  Also, this function
       doesn't warn the user if the ring is not a PID.",
     SeeAlso => {(minimalPresentation,Module)}
     }

document {
     Key => {(getPackage,String), getPackage, CurrentVersion, 
	  [getPackage,CurrentVersion], [getPackage,Version], [getPackage,UserMode],Repository,[getPackage,Repository],
	  [getPackage, DebuggingMode], [getPackage, Configuration]
	  },
     Headline => "download a package from the repository",
     SYNOPSIS (
	  Usage => ///getPackage pkgname///,
     	  BaseFunction => getPackage,
	  Inputs => {
	       "pkgname" => String => {"the name of a package"},
	       Version => String => {"the version to download, instead of the most recent version"},
	       CurrentVersion => String => {"the version currently installed"},
	       Repository => String => {"the URL of the repository"},
	       DebuggingMode => Boolean => {"the debugging mode to be passed to ", TO "installPackage"},
	       UserMode => {"the user mode to be passed to ", TO "installPackage"},
	       Configuration => List => {"the list of configuration values to be passed to ", TO "loadPackage"}
	       },
	  Outputs => {
	       },
	  Consequences => {
	       {"the most recent version of the package is downloaded from the repository and installed, unless it's not newer
		    than the version currently installed (according to the value of the CurrentVersion option)"}
	       }
	  ),
     SYNOPSIS (
	  Usage => ///getPackage()///,
     	  BaseFunction => getPackage,
	  Inputs => {
	       Repository => String => {"the URL of the repository"}
	       },
	  Outputs => {
	       List => "a list of names of available packages from the repository"
	       }
	  ),
     SeeAlso => { installPackage }
     }

document {
     Key => "handleInterrupts",
     Usage => "handleInterrupts = b",
     Inputs => { "b" => Boolean },
     Outputs => { Boolean => { "the value of ", TT "b" }},
     Consequences => {
	  {
	       "If ", TT "b", " is ", TO "false", ", then the default operating system actions for the signals ", TT "SIGINT", ", and ", TT "SIGALRM", " are restored, 
	       and thus typing CTRL-C or the triggering of an ", TO "alarm", " will result in the Macaulay2 process terminating immediately.
	       If ", TT "b", " is ", TO "true", ", then the default Macaulay2 signal handlers are installed,
	       and thus control will be returned to top level after the code currently executing notices that the interrupt flag has been set."
	       }
	  },
     PARA {
	  "The command line option ", TT "--int", " has the same effect as ", TT "handleInterrupts=false", ", and overrides
	  and subsequent setting of ", TT "handleInterrupts", " to ", TT "true", "."
	  }
     }     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
