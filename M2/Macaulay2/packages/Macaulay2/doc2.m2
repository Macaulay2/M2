--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => "timing",
     Headline => "time a computation",
     TT "timing e", " evaluates ", TT "e", " and returns a list of type ", TO "Time", "
     of the form ", TT "{t,v}", ", where ", TT "t", " is the number of seconds
     of cpu timing used, and ", TT "v", " is the value of the the expression.",
     PARA{},
     "The default method for printing such timing results is to display the
     timing separately in a comment below the computed value.",
     EXAMPLE {
	  "timing 3^30",
      	  "peek oo",
	  },
     SeeAlso => "Time"
     }

document {
     Key => "time",
     Headline => "time a computation",
	Usage => "time e",
     TT "time e", " evaluates ", TT "e", ", prints the amount of cpu time
     used, and returns the value of ", TT "e", ".",
     EXAMPLE "time 3^30",
     SeeAlso => "timing"
     }

document {
     Key => Time,
     Headline => "the class of all timing results",
     TT "Time", " is the class of all timing results.  Each timing result
     is ", ofClass BasicList, " of the form ", TT "{t,v}", ", where ", TT "t", " 
     is the number of seconds of cpu time used, and ", TT "v", " is the value 
     of the the expression."
     }

document {
     Key => "null",
     Headline => "the unique member of the empty class",
     "When it is the value of an expression entered into the interpreter, the
     output line doesn't appear.  Empty spots in a list are represented by
     it.",
     PARA{},
     "It is the only member of the class ", TO "Nothing", ", which strictly
     speaking, ought to have no members at all.",
     PARA{},
     "An ", TO "if", " expression with no ", TO "else", " clause returns
     ", TO "null", " when the predicate is false.",
     PARA{},
     "Various routines that prepare data for printing convert ", TO "null", "
     to an empty string.",
     EXAMPLE {
	  "x = {2,3,,4}",
	  "net x",
      	  "toString x#2",
      	  "peek x",
	  }
     }

document {
     Key => "then",
     Headline => "condition testing",
     TT "then", " a keyword used with ", TO "if", ".",
	EXAMPLE {
		"if 5 > 4 then 8 else 7"
		},
	SeeAlso => {"if", "else"}
     }

document {
     Key => "else",
     Headline => "condition testing",
     TT "else", " a keyword used with ", TO "if", ".",
	EXAMPLE {
		"if 4 > 5 then 8 else 7"
		},
	SeeAlso => {"if", "then"}
     }

document {
     Key => "if",
     Headline => "condition testing",
     TT "if p then x else y", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else the value of ", TT "y", " is provided.",
     PARA{},
     TT "if p then x", " computes ", TT "p", ", which must yield the value ", TO "true", " 
     or ", TO "false", ".  If true, then the value of ", TT "x", " is provided,
     else ", TO "null", " is provided."
     }

document {
     Key => "return",
     Headline => "return from a function",
     TT "return x", " returns ", TT "x", " as the value of the function currently
     being evaluated.",BR{},
     TT "return;", " returns ", TO "null", " as the value of the function currently
     being evaluated.",
     PARA{},
     EXAMPLE {
	  "f = x -> (
     if x == 3 then return;
     if x > 3 then return x^2;
     5);",
	  "f 2",
	  "f 3",
	  "f 4"
	  },
     SeeAlso => { "break" }
     }

document {
     Key => "list",
     Headline => "loop control",
     TT "list", " a keyword used with ", TO "while", ", and ", TO "for", "."
     }

document {
     Key => "from",
     Headline => "loop control",
     TT "from", " a keyword used with ", TO "for", " and ", TO "new", "."
     }

document {
     Key => "to",
     Headline => "loop control",
     TT "to", " a keyword used with ", TO "for", "."
     }

document {
     Key => "do",
     Headline => "loop control",
     TT "do", " a keyword used with ", TO "while", ", and ", TO "for", "."
     }

document {
     Key => "try",
     Headline => "catch an error",
     Usage => "try x then y else z",
     Inputs => {
	  "x" => "code",
	  "y" => "code",
	  "z" => "code"
	  },
     Consequences => {
	  {"the code ", TT "x", " is run; if no error or ", TO "alarm", " occurs, then the code ", TT "y", " is run; otherwise, the code ", TT "z", " is run"}
	  },
     "The return value is the value returned by ", TT "y", " or ", TT "z", ", as the case may be.",
     PARA{},
     "The clause '", TT "then y", "' may be omitted, in which case the return value is the value returned by ", TT "x", ", if there is no error or alarm.",
     PARA{},
     "The clauses '", TT "then y else z", "' may both be omitted, in which case the return value is the value returned by ", TT "x", ", unless an error or
     alarm occurs, in which case ", TO "null", " is returned.",
     PARA{},
     "The behavior of interrupts (other than alarms) is unaffected.",
     EXAMPLE "apply(-3..3,i->try 1/i else infinity)",
     Caveat => "We will change the behavior of this function soon so that it will be possible to catch errors of a particular type.  Meanwhile, users are
     recommended to use this function sparingly, if at all."
     }

///
R = QQ[vars(0..24)]
f = () -> (alarm 4; try res coker vars R else "ran out of time")
time f()
///

document {
     Key => openFiles,
     Headline => "list the open files",
     TT "openFiles()", " produces a list of all currently open files.",
     PARA{},
     SeeAlso => { "File" }
     }

document {
     Key => stdio,
     Headline => "the standard input output file",
     TT "stdio", " the standard input output file.",
     PARA{},
     "Use this file to get input from the terminal, or to display information
     on the user's screen.  This is the file always used by ", TO "print", "
     and used ", TO "<<", " if it is not explicitly given a file."
     }

document {
     Key => stderr,
     Headline => "the standard error output file",
     TT "stderr", " the standard error output file.",
     PARA{},
     "Use this file to display error messages on the user's screen."
     }

document {
     Key => openListener,
     Headline => "open a port for listening",
     TT "f = openListener \"$:service\"", "  opens a listener on the local
     host at the specified service port.",
     BR{},
     TT "f = openListener \"$\"", "  opens a listener on the local
     host at the Macaulay2 port (2500).",
     PARA{},
     "Use ", TT "openInOut f", " to accept an incoming connection on the listener,
     returning a new input output file which serves as the connection.  The function
     ", TT "isReady", " can be used to determine whether an incoming connection has
     arrived, without blocking."
     }

document {
     Key => openIn,
     Headline => "open an input file",
     TT "openIn \"fff\"", " opens an input file whose filename is ", TT "fff", ".",
     PARA{},
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document {
     Key => openOut,
     Headline => "open an output file",
     TT "openOut \"fff\"", " opens an output file whose filename is ", TT "fff", ".",
     PARA{},
     "Other options are available.  For details, see ", TO "openInOut", "."
     }

document {
     Key => openInOut,
     Headline => "open an input output file",
     TT "openInOut \"fff\"", " opens an input output file whose 
     filename is ", TT "fff", ".",
     BR{},
     TT "openInOut \"!cmd\"", " opens an input output file which corresponds to a pipe 
     receiving the output from the shell command ", TT "cmd", ".",
     BR{},
     TT "openInOut \"$hostname:service\"", "opens an input output file
     by connecting to the specified service port at the specified host.",
     BR{},
     TT "openInOut \"$:service\"", " opens an input output file by
     listening to the specified service port on the local host, and 
     waiting for an incoming connection.",
     BR{},
     TT "openInOut \"$hostname\"", " opens an input output file
     by connecting to the Macaulay2 service port (2500) at the specified host.",
     BR{},
     TT "openInOut \"$\"", " opens an input output file by listening to the
     Macaulay2 service port (2500) on the local host, and waiting for an
     incoming connection.",
     BR{},
     TT "openInOut f", " opens an input output file by accepting a
     connection to the listener ", TT "f", ", previously created with
     ", TO "openListener", ".",
     PARA{},
     "In order to open a socket successfully, there must be a process
     accepting connections for the desired service on the specified host.",
     PARA{},
     "Socket connections are not available on Sun computers, because Sun 
     doesn't provide static versions of crucial libraries dealing with 
     network communications, or the static version doesn't provide network 
     name service for looking up hostnames.",
     PARA{},
     "The various forms listed above can be used also with all other input
     output operations that open files, such as ", TO "openIn", ",
     ", TO "openOut", ", ", TO "get", ", and ", TO "<<", ", with data transfer 
     possible only in the direction specified.  The only subtlety is that 
     with ", TT ///openIn "!foo"///, " the standard input of the command
     ", TT "foo", " is closed, but with ", TT ///openOut "!foo"///, " the
     standard output of the command ", TT "foo", " is connected to the
     standard output of the parent Macaulay2 process."
     }

document {
     Key => protect,
     Headline => "protect a symbol",
     TT "protect s", " protects the symbol ", TT "s", " from having its value changed.",
     PARA{},
     "There is no unprotect function, because we want to allow the compiler
     to take advantage of the unchangeability.",
     PARA{},
     "The documentation function ", TO "document", " protects the symbols
     it documents."
     }

document {
     Key => isInputFile,
     Headline => "whether a file is open for input",
     TT "isInputFile f", " whether ", TT "f", " is an input file.",
     PARA{},
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isOutputFile,
     Headline => "whether a file is open for output",
     TT "isOutputFile f", " whether ", TT "f", " is an output file.",
     PARA{},
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isOpen,
     Headline => "whether a file or database is open",
     TT "isOpen f", " whether ", TT "f", " is an open file or an
     open database.",
     PARA{},
     "An open file is either an input file, an output file, an
     input output file, or a listener.",
     PARA{},
     "The return value is ", TO "true", " or ", TO "false", "."
     }

document {
     Key => isListener,
     Headline => "whether a file is open for listening",
     TT "isListener f", " whether ", TT "f", " is a listener.",
     PARA{},
     "The return value is ", TO "true", " or ", TO "false", "."
     }

---------------------
---- Operators ------
---------------------
document {
     Key => symbol <<, 
     Headline => "a binary operator (file output, ...)",
     }
document {
     Key => (symbol <<, ZZ, ZZ),
     Headline => "shift bits leftward",
     Usage => "i << j",
     Inputs => { "i", "j" },
     Outputs => {{ "the integer obtained from ", TT "i", " by shifting its binary representation leftward ", TT "j", " places" }},
     EXAMPLE "256 << 5",
     SeeAlso => {(symbol >>,ZZ, ZZ)}
     }

undocumented {(symbol <<, File, Symbol),(symbol <<, File, Net),(symbol <<, File, String)}
document {
     Key => { (symbol <<, File, Thing),(symbol <<, String, Thing), (symbol <<, File, Manipulator),(symbol <<, List, Thing),
	  (symbol <<, Nothing, Thing),(symbol <<, Nothing, Manipulator), (symbol <<, Thing) },
     Headline => "print to a file",
     Usage => "f << x\n  << x",
     Inputs => { 
	  "f" => Nothing => { ofClass {File, String, List, Nothing} },
	  "x"
	  },
     Outputs => {
	  File => "the output file(s) used"
     	  },
     Consequences => {{
	  "The object ", TT "x", " is prepared for printing (with ", TO "net", ") and printed on the output file(s) ", TT "f", ".
	  If ", TT "f", " is a string, then it is interpreted as a filename or pipe to open, an output file is created, used, and returned.
	  If ", TT "f", " is a list, then the output operation is performed on each one.
	  If ", TT "f", " is ", TO "null", ", then the output is discarded; thus ", TO "null", " is useful as a dummy output file.
	  If ", TT "f", " is omitted, as in the second usage line, then the output is sent to ", TO "stdio", ", and it will appear (usually) on the screen."
	  }},
     PARA {
	  "Parsing of ", TO "<<", " associates leftward, so that several objects  may be displayed with an expression such as ", TT "f<<x<<y<<z", "."
	  },
     EXAMPLE lines ///
     	  stderr << "-- hi there --" << endl
     	  << "-- ho there --" << endl
	  fn = temporaryFileName()
	  fn << "hi there" << endl << close
	  get fn
	  R = QQ[x]
	  f = (x+1)^10
	  << f
	  fn << f << close
     	  get fn
	  fn << toExternalString f << close
     	  get fn
	  value get fn
	  removeFile fn
     ///,
     SeeAlso => { stdio, stderr, endl, close }
     }

document {
     Key => symbol >>, 
     Headline => "a binary operator, uses include bit shifting, or attaching optional inputs to functions" 
     }

document {
     Key => (symbol >>, ZZ, ZZ),
     Headline => "shift bits rightward",
     Usage => "i >> j",
     Inputs => { "i", "j" },
     Outputs => {{ "the integer obtained from ", TT "i", " by shifting its binary representation rightward ", TT "j", " places" }},
     EXAMPLE "256 >> 5",
     SeeAlso => {(symbol <<,ZZ, ZZ)}
     }

document {
     Key => symbol :,
     Headline => "a binary operator, uses include repetition; ideal quotients",
     }

undocumented {
     (symbol .., InfiniteNumber, InfiniteNumber),
     (symbol .., ZZ, InfiniteNumber),
     (symbol .., InfiniteNumber, ZZ),
     (symbol .., Thing, Thing)
     }

document {
     Headline => "a binary operator, uses include ranges; sequence of consecutive items",
     Key => {symbol ..,
     	  (symbol .., IndexedVariable, IndexedVariable),
     	  (symbol .., List, List),
     	  (symbol .., Sequence, Sequence),
     	  (symbol .., Symbol, Symbol),
	  (symbol .., ZZ, ZZ)},
     Usage => "a .. b",
     Inputs => { "a" => {"of type ", TO "ZZ", ", ", 
     	       TO "IndexedVariable", ", ", 
     	       TO "List", ", ", 
     	       TO "Sequence", " or ", 
     	       TO "Symbol"},
	  "b" => {"of the same type as ", TT "a", "." },
	  },
     Outputs => {
     	  Sequence => {" of consecutive items in the range of a to b inclusive."},
	  },
     EXAMPLE lines ///
     	  1..5
	  toList(1..5)
	  a..f
	  p_(1,1) .. p_(2,3)
     	  ///,
     PARA{},
     "The most confusing thing about this operator is that it is not a syntactic
     construction, and so the resulting sequences do not splice themselves into
     enclosing lists, as in each of the following examples.  Use ", TO "splice", "
     to fix that.",
     EXAMPLE lines ///
      	  {10..10}
      	  {10..8}
      	  {3..5,8..10}
      	  splice {3..5,8..10}
	  ///,
     PARA{},
     SUBSECTION "Sequences of symbols and index variables for 
     use as variables in polynomial rings",
     EXAMPLE "a .. i",
     EXAMPLE lines ///
	  x_0 .. x_9
      	  x_(t_0) .. x_(t_5)
      	  x_a .. x_e
	  ///,
     SUBSECTION "Rectangular lists",
     "This operator can be used with sequences or lists to produce rectangular
     intervals.",
     EXAMPLE lines ///
	  (0,0)..(1,3)
      	  p_(0,a) .. p_(1,c)
	  p_(1,1) .. p_(2,3)
	  ///,
     SeeAlso => {"ranges and repetitions",
	  "polynomial rings",
	  "subscripted variables"}
     }

document {
     Key => "comparison operators",
     Usage => "",
     UL {
	  "x == y",
	  "x != y",
	  "x < y",
	  "x <= y",
	  "x > y",
	  "x >= y",
	  "x ? y",
	  },
     SeeAlso => {sort, sortColumns, "operators"}
     }
document {
     Key => symbol <,
     Headline => "less than",
     TT "x < y", " yields ", TO "true", " or ", TO "false", 
     " depending on whether x < y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol <=,
     Headline => "less than or equal",
     TT "x <= y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x <= y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol >,
     Headline => "greater than",
     TT "x > y", " yields ", TO "true", " or ", 
     TO "false", " depending on whether x > y.",
     PARA{},
     "Calls upon ", TO "?", " to perform the comparison, if necessary."
     }
document {
     Key => symbol >=,
     Headline => "greater than or equal",
     Usage => "x >= y",
     Inputs => {"x", "y"},
     Outputs => {
	  Boolean => "depending on whether x >= y"
	  },
     "Calls upon ", TO "?", " to perform the comparison, if necessary.",
     }

undocumented {
     (symbol ?, TO2, TO2),
     (symbol ?, TO2, TO),
     (symbol ?, TO, TO2),
     (symbol ?, TOH, TO2),
     (symbol ?, TO2, TOH),
     (symbol ?, TO, TO),
     (symbol ?, String, DocumentTag),
     (symbol ?, DocumentTag, String),
     (symbol ?, TOH, TO),
     (symbol ?, TO, TOH),
     (symbol ?, Thing),
     (symbol ?, InfiniteNumber, InfiniteNumber),
     (symbol ?, DocumentTag, DocumentTag),
     (symbol ?, Thing, InfiniteNumber),
     (symbol ?, TOH, TOH),
     (symbol ?, InfiniteNumber, Thing),
     (symbol ?, Thing, Thing),
     (symbol ?, Function),
     }

document {
     Key => {symbol ?,
     	  (symbol ?, Symbol, IndexedVariable),
     	  (symbol ?, IndexedVariable, IndexedVariable),
     	  (symbol ?, RingElement, RingElement),
     	  (symbol ?, List, List),
     	  (symbol ?, Tally, Tally)
	  },
     Headline => "comparison operator",
     Usage => "x ? y", 
     Inputs => { "x", "y" },
     Outputs => { Boolean },
     "Compares ", TT "x", " and ", TT "y", " (of the same type), returning ", TT "symbol <", ", ",
     TT "symbol >", ", ", TT "symbol ==", ", or ", TO "incomparable", ".",
     PARA{},
     "Many types of objects may be compared.  Numbers are handled as one would expect,
     and strings, lists and sequences are generally compared lexicographically.",
     EXAMPLE lines ///
     	  3 ? 4
	  "book" ? "boolean"
	  3 ? 3.
	  {1,2,3} ? {4,5}
     ///,
     PARA{},
     "Polynomials may also be compared.  The order depends on the 
     monomial order in the ring.",
     EXAMPLE lines ///
     	  R = ZZ[a,b,c]
	  a*c ? b^2
     ///,
     "A set is smaller than another if it is a subset; for tallies, corresponding counts should all be smaller.",
     EXAMPLE lines ///
     	  set {1,2} ? set {2,3}
     	  set {1,2} ? set {1,2,3}
	  tally {1,1,2} ? tally {1,2,3}
	  tally {1,1,2} ? tally {1,1,2,3}
     ///,
     SeeAlso => {sort, rsort}
     }

document {
     Key => getc,
     Headline => "get a byte",
     TT "getc f", " obtains one byte from the input file f and provides it as a 
     string of length 1.  On end of file an empty string of is returned.",
     PARA{},
     SeeAlso => { "File" },
     PARA{},
     "Bug: the name is cryptic and should be changed."
     }



protect incomparable
document {
     Key => incomparable,
     Headline => "a result indicating incomparability",
     TT "incomparable", " a symbol which may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }


document {
     Key => ";",
     Headline => "statement separator",
     TT "(e;f;...;g;h)", " the semicolon can be used for evaluating a sequence of 
     expressions.  The value of the sequence is the value of its
     last expression, unless it is omitted, in which case the value
     is ", TO "null", ".",
     EXAMPLE {
	  "(3;4;5)",
      	  "(3;4;5;)"
	  }
     }

document {
     Key => symbol <-,
     Headline => "assignment with left side evaluated",
     TT "x <- y    ", " assigns the value of y to x, but x is evaluated, too.",
     PARA{},
     "If the value of x is a symbol, then the value of y is assigned as the
     value of that symbol.  If the value of x is a hash table, then the value 
     of y must be one, too, and the contents of y bodily replace the contents
     of x.  If the value of x is a list, then the value of y must be a list, 
     and the contents of y replace the contents of x.",
     PARA{},
     "Warning: if y is a class with instances, these instances
     will NOT become instances of x.  If instances of x are 
     created later, then they will not be compatible with the
     instances of y.  One should try to avoid using ", TO "<-", " in this
     case.",
     PARA{},
     "The value of the expression x <- y is x, with its new contents.",
     PARA{},
     SeeAlso => "="
     }

document {
     Key => "=",
     Headline => "assignment",
     PARA {
     	  "In this section we'll discuss simple assignment to variables, multiple assignment, assignment to parts of objects, assignment covered by various other methods, and 
     	  briefly touch on the possibility of custom installation of assignment methods.  See also the operator ", TO ":=", ", which handles assignment and declaration of
     	  local variables and assignment of methods to operators, as well as the operator ", TO "<-", ", which is an assignment operator that evaluates its left hand side and
     	  can have assignment methods installed for it by the user."
	  },
     SYNOPSIS (
	  Heading => "simple assignment",
	  Usage => "x = e",
	  Inputs => { "x" => Symbol, "e" => Thing},
	  Outputs => {Thing => { "the value of the expression is ", TT "e" }},
	  Consequences => {
	       { TT "e", " is assigned to ", TT "x", ", so future references to the value of ", TT "x", " yield ", TT "e" },
	       { "if ", TT "x", " is a global variable, then the global assignment hook for the class of ", TT "e", ", if any, is run (see ", TO "GlobalAssignHook", "),
		    and the global assignment hook for the symbol itself (see ", TO "globalAssignmentHooks", "), if any, is run." }
	       },
	  EXAMPLE lines ///
	       x
	       x = 4
	       x
	  ///,
	  PARA {
	       "Since the value of the entire expression is ", TT "e", ", and since the operator ", TO "=", " is right-associative (see ", TO "precedence of operators", "), 
	       ", TT "e", " can be easily assigned to more than one variable, as in the following example."
	       },
	  EXAMPLE lines ///
	       x = y = 44
	       x
	       y
	  ///	  
	  ),
     SYNOPSIS {
	  Heading => "multiple assignment",
	  Usage => "(x,y,z,...) = (c,d,e,...)",
	  Inputs => { 
	       { TT "(x,y,z,...)", " a ", TO2 {Sequence,"sequence"}, " of ", TO2 {Symbol,"symbols"} },
	       { TT "(c,d,e,...)", " a ", TO2 {Sequence,"sequence"}, " of ", TO2 {Thing,"things"} }
	       },
	  Outputs => {{ "the value of the expression is ", TT "(c,d,e,...)" }},
	  Consequences => {
	       { "the expressions c,d,e,... are assigned to the variables x,y,z,..., respectively, as above.  Global assignment hooks may be run, as above." },
	       { "If the left hand side has more elements than the right hand side, then the extra symbols on the left side are given the value ", TO "null", "." },
	       { "If the left hand side has fewer elements than the right hand side, then the last symbol on the left hand side is given
     		    as value a sequence containing the trailing elements of the right hand side." 
		    },
	       { "If the right hand side is not a sequence, then it is assigned to the first symbol on the left, and the remaining symbols are assigned the
		    value ", TO "null", "."
		    }
	       },
	  PARA "Multiple assignment makes it easy to switch the values of two variables, or to permute the values of several.",
	  EXAMPLE lines ///
	       x = 444
	       y = foo
	       (y,x) = (x,y)
	       x
	       y
	  ///,
	  PARA "Multiple assignment effectively means that functions can return multiple values usefully.",
	  EXAMPLE lines ///
	       f = i -> (i,i^2)
	       (x,y) = f 9
	       x
	       y
	  ///
	  },
     SYNOPSIS {
	  Heading => "assignment to an element of a mutable list",
	  Usage => "x#i = e",
	  Inputs => { "x" => MutableList, "i" => ZZ, "e" => Thing },
	  Outputs => {{ "the value of the expression is ", TT "e" }},
	  Consequences => {
	       { "the ", TT "i", "-th element of the list ", TT "x", " is replaced by ", TT "e", ", so that future references to the value of ", TT "x#i", "
		    yield ", TT "e" }
	       },
	  EXAMPLE lines ///
	       x = new MutableList from a .. e
	       peek x
	       x#3
	       x#3 = "foo"
	       x#3
	       peek x
	  ///
	  },
     SYNOPSIS {
	  Heading => "assignment to an element of a mutable hash table",
	  Usage => "x#i = e",
	  Inputs => { 
	       "x" => MutableList, 
	       "i" => Thing, 
	       "e" => Thing
	       },
	  Outputs => {{ "the value of the expression is ", TT "e" }},
	  Consequences => {
	       { TT "e", " is stored in the hash table ", TT "x", " under the key ", TT "i", ", so that future references to the value of ", TT "x#i", " yield ", TT "e" },
	       { "future references to the value of ", TT "x#?i", " will yield the value ", TO "true", ", indicating that something has been 
		    stored in ", TT "x", " under the key ", TT "i", ".  See ", TT "#?", "." }
	       },
	  EXAMPLE lines ///
	       x = new MutableHashTable from { "a" => 2, "b" => 3 }
	       peek x
	       x#?"foo"
	       x#"foo" = "bar"
	       x#?"foo"
	       x#"foo"
	       peek x
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing assignment methods for binary operators",
	  Usage => "X OP Y = (x,y,e) -> ...",
	  Inputs => {
	       "X" => Type,
	       "OP" => { "one of the binary operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Binary and operatorAttributes#op#Binary#?Flexible), s -> TO {s}),
		    " .  The operator SPACE, indicating adjacency, may be omitted from the usage above."
		    },
	       "Y" => Type,
	       { TT "(x,y,e) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for assignment to ", TT "X OP Y", ".  See the next subsection below
		    for using it"
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String * String = print;
	       "left" * "right" = "value"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using other assignment methods for binary operators",
	  Usage => "x OP y = e",
	  Inputs => {
	       "x" => { "an object of type ", TT "X" },
	       "OP" => { "one of the binary operators for which users may install methods, listed above.
		    The operator SPACE, indicating adjacency, may be omitted from the usage above."},
	       "y" => { "an object of type ", TT "Y" },
	       "e" => Thing
	       },
	  Outputs => {
	       { "the previously installed method for assignment to ", TT "X OP Y", " is called with arguments ", TT "(x,y,e)", ",
		    and its return value is returned"
		    }
	       },
	  PARA "The return value and the consequences depend on the code of the installed assignment method.
	  References to currently installed assignment methods are given below.",
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String * String = print;
	       "left" * "right" = "value"
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing assignment methods for unary prefix operators",
	  Usage => "OP X = (x,e) -> ...",
	  Inputs => {
	       "OP" => { "one of the unary prefix operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Prefix and operatorAttributes#op#Prefix#?Flexible), s -> TO {s})
		    },
	       "X" => Type,
	       { TT "(x,e) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for assignment to ", TT "OP X", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       - String = print;
	       - "foo" = "value"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using other assignment methods for unary prefix operators",
	  Usage => "OP x = e",
	  Inputs => {
	       "OP" => { "one of the unary prefix operators for which users may install methods, listed above." },
	       "x" => { "an object of type ", TT "X" },
	       "e" => Thing
	       },
	  Outputs => {
	       { "the previously installed method for assignment to ", TT "OP X", " is called with arguments ", TT "(x,e)", ",
		    and its return value is returned."
		    }
	       },
	  PARA "The return value and the consequences depend on the code of the installed assignment method.
	  References to currently installed assignment methods are given below.",
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       - String = print;
	       - "foo" = "value"
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing assignment methods for unary postfix operators",
	  Usage => "X OP = (x,e) -> ...",
	  Inputs => {
	       "OP" => { "one of the unary postfix operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Postfix and operatorAttributes#op#Postfix#?Flexible), s -> TO {s})
		    },
	       "X" => Type,
	       { TT "(x,e) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for assignment to ", TT "OP X", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String ~ = print;
	       "foo" ~ = "value"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using other assignment methods for unary postfix operators",
	  Usage => "x OP = e",
	  Inputs => {
	       "x" => { "an object of type ", TT "X" },
	       "OP" => { "one of the unary postfix operators for which users may install methods, listed above." },
	       "e" => Thing
	       },
	  Outputs => {
	       { "the previously installed method for assignment to ", TT "X OP", " is called with arguments ", TT "(x,e)", ",
		    and its return value is returned."
		    }
	       },
	  PARA "The return value and the consequences depend on the code of the installed assignment method.
	  References to currently installed assignment methods are given below.",
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String ~ = print;
	       "foo" ~ = "value"
	  ///
	  },
     SeeAlso => {":=", "<-", "globalAssignmentHooks" }
     }

document {
     Key => ":=",
     Headline => "assignment of method or new local variable",
     PARA {
	  "In this section we'll discuss simple local assignment to variables, multiple local assignment, and installation and use of method functions.
     	  See also the operator ", TO "=", ", which handles other forms of assignment, as well as the operator ", TO "<-", ", which is an
	  assignment operator that evaluates its left hand side and can have assignment methods installed for it by the user.",
	  },
     SYNOPSIS (
	  Heading => "simple local assignment",
	  Usage => "x := e",
	  Inputs => { "x" => Symbol, "e" => Thing},
	  Outputs => {Thing => { "the value of the expression is ", TT "e" }},
	  Consequences => {
	       { "a new local variable ", TT "x", " is created.  The scope of ", TT "x", " is the current function body, or if there is none, the current file" },
	       { TT "e", " is assigned to ", TT "x", ", so future references to the value of ", TT "x", " yield ", TT "e" },
	       { "a warning message is issued if a local variable with the same name has already been created" }
	       },
	  EXAMPLE lines ///
	       x
	       x := 4
	       x
	  ///,
	  PARA {
	       "In the next example, we see that the scope of the local variable ", TT "p", " is limited to the body of the function."
	       },
	  EXAMPLE lines ///
	       g = () -> ( p := 444; p )
	       g()
	       p
	  ///,
	  PARA {
	       "In this example, we see that a function returned by another function retains access to the values of local variables in its scope."
	       },
	  EXAMPLE lines ///
	       g = () -> ( p := 444; () -> p )
	       g()
	       oo ()
	  ///,
	  PARA {
	       "Functions returned by a function can also modify local variables within their scope, thereby communicating with each other."
	       },
	  EXAMPLE lines ///
	       g = () -> ( p := 444; (() -> p, i -> p = i))
	       (b,c) = g()
	       b()
	       c 555
	       b()
	  ///,
	  PARA {
	       "Since the value of the entire expression is ", TT "e", ", and since the operator ", TO "=", " is right-associative (see ", TO "precedence of operators", "), 
	       ", TT "e", " can be easily assigned to more than one variable, as in the following example."
	       },
	  EXAMPLE lines ///
	       a := b := 44
	       a
	       b
	  ///	  
	  ),
     SYNOPSIS {
	  Heading => "multiple local assignment",
	  Usage => "(x,y,z,...) := (c,d,e,...)",
	  Inputs => { 
	       { TT "(x,y,z,...)", " a ", TO2 {Sequence,"sequence"}, " of ", TO2 {Symbol,"symbols"} },
	       { TT "(c,d,e,...)", " a ", TO2 {Sequence,"sequence"}, " of ", TO2 {Thing,"things"} }
	       },
	  Outputs => {{ "the value of the expression is ", TT "(c,d,e,...)" }},
	  Consequences => {
	       { "new local variables ", TT "x", ", ", TT "y", ", ", TT "z", ", ... are created" },
	       { "the expressions c,d,e,... are assigned to the variables x,y,z,..., respectively, as above." },
	       { "If the left hand side has more elements than the right hand side, then the extra symbols on the left side are given the value ", TO "null", "." },
	       { "If the left hand side has fewer elements than the right hand side, then the last symbol on the left hand side is given
     		    as value a sequence containing the trailing elements of the right hand side." 
		    },
	       { "If the right hand side is not a sequence, then it is assigned to the first symbol on the left, and the remaining symbols are assigned the
		    value ", TO "null", "."
		    }
	       },
	  PARA "Multiple assignment effectively means that functions can return multiple values usefully.",
	  EXAMPLE lines ///
	       f = i -> (i,i^2)
	       (r,s) := f 9
	       r
	       s
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing methods for binary operators",
	  Usage => "X OP Y := (x,y) -> ...",
	  Inputs => {
	       "X" => Type,
	       "OP" => { "one of the binary operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Binary and operatorAttributes#op#Binary#?Flexible), s -> TO {s}),
		    " .  The operator SPACE, indicating adjacency, may be omitted from the usage above."
		    },
	       "Y" => Type,
	       { TT "(x,y) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for ", TT "X OP Y", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String * String := print;
	       "left" * "right"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using methods for binary operators",
	  Usage => "x OP y",
	  Inputs => {
	       "x" => { "an object of type ", TT "X" },
	       "OP" => { "one of the binary operators for which users may install methods, listed above.
		    The operator SPACE, indicating adjacency, may be omitted from the usage above."},
	       "y" => { "an object of type ", TT "Y" }
	       },
	  Outputs => {
	       { "the previously installed method for ", TT "X OP Y", " is called with arguments ", TT "(x,y)", ", and its return value is returned" }
	       },
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String * String := print;
	       "left" * "right"
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing methods for unary prefix operators",
	  Usage => "OP X = (x) -> ...",
	  Inputs => {
	       "OP" => { "one of the unary prefix operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Prefix and operatorAttributes#op#Prefix#?Flexible), s -> TO {s})
		    },
	       "X" => Type,
	       { TT "(x) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for ", TT "OP X", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       - String := print;
	       - "foo"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using methods for unary prefix operators",
	  Usage => "OP x",
	  Inputs => {
	       "OP" => { "one of the unary prefix operators for which users may install methods, listed above." },
	       "x" => { "an object of type ", TT "X" }
	       },
	  Outputs => {
	       { "the previously installed method for ", TT "OP X", " is called with argument ", TT "x", ", and its return value is returned." }
	       },
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       - String := print;
	       - "foo"
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing methods for unary postfix operators",
	  Usage => "X OP = (x) -> ...",
	  Inputs => {
	       "OP" => { "one of the unary postfix operators for which users may install methods, namely: ", 
		    between_" " apply(sort select(keys operatorAttributes, op -> operatorAttributes#op#?Postfix and operatorAttributes#op#Postfix#?Flexible), s -> TO {s})
		    },
	       "X" => Type,
	       { TT "(x) -> ...", ", ", ofClass Function }
	       },
	  Outputs => {{ "the value of the expression is the same as the function on the right hand side" }},
	  Consequences => {{ "the function on the right hand side is installed as the method for ", TT "OP X", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String ~ := print;
	       "foo" ~
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using methods for unary postfix operators",
	  Usage => "x OP",
	  Inputs => {
	       "x" => { "an object of type ", TT "X" },
	       "OP" => { "one of the unary postfix operators for which users may install methods, listed above." }
	       },
	  Outputs => {
	       { "the previously installed method for ", TT "X OP", " is called with argument= ", TT "x", ",
		    and its return value is returned."
		    }
	       },
	  "The second line of the following example illustrates the syntax above.",
	  EXAMPLE lines ///
	       String ~ := print;
	       "foo" ~
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing unary methods for method functions",
	  Usage => "f X := (x) -> ...",
	  Inputs => {
	       "f" => { "a previously defined method function.  A method function may be created with the function ", TO "method", "." },
	       "X" => Type,
	       { TT "(x) -> ...", ", ", ofClass Function }
	       },
	  Consequences => {{ "the function on the right hand side is installed as the method for assignment to ", TT "f X", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above, using ", TO "source", ", which happens to be a method function.",
	  EXAMPLE lines ///
	       source String := print;
	       source "foo"
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2."
	  },
     SYNOPSIS {
	  Heading => "using unary methods for method functions",
	  Usage => "f x",
	  Inputs => {
	       "f" => { "a method function" },
	       "x" => { "an object of type ", TT "X" }
	       },
	  Outputs => {
	       { "the previously installed method for ", TT "f X", " is called with argument ", TT "x", ", and its return value is returned" }
	       },
	  "The second line of the following example illustrates the syntax above, using ", TO "source", ", which happens to be a method function.",
	  EXAMPLE lines ///
	       source String := print;
	       source "foo"
	  ///
	  },
     SYNOPSIS {
	  Heading => "installing binary methods for method functions",
	  Usage => "f(X,Y) := (x,y) -> ...",
	  Inputs => {
	       "f" => { "a previously defined method function.  A method function may be created with the function ", TO "method", "." },
	       "X" => Type,
	       "Y" => Type,
	       { TT "(x,y) -> ...", ", ", ofClass Function }
	       },
	  Consequences => {{ "the function on the right hand side is installed as the method for assignment to ", TT "f(X,Y)", ".  See the next subsection below
		    for using it."
		    }},
	  "The first line of the following example illustrates the syntax above, using ", TO "source", ", which happens to be a method function.",
	  EXAMPLE lines ///
	       source(String,String) := print;
	       source("foo","bar")
	  ///,
	  PARA "Warning: the installation of new methods may supplant old ones, changing the behavior of Macaulay 2.",
	  PARA "The same syntax works for 3 or 4 arguments."
	  },
     SYNOPSIS {
	  Heading => "using binary methods for method functions",
	  Usage => "f(x,y)",
	  Inputs => {
	       "f" => { "a method function" },
	       "x" => { "an object of type ", TT "X" },
	       "y" => { "an object of type ", TT "Y" }
	       },
	  Outputs => {
	       { "the previously installed method for ", TT "f(X,Y)", " is called with arguments ", TT "(x,y)", ", and the return value is returned" }
	       },
	  "The second line of the following example illustrates the syntax above, using ", TO "source", ", which happens to be a method function.",
	  EXAMPLE lines ///
	       source(String,String) := print;
	       source("foo","bar")
	  ///,
	  PARA "The same syntax works for 3 or 4 arguments."
	  },
     -- put synopses for "new" methods here?  or refer to another page?
     SeeAlso => {"=", "<-" }
     }
document {
     Key => abs,
     Headline => "absolute value function", 
	Usage => "abs x",
	Inputs => {
		"x" => "a number"
		},
	Outputs => {
		{"the absolute value of ", TT "x"}
		},
     TT "abs x", " computes the absolute value of ", TT "x", ".",
	EXAMPLE {
		"abs(-pi)"
		},
     }
document {
     Key => exp, Headline => "exponential function",
     Usage => "exp x",
     Inputs => { "x" => RR },
     Outputs => { { "the exponential of ", TT "x" } } }
document {
     Key => log, Headline => "logarithm function",
     Usage => "log x",
     Inputs => { "x" => RR },
     Outputs => { { "the logarithm of ", TT "x"} } }
document {
     Key => sqrt, Headline => "square root function",
     Usage => "sqrt x",
     Inputs => { "x" => RR },
     Outputs => { { "the square root of ", TT "x"} } }
document {
     Key => run,
     Headline => "run an external command", 
     Usage => "run s",
     Inputs => { "s" => String => {"a command understandable by your operating system"} },
     Outputs => { "the exit status of the command (a small integer which is normally zero)" } }
document {
     Key => wait,
     Headline => "wait for child process", 
     TT "wait i", " waits for the completion of child process with process id
     ", TT "i", ".",
     BR{},
     TT "wait f", " waits for the input file to have some input ready.",
     BR{},
     TT "wait s", " waits for at least one of the files in the list 
     ", TT "s", " of input files to be ready, and return the list of positions
     corresponding to ready files.",
     BR{},
     TT "wait v", " checks whether the processes whose id's are in the list
     ", TT "v", " of integers have terminated, and returns a list containing the 
     status codes for those processes that have terminated.  A value of -1 in the 
     list indicates an error for that process id, and a value of -2 in the list
     indicates that the process is still running."
     }

undocumented {
	  (value, Minus),
	  (value, NonAssociativeProduct),
	  (value, Equation),
	  (value, Nothing),
	  (value, BinaryOperation),
	  (value, SparseVectorExpression),
	  (value, Divide),
	  (value, FunctionApplication),
	  (value, Power),
	  (value, ZeroExpression),
	  (value, Subscript),
	  (value, SparseMonomialVectorExpression),
	  (value, OneExpression),
	  (value, Superscript),
	  (value, Sum),
	  (value, RingElement),
	  (value, MatrixExpression),
	  (value, IndexedVariable),
	  (value, Table),
	  (value, Sequence),
	  (value, Product),
	  (value, Adjacent),
	  (value, Holder),
	  (value, ZZ),
	  (value, QQ),
	  (value, RRR),
	  (value, CCC),
	  (value, RR),
	  (value, CC)
	  }

document {
     Key => value,
     Headline => "evaluate"
     }

document {
     Key => (value,Symbol),
     Headline => "retrieve the value of a symbol",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value of ", TT "s" } },
     EXAMPLE {
	  "x = s",
	  "s = 11111111111",
      	  "x",
      	  "value x"
	  }
     }

document {
     Key => (value,String),
     Headline => "evaluate a string",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value obtained by evaluating the code in ", TT "s" } },
     "The contents of ", TT "s", " are treated as code in the
     Macaulay 2 language, parsed it in its own scope (the same way a file is)
     and evaluated.  The string may contain multiple lines.",
     {
	  EXAMPLE {
	       ///value "2 + 2"///,
      	       ///value "a := 33
a+a"///,
     	       ///a///
	       },
     	  "Since the local assignment to ", TT "a", " above occurred in a new scope,
     	  the value of the global variable ", TT "a", " is unaffected."
	  }
     }

document {
     Key => "global",
     Headline => "get a global symbol", 
     TT "global s", " provides the global symbol s, even if s currently has 
     a value.",
     EXAMPLE {
	  "num",
      	  "num = 5",
      	  "num",
      	  "global num",
	  },
     SeeAlso => {"local", "symbol" }
     }

document {
     Key => erase,
     Headline => "remove a global symbol",
     TT "erase s", " removes the global symbol ", TT "s", " from the
     symbol table."
     }

document {
     Key => "local",
     Headline => "get a local symbol",
     TT "local s", " provides the local symbol ", TT "s", ", creating
     a new symbol if necessary.  The initial value of a local
     symbol is ", TO "null", ".",
     EXAMPLE {
	  "f = () -> ZZ[local t]",
      	  "f()",
      	  "t",
	  },
     SeeAlso => {"global", "symbol"
     }
     }

document {
     Key => "symbol",
     Headline => "get a symbol",
     TT "symbol s", " provides the symbol ", TT "s", ", even if ", TT "s", " currently has a value.",
     PARA{},
     {EXAMPLE {
	       "num",
	       "num = 5",
	       "num",
	       "symbol num",
	       },
	  PARA{},
	  "If ", TT "s", " is an operator, then the corresponding symbol is provided.  This
	  symbol is used by the interpreter in constructing keys for methods
	  associated to the symbol.",
	  EXAMPLE "symbol +"
	  },
     SeeAlso => {"local", "global", "value" }
     }

document {
     Key => gcd,
     Headline => "greatest common divisor",
     Usage => "gcd(x,y,...)",
     Inputs => { "x" => ZZ },
     Outputs => { ZZ => { "the greatest commond divisor of the arguments" } },
     EXAMPLE "gcd(2,4,8)",
     SeeAlso => gcdCoefficients
     }

document {
     Key => concatenate,
     Headline => "join strings",
     TT "concatenate(s,t,...,u)", " yields the concatenation of the strings s,t,...,u.",
     PARA{},
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces.",
     EXAMPLE "concatenate {\"a\",(\"s\",3,\"d\"),\"f\"}",
     SeeAlso => { "String"} 
     }

document {
     Key => error,
     Headline => "deliver error message",
	Usage => "error s",
	Inputs => {
		"s" => String => "a string or a sequence which can be converted to strings"
		},
	Consequences => {
		"an error message is displayed"
		},
     TT "error s", " causes an error message s to be displayed.",
     PARA{},
     "The error message ", TT "s", " (which should be a string or a sequence of
     things which can be converted to strings and concatenated) is printed.
     Execution of the code is interrupted, and control is returned
     to top level. Here is an explicit example showing how this command is used:",
	PRE///exampleCode = p -> (
		if not isPrime p	
	    	then error "expected a prime integer";
	    	if p == 2
	    	then error "expected an odd prime";
	    	lift((p+1)/2, ZZ)
	    	);///,
	"Eventually we will have a means of ensuring that the line 
     number printed out with the error message will have more 
     significance, but currently it is the location in the code of 
     the error expression itself."
     }

document {
     Key => getenv,
     Headline => "get value of environment variable",
     TT "getenv s", " yields the value associated with the string s in the 
     environment.",
     PARA{},
     EXAMPLE {
	  ///getenv "HOME"///
	  }
     }

document {
     Key => "currentDirectory",
     Headline => "current working directory",
     TT "currentDirectory", " is the name of the current directory,
     together with an extra slash (or appropriate path separator)."
     }

document {
     Key => symbol ~,
     Headline => "a unary postfix operator",
     }

document {
     Key => copy,
     Headline => "copy an object",
     TT "copy x", " yields a copy of x.",
     PARA{},
     "If x is an hash table, array, list or sequence, then the elements are 
     placed into a new copy. If x is a hash table, the copy is mutable if 
     and only if x is.",
     PARA{},
     "It is not advisable to copy such things as modules and rings,
     for the operations which have already been installed for them will return
     values in the original object, rather than in the copy.",
     PARA{},
     SeeAlso => { "newClass" }
     }

document {
     Key => mergePairs,
     Headline => "merge sorted lists of pairs",
     TT "mergePairs(x,y,f)", " merges sorted lists of pairs.",
     PARA{},
     "It merges ", TT "x", " and ", TT "y", ", which should be lists 
     of pairs ", TT "(k,v)", " arranged in increasing order according
     to the key ", TT "k", ".  The result will be a list of pairs, also
     arranged in increasing order, each of which is either from ", TT "x", "
     or from ", TT "y", ", or in the case where a key ", TT "k", " occurs in
     both, with say ", TT "(k,v)", " in ", TT "x", " and ", TT "(k,w)", "
     in ", TT "y", ", then the result will contain the pair ", TT "(k,f(v,w))", ".
     Thus the function ", TT "f", " is used for combining the values when the keys
     collide.  The class of the result is taken to be the minimal common
     ancestor of the class of ", TT "x", " and the class of ", TT "y", ".",
     PARA{},
     SeeAlso => { "merge" }
     }

document {
     Key => merge,
     Headline => "merge hash tables",
     Usage => "z = merge(x,y,g)",
     Inputs => {
	  "x" => {"a hash table"},
	  "y" => {"a hash table"},
	  "g" => {"a function of two variables to be used to combine a value of ", TT "x", " with a value of ", TT "y", " when the 
	       corresponding keys coincide"
	       } },
     Outputs => {
	  "z" => { "a new hash table whose keys are the keys occurring in ", TT "x", "
	       or in ", TT "y", "; the same values are used, except that if if a key ", TT "k", " occurs in both arguments, then
	       ", TT "g(x#k,y#k)", " is used instead." } },
     "If ", TT "x", " and ", TT "y", " have the same class and parent, then so will ", TT "z", ".",
     PARA "This function is useful for multiplying monomials or adding polynomials.",
     SeeAlso => {"combine"}
     }

document {
     Key => combine,
     Headline => "combine hash tables",
     Usage => "z = combine(x,y,f,g,h)",
     Inputs => {
	  "x" => "a hash table",
	  "y" => {"a hash table of the same class as ", TT "x"},
	  "f" => { "a function of two variables to be used for combining a key
	       of ", TT "x", " with a key of ", TT "y", " to make a new key
	       for ", TT "z", "." },
	  "g" => { "a function of two variables to be used for combining a value
	       of ", TT "x", " with a value of ", TT "y", " to make a new value
	       for ", TT "z", "." },
	  "h" => { "a function of two variables to be used for combining two
	       values returned by ", TT "g", " when the corresponding keys
	       returned by ", TT "f", " turn out to be equal.  Its first argument
	       will be the value accumulated so far, and its second argument will
	       be a value just provided by ", TT "g", "."
	       }
	  },
     Outputs => {
	  "z" => {
	       "a new hash table, of the same class as ", TT "x", " and ", TT "y", ",
	       containing the pair ", TT "f(p,q) => g(b,c)", "
	       whenever ", TT "x", " contains the pair ", TT "p => b", "
	       and ", TT "y", " contains the pair ", TT "q => c", ",
	       except that ", TT "h", " is used to combine values when two keys
	       coincide."
	       }
	  },
     "The function ", TT "f", " is applied to every pair ", TT "(p,q)", "
     where ", TT "p", " is a key of ", TT "x", " and ", TT "q", " is a
     key of ", TT "y", ".  The number of times ", TT "f", " is evaluated is thus 
     the product of the number of keys in ", TT "x", " and the number of 
     keys in ", TT "y", ".",
     PARA{},
     "The function ", TT "h", " should be an associative function, for otherwise 
     the result may depend on internal details about the implementation of hash 
     tables that affect the order in which entries are encountered.  If ", TT "f", ",
     ", TT "g", ", and ", TT "h", " are commutative functions as well, then the 
     result ", TT "z", " is a commutative function of ", TT "x", " and ", TT "y", ".",
     PARA{},
     "The result is mutable if and only if ", TT "x", " or ", TT "y", " is.",
     PARA{},
     "This function can be used for multiplying polynomials, where it
     can be used in code something like this:", 
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)"
     }

document {
     Key => ancestor,
     Headline => "whether one type is an ancestor of another",
     Usage => "ancestor(x,y)",
     Inputs => { "x" => Type, "y" => Type },
     Outputs => { {"whether ", TT "y", " is an ancestor of ", TT "x"} },
     "The ancestors of ", TT "x", " are ", TT "x", ", ", TT "parent x", ", ", TT "parent parent x", ", and so on.",
     SeeAlso => "classes and types"
     }

document {
     Key => unique,
     Headline => "eliminate duplicates from a list",
     TT "unique v", " yields the elements of the list ", TT "v", ", without duplicates.",
     PARA{},
     EXAMPLE {
	  "unique {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}"
	  },
     "The order of elements is maintained.  For something that might be slightly 
     faster, but doesn't maintain the order of the elements, and may different
     answers, try making a set and then getting its elements.",
     EXAMPLE {
	  "toList set {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}"
	  },
     SeeAlso => {"sort"}
     }

document {
     Key => Ring,
     Headline => "the class of all rings",
     SeeAlso => "rings",
     "Common ways to make a ring:",
     UL {
	  TO (symbol /, Ring, Ideal),
	  TO (symbol SPACE, Ring, Array),
	  TO "GF",
	  },
     "Common functions for accessing the variables or elements in a ring:",
     UL {
	  TO (use, Ring),
	  TO (generators, Ring),
	  TO (numgens, Ring),
	  TO (symbol _, Ring, ZZ),
	  TO (symbol _, ZZ, Ring)
	  },
     "Common ways to get information about a ring:",
     UL {
	  TO (char, Ring),
	  TO (coefficientRing, Ring),
	  TO (dim, Ring),
	  },
     "Common ways to use a ring:",
     UL {
	  TO (symbol ^, Ring, ZZ),
	  TO (symbol ^, Ring, List),
	  TO (vars, Ring),
	  },
     }

document {
     Key => (symbol _, ZZ, Ring),
     TT "1_R", " provides the unit element of the ring ", TT "R", ".",
     BR{},
     TT "0_R", " provides the zero element of the ring ", TT "R", ".",
     BR{},
     TT "n_R", " promotes the integer ", TT "n", " to the ring ", TT "R", ".",
     }


document {
     Key => symbol =>,
     Headline => "produce an Option",
     TT "x => y", " a binary operator which produces a type of list called
     an ", TO "Option", "."
     }

document {
     Key => (symbol SPACE, RingElement, Array),
     Headline => "substitution of variables",
     Usage => "f[a,b,c]",
     Inputs => { "f", Nothing => { TT "[a,b,c]", ", an array of ring elements" } },
     Outputs => {
	  "r" => { "the result of replacing the variables in ", TT "f", " by the ring elements provided in brackets." } } ,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "f = x^3 + 99*y;",
	  "f[1000,3]"
	  }
     }
     
document {
     Key => (symbol _, Symbol, Ring),
     Headline => "generator of a ring with a given name",
     Usage => "x_R",
     Inputs => { "x", "R" },
     Outputs => { { "the generator of the ring ", TT "R", " whose name is ", TT "x" } }
     }
     
document {
     Key => (symbol _, RingElement, Ring),
     Headline => "generator of a ring with a given name",
     Usage => "x_R",
     Inputs => { "x", "R" },
     Outputs => { { "the generator of the ring ", TT "R", " whose name is the same as that of ", TT "x" } }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
