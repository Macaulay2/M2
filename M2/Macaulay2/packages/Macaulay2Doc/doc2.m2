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
     SeeAlso => {"Time", "time", "cpuTime", "elapsedTiming", "elapsedTime"}
     }

document {
     Key => "time",
     Headline => "time a computation",
	Usage => "time e",
     TT "time e", " evaluates ", TT "e", ", prints the amount of cpu time
     used, and returns the value of ", TT "e", ".",
     EXAMPLE "time 3^30",
     SeeAlso => {"timing", "cpuTime", "elapsedTiming", "elapsedTime"}
     }

document {
     Key => "elapsedTiming",
     Headline => "time a computation using time elapsed",
     TT "elapsedTiming e", " evaluates ", TT "e", " and returns a list of type ", TO "Time", "
     of the form ", TT "{t,v}", ", where ", TT "t", " is the number of seconds
     of time elapsed, and ", TT "v", " is the value of the the expression.",
     PARA{},
     "The default method for printing such timing results is to display the
     timing separately in a comment below the computed value.",
     EXAMPLE {
	  "elapsedTiming sleep 1",
      	  "peek oo",
	  },
     SeeAlso => {"Time", "elapsedTime", "cpuTime", "timing", "time"}
     }

document {
     Key => "elapsedTime",
     Headline => "time a computation using time elapsed",
	Usage => "elapsedTime e",
     TT "elapsedTime e", " evaluates ", TT "e", ", prints the amount of time
     elapsed, and returns the value of ", TT "e", ".",
     EXAMPLE "elapsedTime sleep 1",
     SeeAlso => {"elapsedTiming", "cpuTime"}
     }

document {
     Key => Time,
     Headline => "the class of all timing results",
     TT "Time", " is the class of all timing results.  Each timing result
     is ", ofClass BasicList, " of the form ", TT "{t,v}", ", where ", TT "t", " 
     is the number of seconds of cpu time used, and ", TT "v", " is the value 
     of the the expression.",
     SeeAlso => {"timing", "time", "cpuTime", "elapsedTiming", "elapsedTime"}
     }

document {
     Key => null,
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
     Usage => "return x",
     Inputs => {
	  "x" => {"If ", TT "x", " is omitted, then ", TO "null", " is used."} 
	  },
     Consequences => {
	  {"Returns ", TT "x", " as the value of the function currently being evaluated."},
	  {"Alternatively, as a debugger command, returns ", TT "x", " as the value of the
	       current expression and stops execution at the next possible point, entering
	       the debugger again."}
	  },
     EXAMPLE {
	  "f = x -> (
     if x == 3 then return;
     if x > 3 then return x^2;
     5);",
	  "f 2",
	  "f 3",
	  "f 4"
	  },
     PARA {
	  "Here is an example of the use of ", TO "return", " as a debugger command."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     code g
     g 2
     code f
     return 1/11
     continue
     ///,
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
     Usage => "stdio",
     Outputs => {
	  { "the standard input output file" }
	  },
     PARA{
	  "Use this file to get input from the terminal, or to display information
	  on the user's screen.  This is the file always used by ", TO "print", "
	  and ", TO "<<", " if it is not explicitly given a file."
	  },
     EXAMPLE lines ///
     << "hi" << endl
     stdio << "hi" << endl
     ///,
     SeeAlso => {stderr, (symbol <<, File, Thing)}
     }

document {
     Key => stderr,
     Headline => "the standard error output file",
     Usage => "stderr",
     Outputs => {
	  {"the standard error output file"}
	  },
     PARA{
	  "Macaulay2 uses this file to display error messages on the user's screen.  In unix, it
	  corresponds to file descriptor 2.  The user may use this file to display warning messages."
	  },
     SeeAlso => {stdio}
     }

document {
     Key => {(openListener, String),openListener},
     Headline => "open a port for listening",
     Usage => "f = openListener s",
     Inputs => {"s" => {"of the form ", TT format "$interface:port", ".  Both parts are optional.  If the port is omitted, the colon is optional." }},
     Outputs => {
	  "f" => {
	       "an open listener on the specified interface of the local host at the specified service port.
	       If the port is omitted, it is taken to be port 2500.  If the interface is omitted, the listener
	       accepts connections on all interfaces."
	       }
	  },
     PARA{
	  "Use ", TT "openInOut f", " to accept an incoming connection on the listener,
	  returning a new input output file that serves as the connection.  The function
	  ", TO "isReady", " can be used to determine whether an incoming connection has
	  arrived, without blocking."
	  },
     SeeAlso => {openInOut, isReady}
     }
document {
     Key => {(openIn, String),openIn},
     Headline => "open an input file",
     Usage => "openIn fn",
     Inputs => { "fn" },
     Outputs => {
	  { "an open input file whose filename is ", TT "fn", ".
	       Filenames starting with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", "." }
	  },
     EXAMPLE lines ///
     "test-file" << "hi there" << close;
     g = openIn "test-file"
     fileLength g
     atEndOfFile g
     read g
     atEndOfFile g
     close g
     removeFile "test-file"
     ///,
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the user's home directory."},
     SeeAlso => {openOut, openOutAppend, openInOut, fileLength, read, close, atEndOfFile, (symbol <<, File, Thing)}
     }
document {
     Key => {(openOut, String),openOut},
     Headline => "open an output file",
     Usage => "openOut fn",
     Inputs => { "fn" },
     Outputs => {
	  { "an open output file whose filename is ", TT "fn", ".
	       Filenames starting with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", "." }
	  },
     EXAMPLE lines ///
     g = openOut "test-file"
     g << "hi there"
     g << close
     get "test-file"
     removeFile "test-file"
     ///,
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the user's home directory."},
     SeeAlso => {openIn, openInOut, openOutAppend, get, removeFile, close, (symbol <<, File, Thing)}
     }
document {
     Key => {(openOutAppend, String),openOutAppend},
     Headline => "open an output file for appending",
     Usage => "openOutAppend fn",
     Inputs => { "fn" },
     Outputs => {
	  { "an open output file whose filename is ", TT "fn" }
	  },
     EXAMPLE lines ///
     g = openOut "test-file"
     g << "hi there" << endl << close
     h = openOutAppend "test-file"
     h << "ho there" << endl << close
     get "test-file"
     removeFile "test-file"
     ///,
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the user's home directory."},
     SeeAlso => {openIn, openInOut, openOut, File, get, removeFile, (symbol <<, File, Thing)}
     }
document {
     Key => {openInOut,(openInOut, String),(openInOut, File)},
     Headline => "open an input output file",
     Usage => "openInOut f",
     Inputs => {
	  "f" => {ofClass{String,File}}
	  },
     Outputs => {
	  {"an open input output file"}
	  },
     PARA {
	  "There are various options for the argument ", TT "f", "."
	  },
     UL {
	  LI {
	       "a string not starting with ", TT "!", " or ", TT "$", ": the string is
	       taken as the name of an input output file to open.  For example, in Unix,
	       it might be a named pipe.  A filename starting with ", TT "~/", " will have 
	       the tilde replaced by the user's home directory."
	       },
	  LI {
	       "a string of the form ", TT format "!cmd", ": the command
	       ", TT "cmd", " will be started, and two pipes will be opened, connected to its standard input and
	       standard output file descriptors.
	       Warning: pipes hold only 4096 bites, so if you write more than that to the resulting input output
	       file (as input for the command) without reading any data, you may block while the
	       command is blocked waiting to write more output; in this case, Macaulay2 will appear to hang."
	       },
	  LI {
	       "a string of the form ", TT format "$hostname:service", ": a connection will be
	       made to the specified service at the specified host.  If the service
	       port is omitted, along with the colon, then port 2500 is used.  If the hostname is omitted,
	       an incoming connection will be waited for."
	       },
	  LI {
	       "a listener created previously by ", TO "openListener", ": a new connection will be created.  To avoid
	       blocking the Macaulay2 process while waiting for the incoming connection, use ", TO "isReady", "."
	       }
	  },
     PARA{
	  "In order to open a socket successfully, there must be a process
	  accepting connections for the desired service on the specified host."
	  },
     PARA{
	  "The various forms listed above can be used also with all other input
	  output operations that open files, such as ", TO "openIn", ",
	  ", TO "openOut", ", ", TO "get", ", and ", TO "<<", ", with data transfer 
	  possible only in the direction specified.  A possibly confusing asymmetry is that 
	  with ", TT ///openIn "!foo"///, " or with ", TT ///get "!foo"///, " the standard input of the command
	  ", TT "foo", " is closed, but with ", TT ///openOut "!foo"///, " the
	  standard output of the command ", TT "foo", " is connected to the
	  standard output of the parent Macaulay2 process."
	  },
     SeeAlso => {openIn, openOut, openListener}
     }

document {
     Key => protect,
     Headline => "protect a symbol",
     Usage => "protect s",
     Inputs => { "s" => Symbol },
     Outputs => { "s" => Symbol },
     Consequences => {
	  { "the symbol ", TT "s", " is protected from having its value changed" }
	  },
     PARA {
	  "There is no function for unprotecting symbols.",
	  },
     EXAMPLE lines ///
     mutable symbol s
     s = 5
     s
     protect symbol s
     mutable symbol s
     try s = 7
     s
     ///,
     SeeAlso => {mutable, "try"}
     }

document {
     Key => {(isInputFile, File),isInputFile},
     Headline => "whether a file is open for input",
     Usage => "isInputFile f",
     Inputs => { "f" },
     Outputs => {
	  Boolean => { " whether ", TT "f", " is an open input file" }
	  },
     EXAMPLE lines ///
     "test-file" << "hi there" << close
     isInputFile oo
     f = openIn "test-file"
     isInputFile f
     isOpen f
     get f
     isInputFile f
     isOpen f
     removeFile "test-file"
     ///,
     SeeAlso => {openIn,openInOut,get,isOpen,close}
     }

document {
     Key => {(isOutputFile, File),isOutputFile},
     Headline => "whether a file is open for output",
     Usage => "isOutputFile f",
     Inputs => { "f" },
     Outputs => {
	  Boolean => { " whether ", TT "f", " is an open output file" }
	  },
     EXAMPLE lines ///
     f = "test-file" << "hi there"
     isOutputFile f
     close f
     isOutputFile f
     get "test-file"
     removeFile "test-file"
     ///,
     SeeAlso => {openOut, openInOut, close, get, removeFile}
     }

document {
     Key => {isOpen,(isOpen, Database),(isOpen, File)},
     Headline => "whether a file or database is open",
     Usage => "isOpen f",
     Inputs => { "f" => {ofClass{File,Database}}},
     Outputs => {
	  Boolean => { " whether ", TT "f", " is an open file or open database" }
	  },
     PARA{
     	  "An open file is either an input file, an output file, an input output file, or a listener.",
	  },
     EXAMPLE lines ///
     f = "test-file" << "hi there"
     isOpen f
     close f
     isOpen f
     get "test-file"
     removeFile "test-file"
     ///,
     SeeAlso => {openIn, openInOut, openListener, close, get, removeFile}
     }

document {
     Key => {(isListener, File),isListener},
     Headline => "whether a file is open for listening",
     Usage => "isListener f",
     Inputs => { "f" },
     Outputs => {
	  Boolean => { " whether ", TT "f", " is an open listener" }
	  },
     SeeAlso => {openListener}
     }

---------------------
---- Operators ------
---------------------
document {
     Key => symbol <<, 
     Headline => "a binary operator (file output, ...)",
     }
document {
     Key => {"left shift", (symbol <<, ZZ, ZZ), (symbol <<, RR, ZZ), (symbol <<, CC, ZZ)},
     Usage => "x << j",
     Inputs => { "x", "j" },
     Outputs => {{ "the number obtained from ", TT "x", " by shifting its binary representation leftward ", TT "j", " places" }},
     EXAMPLE {"256 << 5","256. << 555"},
     SeeAlso => {"right shift"}
     }

document {
     Key => {"right shift", (symbol >>, ZZ, ZZ), (symbol >>, RR, ZZ), (symbol >>, CC, ZZ)},
     Usage => "x >> j",
     Inputs => { "x", "j" },
     Outputs => {{ "the integer obtained from ", TT "x", " by shifting its binary representation rightward ", TT "j", " places" }},
     EXAMPLE {"256 >> 5","256. >> 555"},
     SeeAlso => {"left shift"}
     }

document {
     Key => { (symbol <<, File, Thing),(symbol <<, String, Thing), (symbol <<, File, Manipulator),(symbol <<, List, Thing),
	  (symbol <<, Nothing, Thing),(symbol <<, Nothing, Manipulator), (symbol <<, Thing),
	  (symbol <<, File, Symbol),(symbol <<, File, Net),(symbol <<,File,String) },
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
	  If ", TT "f", " is a string, then it is interpreted as a filename and an output file is opened, used, and returned,
	  unless a single open file with the same name already exists, in which case it is used and returned.
	  Filenames starting with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", ".
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
     Key => symbol :,
     Headline => "a binary operator, uses include repetition; ideal quotients",
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
     (symbol >, Thing, Thing),
     (symbol <, Thing, Thing),
     (symbol <=, Thing, Thing),
     (symbol >=, Thing, Thing)
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
     (symbol ?, InfiniteNumber, InfiniteNumber),
     (symbol ?, DocumentTag, DocumentTag),
     (symbol ?, Thing, InfiniteNumber),
     (symbol ?, TOH, TOH),
     (symbol ?, InfiniteNumber, Thing),
     (symbol ?, ZZ, MonoidElement),
     (symbol ?, MonoidElement, ZZ),
     (symbol ?, RingElement, ZZ),
     (symbol ?, ZZ, RingElement)
     }

document {
     Key => {symbol ?,
     	  (symbol ?, Symbol, IndexedVariable),
     	  (symbol ?, IndexedVariable, IndexedVariable),
     	  (symbol ?, List, List),
	  (symbol ?, VirtualTally, VirtualTally)
	  },
     Headline => "comparison operator",
     Usage => "x ? y", 
     Inputs => { "x", "y" },
     Outputs => {{
	  "One of the symbols ", TT "symbol <", ", ", TT "symbol >", ", ", TT "symbol ==", ", or ", TT "incomparable", ",
	  depending (respectively) on whether ", TT "x < y", ", ", TT "x > y", ", ", TT "x == y", ", or ", TT "x", " and ", TT "y", " are not comparable."	  
	  }},
     "Many types of objects may be compared.  Numbers are handled as one would expect,
     and strings, lists and sequences are generally compared lexicographically.",
     EXAMPLE lines ///
     	  3 ? 4
	  "book" ? "boolean"
	  3 ? 3.
	  {1,2,3} ? {4,5}
     ///,
     PARA{},
     "Polynomials from the same ring may also be compared.  The order depends on the 
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
     Key => {getc,(getc, File)},
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
     TT "incomparable", " a symbol that may be returned by ", TO "?", "
     when the two things being compared are incomparable."
     }


document {
     Key => {";"},
     Headline => "expression separator",
     Usage => "(e;f;...;g;h)",
     PARA {
     	  "The semicolon can be used for evaluating a sequence of 
     	  expressions.  The value of the sequence is the value of its
     	  last expression, unless it is omitted, in which case the value
     	  is ", TO "null", "."
	  },
     EXAMPLE {
	  "(3;4;5)",
      	  "(3;4;5;)"
	  },
     PARA {
	  "Putting expressions on separate lines is not a substitute for 
	  the semicolons."
	  }
     }

document {
     Key => {abs,(abs, RR),(abs, CC),(abs, ZZ),(abs, QQ)},
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
		"abs(-pi)",
		"abs(1+ii)"
		},
     }

document {
     Key => (exp,RingElement),
     Usage => "exp x",
     Inputs => { "x" },
     Outputs => { { "the exponential of ", TT "x", ", provided ", TT "x", " is nilpotent,
	       and the denominators required have reciprocals in the ring of ", TT "x", "." } } ,
     EXAMPLE lines ///
     R = ZZ/11[x]/x^9
     exp x
     ///
     }

document {
     Key => {exp,(exp,RR),(exp,CC),(exp,ZZ),(exp,QQ),(exp,Constant)},
     Headline => "exponential function",
     Usage => "exp x",
     Inputs => { "x" => RR },
     Outputs => { { "the exponential of ", TT "x" } } ,
     EXAMPLE lines ///
     exp 1p300
     exp(pi*ii)
     ///
     }

document {
     Key => {log,(log, RR),(log, QQ),(log, ZZ),(log,CC),(log,QQ,CC),(log,RR,CC),(log,ZZ,CC),
	  (log, ZZ, ZZ),(log, QQ, ZZ),(log, ZZ, QQ),(log, QQ, QQ),(log, RR, ZZ),
	  (log, ZZ, RR),(log, QQ, RR),(log, RR, QQ),(log, RR, RR)},
     Headline => "logarithm function",
     Usage => "log x\nlog(b,x)\nlog_b x",
     Inputs => { "x" => RR, "b" => RR => {"the base for the logarithm"} },
     Outputs => { { "the logarithm of ", TT "x"} },
     EXAMPLE lines ///
     	  log 10
	  log_2 10
	  log_10 2p100
     ///
     }
document {
     Key => {sqrt,(sqrt, CC),(sqrt, QQ),(sqrt, ZZ),(sqrt, RR)},
     Headline => "square root function",
     Usage => "sqrt x",
     Inputs => { "x" => RR },
     Outputs => { { "the square root of ", TT "x"} },
     EXAMPLE lines ///
     sqrt 2p200
     sqrt (+ii)
     ///
     }
document {
     Key => run,
     Headline => "run an external command", 
     Usage => "run s",
     Inputs => { "s" => String => {"a command understandable by the operating system"} },
     Outputs => {
	  ZZ => "the return code"
	  },
     PARA {
	  "The process is run in the same process group, so signals invoked by control characters at the terminal will
	  go both to it and to Macaulay2."
	  },
     PARA {
	  "Under Linux and Mac OS, the return code is 256 times the exit status
	  code of the command, if the command exited normally; by convention,
	  an exit status code of 0 indicates error free execution or the
	  Boolean value ", TT "true", ", an exit status code of 1 indicates an
	  error or the Boolean value ", TT "false", "; if the command
	  terminated in response to a signal or fault, the signal number (in
	  the range 1-126) is returned, added to 128 if a core dump was
	  created; if the shell (command interpreter) could not be executed,
	  then 127 is returned.  Signal numbers typically include 2 for
	  interrupt, 3 for quit, 6 for abort, 9 for kill, 11 for segmentation
	  fault, and 15 for termination.  For details, see the man page of the
	  libc routine ", TT "system()", "."
	  }
     }
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
     Key => (value, IndexedVariable),
     Headline => "retrieve the value of an indexed variable",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value of ", TT "s" } },
     EXAMPLE lines ///
     	  y = x_3
	  x_3 = 4
	  x_3
	  y
	  value y
     ///
     }

document {
     Key => {(value,String),"currentString"},
     Headline => "evaluate a string",
     Usage => "value s",
     Inputs => { "s" },
     Outputs => { {"the value obtained by evaluating the code in ", TT "s" } },
     "The contents of ", TT "s", " are treated as code in the
     Macaulay2 language, parsed it in its own scope (the same way a file is)
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
	  },
     PARA{
	  "During evaluation of the string, error messages will refer to the location of the error 
	  as ", TT "currentString", " with a line number and a column number,
	  and the value of the variable ", TO "currentString", " is set to the string, to aid in debugging."
	  },
     EXAMPLE lines ///
     debuggingMode = stopIfError = false;
     value "1/0"
     debuggingMode = true;
     value "1/0"
     break
     ///,
     SeeAlso => {"debugging", "stopIfError", "debuggingMode"}
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
     Key => {gcd,
	  (gcd, List),
	  (gcd, Sequence),
	  (gcd, QQ, QQ),
	  (gcd, RingElement, RingElement),
	  (gcd, ZZ, QQ),
	  (gcd, QQ, ZZ),
	  (gcd, ZZ, ZZ),
	  (gcd,RingElement,ZZ),
	  (gcd,ZZ,RingElement)},
     Headline => "greatest common divisor",
     Usage => "gcd(x,y,...)",
     Inputs => { "x" => ZZ, ", or ", ofClass QQ, ", or ",ofClass RingElement },
     Outputs => { ZZ => { ", or ", ofClass QQ, ", or ",ofClass RingElement, ",
	       the greatest commond divisor of the arguments" } },
     EXAMPLE lines ///
     gcd(12,8,48)
     R = QQ[x,y,z];
     gcd(x^2*y,x*y^3^6)
     gcd(x^36-1,x^24-1)
     ///,
     SeeAlso => {gcdCoefficients, lcm}
     }

doc ///
  Key
    lcm
    (lcm, List)
    (lcm, Sequence)
    (lcm, QQ, QQ)
    (lcm, RingElement, RingElement)
    (lcm, ZZ, QQ)
    (lcm, QQ, ZZ)
    (lcm, ZZ, ZZ)
    (lcm,RingElement,ZZ)
    (lcm,ZZ,RingElement)  
  Headline
    least common multiple
  Usage
    lcm(x,y,...)
  Inputs
    x:RingElement
      all the elements should be integers, rational numbers or ring elements
  Outputs
    m:RingElement
      the least common multiple of the elements x,y,...
  Description
   Example
     lcm(-6,15,14)
     lcm(-6/7,15,14)
     R = QQ[a..d];
     lcm(a^2-d^2,(a-d)*(b+c))
     factor oo
  SeeAlso
    gcd
///

doc ///
  Key
    (lcm,MonomialIdeal)
  Headline
    least common multiple of all minimal generators
  Usage
    m = lcm I
  Inputs
    I:MonomialIdeal
  Outputs
    m:RingElement
  Description
   Text
     This function is implemented in the engine, as it is used in many algorithms involving monomial ideals.
   Example
     R = QQ[a..d];
     I = monomialIdeal "a4,a3b6,a2b8c2,c4d5"
     lcm I
     first exponents lcm I
  SeeAlso
    (dual,MonomialIdeal)
    "PrimaryDecomposition::irreducibleDecomposition(MonomialIdeal)"
    "PrimaryDecomposition::primaryDecomposition(MonomialIdeal)"
///

document {
     Key => {concatenate,(concatenate, ZZ), (concatenate, BasicList), (concatenate, String), (concatenate, Nothing), (concatenate, Symbol)},
     Headline => "join strings",
     TT "concatenate(s,t,...,u)", " yields the concatenation of the strings s,t,...,u.",
     PARA{},
     "The arguments may also be lists or sequences of strings and symbols, in
     which case they are concatenated recursively.  Additionally,
     an integer may be used to represent a number of spaces, and ", TO "null", " will be represented by the empty string.",
     EXAMPLE ///concatenate {"a",("s",3,"d",),"f"}///,
     SeeAlso => { "String"} 
     }

document {
     Key => error,
     Headline => "deliver error message",
	Usage => "error s",
	Inputs => {
		"s" => String => "a string or a sequence of things that can be converted to strings"
		},
	Consequences => {
		"an error message is displayed"
		},
     TT "error s", " causes an error message s to be displayed.",
     PARA{},
     "The error message ", TT "s", " (which should be a string or a sequence of
     things that can be converted to strings and concatenated) is printed.
     Execution of the code is interrupted, and control is returned
     to top level. Here is an explicit example showing how this command is used:",
     PRE///exampleCode = p -> (
     if not isPrime p	
     then error "expected a prime integer";
     if p == 2
     then error "expected an odd prime";
     lift((p+1)/2, ZZ)
     );///,
     PARA {	   
	  "Eventually we will have a means of ensuring that the line 
	  number printed out with the error message will have more 
	  significance, but currently it is the location in the code of 
	  the error expression itself."
	  },
     PARA {
	  "If the error message begins with a hyphen, then the word \"error\" will not be
	  inserted into the error message."
	  }
     }

document {
     Key => {getenv,(getenv, String)},
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
     Usage => "currentDirectory()",
     Outputs => {
	  "the complete path to the current directory, together with an extra slash"
	  },
     EXAMPLE lines ///
     currentDirectory()
     ///,
     PARA {
	  "If a component of the path to the current directory no longer exist, an error will be signalled."
	  }
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
     for: (1) the operations that have already been installed for them will return
     values in the original object, rather than in the copy; and (2) the copy
     operation is shallow, not copying keys and values that happen to be hash tables.",
     PARA{},
     SeeAlso => { "newClass" }
     }

document {
     Key => {mergePairs,(mergePairs, BasicList, BasicList, Function)},
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
	       coincide.  If ", TT "f", " or ", TT "g", " evaluates ", TO "continue", ", then
	       nothing is contributed to the resulting hash table.  If ", TT "h", " evaluates
	       ", TO "continue", ", then, at that point, the entry stored under the key ", TT "f(p,q)", " 
	       in the hash table under construction is removed."
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
     PRE "     combine(x, y, monomialTimes, coeffTimes, coeffPlus)",
     "We illustrate that with a simple-minded implmentation of the free ring on the English alphabet, representing words
     as string and polynomials as hash tables that associate coefficients to words.",
     EXAMPLE lines ///
     	  Poly = new Type of HashTable
     	  p = new Poly from { "" => 1, "x" => 2, "y" => 3, "cat" => 5 }
	  Poly * Poly := (p,q) -> combine(p,q,concatenate,times,plus);
	  p*p
     ///,
     SeeAlso => {merge}
     }

document {
     Key => {(ancestor, Type, Type),ancestor},
     Headline => "whether one type is an ancestor of another",
     Usage => "ancestor(x,y)",
     Inputs => { "x" => Type, "y" => Type },
     Outputs => { {"whether ", TT "x", " is an ancestor of ", TT "y"} },
     PARA {
     	  "The ancestors of ", TT "y", " are ", TT "y", ", ", TT "parent y", ", ", TT "parent parent y", ", and so on."
	  },
     PARA {
	  "If ", TT "x", " is an ancestor of ", TT "y", ", then we also say that ", TT "y", " is a ", EM "specialization", " of ", TT "x", "."
	  },
     EXAMPLE lines ///
     parent String
     parent parent String
     parent parent parent String
     ///,
     SeeAlso => {ancestors}
     }

document {
     Key => ancestors,
     Headline => "the ancestors of something",
     Usage => "ancestors x",
     Inputs => {"x"},
     Outputs => {{"the list of ancestors of ", TT "x"}},
     EXAMPLE lines ///
     ancestors String
     ancestors class 3
     ancestors class 3.
     ancestors ring 3.
     ancestors class ring 3.
     ancestors 3
     ///,
     SeeAlso => {ancestor, showStructure}
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
	  TO (symbol _, Ring, ZZ)
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
     Key => {symbol =>,(symbol =>, Thing, Thing)},
     Headline => "produce an Option",
     TT "x => y", " a binary operator that produces a type of list called
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
     

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
