--		Copyright 1993-1998 by Daniel R. Grayson

document {
     Key => File,
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", ".",
     }

document {
     Key => "printing to the screen",
     "Use the operator ", TO "<<", " to print something to the screen.",
     EXAMPLE {
	  "<< 2^100"
	  },
     "Notice that the value returned is the standard input output file
     ", TO "stdio", ".  We can also use ", TO "<<", " as a binary
     operator to print explicitly to this file, and the output will 
     appear on the screen.",
     EXAMPLE {
	  "stdio << 2^100"
	  },
     "The associativity of the operator ", TO "<<", " during parsing is
     arranged so that the following code will work in a useful way.",
     EXAMPLE ///<< "The answer is " << 2^100 << ".";///,
     "Printing works fine with nets, too.",
     EXAMPLE ///<< "The answer is " << "aaa"||"bb"||"c" << ".";///,
     "In the presence of nets, which are used whenever you print something
     that is formatted two-dimensionally, the only correct way to terminate a line
     is with ", TO "endl", ", even though a string called ", TO "newline", "
     is available to you.",
     EXAMPLE {
	  ///R = ZZ[x,y];///,
	  ///f = (x+y+1)^2; g = (x-y+1)^2///,
	  ///<< "f = " << f << endl << "g = " << g << endl;///,
	  },
     "Output is saved in a buffer until the end of the line is encountered.
     If nets are not involved, you may use ", TO "flush", " to force
     the output to appear on the screen before the end of the line is
     encountered.  This may be useful in displaying some tiny indication
     of computational progress to the user.",
     EXAMPLE ///scan(0 .. 20, i -> << "." << flush)///,
     PARA{},
     "If long lines get displayed too slowly, such as in Emacs, then the user may choose
     to put a line such as ", TT "truncateOutput 100", " into an ", TO "initialization file", ".
     Time is still spent creating the wide output that is eventually truncated.",
     EXAMPLE {
	  "truncateOutput 50",
	  "41!",
	  "42!",
	  "43!",
	  "truncateOutput infinity",
	  "43!"
	  },
     Subnodes => {
	 TO print,
	 TO printerr,
	 TO pretty,
	 TO "printWidth",
	 TO truncateOutput,
         }
     }

document {
     Key => { "printing to a file", (symbol <<, File, Thing),(symbol <<, String, Thing), (symbol <<, File, Manipulator),
	  (symbol <<, Nothing, Thing),(symbol <<, Nothing, Manipulator), (symbol <<, Thing),
	  (symbol <<, File, Symbol),(symbol <<, File, Net),(symbol <<,File,String) },
     Headline => "print to a file",
     Usage => "f << x\n  << x",
     Inputs => {
	  "f" => Nothing => { ofClass {File, String, Nothing} },
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
     SeeAlso => { endl, flush, close },
     Subnodes => {
	 TO printString,
         TO stdio,
         TO stderr,
         },
     }

document {
     Key => "reading files",
     "Sometimes a file will contain a single expression whose value you wish
     to have access to.  For example, it might be a polynomial produced by
     another program.  The function ", TO "get", " can be used to obtain 
     the entire contents of a file as a single string.  We illustrate this
     here with a file whose name is ", TT "expression", ".",
     PARA{},
     "First we create the file by writing the desired text to it.",
     EXAMPLE {
	  "fn = temporaryFileName()",
	  "fn << \"z^6+3*x*z^4+6*y*z^4+3*x^2*z^2+12*x*y*z^2+12*y^2*z^2+x^3+6*x^2*y+12*x*y^2+8*y^3\" << endl << close"
	  },
     "Now we get the contents of the file, as a single string.",
     EXAMPLE "get fn",
     "This isn't quite what you want, because a string containing a description
     of a polynomial is not the same as a polynomial.  We may use
     ", TO "value", " to evaluate the string and produce the corresponding
     polynomial, after first setting up a polynomial ring to contain it.",
     EXAMPLE {
	  ///R = ZZ/101[x,y,z]///,
	  ///f = value get fn///,
	  ///factor f///,
	  },
     "Often a file will contain code written in the Macaulay2 language.
     Let's create such a file.",
     EXAMPLE {
	  "fn << \"sample = 2^100\nprint sample\n\" << close"
	  },
     "Now verify that it contains the desired text with ", TO "get", ".",
     EXAMPLE ///get fn///,
     "To load and execute that code, use ", TO "load", ".  This is the function
     you will use most often for using bits of code you have previously written,
     unless you have incorporated them into a package, in which case you would
     use ", TO "loadPackage", ".",
     EXAMPLE ///load fn///,
     "The command ", TO "needs", " can be used to load a file only if
     it hasn't already been loaded.",
     EXAMPLE ///needs fn///,
     PARA {
	  "For debugging or display purposes, it is sometimes useful to be able 
	  to simulate entering the lines of a file one by one, so they appear
	  on the screen along with prompts and output lines.  One may use
	  ", TO "input", " for that."
	  -- we don't illustrate the use of 'input' here, because the documentation example parser can't handle it
	  },
     PARA {
	  "There are other ways to manipulate the contents of a file with
	  multiple lines.  First, let's use ", TO "peek", " to observe the 
	  extent of this string returned by ", TO "get", "."},
     EXAMPLE ///peek get fn///,
     "The resulting string has newlines in it; we can use ", TO "lines", "
     to break it apart into a list of strings, with one row in each string.",
     EXAMPLE ///lines get fn///,
     "We may use ", TO "peek", " to observe the extent of these strings.",
     EXAMPLE ///peek lines get fn///,
     "We could even use ", TO "stack", " to assemble the lines of the
     file into a net.",
     EXAMPLE ///stack lines get fn///,
     "Now let's remove that file.",
     EXAMPLE ///removeFile fn///
     }

document {
     Key => "getting input from the user",
     "The function ", TO "read", " can be used to prompt the user and obtain
     a line of input as a string.  In response to the prompt, the user enters
     ", TT "sample", " and press return.",
     EXAMPLE {
	  ///filename = read "file name : "
sample///,
     	  },
     "Let's use ", TO "peek", " to verify that this string contains no newline
     characters.",
     EXAMPLE ///peek filename///,
     "If necessary, we may use ", TO "value", " to evaluate the string provided
     by the user.  In this example, we enter ", TT "(x+y)^2", " in response to
     the prompt.",
     EXAMPLE {
	  ///R = ZZ[x,y];///,
	  ///s = read "polynomial : "
(x+y)^2///,
     	  ///value s///
	  }
     }

document {
     Key => "creating and writing files",
     "We can print to a file in essentially the same way we print to the screen.
     In the simplest case, we create the entire file with one command; we
     give the file name as the initial left hand operand of ", TO "<<", ",
     and we close the file with ", TO "close", ".  Files must be closed
     before they can be used for something else.",
     EXAMPLE {
	  ///"testfile" << 2^100 << endl << close///,
	  ///value get "testfile"///,
	  },
     "More complicated files may require printing to the file multiple times.  One
     way to handle this is to assign the open file created the first time we
     use ", TO "<<", " to a variable, so we can use it for subsequent print operations
     and for closing the file.",
     EXAMPLE {
	  ///f = "testfile" << ""///,
	  ///f << "hi" << endl///,
	  ///f << "ho" << endl///,
	  ///f << close///,
	  ///get "testfile"///,
	  ///removeFile "testfile"///
	  }
     }

document {
     Key => "saving polynomials and matrices in files",
     "Use the function ", TO "toString", " to convert polynomials, matrices, and other mathematical
     objects to a linear format that can be saved in a file.",
     EXAMPLE lines ///
          R = QQ[x..z]
	  p = (x-y-1)^3
	  m = matrix {{x^2, x^2-y^2, x*y*z^7 }}
	  M = image m
	  f = temporaryFileName()
	  f << toString (p,m,M) << close
	  get f
	  (p',m',M') = value get f
	  p == p'
	  m == m'
	  M == M'
	  removeFile f
     ///
     }

document {
     Key => "two dimensional formatting",
     "We have seen that nets (see ", TO Net, ") are potentially useful for two
     dimensional formatting of output to an ascii terminal with limited
     graphical ability.  We present now a few more hints about putting
     this idea into practice.  Nets are used extensively in Macaulay2
     for formatting, for example, for formatting of polynomials and
     matrices.",
     EXAMPLE lines ///
	  R = ZZ/101[x,y,z];
	  f = random(R^1,R^{5:-3})
     ///,
     "Output of routines such as ", TO "betti", " and ", TO "net", " that
     return nets can be easily incorporated into more complex displays 
     using standard operations on nets (see ", TO "Net", ").",
     EXAMPLE lines ///
	  C = resolution cokernel f
	  be = betti C
	  "Betti numbers of " | net C | " are " | (net be)^2
     ///,
     "You could even learn how to display algebraic expressions with nets.",
     EXAMPLE ///"x" | "2"^1///,
     "There is an easier way to display algebraic expressions, using a
     type of thing called an ", TO "Expression", ".  It allows you
     to set up things that print out as powers, sums, products,
     matrices, and so on.  There are various types of expression, such as
     ", TO "Power", ", ", TO "Sum", ", ", TO "Divide", ", ", TO "Minus", ",
     and ", TO "Product", " that we can use for this.",
     EXAMPLE {
	  "Divide(Minus a,b)",
	  "Power(Sum(3,4,5),7)",
	  "Sum(1,2, Minus 3, 4,5)",
     	  },
     "Actually, the formation of such expressions is contagious, in the sense
     that the basic algebraic operations will construct expressions for you if
     one of their two operands is already an expression.",
     EXAMPLE {
	  "Minus a / b",
	  "(Sum(3,4,5))^7",
	  "1 + 2 + Minus 3 + 4 + 5",
	  },
     "In the last example above, ", TT "1 + 2", " was evaluated first, so it
     yielded ", TT "3", " but after that the contagion set in.",
     PARA{},
     "The function ", TO "expression", " can be used to prepare things such
     as polynomials for formatting using the mechanism introduced above.",
     EXAMPLE {
	  "g = (x+y)^2",
	  "e = expression g",
	  "peek e",
	  },
     "In the example above, we see that ", TO "peek", " extracts only one
     level of the structure.  We may use ", TO "peek'", " to display
     the structure of ", TT "e", " to depth 2.",
     EXAMPLE {
	  "peek'(2,e)",
	  },
     "Other types of ", TO "Expression", " that can be used for formatting
     nested lists as two dimensional arrays are ", TO "MatrixExpression", "
     and ", TO "Table", ".",
     EXAMPLE {
	  "Table{{1,2,3},{a,bb,ccc}}",
	  "MatrixExpression{{1,2,3},{a,bb,ccc}}",
	  ///Table{{"Example 1","Example 2"},
      {Table{{1,2},{3,4}},Table{{11,22},{3,444}}}}///
	  },
     }

document {
     Key => "file manipulation",
     Headline => "Unix file manipulation functions",
     Subnodes => {
	  TO isRegularFile,
	  TO isDirectory,
	  TO baseFilename,
	  -- TODO: change FileName to Filename
	  TO temporaryFileName,
	  TO fileMode,
	  TO fileExists,
	  TO fileReadable,
	  TO fileWritable,
	  TO fileExecutable,
	  TO fileTime, -- can use to get or set (currentTime)
	  TO copyFile,
	  TO moveFile,
	  TO removeFile,
	  TO findFiles,
	  TO symlinkFile,
	  TO linkFile,
	  TO readlink,
	  TO realpath,
	  "Directories",
	  TO currentDirectory,
	  TO readDirectory,
	  TO makeDirectory,
	  TO mkdir,
	  TO changeDirectory,
	  TO copyDirectory,
	  TO removeDirectory,
	  TO symlinkDirectory,
	  }
     }

document {
     Key => {(isReady, File),isReady}, 
     Headline => "whether a file has data available for reading",
     Usage => "isReady f",
     Outputs => { Boolean => { "whether the input file ", TT "f", " has data available for reading" } },
     EXAMPLE lines ///
         f = openInOut "!cat"
     	 isReady f
     	 f << "hi there" << flush;
     	 isReady f
     ///
     }

document {
     Key => {(atEndOfFile, File),atEndOfFile},
     Headline => "test for end of file",
     Usage => "atEndOfFile f",
     Inputs => { "f" },
     Outputs => { Boolean => { "whether the input file ", TT "f", " is at the end" } },
     EXAMPLE lines ///
         f = openInOut "!cat"
     	 f << "hi there" << closeOut;
     	 atEndOfFile f
	 peek read f
     	 atEndOfFile f
     ///
     }

document {
     Key => "communicating with programs",
     "The most naive way to interact with another program is simply to run
     it, let it communicate directly with the user, and wait for it to
     finish.  This is done with the ", TO "run", " command.",
     EXAMPLE ///run "uname -a"///,
     "To run a program and provide it with input, one way is use the operator ", TO "<<", ",
     with a file name whose first character is an
     exclamation point; the rest of the file name will be taken as the
     command to run, as in the following example.",
     EXAMPLE ///"!grep a" << " ba \n bc \n ad \n ef \n" << close///,
     "More often, one wants to write Macaulay2 code to obtain
     and manipulate the output from the other program.  If the program
     requires no input data, then we can use ", TO "get", " with a file name whose first character is an
     exclamation point.  In the following example, we also peek at the string
     to see whether it includes a newline character.",
     EXAMPLE {
	  ///peek get "!uname -a"///,
	  },
     "Bidirectional communication with a program is also possible.  We use
     ", TO "openInOut", " to create a file that serves as a bidirectional
     connection to a program.  That file is called an input output file.  In
     this example we open a connection to the unix utility ", TT "egrep", "
     and use it to locate the symbol names in Macaulay2 that begin with
     ", TT "in", ".",
     EXAMPLE {
	  ///f = openInOut "!egrep '^in'"///,
	  ///scan(keys Core.Dictionary, key -> f << key << endl)///,
	  ///f << closeOut///,
	  ///get f///
	  },
     "With this form of bidirectional communication there is always a danger
     of blocking, because the buffers associated with the communication
     channels (pipes) typically hold only 4096 bytes.  In this example we
     succeeded because the entire output from ", TT "egrep", " was smaller
     than 4096 bytes.  In general, one should be careful to arrange things
     so that the two programs take turns using the communication channel, so
     that when one is writing data, the other is reading it.",
     PARA{},
     "A useful function in this connection is ", TO "isReady", ", which will
     tell you whether an input file has any input available for reading, or
     whether it has arrived at the end.  We illustrate it in the following
     example by simulating a computation that takes 5 seconds to complete,
     printing one dot per second while waiting.",
     if version#"operating system" === "Windows-95-98-NT"
     then PRE "<< example doesn't work under Windows >>"
     else
     EXAMPLE {
	  ///f = openIn "!sleep 5; echo -n the answer is 4"///,
	  ///isReady f///,
	  ///while not isReady f do (sleep 1; << "." << flush)///,
	  ///read f///,
	  ///isReady f///,
	  ///atEndOfFile f///,
	  ///close f///
	  },
     "We also allow for bidirectional communication
     through sockets over the internet.  See ", TO "openInOut", "
     and ", TO "openListener", ", or the next section.",
     PARA{},
     "Another useful function is ", TO "wait", ", which can be used
     to wait for input to be available from any of a list of input
     files.",
     Subnodes => {
	 TO run,
	 TO findProgram,
	 TO runProgram,
	 TO (symbol <<, Program, Thing),
         },
     }

document {
     Key => "using sockets",
     PARA{
	  "It's easy to use sockets as though they were files.  Simply replace
	  the file name by a string of the form ", TT "$host:service", " where
	  ", TT "host", " is the name of IP number of host to contact, and
	  ", TT "service", " is the port number or name to use.  If ", TT "service", "
	  is omitted, then port 2500 is used.  If ", TT "host", " is omitted, then
	  an incoming connection will be listened for."
	  },
     PARA{
	  "The following code will illustrate two-way communication using sockets
	  similar to the interaction used by web servers,
	  and you may try it out on your machine, unless a firewall prevents it."
	  },
     PRE ///if (pid = fork()) == 0 then (
     try "$:7500" << "hi there" << close;
     exit 0;
     )
sleep 2
get "$localhost:7500"
wait pid///,
     PARA {
	  "The code uses ", TO "fork", " to create a separate process that will listen for a connection on port
	  7500 and then send us a message.  The ", TO "sleep", " command pauses
	  for a while to make sure the child process has had time to start listening.
	  Then we use an ordinary input command, namely ", TO "get", ", to obtain the message.
	  Finally, we ", TO "wait", " for the child process to finish, as we should."
	  },
     SeeAlso => { getWWW },
     Subnodes => {
	 "Creating a socket",
	 TO openListener,
	 TO openIn,
	 TO openInOut,
	 TO openOut,
	 TO openOutAppend,
	 "Manipulating a socket",
	 TO endl,
	 TO flush,
	 TO close,
	 TO closeIn,
	 TO closeOut,
	 "Interacting a socket",
	 TO get,
	 TO getc,
	 TO read,
	 TO scanLines,
	 TO fileLength,
	 TO(height, File),
	 TO(width, File),
	 TO atEndOfFile,
	 TO echoOn,
	 TO echoOff,
	 TO isOpen,
	 TO isReady,
	 TO isOutputFile,
	 TO isInputFile,
	 TO isListener,
	 TO openFiles,
	 TO connectionCount,
         },
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
