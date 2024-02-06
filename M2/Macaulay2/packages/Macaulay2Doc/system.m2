document {
     Key => GCstats,
     Headline => "information about the status of the garbage collector",
     PARA {
	  "Macaulay2 uses the Hans Boehm ", TO2 {"GC garbage collector", "garbage collector"}, " to reclaim unused memory.  The function ", TT "GCstats", " 
	  provides information about its status."
	  },
     EXAMPLE lines ///
     s = GCstats()
     ///,
     PARA {
	  "The value returned is a hash table, from which individual bits of information can be
	  easily extracted, as follows."
	  },
     EXAMPLE lines ///
     s#"heap size"
     ///,
     PARA {
	  "The entries whose keys are upper case give the values of environment variables affecting the operation of the 
	  garbage collector that have been specified by the user."
	  },
     PARA {
	  "For further information about the individual items in the table, we refer the user to the source code and documentation of the garbage collector."
	  },
     SeeAlso => { "GC garbage collector" }
     }

document {
    Key => "system facilities",
    Subnodes => {
	"Loading files:",
	  TO "autoload",
	  TO "initialization file",
	  TO "input",
	  TO "load",
	  TO "needs",
	  TO "end",
	"Echoing characters:",
	  TO "clearEcho",
	  TO "setEcho",
	"Dumping and restoring the state of the system:",
	  TO "top level loop", -- see repl.m2
	  TO "restart",
	  TO "addStartFunction",
	  TO "addEndFunction",
	"Interface to the operating system:",
	  TO "alarm",
	  TO "currentDirectory",
	  TO "exec",
	  TO "exit",
	  TO "fork",
	  TO "getenv",
	  TO "processID",
	  TO "path",
	  TO "quit",
	  TO "run",
	  TO "sleep",
	  TO "nanosleep",
	  TO "Time",
	  TO "time",
	  TO "timing",
	  TO "elapsedTime",
	  TO "elapsedTiming",
	  TO "wait",
	"Variables with information about the state of the current process:",
	  TO "commandLine",
	  TO "environment",
	  TO "version",
	"Dealing with the garbage collector:",
	  TO "collectGarbage",
	  TO "registerFinalizer"
	  }
     }

undocumented {(autoload, Function, String)}
document {
     Key => {autoload, (autoload, Symbol, String)},
     Headline => "arrange for a function to be loaded automatically",
     Usage => "autoload(f,x)",
     Inputs => { "f", "x" },
     Consequences => {{ "arranges for a function named ", TT "f", " to be automatically loaded from the file ", TT "x", " the first time it is used.
	  This is accomplished by creating a suitable function that will load the file and assigning the function to ", TT "f", "." }},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  fn << "f = x -> x+1\n" << close
	  autoload(f,fn)
	  code f
	  f 4
	  removeFile fn
     ///
     }
document {
     Key => "initialization file",
     "The file ", TT "init.m2", " is loaded automatically when the
     program is started, if it exists.",
     PARA{
	  "On most systems the file is sought in the directory ", TT "$HOME/.Macaulay2/", ",
	  where ", TT "$HOME", " is replaced by the path to the user's home
	  directory."
	  },
     PARA{
	  "Under Mac OS X, the file is sought instead in the
	  directory ", TT "$HOME/Library/Application Support/Macaulay2/", "."
	  },
     PARA{
	  "If the user wants a file called, say, ", TT "start.m2", " in the current
	  directory to be loaded automatically when the program is started, then the
	  following line of code can be placed in the file ", TT "init.m2", "."
	  },
     PRE {
	  ///if fileExists "start.m2" then load(currentDirectory()|"start.m2")///
	  },
     PARA {
	  "Warning: former versions of the program would also load a file named ", TT "init.m2", " found in the current directory."
	  }
     }
document {
     Key => input,
     Headline => "read Macaulay2 commands and echo",
     Usage => "input fn",
     Inputs => { "fn" => String },
     Consequences => {
	  {"reads and executes the commands found in the file whose name is contained in the string ", TT "fn", ",
	       echoing the input, printing the values, and incrementing the line number"
	       }
	  },
     PARA{
	  "The file is sought in the directory containing the file currently being loaded, if any, and then along
	  the ", TO "path", ", unless the name of the file begins with ", TT "/", ", ", TT "~/", ", ", TT "$", ", or ", TT "!", ".
	  If the file begins with ", TT "./", " or ", TT "../", ", then it looks instead in the directory of the
	  file currently being loaded (see ", TO "currentFileDirectory", ").  If no file is being loaded it will
	  look in the current directory (see ", TO "currentDirectory", ").",
	  },
     PARA{
	  "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
	  the reading of the file is stopped at that point.",
	  },
     PARA{
	  "If an error occurs while evaluating the expressions in the file,
	  reading is stopped.",
	  },
     SeeAlso =>{ "path", "needs", "load"}
     }
document {
     Key => load,
     Headline => "read Macaulay2 commands",
     TT "load \"f\"", " -- reads and executes Macaulay2 expressions found
     in the file named ", TT "f", ".",
     PARA{
	  "The file is sought in the directory containing the file currently being loaded, if any, and then along
	  the ", TO "path", ", unless the name of the file begins with ", TT "/", ", ", TT "$", ", or ", TT "!", ".
	  If the file begins with ", TT "./", " or ", TT "../", ", then it looks instead in the directory of the
	  file currently being loaded (see ", TO "currentFileDirectory", ").  If no file is being loaded it will
	  look in the current directory (see ", TO "currentDirectory", ")."
	  },
     PARA{
	  "The file is read without echoing the input, printing the values, or incrementing the line number."
	  },
     PARA{
	  "If one of the top level expressions in the file evaluates to the symbol ", TO "end", "
	  the reading of the file is stopped at that point."
	  },
     PARA {
	  "If the variable ", TO "notify", " is set to true, then an informational message is displayed after the file is loaded."
	  },
     SeeAlso =>{ "path", "needs", "input", "notify"}
     }
document {
     Key => needs,
     Headline => "read Macaulay2 commands if necessary",
     Usage => "needs \"f\"",
     Consequences => {{"The file named ", TT "f", " is loaded with ", TO "load", " if it hasn't been loaded yet; if it
	       has changed since the last time it was loaded, it will be loaded again, from
	       the same location as the time before, without searching along the ", TO "path", "."}},
     PARA {
	  "If the variable ", TO "notify", " is set to true, then an informational message is displayed after the file is loaded."
	  }
     }
document {
     Key => end,
     Headline => "stop loading a file",
     Usage => "end",
     Consequences => {
	  {"This symbol, encountered at top level, causes loading of the current input file to be stopped."},
	  {"Alternatively, in the debugger it causes the current code to be abandoned, and the
	       debugger to be re-entered one level further up.  If there are no more suspended levels
	       of execution, then control is returned to the top level."
	       }
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo3.m2"
     get loadedFiles#(#loadedFiles-1)
     ///,
     PARA {
	  "Here is an example of its use in the debugger."
	  },
     EXAMPLE lines ///
     load "Macaulay2Doc/demo1.m2"
     g 2
     end
     end
     g 3
     ///,
     SeeAlso =>{ "needs", "load", "input" }
     }
document {
     Key => read,
     Headline => "read from a file", }
document {
     Key => (read,Sequence),
     Usage => "read()",
     Inputs => {
	  "()"
	  },
     Outputs => {
	  { "a string obtained by reading a line from the standard input file, ", TO "stdio", "." }
	  },
     }
document {
     Key => (read,String),
     Usage => "read p",
     Inputs => {
	  "p" => "a string containing a prompt to be displayed for the user"
	  },
     Outputs => {
	  { "a string obtained by reading from the standard input file ", TO "stdio" }
	  },
     }
document {
     Key => (read,File),
     Usage => "read f",
     Inputs => {
	  "f" => "an input file"
	  },
     Outputs => {
	  { "a string obtained by reading from ", TT "f", "." }
	  },
     EXAMPLE {
	  ///f = openInOut "!cat"///,
	  ///isReady f///,
	  ///f << "hi there" << flush;///,
	  ///isReady f///,
	  ///read f///,
	  ///isReady f///,
	  },
     SeeAlso => {"openIn", "get", "isReady"}
     }
document {
     Key => (read,File,ZZ),
     Usage => "read(f,n)",
     Inputs => {
	  "f" => "a file",
	  "n" => "an integer specifying the maximum number of bytes to read"
	  },
     Outputs => {
	  { "a string obtained by reading from ", TT "f" }
	  },
     "Input files are buffered, so the current contents of the buffer are returned
     if the buffer is not empty, otherwise reading from the file is attempted first.",
     SeeAlso => {"openIn", "get", "isReady"}
     }
document {
     Key => {get,(get, File),(get, String)},
     Headline => "get the contents of a file",
     Usage => "get f",
     Inputs => {
	  "f" => {ofClass{File,String}, ".  If ", TT "f", " is a string, then it is opened, as with
	       ", TO "openIn", ".  Filenames starting
	       with ", TT "!", " or with ", TT "$", " are treated specially, see ", TO "openInOut", "."
	       }
	  },
     Outputs => {
	  {"a string containing the contents of the file.  If the file was already open and partially
	       read, the remainder of the contents of the file are returned."}
	  },
     Consequences => {
	  {"In the case where ", TT "f", " is a file, its input side is closed."}
	  },
     EXAMPLE lines ///
     "test-file" << "hi there" << close
     get "test-file"
     removeFile "test-file"
     get "!date"
     ///,
     SeeAlso =>{ read, removeFile, "close", (symbol <<, File, Thing) }
     }
document {
     Key => {scanLines, (scanLines,Function,String), (scanLines,Function,List)},
     Headline => "apply a function to each line of a file",
     Usage => "scanLines(f,fn)",
     Inputs => { "f", "fn" => "the name of a file, or a list of names of files" },
     Consequences => {
	  {"applies ", TT "f", " to each line of the file(s)"}
	  },
     Outputs => {
	  {
	       "returns ", TO "null", " unless the function uses ", TT "break x", " with a non-null value for ", TT "x", ",
	       in which case scanning stops and ", TT "x", " is returned immediately"
	       }
	  },
     PARA {
	  "The file is read and processed one block at a time, making this procedure potentially much better at conserving
	  memory than ", TT "scan(lines get fn,f)", " when the file is very large."
	  }
     }


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
     Key => {getc,(getc, File)},
     Headline => "get a byte",
     TT "getc f", " obtains one byte from the input file f and provides it as a
     string of length 1.  On end of file an empty string of is returned.",
     PARA{},
     SeeAlso => { "File" },
     PARA{},
     "Bug: the name is cryptic and should be changed."
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
     Key => restart,
     Headline => "restart Macaulay2",
     Usage => "restart",
     Consequences => { {"the program will be restarted from the beginning"} },
     PARA{
	  "Functions previously registered with ", TO "addEndFunction", " will
	  be called before the current instance of the program terminates.  Then the program will be invoked
	  afresh, as described in ", TO "Invoking the program", "."
	  }
     }
document {
     Key => addStartFunction,
     Headline => "add a startup function",
     Usage => "addStartFunction f",
     Inputs => { "f" => Function },
     Consequences => {
	  {"When the program restarts, the function ", TT "f", " will be called, with no arguments."}
	  },
     SeeAlso => {"addEndFunction"}
     }
document {
     Key => addEndFunction,
     Headline => "add an ending function",
     Usage => "addEndFunction f",
     Inputs => { "f" => Function },
     Consequences => {
	  {"When the program is about the exit, the function ", TT "f", " will be called, with no arguments."}
	  },
     SeeAlso => {"addStartFunction"}
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
     Key => exec,
     Headline => "execute another program",
	Usage => "exec argv",
     TT "exec argv", "  uses the 'exec' operating system call to
     start up another program, replacing the current Macaulay2 process.
     Here ", TT "argv", " is a string, or a sequence or list of strings
     to be passed as arguments to the new process.  The first string
     is the name of the executable file."
     }
document {
     Key => exit,
     Headline => "exit the program",
     TT "exit n", " -- terminates the program and returns ", TT "n", " as return code.",
     BR{},
     TT "exit", " -- terminates the program and returns 0 as return code.",
     PARA{},
     "Files are flushed and closed.  Functions registered with ", TO "addEndFunction", "
     are called, unless a nonzero return value has been provided.  Another
     way to exit is to type the end of file character, which is typically
     set to Control-D in unix systems, and is Control-Z under Windows.",
     SeeAlso => {"quit"}
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
     Key => "path",
     Headline => "list of directories to look in",
     PARA {
	  "A list of strings containing names of directories in which ", TO "load",
	  ", ", TO "input", ", ", TO "loadPackage", ", ", TO "needsPackage", ", and
	  ", TO "installPackage", " should seek files.  These strings are simply
	  concatenated with the filename being sought, so should include a
	  terminal slash.  One further directory is implicitly searched
	  first: the directory containing the current input file; when input is
	  coming from the standard input, that directory is the current directory of
	  the process."
	  },
     PARA {
	  "After the core Macaulay2 files are loaded, unless the command line option
	  ", TT "-q", " is encountered, the following subdirectories will be
	  prepended to the path, based on the value of the ", TO "applicationDirectory", "
	  for your system."
	  },
     PRE replace(regexQuote homeDirectory, "/home/m2user/",
	   concatenate between_"\n" apply(core "userpath", s -> (5, s))),
     EXAMPLE {
	  "stack path",
	  ///path = append(path, "~/resolutions/"); stack path///
	  }
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
    Key => sleep,
    Headline => "sleep for a while",
    TT "sleep n", " -- sleeps for ", TT "n", " seconds.",
    SeeAlso => {nanosleep}
    }
document {
    Key => nanosleep,
    Headline => "sleep for a given number of nanoseconds",
    TT "nanosleep n", " -- sleeps for ", TT "n", " nanoseconds.",
    EXAMPLE "elapsedTime nanosleep 500000000",
    SeeAlso => {sleep}
    }
document {
     Key => "timing",
     Headline => "time a computation",
     TT "timing e", " evaluates ", TT "e", " and returns a list of type ", TO "Time", "
     of the form ", TT "{t,v}", ", where ", TT "t", " is the number of seconds
     of cpu timing used, and ", TT "v", " is the value of the expression.",
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
     of time elapsed, and ", TT "v", " is the value of the expression.",
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
     of the expression.",
     SeeAlso => {"timing", "time", "cpuTime", "elapsedTiming", "elapsedTime"}
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
     Key => "commandLine",
     Headline => "the command line arguments",
     Usage => "commandLine",
     "A constant whose value is the list of arguments passed to the interpreter,
     including argument 0, the name of the program.",
     SeeAlso => {"scriptCommandLine"}
     }
document {
     Key => "scriptCommandLine",
     Headline => "the command line arguments to be used when running a script",
     Usage => "scriptCommandLine",
     "A constant whose value is the list of arguments passed to the interpreter when a script
     is started, excluding argument 0, the name of the program, and excluding argument 1, the
     option \"--script\".",
     SeeAlso => {"commandLine"}
     }
document {
     Key => "environment",
     Headline => "the environment variables",
     "A constant whose value is the list containing the
     environment strings for the process."
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
     Key => collectGarbage,
     Headline => "collect the garbage in memory",
     Usage => "collectGarbage()",
     Consequences => {
	  {"garbage is collected"}
	  },
     SeeAlso => "GC garbage collector"
     }
document {
     Key => {registerFinalizer},
     Headline => "register a string that will be displayed when an object is garbage collected",
     Usage => "registerFinalizer(x,str)",
     Inputs => {
	  "x" => Thing,
	  "str" => String
	  },
     Consequences => {
	  "A finalizer is registered with the garbage collector to print a string
	  when that object is collected as garbage"
	  },
     EXAMPLE lines ///
	  for i from 1 to 9 do (x := 0 .. 10000 ; registerFinalizer(x, "-- finalizing sequence #"|i|" --"))
	  collectGarbage() -* no-capture-flag *-
	  ///,
     Caveat => "This function should mainly be used for debugging.  Having a large number of finalizers
     might degrade the performance of the program.  Moreover, registering two or more objects that are members of a circular chain
     of pointers for finalization will result in a memory leak, with none of the objects in the chain
     being freed, even if nothing else points to any of them.",
     SeeAlso => {
	  collectGarbage
	  }
     }


document {
     Key => File,
     Headline => "the class of all files",
     "Files may be input files, output files, pipes, or sockets.
     A list of currently open files may be obtained with ", TO "openFiles", ".",
     DIV {
	  "class" => "waystouse",
	  SUBSECTION {"Functions operating on path names:"},
	  UL {
	       TO minimizeFilename,
	       TO "path",
	       TO relativizeFilename,
	       TO searchPath,
	       TO temporaryFileName,
	       TO toAbsolutePath
	       }
	  }
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

document { Key => {(copyDirectory, String, String),copyDirectory,[copyDirectory, Exclude],[copyDirectory, UpdateOnly],[copyDirectory, FollowLinks],[copyDirectory, Verbose]},
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
document { Key => {(moveFile, String, String),(moveFile, String),moveFile,[moveFile,Verbose]},
     Usage => "moveFile(src,dst)",
     Inputs => {
	  "src" => "the filename or path to an existing file",
	  "dst" => "the new filename or path to a location (on the same file system).
		    Omit this argument and an appropriately numbered backup filename will be invented.",
	  Verbose => Boolean => {"whether to report individual file operations"}
	  },
     Outputs => {
	  {"the name of the backup file if one was created, or ", TO "null"}
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
	  bak = moveFile(dst,Verbose=>true)
	  removeFile bak
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
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the home directory."},
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
     PARA{"A filename starting with ", TT "~/", " will have the tilde replaced by the home directory."},
     SeeAlso => {mkdir}
     }

document { Key => Wrap,
     Headline => "a key for methods for wrapping printed output",
     "The default method for printing results of computations (stored in ", TT "Thing#{Standard,Print}", ") searches for a method for wrapping the output by search in the class of
     the result (and its ancestors) for a function stored under the key ", TT "Wrap", ".  The preinstalled wrapping methods use ", TO "wrap", ", they differ in their choice of
     separator between wrapped lines, or in whether to wrap at all.",
     EXAMPLE lines ///
	  QQ[x_0 .. x_40 ]
	  concatenate(50:"abcd ")
     ///
     }

document { Key => "fileDictionaries",
     Headline => "local dictionaries for loaded files",
     Usage => "fileDictionaries#fn",
     Inputs => { "fn" => String },
     Outputs => {{"the local dictionary in effect for the scope of the file loaded from the path ", TT "fn"}},
     }
document { Key => fileMode,
     Headline => "set or get file mode"
     }
document { Key => (fileMode,String),
     Headline => "get file mode",
     Usage => "fileMode fn",
     Inputs => {"fn"},
     Outputs => {{"the mode of the file located at the filename or path ", TT "fn"}},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  fn << "hi there" << close
	  fileMode fn
	  removeFile fn
     ///
     }
document { Key => (fileMode,File),
     Headline => "get file mode",
     Usage => "fileMode f",
     Inputs => {"f"},
     Outputs => {{"the mode of the open file ", TT "f"}},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  f = fn << "hi there"
	  fileMode f
	  close f
	  removeFile fn
     ///
     }
document { Key => (fileMode,ZZ,String),
     Headline => "set file mode",
     Usage => "fileMode(mo,fn)",
     Inputs => {"mo","fn"},
     Consequences => {{"the mode of the file located at the filename or path ", TT "fn", " is set to ", TT "mo"}},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  fn << "hi there" << close
	  m = fileMode fn
	  fileMode(m|7,fn)
	  fileMode fn
	  removeFile fn
     ///
     }

document { Key => (fileMode,ZZ,File),
     Headline => "set file mode",
     Usage => "fileMode(mo,f)",
     Inputs => {"mo","f"},
     Consequences => {{"the mode of the open file ", TT "f", " is set to ", TT "mo"}},
     EXAMPLE lines ///
	  fn = temporaryFileName()
	  f = fn << "hi there"
	  m = 7 + 7*8 + 7*64
	  fileMode(m,f)
	  fileMode f
	  close f
	  fileMode fn
	  removeFile fn
     ///
     }

document { Key => toAbsolutePath,
     Headline => "the absolute path version of a file name",
     Usage => "toAbsolutePath filename",
     Inputs => { "filename" => String },
     Outputs => {
	  String => {"the absolute (real) path name of ", TT "filename"}
	  },
     EXAMPLE lines ///
	 toAbsolutePath "a/b.m2"
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,minimizeFilename, relativizeFilename, baseFilename, "path", "rootPath", "rootURI"}
     }
document { Key => baseFilename,
     Headline => "the base part of a filename or path",
     Usage => "baseFilename fn",
     Inputs => { "fn" => String => "a filename or path" },
     Outputs => { "the last component of the path" },
     EXAMPLE lines ///
	  baseFilename "/a/b/dir/"
	  baseFilename "/a/b/file"
     ///,
     SeeAlso => {File,minimizeFilename, relativizeFilename, toAbsolutePath, searchPath, "path"}
     }
document { Key => {(searchPath, List, String), (searchPath, String), searchPath},
     Headline => "search a path for a file",
     Usage => "searchPath(pa,fn)\nsearchPath fn",
     Inputs => { "pa" => {"a list of strings giving paths to directories.  Each one ends with a slash.
	  If omitted, then ", TO "path", " is used"}, "fn" },
     Outputs => {{"a list of those directories in ", TT "pa", " containing files named ", TT "fn" }},
     SeeAlso => {File,minimizeFilename, relativizeFilename, baseFilename, toAbsolutePath, "path"}
     }
document { Key => minimizeFilename,
     Headline => "minimize a file name",
     Usage => "minimizeFilename fn",
     Inputs => { "fn" => "a path to a file" },
     Outputs => {{"a minimized path, equivalent to ", TT "fn"}},
     EXAMPLE lines ///
	 minimizeFilename "a/b/c/../d"
	 minimizeFilename "../../../../../../"
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,relativizeFilename, baseFilename, toAbsolutePath, searchPath, "path"}
     }
document { Key => relativizeFilename,
     Headline => "relativize a file name",
     Usage => "relativizeFilename(dir,fn)",
     Inputs => { "dir" => "a path to a directory", "fn" => "a path to a file" },
     Outputs => {{"a relativized path, equivalent to ", TT "fn", " when starting from ", TT "dir"}},
     EXAMPLE lines ///
	 relativizeFilename("a/b/","a/b/c/d")
	 relativizeFilename("a/b/c/d","a/b/")
	 relativizeFilename("a/b/c/d","a/b/e/f")
     ///,
     PARA {
	  "Paths of the form ", TT "foo/x/../bar", ", are shortened to ", TT "foo/bar", " without
	  checking the file system to see whether ", TT "x", " is a symbolic link.  For the other
	  behavior, see ", TO "realpath", "."
	  },
     SeeAlso => {File,minimizeFilename, baseFilename, toAbsolutePath, searchPath, "path"}
     }
document {
     Key => temporaryFileName,
     Headline => "make a temporary file name",
     Usage => "temporaryFileName()",
     Outputs => { "a unique temporary file name." },
     "The file name is so unique that even with various suffixes
     appended, no collision with existing files will occur.  The files will be removed
     when the program terminates, unless it terminates as the result of an error.",
     EXAMPLE {
	  ///temporaryFileName () | ".tex"///,
	  ///temporaryFileName () | ".html"///,
	  },
     PARA{
	  "This function will work under Unix, and also under Windows
	  if you have a directory on the same drive called ", TT "/tmp", "."
	  },
     PARA {
	  "If the name of the temporary file will be given to an external program, it may be necessary to
	  concatenate it with ", TO "rootPath", " or ", TO "rootURI", " to enable the external program to find the file."
	  },
     PARA {
	  "The temporary file name is derived from the value of the environment variable ", TT "TMPDIR", ", if it has one."
	  },
     PARA {
	  "If ", TO "fork", " is used, then the parent and child Macaulay2 processes will each remove their
	  own temporary files upon termination, with the parent removing any files created before ", TO "fork",
	  " was called.",
	  },
     SeeAlso => {File, "rootPath", "rootURI"}
     }

document { Key => "fileExitHooks",
     Headline => "a list of hooks (functions) to execute when the current file has been loaded"
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
     process in case the file was a pipe.",
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
     in case the file was a pipe.",
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
     Headline => "make a new link to a file",
     Key => {(linkFile, String, String),linkFile},
     Usage => "linkFile(o,n)",
     Inputs => {
	  "o" => String => "the path to an existing file",
	  "n" => String => "a new path to the file"
	  },
     Consequences => {
	  {"a new link ", TT "n", " is made to the existing file reachable using the path ", TT "o"}
	  },
     SeeAlso => { "moveFile", "copyFile" }
     }

document {
     Key => removeFile,
     Headline => "remove a file",
     Usage => "removeFile f",
     Inputs => { "f" => String },
     Consequences => {{ "the file reachable by the path ", TT "f", " is removed" }},
     PARA {
	  "Under a unix system such as GNU/Linux, what really happens is that the link to the file
	  specified by ", TT "f", " is removed.  The file itself disappears after all the links to
	  it are removed.  See ", TO "linkFile", "."
	  }
     }

document {
     Key => "minExponent",
     "This constant is the smallest exponent (of 2) that can be stored internally in the binary representation of
     an inexact real (or complex) number.  It cannot be changed.",
     EXAMPLE "minExponent"
     }

document {
     Key => "maxExponent",
     "This constant is the largest exponent (of 2) that can be stored internally in the binary representation of
     an inexact real (or complex) number.  It cannot be changed.",
     EXAMPLE "maxExponent"
     }

document {
     Key => {(httpHeaders,String),httpHeaders},
     Headline => "prepend http headers to a string",
     Usage => "httpHeaders s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  String => {"the string obtained from ", TT "s", " by prepending appropriate headers to it"}
	  },
     PARA {
	  "This function is experimental, and is intended to support the development of web servers."
	  },
     EXAMPLE ///httpHeaders "hi there"///
     }

document {
     Key => {getWWW,(getWWW, String),(getWWW, String, Nothing),(getWWW, String, String)},
     Headline => "get a web page",
     SYNOPSIS (
	  Usage => "getWWW URL",
	  Inputs => {"URL" => String},
	  Outputs => {{"the contents of the web page, together with the http headers, at the address given by ", TT "URL", ""}}
	  ),
     SYNOPSIS (
	  Usage => "getWWW(URL,TEXT)",
	  Inputs => {"URL" => String, "TEXT" => String},
	  Outputs => {{"obtain the contents of the web page addressed by ", TT "URL", " from
		    an http server, using the POST method, provided with ", TT "TEXT"}}
	  ),
     PARA{
	  "Accessing a secure web site (whose URL begins with ", TT "https:", ")
	  depends on your having installed ", TT "openssl", " on your system."
	  },
      SeeAlso => {splitWWW}
     }

doc ///
   Key
     splitWWW
     (splitWWW,String)
   Headline
     separate an http response into header and body
   Usage
     (head, body) = splitWWW str
   Inputs
     str:String
       an http response, such as that returned by @TO getWWW@.
   Outputs
     head:String
       the header of the response
     body:String
       the response body, which has been 'unchunked', if the response type is chunked.
   Description
    Text
      The format of chunked data is described @HREF{"https://www.w3.org/Protocols/", "here"}@.

      The following is an example obtaining 5 examples from the Kreuzer-Skarke database for
      4 dimensional reflexive polytopes.  We retrieve 5 examples each having the anti-canonical
      divisor a Calabi-Yau with $h^{(1,1)} = 10$.
    CannedExample
	i1 : str = getWWW "http://quark.itp.tuwien.ac.at/cgi-bin/cy/cydata.cgi?h11=10&L=5";

	i2 : (head,body) = splitWWW str;

	i3 : head

	o3 = HTTP/1.1 200 OK
	     Date: Thu, 23 Jun 2016 12:10:58 GMT
	     Server: Apache/2.2
	     Vary: Accept-Encoding
	     Connection: close
	     Transfer-Encoding: chunked
	     Content-Type: text/html; charset=UTF-8

	i4 : body

	o4 = <head><title>SEARCH RESULTS</title></head>
	     <body><pre><b>Search command:</b>
	     class.x -di x -He EH10:MVNFL5

	     <b>Result:</b>
	     4 9  M:22 9 N:14 8 H:10,18 [-16]
		1   0   1   0   2   0  -2  -2  -2
		0   1   0   0  -1   1   1  -1   1
		0   0   2   0   1   1  -3  -1  -4
		0   0   0   1   1   1  -1  -1  -2
	     4 10  M:23 10 N:15 10 H:10,18 [-16]
		 1    0    0    0   -1    1   -2    2    0   -1
		 0    1    0    0    1   -1    2   -1   -2    0
		 0    0    1    0   -1    1   -1    0    2   -2
		 0    0    0    1    1   -1    0   -2   -1    2
	     4 9  M:24 9 N:14 8 H:10,20 [-20]
		1   0   1   0   1  -1  -2   1  -2
		0   1   0   0   0   2  -2  -1   2
		0   0   2   0  -1  -1   0  -2  -2
		0   0   0   1  -1  -1   1  -1  -1
	     4 11  M:25 11 N:15 10 H:10,20 [-20]
		1   0   0   0   2  -2   0   2  -2  -2   2
		0   1   0   0  -1   1   1  -1   0   1  -2
		0   0   1   0  -1   1  -1   0   2   0  -2
		0   0   0   1  -1   1   1  -2   1   0  -1
	     4 10  M:25 10 N:15 10 H:10,20 [-20]
		 1    0    0    0   -1    0   -1   -1    2    1
		 0    1    0    0    0    0    2    0   -1   -2
		 0    0    1    0    0   -2    2    2   -2   -2
		 0    0    0    1    0   -1    0    2    0   -2
	     Exceeded limit of 5
	     </pre></body>
   SeeAlso
     getWWW
///

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

document { Key => "homeDirectory",
     Headline => "the home directory of the user",
     Usage => "homeDirectory",
     Outputs => { String => "the home directory of the user" },
     PARA {"In file operations, file names beginning with ", TT "~/", " will have it replaced
	  with the home directory."
	  },
     EXAMPLE "homeDirectory"
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
document {
     Key => "rootPath",
     Usage => "rootPath",
     Outputs => {
	  String => "the path, as seen by external programs, to the root of the file system seen by Macaulay2"
	  },
     PARA {
	  "This string may be concatenated with an absolute path to get one understandable by external programs.
	  Currently, this makes a difference only under Microsoft Windows with Cygwin, but there it's crucial
	  for those external programs that are not part of Cygwin.  Fortunately, programs compiled under Cygwin
	  know were to look for files whose paths start with something like ", TT "C:/", ", so it is safe
	  always to concatenate with the value of ", TO "rootPath", ", even when it is unknown whether the
	  external program has been compiled under Cygwin."
	  },
     EXAMPLE lines ///
     fn = temporaryFileName()
     rootPath | fn
     ///,
     SeeAlso => {"rootURI"}
     }

document {
     Key => "rootURI",
     Usage => "rootURI",
     Outputs => {
	  String => "the path, as seen by an external browser, to the root of the file system seen by Macaulay2"
	  },
     PARA {
	  "This string may be concatenated with an absolute path to get one understandable by an external browser.
	  Currently, this makes a difference only under Microsoft Windows with Cygwin, but there it's crucial
	  for those external programs that are not part of Cygwin.  Fortunately, programs compiled under Cygwin
	  know were to look for files whose paths start with something like ", TT "C:/", ", so it is safe
	  always to concatenate with the value of ", TO "rootPath", ", even when it is unknown whether the
	  external program has been compiled under Cygwin."
	  },
     EXAMPLE lines ///
     fn = temporaryFileName()
     rootURI | fn
     ///,
     SeeAlso => {"rootPath"}
     }

doc ///
Node
  Key
    limitFiles
  Usage
    limitFiles n
  Inputs
    n:ZZ
  Consequences
    Item
      the number of open file descriptors for the current process will be limited to {\tt n}
Node
  Key
    limitProcesses
  Usage
    limitProcesses n
  Inputs
    n:ZZ
  Consequences
    Item
      the number of simultaneous processes for the current user will be limited to {\tt n}
Node
  Key
     fileWritable
    (fileWritable,String)
  Usage
    fileWritable n
  Inputs
    n:
  Outputs
    :
      whether the file whose name is given by the string {\tt n} is writable
  Description
    Example
      fileWritable "."
  SeeAlso
    fileReadable
    fileExecutable
Node
  Key
     fileReadable
    (fileReadable,String)
  Usage
    fileReadable n
  Inputs
    n:
  Outputs
    :
      whether the file whose name is given by the string {\tt n} is readable
  Description
    Example
      fileReadable "."
  SeeAlso
    fileWritable
    fileExecutable
Node
  Key
     fileExecutable
    (fileExecutable,String)
  Usage
    fileExecutable n
  Inputs
    n:
  Outputs
    :
      whether the file whose name is given by the string {\tt n} is executable
  Description
    Example
      fileExecutable "."
  SeeAlso
    fileWritable
    fileReadable
///
