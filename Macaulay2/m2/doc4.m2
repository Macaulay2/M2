--		Copyright 1993-1998 by Daniel R. Grayson

oldexit := exit
erase quote exit
exit = method(SingleArgumentDispatch => true)
exit ZZ := i -> oldexit i
exit Sequence := () -> oldexit 0
exit = new Command from exit
quit = new Command from (() -> oldexit 0)

document { quote exit,
     TT "exit n", " -- terminates the program and returns ", TT "n", " as return code.",
     BR,
     NOINDENT, 
     TT "exit", " -- terminates the program and returns 0 as return code.",
     PARA,
     "Files are flushed and closed.  Another way to exit is to type the end of
     file character, which is typically set to Control-D in unix systems, and is
     Control-Z under MS-DOS.",
     SEEALSO "quit"
     }

document { quote quit,
     TT "quit", " -- terminates the program and returns 0 as return code.",
     PARA,
     "Files are flushed and closed.  Another way to exit is to type the end of
     file character, which is typically set to Control-D in unix systems, and is
     Control-Z under MS-DOS.",
     SEEALSO "exit"
     }

document { quote fork,
     TT "fork()", " -- forks the process, returning the process id of the child
     in the parent, and returning 0 in the child."
     }

document { quote sleep,
     TT "sleep n", " -- sleeps for ", TT "n", " seconds."
     }

document { quote processID,
     TT "processID()", " -- returns the process id of the current Macaulay 2 process."
     }

document { quote string,
     TT "string x", " -- convert ", TT "x", " to a string unless it is one already.",
     PARA,
     "Not intended for general use, as it is primitive, and doesn't display
     contents or names, for example.  Use ", TO "name", " instead."
     }

document { quote BinaryPowerMethod,
     TT "BinaryPowerMethod(x,n)", " -- computes ", TT "x^n", " using successive squaring",
     PARA,
     "The technique depends in a standard way on the binary expansion of ", TT "n", ",
     hence the name.",
     PARA,
     SEEALSO "SimplePowerMethod"
     }

document { quote SimplePowerMethod,
     TT "SimplePowerMethod(x,n)", " -- computes x^n using repeated multiplication",
     PARA,
     SEEALSO "BinaryPowerMethod"
     }

document { quote dumpdata,
     TT "dumpdata s", " -- dump all data segments for the current process to 
     the file whose name is stored in the string s.",
     PARA,
     "This effectively saves the entire state of the system, except that the
     input buffer for the file ", TO "stdio", " appears to have been emptied,
     and care is taken so that the environment and the command line arguments
     maintain their new values when the data is reloaded later with 
     ", TO "loaddata", "."
     }
document { quote loaddata,
     TT "loaddata s", " -- load all data segments for the current process from 
     the file whose name is stored in the string s.  The file must have been
     created with ", TO "dumpdata", " and the same version of Macaulay 2.",
     PARA,
     "The file should have been created with ", TO "dumpdata", ".  Everything will
     be returned to its former state except:",
     MENU {
	  {TO "reloaded", ", which counts how many times data has been 
	       dumped and restored."},
	  {TO "environment", ", which now reflects the current environment."},
	  {TO "commandLine", ", which now reflects the current command line."},
	  "Whether the standard input is echoed and prompts to the 
	  standard output are properly flushed, which depends on whether 
	  the standard input is a terminal."
	  },
     "After the data segments have been reloaded, the command line arguments
     will be dealt with in the usual way, except that only the arguments
     after the i-th '--' and before the i+1-st '--' (if any) will be considered,
     where ", TT "i", " is the current value of ", TO "reloaded", ".",
     SEEALSO {"listUserSymbols"}
     }
     
document { quote reloaded,
     TT "reloaded", " -- a constant whose value is the number of 
     times ", TO "loaddata", " has been executed by the current process.  Since
     loaddata completely resets the state of the system, something like this
     is needed."
     }

document { quote buckets,
     TT "buckets x", " -- returns a list of the buckets used internally in an 
     hash table ", TT "x", ".",
     PARA,
     "Each bucket is represented as a list of key/value pairs."
     }

document { quote ggPush,
     TT "ggPush h", " -- provides a string which when sent to the engine will
     cause it to push the object ", TT "h", " onto the engine's stack.",
     PARA,
     "This command is intended for internal use only.",
     PARA,
     "Warning: in an expression of the form ", TT "ggPush f()", " where ", TT "f", "
     is a function that returns an object with a handle, there is no pointer to
     the object retained in the string provided, so the garbage collector may
     cause the object and its handle to be freed before the arrival of the
     command!  The solution is to store the result in a local variable until
     the command has been sent."
     }

document { quote identity,
     TT "identity x", " -- returns x.",
     PARA,
     "This is the identity function."
     }

document { quote modulus,
     TT "modulus", " -- a key used in quotient rings of the form ZZ/n to store 
     the number n.",
     PARA,
     "This may go away when more general quotient rings are working."
     }

if class XCreateWindow === Function then (
document { quote XCreateWindow,
     TT "XCreateWindow(pid,x,y,a,b,w,n)", " -- makes a new window.",
     PARA,
     "Here ", TT "pid", " is the id of the parent window, ", TT "x", " 
     and ", TT "y", " are the coordinates of the upper left corner of the 
     window, ", TT "a", " and ", TT "b", " are the width and
     height, ", TT "w", " is the width of the border, and ", TT "n", " is the name of the window."
     }
) else erase quote XCreateWindow

if class XDefaultRootWindow === Function then (
document { quote XDefaultRootWindow,
     TT "XDefaultRootWindow()", " -- returns the id of the root window."
     }
) else erase quote XDefaultRootWindow

document { quote format,
     TT "format s", " -- prepare a string s for output by converting nonprintable
     characters to printable ones, or to escape sequences."
     }


document { quote generatorSymbols,
     TT "generatorSymbols", " -- a key used in a ", TO "Monoid", " under
     which is stored a list of the symbols used as generators for the monoid."
     }

document { quote generatorExpressions,
     TT "generatorSymbols", " -- a key used in a ", TO "Monoid", " under which is stored a list
     of the symbols used as generators for the monoid."
     }

document { quote match,
     TT "match(s,p)", " -- whether the string s matches the pattern ", TT "p", ".",
     PARA,
     "The pattern p may contain '*'s, which serve as wild card characters.
     This is provisional - eventually it will provide regular expression
     matching."
     }

document { quote gg,
     TT "gg x", " -- converts an integer, handle, or list of integers to the format
     required for communication with the engine.",
     PARA,
     SEEALSO "engine communication protocol"
     }

document { quote pairs,
     TT "pairs x", " -- makes a list of all key/value pairs ", TT "(k,v)", " in
     a hash table ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "x = new HashTable from {a => 1, b => 2, c => 3}",
	  "pairs x",
	  }
     }

document { quote sequence,
     TT "sequence v", " -- returns ", TT "v", " if ", TT "v", " is a sequence, otherwise makes
     a sequence of length one containing ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "sequence 4",
      	  "sequence {4,5}",
      	  "sequence (4,5)",
	  },
     PARA,
     SEEALSO { "singleton", "sequences" }
     }

document { quote xor,
     TT "xor(i,j)", " -- produces the bitwise logical exclusive-or of
     the integers ", TT "i", " and ", TT "j", ".",
     PARA,
     EXAMPLE "xor(10,12)"
     }

document { quote mingle,
     TT "mingle {v,w,...}", " -- produces a new list from the lists or
     sequences v,w,... by taking the first element from each, then the second, 
     and so on.",
     BR, NOINDENT,
     TT "mingle (v,w,...)", " -- does the same.",
     PARA,
     "After one of the lists is exhausted, it is silently ignored.",
     EXAMPLE {
	  "mingle({1,2,3,4},{a},{F,F,F,F,F,F,F,F,F,F})",
      	  ///concatenate mingle( {"a","b","c"} , {",",","} )///,
	  },
     "It is easy to transpose a nested list (thinking of it as a matrix)
     using ", TO "mingle", " and ", TO "pack", " together.",
     EXAMPLE {
      	  "pack(mingle {{1,2,3,4},{5,6,7,8}}, 2)"
	  }
     }

document { quote SelfInitializingType,
     TT "SelfInitializingType", " -- the class of all self initializing types.",
     PARA,
     "A self initializing type ", TT "X", " will produce an instance of X from
     initial data ", TT "v", " with the expression ", TT "X v", ".",
     PARA,
     EXAMPLE {
	  "X = new SelfInitializingType of BasicList",
      	  "x = X {1,2,3}",
      	  "class x",
	  },
     PARA,
     TO "Command", " is an example of a self initializing type.",
     SEEALSO {"HeaderType", "WrapperType"}
     }

document { quote Manipulator,
     TT "Manipulator", " -- the class of all file manipulators.",
     PARA,
     "A ", TT "Manipulator", " is a type of list which, when put out to
     a file with ", TO "<<", " causes a particular function to be applied
     to the file.",
     PARA,
     "Examples:",
     MENU { 
	  TO "endl",
	  TO "flush",
	  TO "close"
	  }
     }

document { quote close,
     TT "f << close", " -- closes the file ", TT "f", ".",
     BR, NOINDENT,
     TT "close f", " -- closes the file ", TT "f", ".",
     PARA,
     "In the case of an output file, any buffered output is first
     written to the file, and the return value is an integer,
     normally 0, or -1 on error, or the return status of the child
     process in case the the file was a pipe.",
     PARA,
     "If the file was open for both input and output, both directions
     are closed.",
     PARA,
     SEEALSO {"File", "Manipulator", "closeIn", "closeOut"}
     }

document { quote closeIn,
     TT "f << closeIn", " -- closes the input file ", TT "f", ".",
     BR, NOINDENT,
     TT "closeIn f", " -- closes the input file ", TT "f", ".",
     PARA,
     "If the file was open for both input and output, it remains
     open for output.",
     PARA,
     SEEALSO {"File", "Manipulator", "close", "closeOut"}
     }

document { quote closeOut,
     TT "f << closeOut", " -- closes the output file ", TT "f", ".",
     BR, NOINDENT,
     TT "close f", " -- closes the output file ", TT "f", ".",
     PARA,
     "Any buffered output is first written to the file,
     and the return value is an integer, normally 0, or -1
     on error, or the return status of the child process
     in case the the file was a pipe.",
     PARA,
     "If the file was open for both input and output, it remains
     open for input.",
     PARA,
     SEEALSO {"File", "Manipulator", "closeIn", "close"}
     }

document { quote flush,
     TT "f << flush", " -- writes out any buffered output for the output file f.",
     PARA,
     SEEALSO {"File", "Manipulator"}
     }

document { quote endl,
     TT "f << endl", " -- ends the line currently being put out to the
     file ", TT "f", ".",
     PARA,
     "It is an essential portable programming practice to use ", TT "endl", "
     always, for writing newline characters (see ", TO "newline", ") to a
     file will not terminate a line containing nets properly,
     and it will not flush the output buffer.",
     PARA,
     SEEALSO {"File", "Manipulator", "Net"}
     }

document { quote newline,
     TT "newline", " -- a string containing the character or sequence of
     characters which represents the end of a line.  To end an output line,
     you should use ", TO "endl", " instead, because there is more to 
     ending an output line than emitting the characters in ", TT "newline", ",
     especially when nets are being used.",
     PARA,
     "This string depends on what your operating system is: on Unix systems
     it is the ascii character 10; on Macintoshes it is the ascii character
     13, and under MS-DOS and Windows 95 it is a string of length 2 containing
     ascii characters 13 and 10.",
     PARA,
     "Try to avoid confusing the newline string described here with the
     ASCII character called ", TT "newline", ".  That character can be
     incorporated into a string with the escape sequence ", TT "\\n", ",
     and it always has ASCII code 10.",
     EXAMPLE ///ascii "\n"///,
     SEEALSO "Net"
     }

document { quote linkFilename,
     TT "linkFilename s", " -- convert a string ", TT "s", " into a string 
     which can be used as a file name to contain HTML about the topic 
     referred to by ", TT "s", ".",
     PARA,
     "The value returned is a sequence number, and hence may not be
     the same in subsequent sessions.  Hence this is mainly useful
     for creating html for all the online documentation, and the general 
     user will not find it useful.",
     PARA,
     SEEALSO "linkFilenameKeys"
     }

document { quote linkFilenameKeys,
     TT "linkFilenameKeys()", " -- returns a list of the strings which
     have been given to ", TO "linkFilename", ".",
     PARA,
     "This function is intended mainly for internal use in generating
     the documentation for Macaulay 2."
     }

document { quote collectGarbage,
     TT "collectGarbage()", " -- attempt a garbage collection.",
     PARA,
     SEEALSO "GC garbage collector"
     }

document { quote gcDump,
     TT "gcDump()", " -- produces a dump of the status of the garbage collector.",
     PARA,
     "Users will normally not want to use this function.  It calls the 
     function ", TT "GC_dump", " in the garbage collector, and the output can
     be used to debug problems with memory allocation.",
     PARA,
     SEEALSO "GC garbage collector"
     }

document { quote lookupCount,
     TT "lookupCount s", " -- the number of times the symbol ", TT "s", " has been
     encountered in source code presented to the interpreter."
     }

document { quote version,
     TT "version", " -- a hash table describing this version of the program.",
     PARA,
     EXAMPLE "version"
     }

document { quote Database,
     TT "Database", " -- the class of all database files.",
     PARA,
     EXAMPLE {
	  ///filename = tmpname "test.dbm"///,
      	  ///x = openDatabaseOut filename///,
      	  ///x#"first" = "hi there"///,
      	  ///x#"first"///,
      	  ///x#"second" = "ho there"///,
      	  ///scanKeys(x,print)///,
      	  ///close x///,
      	  ///run ("rm -f " | filename)///,
	  },
     "Functions:",
     MENU {
	  {TO "openDatabase", "    -- open a database file"},
	  {TO "openDatabaseOut", " -- open a database file for writing"},
	  {TO "close", "           -- close a database file"},
	  {TO quote #, " -- fetch or store in a database file"},
	  {TO quote #?, " -- query a database file"},
	  {TO "firstkey", "        -- get the first key"},
	  {TO "mutable", "         -- whether changes can be made"},
	  {TO "nextkey", "         -- get the next key"},
	  {TO "reorganize", "      -- compactify a database file"},
	  {TO "scanKeys", "        -- apply a function to each key"}
	  }
     }

document { quote reorganize,
     TT "reorganize x", " -- reorganize the database ", TT "file", " x, compactifying it.",
     PARA,
     SEEALSO "Database"
     }

document { quote openDatabase,
     TT "openDatabase s", " -- open a database file corresponding to the file
     whose name is contained in the string ", TT "s", ".",
     PARA,
     SEEALSO "Database"
     }

document { quote openDatabaseOut,
     TT "openDatabaseOut s", " -- open a database file corresponding to the file
     whose name is contained in the string ", TT "s", ", and allow changes to be made
     to it.",
     PARA,
     SEEALSO "Database"
     }

document { quote firstkey,
     TT "firstkey f", " -- return the first key available in the database
     file ", TT "f", ".",
     PARA,
     "Returns ", TT "null", " if none.",
     PARA,
     SEEALSO "Database"
     }

document { quote nextkey,
     TT "nextkey f", " -- return the next key available in the database
     file f.",
     PARA,
     "Returns ", TT "null", " if none.",
     PARA,
     SEEALSO "Database"
     }

document { quote addStartFunction,
     TT "addStartFunction (() -> ...)", " -- record a function for later 
     execution, when the program is restarted after loading dumped data."
     }

document { quote addEndFunction,
     TT "addEndFunction (() -> ...)", " -- record a function for later 
     execution, when the program is exited."
     }

document { quote runStartFunctions,
     TT "runStartFunctions()", " -- call all the functions previously recorded
     by ", TO "addStartFunction", " passing ", TT "()", " as argument sequence."
     }

document { quote runEndFunctions,
     TT "runEndFunctions()", " -- call all the functions previously recorded
     by ", TO "addEndFunction", " passing ", TT "()", " as argument sequence."
     }

document { "emacs",
     "The best way to edit Macaulay 2 code or to run Macaulay 2 is with GNU 
     emacs, a versatile text editor written by Richard Stallman which
     runs well under most UNIX systems.  Its
     web page is ", HREF "http://www.gnu.org/software/emacs/emacs.html", "
     and the software can be obtained from one of the ftp sites listed
     at ", HREF "http://www.gnu.org/order/ftp.html", "; the primary ftp
     site is ", HREF "ftp://ftp.gnu.org/pub/gnu", ".",
     PARA,
     "There is a version of emacs for Windows NT and Windows 95 called ", TT "NTemacs", ".
     See ", HREF "http://www.cs.washington.edu/homes/voelker/ntemacs.html", " for
     details about how to get it, as well as information about how to swap your
     caps lock and control keys.",
     PARA,
     MENU {
	  TO "running Macaulay 2 in emacs",
	  TO "editing Macaulay 2 code with emacs",
	  },
     }

document { "running Macaulay 2 in emacs",
-- don't indent
"Because some answers can be very wide, it is a good idea to run Macaulay 2
in a window which does not wrap output lines and allows the
user to scroll horizontally to see the rest of the output.  We
provide a package for ", TO "emacs", " which implements this, in
", TT "emacs/M2.el", ".  It also provides for dynamic completion
of symbols in the language.",
PARA,
"There is an ASCII version of this section of the documentation distributed
in the file ", TT "emacs/emacs.hlp", ".  It might be useful for you to visit
that file with emacs now, thereby avoiding having to cut and paste bits of
text into emacs buffers for the deomonstrations below.",
PARA,
"If you are a newcomer to emacs, start up emacs with the command 
", TT "emacs", " and then start up the emacs tutorial with the keystrokes 
", TT "C-H t", ".  (The notation ", TT "C-H", " indicates that you should type 
", TT "Control-H", ", by holding down the control key, 
and pressing ", TT "H", ".)  The emacs tutorial will introduce you to the
basic keystrokes useful with emacs.  After running through that you will want
to examine the online emacs manual which can be read with ", TT "info", "
mode; you may enter or re-enter that mode with the keystrokes ", TT "C-H i", ".  
You may also want to purchase (or print out) the emacs manual.  It is cheap,
comprehensive and informative.  Once you have spent an hour with the emacs
tutorial and manual, come back and continue from this point.",
PARA,
"Edit your ", TT ".emacs", " initialization file, located in your home directory,
creating one if necessary.  (Under Windows, this file is called ", TT "_emacs", ".)
Insert into it the following lines of emacs-lisp code.",
PARA,
CODE ///(setq auto-mode-alist (append auto-mode-alist '(("\\.m2$" . M2-mode))))
(autoload 'M2-mode "M2-mode.el" "Macaulay 2 editing mode" t)
(global-set-key "\^Cm" 'M2) (global-set-key [ f12 ] 'M2)
(global-set-key "\^Cm" 'M2) (global-set-key [ SunF37 ] 'M2)
(autoload 'M2 "M2.el" "Run Macaulay 2 in a buffer." t)
(setq load-path (cons "/usr/local/Macaulay2/emacs" load-path))
(make-variable-buffer-local 'transient-mark-mode)
(add-hook 'M2-mode-hook '(lambda () (setq transient-mark-mode t)))
(add-hook 'comint-M2-hook '(lambda () (setq transient-mark-mode t)))///,
PARA,
"The first two lines cause emacs to enter a special mode for editing Macaulay 2
code whenever a file whose name has the form ", TT "*.m2", " is encountered.  
The next three lines provide a special mode for running Macaulay 2 in an emacs buffer.
The sixth line tells emacs where to find the emacs-lisp files provided in the
Macaulay 2 emacs directory - you must edit the string in that line to
indicate the correct path on your system to the Macaulay 2 emacs directory.
The files needed from that directory are ", TT "M2-mode.el", ",
", TT "M2-symbols.el", ", and ", TT "M2.el", ".  The seventh line sets
the variable ", TT "transient-mark-mode", " so that it can
have a different value in each buffer.  The eighth and ninth lines set
hooks so that ", TT "transient-mark-mode", " will be set to ", TT "t", " 
in M2 buffers.  The effect of this is that the mark is only active occasionally,
and then emacs functions which act on a region of text will refuse to proceed 
unless the mark is active.  The ", TT "set-mark", " function or the
", TT "exchange-point-and-mark", " function will activate the mark, and it
will remain active until some change occurs to the buffer.  The only reason
we recommend the use of this mode is so the same key can be used to evaluate 
a line or a region of code, depending on whether the region is active.",
PARA,
"Exit and restart emacs with your new initialization file.  
If you are reading this file with emacs, then use the keystrokes
", TT "C-x 2", " to divide the buffer containing this file into two windows.
Then press the ", TT "F12", " function key to start up 
Macaulay 2 in a buffer named ", TT "*M2*", ".",
PARA,
"If this doesn't start up Macaulay 2, one reason may be that your function
keys are not operable.  In that case press ", TT "C-C m", " instead.  (The 
notation ", TT "C-C", " is standard emacs notation for Control-C.)  Another
reason may be that you have not installed Macaulay 2 properly - the executables
should be on your path.",
PARA,
"You may use ", TT "C-x o", " freely to switch from one window to the other.
Verify that Macaulay 2 is running by entering a command such as ", TT "2+2", ".  
Now paste the following text into a buffer, unless you have the ASCII
version of this documentation in an emacs buffer already, position
the cursor on the first line of code, and press the ", TT "F11", " function 
key (or ", TT "C-C s", ") repeatedly to present each line to Macaulay 2.",
PARA,
CODE ///i1 = R = ZZ/101[x,y,z]
i2 = f = symmetricPower(2,vars R)
i3 = M = cokernel f
i4 = C = resolution M
i5 = betti C///,
PARA,
"Notice that the input prompts are not submitted to Macaulay 2.",
PARA,
"Here is a way to conduct a demo of Macaulay 2 in which the code to be
submitted is not visible on the screen.  Paste the following text into
an emacs buffer.",
PARA,
CODE ///20!
4 + 5 2^20
-- that's all folks!///,
PARA,
"Press ", TT "M-F11", " with your cursor in this buffer to designate it as
the source for the Macaulay 2 commands.  (The notation ", TT "M-F11", " means 
that while holding the ", TT "Meta", " key down, you should press the ", TT "F11", " 
key.  The Meta key is the Alt key on some keyboards, or it can be simulated by 
pressing Escape (just once) and following that with the key you wanted to press 
while the meta key was held down.)  Then position your cursor (and thus the 
emacs point) within the line containing ", TT "20!", ".  Now press ", TT "M-F12", "
to open up a new frame called ", TT "DEMO", " for the ", TT "*M2*", " window with
a large font suitable for use with a projector, and with your cursor in that
frame, press ", TT "F11", " a few times to conduct the demo.  (If the font or frame is the
wrong size, you may have to create a copy of the file ", TT "M2.el", "
with a version of the function ", TT "M2-demo", " modified to fit your screen.)",
PARA,
"One press of ", TT "F11", " brings the next line of code forward into the
", TT "*M2*", " buffer, and the next press executes it.  Use ", TT "C-x 5 0", " 
when you want the demo frame to go away.",
PARA,
"There is a way to send a region of text to Macaulay 2: simply select a region
of text, making sure the mark is active (as described above) and press ", TT "F11", ".
Try that on the list below; put it into an emacs buffer, move your cursor to the 
start of the list, press ", TT "M-C-@", " or ", TT "M-C-space", " to mark the list, 
and then press ", TT "F11", " to send it to Macaulay 2.  (The notation ", TT "M-C-@", " 
means: while holding down the Meta key and the Control key press the ", TT "@", " key, 
for which you'll also need the shift key.)",
PARA,
CODE ///{a,b,c,d,e,f,
g,h,i,j,k,l,
m,n}///,
PARA,
"We have developed a system for incorporating Macaulay 2 interactions into TeX
files.  Here is an example of how that looks.  Paste the following text
into an emacs buffer.",
PARA,
CODE ///The answer, 4, is displayed after the output label ``{\tt o1\ =}''.
Multiplication is indicated with the traditional {\tt *}.
<<<1*2*3*4>>>
Powers are obtained as follows.
<<<2^100>>>///,
PARA,
"The bits in brackets can be submitted to Macaulay 2 easily.  Position your
cursor at the top of the buffer and press ", TT "F10.", "  The cursor will move 
just past the first ", TT "<<<", ", and the emacs mark will be positioned just 
before the ", TT ">>>", ".  Thus ", TT "1*2*3*4", " is the region, and it will
even be highlighted if you have set the emacs variable ", TT "transient-mark-mode", "
to ", TT "t", " for this buffer.  Pressing ", TT "F11", " will send ", TT "1*2*3*4", " 
to Macaulay 2 for execution: try it now.  A sequence of such Macaulay 2 commands 
can be executed by alternately pressing ", TT "F10", " and ", TT "F11", ".  You may
also use ", TT "M-F10", " to move backward to the previous bracketed expression.",
PARA,
"Now let's see how we can handle wide and tall Macaulay 2 output.  Execute the
following line of code.",
PARA,
CODE ///random(R^20,R^{6:-2})///,
PARA,
"Notice that the long lines in the Macaulay 2 window, instead of being wrapped
around to the next line, simply disappear off the right side of the screen,
as indicated by the dollar signs in the rightmost column.  Switch to the
other window and practice scrolling up and down with ", TT "M-v", " and ", TT "C-v", ", 
and scrolling left and right with the function key ", TT "F3", " (or ", TT "C-C <", ") 
and the function key ", TT "F4", " (or ", TT "C-C >", ").  Notice how the use of
", TT "C-E", " to go to the end of the line
sends the cursor to the dollar sign at the right hand side of the screen;
that's where the cursor will appear whenever you go to a position off the
screen to the right.  Then use the ", TT "F2", " function key (or ", TT "C-C .", ") to 
scroll the text so the cursor appears at the center of the screen.  Use ", TT "C-A", " to 
move to the beginning of the line and then the ", TT "F2", " function key 
(or ", TT "C-C .", ") to bring the left margin back into view.",
PARA,
"You may use the ", TT "F5", " function key or (or ", TT "C-C ?", ") to 
toggle whether long lines are truncated or wrapped; initially they are truncated.",
PARA,
"Now go to the very end of the ", TT "*M2*", " buffer with ", TT "M->", " and 
experiment with keyword completion.  Type ", TT "reso", " and then press the 
", TT "TAB", " key.  Notice how the word is completed to ", TT "resolution", "
for you.  Delete the word with ", TT "M-DEL", ", type ", TT "res", "
and then press the ", TT "TAB", " key.  The possible completions are displayed 
in a window.  Switch to it with the ", TT "F8", " key, move to the desired 
completion, select it with the ", TT "RETURN", " key, and then return to the 
", TT "*M2*", " buffer with ", TT "C-X o", ".  Alternatively, if you have a
mouse, use the middle button to select the desired completion.",
PARA,
"Experiment with command line history in the ", TT "*M2*", " buffer.  Position 
your cursor at the end of the buffer, and then use ", TT "M-p", " and ", TT "M-n", " 
to move to the previous and next line of input remembered in the history.  When you 
get to one you'd like to run again, simply press return to do so.  Or edit it
slightly to change it before pressing return."
}

document { "editing Macaulay 2 code with emacs",
-- don't indent
"In this section we learn how to use emacs to edit Macaulay 2 code.  Assuming you
have set up your emacs init file as described in ", TO "running Macaulay 2 in emacs", "
when you visit a file whose name ends with ", TT ".m2", " 
you will see on the mode line the name ", TT "Macaulay 2", " in
parentheses, indicating that the file is being edited in Macaulay 2 mode.  (Make
sure that the file ", TT "emacs/M2-mode.el", " is on your ", TT "load-path", ".)",
PARA,
"To see how electric parentheses, electric semicolons, and indentation work,
move to a blank line of this file and type the following text.",
PARA,
CODE ///f = () -> (
     a := 4;
     b := {6,7};
     a+b)///,
PARA,
"Observe carefully how matching left parentheses are indicated briefly when a
right parenthesis is typed.",
PARA,
"Now position your cursor in between the 6 and 7.  Notice how
pressing ", TT "M-C-u", " moves you up out of the list to its left.  Do it 
again.  Experiment with ", TT "M-C-f", " and ", TT "M-C-b", " to move forward
and back over complete parenthesized
expressions.  (In the emacs manual a complete parenthesized expression is
referred to as an sexp, which is an abbreviation for S-expression.)  Try out
", TT "C-U 2 M-C-@", " as a way of marking the next two complete parenthesized
expression, and see how to use ", TT "C-W", " to kill them and ", TT "C-Y", " to yank 
them back.  Experiment with ", TT "M-C-K", " to kill the next complete parenthesized 
expression.",
PARA,
"Position your cursor on the 4 and observe how ", TT "M-;", " will start a comment 
for you with two hyphens, and position the cursor at the point where commentary
may be entered.",
PARA,
"Type ", TT "res", " somewhere and then press ", TT "C-C TAB", " to bring up the
possible completions of the word to documented Macaulay 2 symbols.",
PARA,
"Finally, notice how ", TT "C-H m", " will display the keystrokes peculiar to 
the mode in a help window."
}

document { quote oo,
     TT "oo", " -- denotes the value of the expression on the previous output
     line.",
     SEEALSO { "oo", "ooo", "oooo" }
     }

document { quote ooo,
     TT "ooo", " -- denotes the value of the expression on the output line
     two lines above.",
     SEEALSO { "oo", "oooo" }
     }

document { quote oooo,
     TT "oooo", " -- denotes the value of the expression on the output line
     three lines above.",
     SEEALSO { "oo", "ooo" }
     }

document { quote InverseMethod,
     TT "InverseMethod", " -- a key used under which is stored a method
     for computing multiplicative inverses.",
     PARA,
     "Internal routines for computing powers call upon that method when
     the exponent is negative."
     }

document { quote or,
     TT "t or u", " -- returns true if ", TT "t", " is true or ", TT "u", "
     is true.",
     PARA,
     "If ", TT "t", " is true, then the code in ", TT "u", " is not evaluated.",
     SEEALSO{ "and", "not" }
     }

document { quote and,
     TT "t and u", " -- returns true if ", TT "t", " is true and ", TT "u", "
     is true.",
     PARA,
     "If ", TT "t", " is false, then the code in ", TT "u", " is not evaluated.",
     SEEALSO{ "or", "not" }
     }

document { quote locate,
     TT "locate f", " -- for an interpreted function ", TT "f", " 
     returns a sequence ", TT "(n,i,j)", " describing the location of
     the source code.  The name of the source file is ", TT "n", " and
     the code is occupies lines ", TT "i", " through ", TT "j", "."
     }

document { quote MutableHashTable,
     TT "MutableHashTable", " -- the class of all mutable hash tables.",
     PARA,
     "A mutable hash table is a type of hash table whose entries can be changed.",
     PARA,
     "Normally the entries in a mutable hash table are not printed, to prevent
     infinite loops in the printing routines.  To print them out, use 
     ", TO "peek", ".",
     EXAMPLE {
	  "x = new MutableHashTable",
      	  "scan(0 .. 30, i -> x#i = i^2)",
      	  "x # 20",
      	  "x #? 40",
	  },
     SEEALSO "HashTable"
     }

document { quote map,
     TT "map(Y,X,d)", " -- constructs a map to ", TT "Y", " from ", TT "X", " defined by data ", TT "d", ".",
     PARA,
     "This is intended to be a general mechanism for constructing maps
     (homomorphisms) between objects in various categories.",
     PARA,
     "Installed methods:",
     MENU {
	  (TO "making ring maps"),
	  (TO "making module maps")
	  }
     }
document { quote precedence,
     TT "precedence x", " -- returns the parsing precedence of ", TT "x", " for use in
     the printing routines.",
     PARA,
     SEEALSO {"Expression", "net", "name"}
     }

document { quote hashTable,
     TT "hashTable v", " -- produce a hash table from a list ", TT "v", " of key-value
     pairs.",
     PARA,
     "The pairs may be of the form ", TT "a=>b", ", ", TT "{a,b}", ",
     or ", TT "(a,b)", ".",
     PARA,
     EXAMPLE {
	  "x = hashTable {a=>b, c=>d}",
      	  "x#a"
	  },
     }

document { quote toList,
     TT "toList x", " -- yields a list of the elements in a list, sequence,
     or set ", TT "x", ".",
     PARA,
     "This is a good way to convert a list of some type to a list of type
     ", TO "List", ".",
     EXAMPLE {
	  "x = set {a,b,c,d}",
      	  "toList x"
	  },
     }

document { quote saturate,
    TT "saturate(I,J,options)", " -- computes the saturation ", TT "(I : J^*)", " 
    of I with respect to ", TT "J", ".  If ", TT "J", " is not given, the 
    ideal ", TT "J", " is taken to be the ideal generated by the variables of 
    the ring ", TT "R", " of ", TT "I", ".",
    BR,NOINDENT,
    TT "saturate(Ideal)",
    BR,NOINDENT,
    TT "saturate(Ideal,Ideal)",
    BR,NOINDENT,
    TT "saturate(Ideal,RingElement)",
    BR,NOINDENT,
    TT "saturate(Module)",
    BR,NOINDENT,
    TT "saturate(Module,Ideal)",
    BR,NOINDENT,
    TT "saturate(Module,RingElement)",
    BR,NOINDENT,
    TT "saturate(Vector)",
    PARA,
    "If I is either an ideal or a submodule of a module M,
    the saturation (I : J^*) is defined to be the set of elements
    f in the ring (first case) or in M (second case) such that
    J^N * f is contained in I, for some N large enough.",
    PARA,
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
    "Allowable options include:",
    MENU {
        TO (saturate => DegreeLimit),
	TO (saturate => Strategy),
	TO (saturate => MinimalGenerators)
        },
    PARA,
    "The computation is currently not stored anywhere: this means
    that the computation cannot be continued after an interrupt.
    This will be changed in a later version."
    }

document { saturate => Strategy,
     "The strategy option value should be one of the following:",
    MENU {
        (TO "Linear", "      -- use the reverse lex order"),
	(TO "Iterate", "     -- use successive ideal quotients (the default)"),
	(TO "Bayer", "       -- use the method in Bayer's thesis"),
	(TO "Elimination", " -- compute the saturation ", TT "(I:f)", " by eliminating ", TT "z", " from ", TT "(I,f*z-1)", "")
        },
     }


document { saturate => DegreeLimit,
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "saturate", " which specifies that the computation should halt after dealing 
     with degree n."
     }

document { quote profile,
     TT "f = profile f", " -- replace a global function f by a profiled version.",
     PARA,
     "The new function is the same as the old one, except that when
     the new function is run, it will record the number of times it
     is called and the total execution time.  Use ", TO "profileSummary", "
     to display the data recorded so far."
     }

document { quote profileSummary,
     TT "profileSummary", " -- a command which will display the data
     accumulated by running functions produced with ", TO "profile", "."
     }

document { quote globalAssignFunction,
     TT "globalAssignFunction", " -- the standard function which can be used
     as a method for ", TO GlobalAssignHook, " so that certain types of
     mutable hash tables ", TT "X", ", when assigned to a global variable, will acquire
     the name of the global variable as their name.  The companion function
     ", TO "globalReleaseFunction", " is used to release the name when the
     global variable gets reassigned.",
     PARA,
     "The current way this function works is by storing the string used for
     printing under ", TT "X.name", " and storing the global variable under
     ", TT "X.symbol", ".",
     PARA,
     "Another thing done by this function is to apply ", TO use, " to the thing.
     This is used for polynomial rings to assign values to the symbols representing
     the variables (indeterminates) in the ring.",
     PARA,
     EXAMPLE {
	  "X = new Type of MutableHashTable",
      	  "x = new X",
      	  "GlobalAssignHook X := globalAssignFunction",
      	  "GlobalReleaseHook X := globalReleaseFunction",
      	  "x' = new X",
      	  "t = {x,x'}",
      	  "x = x' = 44",
      	  "t",
      	  "code globalAssignFunction",
	  },
     SEEALSO { "name", "symbol", "SelfInitializingType" }
     }

document { quote globalReleaseFunction,
     TT "globalReleaseFunction", " -- the standard function which can be used as
     a method for ", TO GlobalReleaseHook, " so that certain types of things, which
     have acquired as their name the name of a global variable to which they have
     been assigned, will lose that name when a different value is assigned to
     the variable.",
     PARA,
     SEEALSO "globalAssignFunction"
     }

document { "MP: Multi Protocol",
     "Macaulay 2 incorporates code from the MP (Multi Protocol) subroutine
     library written by S. Gray, N. Kajler, and P. Wang.  The license is
     contained in its README file, part of which we provide in the file
     ", TT "Macaulay2/licenses/mp.lic", ".",
     PARA,
     "The implementation is only experimental at this stage.",
     PARA,
     MENU {
	  TO "PutAnnotationPacket",
	  TO "PutCommonMetaOperatorPacket",
	  TO "PutCommonMetaTypePacket",
	  TO "PutCommonOperatorPacket",
	  TO "PutOperatorPacket",
	  TO "WritePacket",
	  TO "closeLink",
	  TO "openLink",
	  TO "writeMessage",
	  TO "writePacket",
	  TO "writeRawPacket",
	  }
     }

document { quote PutAnnotationPacket,
     TT "PutAnnotationPacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote PutCommonMetaOperatorPacket,
     TT "PutCommonMetaOperatorPacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote PutCommonMetaTypePacket,
     TT "PutCommonMetaTypePacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote PutCommonOperatorPacket,
     TT "PutCommonOperatorPacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote PutOperatorPacket,
     TT "PutOperatorPacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote WritePacket,
     TT "WritePacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote closeLink,
     TT "closeLink", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote openLink,
     TT "openLink", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote writeMessage,
     TT "writeMessage", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote writePacket,
     TT "writePacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }
document { quote writeRawPacket,
     TT "writeRawPacket", " -- a function used for our experimental implementation of
     MP (Multi Protocol).",
     PARA,
     SEEALSO "MP: Multi Protocol"
     }

document { quote symbol,
     TT "symbol", " -- a symbol used as a key in a hash table under which to store
     the variable to which the hash table has been assigned as value.",
     PARA,
     "This is intended mainly for internal use.",
     PARA,
     SEEALSO "globalAssignFunction"
     }

document { quote Entity,
     TT "Entity", " -- the class of all entities, special typsettable objects which have
     different realizations in various typesetting systems.",
     PARA,
     "An example of an entity is ", TO "DownArrow", ", a downward pointing arrow.",
     EXAMPLE {
	  "DownArrow",
	  "peek (DownArrow,2)"
	  },
     "Here is a list of all entities.",
     MENU apply( select( values symbolTable(), v -> class value v === Entity ), v -> TO string v )
     }

document { quote netRows,
     TT "netRows x", " -- produces a list of strings, each containing the
     characters in one row of the ", TT "Net", " ", TT "x", ".",
     PARA,
     "The orginal net, adjusted so its height is 1, may be recovered
     with ", TO "stack", ". The individual strings will have 
     all trailing spaces removed, unless this would make all of them 
     narrower than the original net, in which case the first string
     retains its trailing spaces."
     }

-- these files are made at compile time
load "gbdoc.m2"
load "gbfunctions.m2"

