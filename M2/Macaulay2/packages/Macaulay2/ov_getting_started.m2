-- Nodes for the getting started section of the overview

document {
     Key => "getting started",
     PARA {
	  "Macaulay2 is available from our web page ", 
	  HREF "http://www.math.uiuc.edu/Macaulay2/",
	  " or from our sourceforge web site ",
	  HREF "http://sourceforge.net/projects/macaulay2",
	  ".  There you will find online documentation, 
	  the source code, and precompiled versions
	  for MacOSX, Linux, Microsoft Windows, and for 
	  various unix boxes."
	  },
     PARA {
	  "After you or your system administrator has installed
	  Macaulay2, use the information here to set up the emacs
	  interface (the recommended way to run Macaulay2),
	  bookmark the html documentation, and to try to
	  run Macaulay2.
	  "
	  },
     Subnodes => {
	  TOH "Checking your Macaulay2 installation",
	  TOH "Setting up the Macaulay2 emacs interface",
	  TOH "Using Macaulay2 with emacs",
	  TOH "Your first input prompt",
	  TOH "Finding documentation",
	  TOH "Getting help or reporting bugs",
	  TOH "What to read next?"
	  }
     }

document {
     Key => "Checking your Macaulay2 installation",
     Headline => "and bookmarking the Macaulay2 documentation",
     PARA {"At a command prompt in a terminal window, type ", TT "M2", 
	  ".  If Macaulay2 has been installed correctly on your machine, 
	  you should see a prompt such as this:"},
PRE///    indigo% M2
    Macaulay 2, version 1.0
    i1 : ///,
     PARA {"
	  If this is the first time that you are running Macaulay2, then Macaulay2
	  creates a directory ", TT ".Macaulay2", 
	  " in your home directory (on unix machines)
	  or ", TT "Library/Application Support/Macaulay2", " under MacOS X.  Inside this 
	  directory are several files of interest, including an ", 
	  TT "index.html", " file
          which contains links to your local copy of the Macaulay2 documentation.
	  "},
     PARA {"
	  The ", TO viewHelp, " command in Macaulay2 starts up your web browser (if it is not
	  already running) and places you at this ", TT "index.html", " page.
	  "},
PRE///    viewHelp///,
     PARA {"
	  This web page includes links to the main Macaulay2 documentation, as well as
	  documentation for any installed packages.   This is a good time to
	  bookmark the page in your browser.
	  "},
     PARA {"
	  At this point you should try something simple in Macaulay2, such as
	  "},
PRE///    printWidth = 60
    R = QQ[a..d]
    (a+b+c+d)^4///,
     PARA {"
	  To exit Macaulay2, type one of: exit, end, or quit.
	  "},
	  PRE///    exit///,
     PARA {"
	  Macaulay2 can be run in this way from the command line, but it is
	  generally much more convenient to run Macaulay2 from inside the emacs
	  text editor.  This is because you can more easily view larger objects,
	  do command completion, use cut and paste, search, save your session,
	  and so on.  There is a very nice mode for running Macaulay2 inside
	  emacs.
	  "},
     }

document {
     Key => "Setting up the Macaulay2 emacs interface",
     PARA {"If you are a newcomer to emacs, you should spend a few minutes
	  going through the emacs tutorial.  Start up emacs with the command 
	  ", TT "emacs", " and then start up the emacs tutorial with the keystrokes 
	  ", TT "C-H t", ".  (The notation ", TT "C-H", " indicates that you should type 
          ", TT "Control-H", ", by holding down the control key, 
	  and pressing ", TT "H", ".)  The emacs tutorial will introduce you to the
          basic keystrokes useful with emacs.  After running through that you will want
          to examine the online emacs manual which can be read with ", TT "info", "
          mode; you may enter or re-enter that mode with the keystrokes ", TT "C-H i", ".  "
          },
     PARA {"
	  The Macaulay2 emacs interface consists of several files
	  in the directory ", 
	  TT "Macaulay2/share/emacs/site-lisp/",
	  " in the Macaulay2 distribution tree.  In order for
	  emacs to be able to find these files, place the following lines
	  in the ", TT ".emacs", " file in your home directory.  If you don't have a 
	  ", TT ".emacs", " file, just make one and cut and paste these lines in.",
	  },
PRE///    ;; .emacs file in your home directory
    (setq load-path 
          (append
           '( "usr/local/Macaulay2/share/emacs/site-lisp/" )
           load-path
           ))

    (load "M2-init.el" t)

    ; comment out the following line with an initial semicolon 
    ; if you want to use your f12 key for something else
    ; or change f12 to (for example) f8
    (global-set-key [ f12 ] 'M2)
///,
     "where ", TT "/usr/local", " on the fourth line should be replaced by 
     the actual path to the installed Macaulay2.",
     PARA {"
	  After saving that file, try running emacs.  Start Macaulay2
	  by pressing the f12 function key.  On MacOS X systems, f12 is usurped
	  by either DashBoard, SpotLight, or something else, so either you must
	  change the f12 to some other key, e.g. f8 in the .emacs file, or disable
	  the systems use of the f12 key."
	  },
     PARA {"
	  If Macaulay2 doesn't start up (in a buffer named *M2*), check that you
	  typed in the above lines correctly into .emacs, and also check that
	  you have the correct path to the Macaulay2 emacs files."
	  },
     }

document {
     Key => "Using Macaulay2 with emacs",
     PARA {"
	  In this section, we show by example how to use the Macaulay2 emacs interface.
	  We assume that you have already set up this interface, as described in 
	  ", TO "Setting up the Macaulay2 emacs interface", ".  After creating or changing
	  the .emacs file mentioned there, you need to exit and restart emacs.  For the rest
	  of this section, we assume that you are running emacs."
	  },
     "The aspects of this interface that we describe include",
     UL {
	  "Starting Macaulay2 with the f12 key,  or with M-x M2",
	  "Working with 2 buffers",
	  "Sending lines or selected text to Macaulay2 using the f11 key",
	  "Command completion with TAB",
	  "Horizontal scrolling with f3,f4,f5,f6,f7"
	  },
     PARA {"Before starting, note that when we say to type M-x M2
	  what we really mean is to hold down the meta key (on Macs this is either
	       the option key or the apple key, depending on how your emacs is set up)
	  pressing the x key, and then type M2, and press the return after that.  Similarly, C-c
	  means hold down the control character and press the c key."},
     PARA {"Use the keystrokes
     	  ", TT "C-x 2", " to divide the buffer containing this file into two windows.
     	  Then press the f12 key or type ", TT "M-x M2", " to start up Macaulay 2 in a buffer
     	  named ", TT "*M2*", ".  (The command line used to start Macaulay 2 may
	  be edited before being run if you use a prefix argument with the above
	  command: press ", TT "C-u", " just before.)"
          },
     PARA {
          "If f12 doesn't start up Macaulay 2, one reason may be that your function
	  keys are not operable.  In that case press ", TT "C-C m", " instead. 
       	  Another
          reason may be that you have not installed Macaulay 2 properly - the startup
	  script (", TT "M2", " or ", TT "M2.bat", ") should be on your path.
	  A third reason may be that you are in Windows-98 and are using anti-virus 
	  software such as ", TT "Dr. Solomon's", ", which can interfere with emacs 
	  when it tries to run a subprocess."
	  },
     PARA {
     	  "You may use ", TT "C-x o", " freely to switch from one window to the other.
	  Verify that Macaulay 2 is running by entering a command such as ", TT "2+2", ".  
	  Now create (using C-x C-f) a file, named something like foo.m2 (the final .m2 is
	  important, as it informs emacs to use the Macaulay2 mode).
	  Paste the following text into a buffer.  If you wish, save the file using C-x C-s."
	  },
     CODE ///    R = QQ[x,y,z]
    f = symmetricPower(2,vars R)
    M = cokernel f
    C = resolution M
    betti C///,
     PARA {     
     "Position the cursor on the first line of code, and press the ", TT "f11", " function 
	  key repeatedly to present each line to Macaulay 2.  If you select several lines 
	  using the mouse, then pressing f11 will present the entire selection to
	  Macaulay2.  Try this on some of these lines."
	  },
     HR,
"Now go to the very end of the ", TT "*M2*", " buffer with ", TT "M->", " and 
experiment with keyword completion.  Type ", TT "reso", " and then press the 
", TT "TAB", " key.  Notice how the word is completed to ", TT "resolution", "
for you.  Delete the word with ", TT "M-DEL", ", type ", TT "res", "
and then press the ", TT "TAB", " key.  The possible completions are displayed 
in a window.  Switch to it with the ", TT "F8", " key, move to the desired 
completion, select it with the ", TT "RETURN", " key, and then return to the 
", TT "*M2*", " buffer with ", TT "C-X o", ".  Alternatively, if you have a
mouse, use the middle button to select the desired completion. (On the mac, hold down the
     option key while clicking the mouse)",
HR,
PARA,
"Experiment with command line history in the ", TT "*M2*", " buffer.  Position 
your cursor at the end of the buffer, and then use ", TT "M-p", " and ", TT "M-n", " 
to move to the previous and next line of input remembered in the history.  When you 
get to one you'd like to run again, simply press return to do so.  Or edit it
slightly to change it before pressing return.",
     HR,
     PARA,
     "Now let's see how we can handle wide and tall Macaulay 2 output.  Execute the
     following line of code (put it in your foo.m2 buffer, and then press f11)",
     PARA,
     CODE ///printWidth=0\nrandom(R^20,R^{6:-2})///,
     "Setting printWidth to zero removes line wrapping in the buffer, sometimes useful to 
     view large matrices.",
PARA,
"Notice that the long lines in the Macaulay 2 window, instead of being wrapped
around to the next line, simply disappear off the right side of the screen,
as indicated by the dollar signs or little arrows in the rightmost column.  Switch to the
other window and practice scrolling up and down with ", TT "M-v", " and ", TT "C-v", ", 
and scrolling left and right with the function key ", TT "F3", " (or ", TT "C-C <", ") 
and the function key ", TT "F4", " (or ", TT "C-C >", ").  In modern emacs implementations
where mouse clicking works, click on the arrow to scroll in that direction.  In
these versions of emacs, typing C-e, or C-a to get at the end or beginning of the line
also horizontally scrolls the text to that position.  Older emacs tend to need
a bit more:
Notice how the use of
", TT "C-e", " to go to the end of the line
sends the cursor to the dollar sign at the right hand side of the screen;
that's where the cursor will appear whenever you go to a position off the
screen to the right.  Then use the ", TT "f2", " function key (or ", TT "C-c .", ") to 
scroll the text so the cursor appears at the center of the screen.  Use ", TT "C-a", " to 
move to the beginning of the line and then the ", TT "f2", " function key 
(or ", TT "C-c .", ") to bring the left margin back into view.",
PARA,
"You may use the ", TT "f5", " function key or (or ", TT "C-c ?", ") to 
toggle whether long lines are truncated or wrapped; initially they are truncated.",
     SeeAlso => { "editing Macaulay 2 code with emacs" }
     }

document {
     Key => "Finding documentation",
     PARA {"The documentation for Macaulay 2 is available in several
     	  formats: text format while in Macaulay2, 
     	  in info format, and the most important, html format.
     	  "},
     "Functions for accessing the documentation:",
     UL {
	  TOH "apropos",
	  TOH "examples",
	  TOH "help", 
	  TOH "topics",
	  TOH "viewHelp"
	  },
     PARA {"
	  Type help to get a summary of the most useful ways of obtaining
	  help on a topic or function.
     	  "},
     PARA {"
	  While in Macaulay2, type",
	  PRE "    viewHelp",
	  " to start the web browser (if necessary) and to point it at
	  the page index.html in your .Macaulay2 directory.
	  For help on a specific topic, e.g. the jacobian function, use ",
	  PRE"viewHelp jacobian",
          " or ",
	  PRE///    viewHelp "jacobian"///,
	  " or if you want the documentation for jacobian of an Ideal, use ",
	  PRE ///    viewHelp (jacobian,Ideal)///
     	  },
     PARA {"
          Using 'help' instead of 'viewHelp' results in the help text appearing 
	  in your Macaulay2 session. ",
	  EXAMPLE ///help "jacobian"///,
	  "A useful tip: in emacs, if you place your cursor on one of the lines which starts with a '*',
	  and press return, then you will get help on that topic.",
	  EXAMPLE ///* "jacobian Ideal"///
     	  },	  
     PARA {"
	  The function ", TO apropos, " is useful to find functions and other defined symbols 
	  which match a search string.  For example, to find all symbols
	  in Macaulay2 which contain the string 'deal', use",
	  EXAMPLE ///apropos "deal"///
     	  },
     PARA {"
          The documentation for most functions comes with example code.  You can 
	  obtain the text of this example code using ", TO examples, ".",
	  EXAMPLE ///examples "jacobian Ideal"///,
     	  "which returns it as a string. To place these on their own lines, print the string.",
	  EXAMPLE ///print examples "jacobian Ideal"///
     	  },
     }


document {
     Key => "Getting help or reporting bugs",
     PARA {"
	  An easy way to get help or to report a bug is to go to ", HREF "http://sourceforge.net/projects/macaulay2",
	  " and choose the appropriate section in the 'Public areas' part of that webpage.  In order to
	  submit a feature request, support request, bug report, etc, you might need to create a 
	  sourceforge user id for yourself.  This is free and easy (Choose 'create account' near the top of
     	  the top of the page).
	  "},
     PARA "These requests are automatically emailed to Dan Grayson and Mike Stillman, and they 
     try to handle these requests quickly."
     }

document {
     Key => "What to read next?",
     }


document {
     Key => "Invoking the program",
     "On systems with a command line interface, the following commands
     can be used to start the program.  When the program starts up,
     the ", TO "initialization file", ", ", TT "init.m2", ", will be loaded.",
     PARA, NOINDENT,
     TT "M2", " -- starts the program.",
     PARA, NOINDENT,
     TT "M2 file1 file2 ... ", " -- starts the program, reading and 
     executing the specified files.",
     PARA,
     "The options that can be provided on the command line may be displayed by running ", TT "M2 --help", ", as follows.",
     EXAMPLE ///run "M2 --help";///,
     PARA,
     "To terminate the program, one may type ", TO "exit", ", ", TO "quit", ",
     ", TO "end", ", or the end of file character."
     }

document {
     Key => "Your first input prompt",
     PARA {
	  "Your first input prompt will be ", TT "i1 : ", ".  In response to the prompt,
	  type ", TT "2+2", " and press return.  The expression you entered will be
	  evaluated - no punctuation is required at the end of the line."},
     EXAMPLE "2+2",
     "The answer is displayed to the right of the output label ", TT "o1 =", ".",
     PARA,
     "Here is some arithmetic with fractions.",
     EXAMPLE "3/5 + 7/11",
     PARA {"Notice the additional line of output labeled with ", TT "o2 :", ".  Output 
	  lines labeled with colons provide information about the type of output.  In 
	  this case, the symbol ", TO "QQ", " is our notation for the class of all 
	  rational numbers, and indicates that the answer on the previous line is a 
	  rational number."},
     PARA,
     "Multiplication is indicated with ", TO "*", ".",
     EXAMPLE "1*2*3*4",
     "Powers are obtained with ", TO "^", ".",
     EXAMPLE "2^200",
     "Factorials are obtained with ", TO "!", ".",
     EXAMPLE "40!",			  -- this is o4 and is retrieved below
     "Because some answers can be very long, it is a good idea to run the
     program in a window which does not wrap output lines, and allows the
     user to scroll left horizontally to see the rest of the output.  (See
	  ", TO "emacs", ".)",
     EXAMPLE "100!",
     "Multiple expressions may be separated by semicolons.",
     EXAMPLE "1;2;3*4",
     "A semicolon at the end of the line suppresses the printing of the value.",
     EXAMPLE "4*5;",
     "The output from the previous line can be obtained with ", TO "oo", ", even if 
     a semicolon prevented it from being printed.",
     EXAMPLE "oo",
     "Lines before that can be obtained with ", TO "ooo", " and ", TO "oooo", ".  
     Alternatively, the symbol labeling an output line
     can be used to retrieve the value, as in the following example.",
     EXAMPLE "o5 + 1",
     "To enter a string, use quotation marks.",
     EXAMPLE "\"hi there\"",
     "A value can be assigned to a variable with ", TO "=", ".",
     EXAMPLE "s = \"hi there\"",
     "Strings may be concatenated horizontally with ", TT "|", ", (see 
     ", TO (symbol |, String, String), ").",
     EXAMPLE "s | \" - \" | s",
     "or vertically with ", TT "||", ", (see ", TO (symbol ||, Net, Net), ").",
     EXAMPLE "s || \" - \" || s",
     "A list of expressions can be formed with braces.",
     EXAMPLE "{1, 2, s}",
     "Lists behave like vectors.",
     EXAMPLE "10*{1,2,3} + {1,1,1}",
     "A function can be created with the arrow operator, ", TO "->", " .",
     EXAMPLE "f = i -> i^3",
     "To evaluate a function, place its argument to the right of the function.",
     EXAMPLE "f 5",
     "Functions of more than one variable take a parenthesized sequence of arguments.",
     EXAMPLE {
	  "g = (x,y) -> x * y",
      	  "g(6,9)",
	  },
     "The function ", TO "apply", " can be used to apply a function to each element of a list.",
     EXAMPLE {
	  "apply({1,2,3,4}, i -> i^2)",
      	  "apply({1,2,3,4}, f)",
	  },
     "The operator ", TO "..", " may be used to generate sequences of consecutive numbers.",
     EXAMPLE "apply(1 .. 4, f)",
     "If the first argument to ", TT "apply", " is an integer ", TT "n", " then it stands for the list ", TT "{0, 1, ..., n-1}", ".",
     EXAMPLE "apply(5, f)",
     "The function ", TO "scan", " is analogous to ", TO "apply", " except that no value is returned.  It may be used to implement loops in
     	  programs.",
     EXAMPLE {
	  "scan(5, i -> print (i, i^3))",
	  "j=1; scan(10, i -> j = 2*j); j"},
     "Most computations with polynomials take place in rings that may be specified in usual mathematical notation.",
     EXAMPLE "R = ZZ/5[x,y,z];",
     "(We reserve single letter symbols such as ", TT "Z", " for use as variables in rings,
     hence we must use something like ", TO "ZZ", " to stand for the ring of integers.
     It may remind you of the \"blackboard bold\" font of AMSTeX.  If you prefer
     ", TT "Z", " to ", TO "ZZ", ", you may put ", TT "Z=ZZ", " in your
     ", TO "initialization file", ".  The symbols ", TT "ZZ/5", "
     represent the quotient ring ", TT "Z/5Z", ", and then ", TT "ZZ/5[x,y,z]", "
     represents the ring of polynomials in the variables x,y, and z with coefficients 
     in the ring ", TT "Z/5Z", ".)",
     EXAMPLE "(x+y)^5",
     "Rings and certain other types of things acquire the name of the global variable they are assigned to.",
     EXAMPLE "R",
     "To see the original description of a ring, use ", TO "describe", ".",
     EXAMPLE "describe R",
     "A free module can be created as follows.",
     EXAMPLE "F = R^3",
     "The i-th basis element of ", TT "F", " can be obtained as ", TT "F_i", ".  In this example, the valid values for ", TT "i", " are 0, 1, and 2.",
     EXAMPLE "F_1",
     "Using a list of indices instead will produce the homomorphism corresponding to the basis vectors indicated.",
     EXAMPLE "F_{1,2}",
     "Repetitions are allowed.",
     EXAMPLE "F_{2,1,1}",
     "We can create a homomorphism between free modules with ", TO "matrix", "
     by providing the list of rows of the matrix, each of which is in turn
     a list of ring elements.",
     EXAMPLE "f = matrix {{x,y,z}}",
     "Use ", TO "image", " to get the image of f.",
     EXAMPLE "image f",
     "We may use ", TO "ideal", " to produce the corresponding ideal.",
     EXAMPLE "ideal (x,y,z)",
     "We may use ", TO "kernel", " to compute the kernel of f.",
     EXAMPLE "kernel f",
     "The answer comes out as a module which is expressed as the image of
	  a homomorphism whose matrix is displayed.  In case the matrix itself
	  is desired, it can be obtained with ", TO "generators", ".",
     EXAMPLE "generators oo",
     "We may use ", TO "poincare", " to compute the Poincare polynomial.",
     EXAMPLE "poincare kernel f",
     "We may use ", TO "rank", " to compute the rank.",
     EXAMPLE "rank kernel f",
     "A presentation for the kernel can be obtained with ", TO "presentation", ".",
     EXAMPLE "presentation kernel f",
     "We can produce the cokernel with ", TO "cokernel", "; no computation is performed.",
     EXAMPLE "cokernel f",
     "The direct sum is formed with ", TO (symbol ++,Module,Module), ".",
     EXAMPLE "N = kernel f ++ cokernel f",
     "The answer is expressed in terms of the ", TO "subquotient", " function, which
	  produces subquotient modules.  Each subquotient module is accompanied
	  by its matrix of generators and its matrix of relations.  These matrices
	  can be recovered with ", TO "generators", " and ", TO "relations", ".",
     EXAMPLE {
	  "generators N",
      	  "relations N",
	  },
     "The function ", TO "prune", " can be used to convert a subquotient
     	  module to a quotient module.",
     EXAMPLE "prune N",
     "We can use ", TO "resolution", " to compute a projective resolution of the 
     	  cokernel of ", TT "f", ".",
     EXAMPLE "C = resolution cokernel f",
     "To see the differentials we examine 'C.dd'.",
     EXAMPLE "C.dd",
     "We can verify that ", TT "C", " is a complex by squaring the differential map.",
     EXAMPLE "C.dd^2 == 0",
     "We can use ", TO "betti", " to see the degrees of the components of C.",
     EXAMPLE "betti C",
     "Let's try a harder example.  We can use ", TO "vars", " to create a sequence
     	  of variables.",
     EXAMPLE "R = ZZ/101[a .. r];",
     "We use ", TO "genericMatrix", " to make a 3 by 6 generic matrix whose
     	  entries are drawn from the variables of the ring ", TT "R", ".",
     EXAMPLE "g = genericMatrix(R,a,3,6)",
     "Then we construct its cokernel with ", TO "cokernel", ".",
     EXAMPLE "M = cokernel g",
     "We may use ", TO "resolution", " to produce a projective resolution of it, and
     	  ", TO "time", " to report the time required.",
     EXAMPLE "time C = resolution M",
     "As before, we may examine the degrees of its components, or display it.",
     EXAMPLE "betti C",
     "We can make a polynomial ring with 18 ", TO "IndexedVariable", "s.",
     EXAMPLE "S = ZZ/101[t_1 .. t_9, u_1 .. u_9];",
     "We can use ", TO "genericMatrix", " to pack the variables into 
     	  3-by-3 matrices.",
     EXAMPLE {
	  "m = genericMatrix(S, t_1, 3, 3)",
      	  "n = genericMatrix(S, u_1, 3, 3)",
	  },
     "We may look at the matrix product.",
     EXAMPLE "m*n",
     "Let's produce the equations generated by the equations which assert
     	  that m and n commute with each other.  (See ", TO "flatten", ".)",
     EXAMPLE "j = flatten(m*n - n*m)",
     "Let's compute a Groebner basis for the image of ", TT "j", " with ", TO "gb", ".",
     EXAMPLE "gb j",
     "The resulting Groebner basis contains a lot of information.
	  We can get the generators of the basis, and even though we call upon
	  ", TO "gb", " again, the computation will not be repeated.",
     EXAMPLE "generators gb j;",
     "The semicolon prevents the matrix of generators from appearing on the 
	  screen, but the class of the matrix appears -- we see that there are 26
	  generators.",
     PARA,
     "We can use ", TO "betti", " to see the degrees involved in the Groebner basis.",
     EXAMPLE "betti gb j"
     }

document {
     Key => "running Macaulay 2 in emacs",
-- don't indent
"Because some answers can be very wide, it is a good idea to run Macaulay 2
in a window which does not wrap output lines and allows the
user to scroll horizontally to see the rest of the output.  We
provide a package for ", TO "emacs", " which implements this, in
", TT "emacs/M2.el", ".  It also provides for dynamic completion
of symbols in the language.",
PARA,
"There is an ASCII version of this section of the documentation distributed
in the file ", TT "emacs/emacs-hlp.txt", ".  It might be useful for you to visit
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
creating one if necessary.  (Under Windows, this file might also be called
", TT "_emacs", ", and your home directory is the one where the environment 
variable ", TT "HOME", " says it is, if that variable has been set.  If it has not
been set, the value of the registry entry named HKEY_CURRENT_USER\\SOFTWARE\\GNU\\Emacs 
will be used, if set.  The default if neither of those is set is C:/.  In 
any case, the name of the home directory is abbreviated to a single
tilde ", TT "~", " under emacs.)  Insert into your initialization
file the following line of emacs-lisp code (which can also be found
in the file ", TT "emacs/emacs-hlp.txt", ", or obtained by running
", TT ///help "running Macaulay 2 in emacs"///, " in Macaulay 2).",
PARA,
CODE ///(load "/usr/local/Macaulay2-0.9/lib/Macaulay2-0.9/emacs/M2-init.el")///,
PARA,
"Now edit that line and replace the path by the correct path to Macaulay2's
files on your system.  To find out what that path is, evaluate the variable 
", TT ///sourceHomeDirectory///, " in Macaulay 2.  Then append 
", TT "emacs/M2-init.el", " to the value returned and incorporate it into 
the ", TT "load", " command above.",
PARA,
"Loading the file will cause emacs to enter a special mode for editing
Macaulay 2 code whenever a file whose name has the form ", TT "*.m2", " is
encountered.  It will also provide a special mode for running Macaulay 2 in
an emacs buffer.  It sets the variable ", TT "transient-mark-mode", " to have
a different value in each buffer, and sets hooks so that 
", TT "transient-mark-mode", " will be set to ", TT "t", " in M2 buffers.  The
effect of this is that the mark is only active occasionally, and then emacs
functions which act on a region of text will refuse to proceed unless the
mark is active.  The ", TT "set-mark", " function or the 
", TT "exchange-point-and-mark", " function will activate the mark, and it will
remain active until some change occurs to the buffer.  The only reason we
recommend the use of this mode is so the same key can be used to evaluate a
line or a region of code, depending on whether the region is active.",
PARA,
"Exit and restart emacs with your new initialization file.  
If you are reading this file with emacs, then use the keystrokes
", TT "C-x 2", " to divide the buffer containing this file into two windows.
Then press the ", TT "M-x M2", " to start up Macaulay 2 in a buffer
named ", TT "*M2*", ".  (The command line used to start Macaulay 2 may
be edited before being run if you use a prefix argument with the above
command: press ", TT "C-U", " just before.)",
PARA,
"If this doesn't start up Macaulay 2, one reason may be that your function
keys are not operable.  In that case press ", TT "C-C m", " instead.  (The 
notation ", TT "C-C", " is standard emacs notation for Control-C.)  Another
reason may be that you have not installed Macaulay 2 properly - the startup
script (", TT "M2", " or ", TT "M2.bat", ") should be on your path.
A third reason may be that you are in Windows-98 and are using anti-virus 
software such as ", TT "Dr. Solomon's", ", which can interfere with emacs 
when it tries to run a subprocess.",
PARA,
"You may wish to bind the emacs function ", TT "M2-send-to-program", "
to a global keystroke for ease of use; this is done automatically for
in Macaulay 2 buffers.  For example, the following emacs code
will bind it to the function key ", TT "f11", ".",
PARA,
CODE ///(global-set-key [ f11 ] 'M2-send-to-program)
///,
PARA,
"You may use ", TT "C-x o", " freely to switch from one window to the other.
Verify that Macaulay 2 is running by entering a command such as ", TT "2+2", ".  
Now paste the following text into a buffer, unless you have the ASCII
version of this documentation in an emacs buffer already, position
the cursor on the first line of code, and press the ", TT "f11", " function 
key repeatedly to present each line to Macaulay 2.",
PARA,
CODE ///i1 : R = ZZ/101[x,y,z]
i2 : f = symmetricPower(2,vars R)
i3 : M = cokernel f
i4 : C = resolution M
i5 : betti C///,
PARA,
"Notice that the input prompts are not submitted to Macaulay 2.",
PARA,
"Here is a way to conduct a demo of Macaulay 2 in which the code to be
submitted is not visible on the screen.  Paste the following text into
an emacs buffer.",
PARA,
CODE ///20!
4 + 5 * 2^20
-- that's all folks!///,
PARA,
"Press ", TT "M-f11", " with your cursor in this buffer to designate it as
the source for the Macaulay 2 commands.  (The notation ", TT "M-f11", " means 
that while holding the ", TT "Meta", " key down, you should press the ", TT "f11", " 
function key.  The Meta key is the Alt key on some keyboards, or it can be simulated by 
pressing Escape (just once) and following that with the key you wanted to press 
while the meta key was held down.)  Then position your cursor (and thus the 
emacs point) within the line containing ", TT "20!", ".  Now press 
", TT "M-x M2-demo", " to open up a new frame called ", TT "DEMO", "
for the ", TT "*M2*", " window with a large font suitable for use with
a projector, and with your cursor in that frame, press ", TT "f11", "
a few times to conduct the demo.  (If the font or frame is the wrong
size, you may have to create a copy of the file ", TT "M2.el", " with
a version of the function ", TT "M2-demo", " modified to fit your
screen.)",
PARA,
"One press of ", TT "f11", " brings the next line of code forward into the
", TT "*M2*", " buffer, and the next press executes it.  Use ", TT "C-x 5 0", " 
when you want the demo frame to go away.",
PARA,
"There is a way to send a region of text to Macaulay 2: simply select a region
of text, making sure the mark is active (as described above) and press ", TT "f11", ".
Try that on the list below; put it into an emacs buffer, move your cursor to the 
start of the list, press ", TT "M-C-@", " or ", TT "M-C-space", " to mark the list, 
and then press ", TT "f11", " to send it to Macaulay 2.  (The notation ", TT "M-C-@", " 
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
to ", TT "t", " for this buffer.  Pressing ", TT "f11", " will send ", TT "1*2*3*4", " 
to Macaulay 2 for execution: try it now.  A sequence of such Macaulay 2 commands 
can be executed by alternately pressing ", TT "F10", " and ", TT "f11", ".  You may
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
slightly to change it before pressing return.",
PARA,
"Assuming you have installed the ", TO "w3", " emacs web browser,
you may explore the documentation by positioning the cursor near a documented
word such as ", TT "List", " and pressing ", TT "C-C d", ".  Alternatively,
when the prompt appears, you can type the key whose documentation should be
found."
}

document {
     Key => "editing Macaulay 2 code with emacs",
-- don't indent
"In this section we learn how to use emacs to edit Macaulay 2 code.  Assuming you
have set up your emacs init file as described in ", TO "running Macaulay 2 in emacs", "
 when you visit a file whose name ends with ", TT ".m2", " 
you will see on the mode line the name Macaulay 2 in
parentheses, indicating that the file is being edited in Macaulay 2 mode.  (Make
sure that the file ", TT "emacs/M2.el", " is on your ", TT "load-path", ".)",
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
"Notice how ", TT "C-H m", " will display the keystrokes peculiar to 
the mode in a help window.",
PARA,
"Assuming you have installed the ", TO "w3", " emacs web browser,
you may explore the documentation by positioning the cursor near a documented
word such as ", TT "List", " and pressing ", TT "C-C d", ".  Alternatively,
when the prompt appears, you can type the key whose documentation should be
found."
}

document {
     Key => "How Macaulay 2 finds its files",
     "When you run Macaulay 2, it has to find and load a sequence of
     startup files containing code written in the Macaulay 2 language,
     or it has to find a data file containing a memory dump from a
     previous session.  Here is the way it does that.",
     PARA,
     "Its first task is to discover the path to the binary file ", TT "M2", " that is currently running.  On some systems, that
     information is available from the ", TT "/proc", " file system.  Otherwise, it examines the command name you used to run the
     program, which is provided to it as the argument in position number 0 on the command line.  If it's not an absolute path, it searches
     along the path of directories mentioned in the environment variable PATH until it finds a file with the same name.  If the
     result is a symbolic link, the link is followed.  The final
     result is assumed to be in a directory named \"", TT LAYOUT#"bin", "\", and the
     startup files are located relative to that.  The path to the top level directory is stored in the variable
     ", TO "prefixDirectory", ", which you can examine to see whether it all worked out.
     For detailed information about the relative location of Macaulay 2 files,
     see ", TO "LAYOUT", ".  Special arrangements are made during compilation to allow the program to be run and
     tested; see ", TO "buildHomeDirectory", " and ", TO "sourceHomeDirectory", ".",
     PARA,
     "A possible data memory dump file may be located in the directory ", TT LAYOUT#"cache", " and loaded with ", TO "loaddata", ".
     If the file is present and and loading it works, then startup will be quicker.  If it's absent then the necessary setup files will be loaded instead;
     if problems with it are encountered, it is always safe to delete it.
     The name of the file data dump file is of the form \"Macaulay2-*-data\",
     where \"*\" is replaced by the value of the environment
     variable name M2ARCH if present, or else is a value computed at compile time and
     stored in the hash table called ", TO "version", " and accessible as ", TT "version#\"architecture\"", ".",
     UL {
	  TOH "LAYOUT",
     	  TOH "prefixDirectory",
	  TOH "buildHomeDirectory",
	  TOH "sourceHomeDirectory"
	  }
     }


document {
     Key => "Reading the documentation",
     "The documentation for Macaulay 2 is available in several formats.
     The directory ", TT (LAYOUT#"packagehtml" "Macaulay2"), " 
     contains the documentation in html form, suitable for viewing with a web 
     browser, and this is the best way to view it.  The command ", TT "M2-help", "
     will start your favorite web browser and point direct it to the web 
     pages there.  Each documentation page has a text box for entering a
     search string.  This works by viewing the documentation at our web site.",
     PARA,
     "All the documentation can also be viewed within the program in
     text form using ", TO "help", ".",
     PARA,
     NOINDENT,
     "Functions for accessing the documentation:",
     UL {
	  TOH "apropos",
	  TOH "examples",
	  TOH "help", 
	  TOH "topics"
	  }
     }

document {
     Key => "emacs",
     "The best way to edit Macaulay 2 code or to run Macaulay 2 is
     with ", TO2{ "http://www.gnu.org/software/emacs/emacs.html", "GNU emacs"}, ", a versatile text 
     editor written by Richard Stallman.",
     PARA,
     "There is a version of emacs for Windows NT and Windows 95 called ", TT "NTemacs", ".
     See ", HREF "http://www.cs.washington.edu/homes/voelker/ntemacs.html", " for
     details about how to get it, as well as information about how to swap your
     caps lock and control keys.",
     PARA,
     Subnodes => {
	  TO "running Macaulay 2 in emacs",
	  TO "editing Macaulay 2 code with emacs",
	  },
     }



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:

end


Setting up the Macaulay2 emacs interface
----------------------------------------

is a file called "dot-emacs".  You should place this into your ".emacs"
file in your home directory.  If that file doesn't exist, then copy "dot-emacs"
to the file ".emacs" (in your home directory).

If you copied the "dot-emacs" file to the ".emacs" file in your home
directory, then your emacs interface should be all setup.  If not,
place the following lines into your .emacs file in your home
directory.  If this file does not exist, create it.

    ;; .emacs file in your home directory
    (setq load-path 
          (append
           '( "/Applications/Macaulay2/share/emacs/site-lisp/" )
           load-path
           ))

    (load "M2-init.el" t)

    ; comment out the following line with an initial semicolon 
    ; if you want to use your f12 key for something else
    (global-set-key [ f12 ] 'M2)

You should replace the path 
  "/Applications/Macaulay2/share/emacs/site-lisp/"  
with wherever Macaulay2 is stored on your machine.

Using the Macaulay2 emacs interface
-----------------------------------
A very convenient way to run Macaulay2 is inside of the emacs text editor.
We strongly recommend that you run Macaulay2 inside of emacs.  If you
are using a version of emacs that allows you to use the mouse, and click
on menu items, then emacs is quite straightforward to use.  If you are
using a version of emacs which runs in a terminal window, then it is
probably best to work through the emacs tutorial.


Inside of emacs, start Macaulay2 by pressing either the f12 key, or
typing M-x M2.  If Macaulay2 starts up, then you have set up your .emacs
file correctly.  If not, check to make sure that your .emacs file is in 
your home directory, and contains the correct path to the Macaulay2
emacs files.

Now create a new file in emacs whose name ends in .m2, e.g. foo.m2.  Inside
this file, put in the following lines

R = ZZ/101[x,y,z]
f = symmetricPower(2,vars R)
M = cokernel f
C = resolution M
betti C

Position your cursor to the first line, and press the f11 function key.
This line should appear in your Macaulay2 session (in a buffer named *M2*).
Pressing f11 sends the current line, or, if you have selected some text,
sends the current selection to be run by Macaulay2.



Documentation
-------------

Where to find help or report bugs
---------------------------------

