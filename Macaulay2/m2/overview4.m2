--		Copyright 1993-1999 by Daniel R. Grayson


document {
     "hypertext",
     "All the online documentation for Macaulay 2 is maintained in
     hypertext form in a special internal format which can be easily
     manipulated or examined.  The function ", TO "html", " can be used to 
     convert it to standard world-wide web format, suitable for use with
     a world-wide web server such as netscape.  The function ", TO "text", "
     can be used to convert it to straight ascii text, suitable for
     viewing on an ascii terminal.",
     SEEALSO {"MarkUpList", "MarkUpType", "Entity"}
     }

document { "functions",
     "There are two types of functions, those functions built in
     to the system, and those created by the user, but in practice
     the user has no way of distinguishing the two types.  The user
     creates new functions with the ", TO "->", " operator.",
     PARA,
     SEEALSO {"Function"}
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
(add-hook 'comint-M2-hook '(lambda () (setq transient-mark-mode t)))
(defvar M2HOME "/home/dan/src/M2/Macaulay2")///,
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
a line or a region of code, depending on whether the region is active.  The
tenth line tells emacs where to find the directory in which the Macaulay2
files have been installed - you should change this to the appropriate
directory for your installation, so emacs can find the Macaulay 2 documentation.",
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
reason may be that you have not installed Macaulay 2 properly - the startup
script (", TT "M2", " or ", TT "M2.bat", ") should be on your path.
A third reason may be that you are in Windows-98 and are using anti-virus 
software such as ", TT "Dr. Solomon's", ", which can interfere with emacs 
when it tries to run a subprocess.",
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
slightly to change it before pressing return.",
PARA,
"Assuming you have installed the ", TO "w3", " emacs web browser,
you may explore the documentation by positioning the cursor near a documented
word such as ", TT "List", " and pressing ", TT "C-C d", ".  Alternatively,
when the prompt appears, you can type the key whose documentation should be
found."
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
"Notice how ", TT "C-H m", " will display the keystrokes peculiar to 
the mode in a help window.",
PARA,
"Assuming you have installed the ", TO "w3", " emacs web browser,
you may explore the documentation by positioning the cursor near a documented
word such as ", TT "List", " and pressing ", TT "C-C d", ".  Alternatively,
when the prompt appears, you can type the key whose documentation should be
found."
}

document { "w3",
     "You may download the package ", TT "w3", ", by William M. Perry, from 
     ", TT "http://www.cs.indiana.edu/elisp/w3/docs.html", ".  It is an
     emacs package that implements a web browser that displays web pages
     within emacs."
     }

document { "engine communication protocol",
     "Here is a description of the protocol for communication between the 
     front end and the engine.  At the moment, this protocol is used only
     for transmissions from the engine to the front end.",
     PARA,
     MENU {
	  TO "transmitting a positive integer",
	  TO "transmitting an integer",
	  TO "transmitting an integer mod n",
     	  TO "transmitting a sequence",
	  TO "transmitting a monomial",
	  TO "transmitting a polynomial",
	  TO "transmitting a vector",
	  TO "transmitting a matrix",
	  TO "convert"
	  }
     }

document { "transmitting a vector",
     "The method for transmitting a vector depends on the ring involved.",
     PARA,
     "If the ring is a monoid ring (e.g., a polynomial ring), then
     the vector is transmitted as a sequence of triples ", TT "(i,m,a)", ", 
     where ", TT "i", " is the number of the row, ", TT "m", " is the monomial,
     and ", TT "a", " is the coefficient.",
     PARA,
     "If the ring is not a monoid ring, then the vector is transmitted
     as a sequences of pairs ", TT "(i,r)", " where ", TT "i", " is the 
     number of the row, and ", TT "r", " is the entry.",
     PARA,
     "The columns of a matrix are transmitted as vectors.",
     SEEALSO {"transmitting a monomial", "transmitting a matrix"}
     }

document { "transmitting a matrix",
     "Most objects in the engine are stored as matrices.  Even single
     polynomials are usually stored as 1 by 1 matrices.",
     PARA,
     "A matrix is transmitted by sending the columns as a sequence of
     vectors.",
     EXAMPLE "R = ZZ/101[x,y,z];",
     EXAMPLE "f = matrix ( R, {{11,0,33},{0,22,34}} )",
     EXAMPLE "ascii sendgg(ggPush f, ggtonet)",
     SEEALSO "transmitting a vector"
     }

document { "transmitting an integer mod n",
     "An integer mod n is sent as an integer.",
     PARA,
     EXAMPLE "ZZ/101[x];",
     EXAMPLE "s = 44 + x - x",
     EXAMPLE "ascii sendgg( ggPush s, ggleadcoeff, ggtonet)"
     }

document { "transmitting a polynomial",
     "A polynomial is transmitted as a sequence of pairs (m,c), where
     m is a monomial and c is a coefficient.",
     PARA,
     EXAMPLE "ZZ/101[x,y,z];",
     EXAMPLE "ascii callgg(ggtonet, 22*x^66+11*y^77)"
     }

document { "transmitting a monomial",
     "A monomial is transmitted as a sequence of pairs (i,e) of integers,
     where i refers to the i-th variable in the ring, and e is the exponent.",
     PARA,
     EXAMPLE "ZZ/3[t,u,x,y,z];",
     EXAMPLE "ascii sendgg(ggPush (t^22 * y^33 * z^55), ggleadmonom, ggtonet)"
     }

document { "transmitting a sequence",
     "Several items of the same type are transmitted as follows.  The
     number of items is transmitted first, as a positive integer of
     28 bits or less.  See ", TO "transmitting a positive integer", ".
     Then the items are transmitted.",
     PARA,
     EXAMPLE "ascii gg {33,44,55}"
     }

document { "transmitting a positive integer",
     "The integer 0 is transmitted as a single zero byte.
     A positive integer of 28 bits or less is sent 7 bits at a time, with
     the high-order nonzero seven bits sent first.  The highest bit of
     each byte except the last one is set to 1.",
     PRE "
     00000000    or
     0xxxxxxx    or
     1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 0xxxxxxx    or
     1xxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx",
     PARA,
     "A positive integer of more than 28 bits is sent as follows.  First
     come four bytes, with seven bits of the number in each one, with 
     the high bit of each byte set to 1.  Then comes the number of succeeding
     bytes, transmitted as described above for a positive integer of 28
     bits or less.  Finally come the succeeding bytes, each containing 8
     bits of the intger.  Format:",
     PRE "
     1xxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx  (first 28 bits)
     1xxxxxxx 0xxxxxxx                    (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx  (succeeding bytes)
     xxxxxxxx xxxxxxxx                    (succeeding bytes)",
     "It may happen that the first byte in the sequence above has the
     form 10000000."
     }

document { "transmitting an integer",
     "The integer 0 is transmitted as a single zero byte.
     Otherwise, the sign of the integer is put into bit 6 of the first byte,
     and the bits of the absolute value of the integer are packed as follows:
     6 bits into the first byte, 7 bits into 1, 2, or 3 more bytes, and 
     8 bits into each of the succeeding bytes.  If 8 bit bytes are needed,
     then the number of them is sent as a positive integer after the
     first four bytes are sent.  See also ", 
     TO "transmitting a positive integer", ".
     In the following illustration, S denotes the sign bit, and x's denote
     the bits of the integer.",
     PRE "
     00000000     or

     0Sxxxxxx     or

     1Sxxxxxx 0xxxxxxx	or

     1Sxxxxxx 1xxxxxxx 0xxxxxxx	 or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 0xxxxxxx   or

     1Sxxxxxx 1xxxxxxx 1xxxxxxx 1xxxxxxx   (first 27 bits)
     1xxxxxxx 0xxxxxxx    	      	   (number of succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     ...
     xxxxxxxx xxxxxxxx xxxxxxxx xxxxxxxx   (succeeding bytes)
     xxxxxxxx xxxxxxxx	      	           (succeeding bytes)",
     EXAMPLE "binary = s -> concatenate between (\" \",
          apply(s,x -> apply(8, i-> toString ((x >> 7-i) % 2))));",
     EXAMPLE "<< binary ascii gg 63 << endl;",
     EXAMPLE "<< binary ascii gg 64 << endl;",
     EXAMPLE "<< binary ascii gg 127 << endl;",
     EXAMPLE "<< binary ascii gg 128 << endl;",
     EXAMPLE "<< binary ascii gg 2^10 << endl;",
     EXAMPLE "<< binary ascii gg 2^20 << endl;",
     EXAMPLE "<< binary ascii gg (-2^20) << endl;",
     EXAMPLE "<< binary ascii gg 2^30 << endl;",
     EXAMPLE "<< binary ascii gg 2^40 << endl;",
     EXAMPLE "<< binary ascii gg 2^50 << endl;"
     }

document { "engine", 
     "The engine is the part of the program that is dedicated to
     performing the computation of Groebner bases with Buchberger's
     algorithm.  It is coded directly in C++ for speed, and it communicates
     with the front-end interpreter through a bidirectional stream of bytes,
     so that in future implementations the engine may reside in a separate
     process on a distant machine.",
     MENU {
	  TO "engine communication protocol",
     	  TO "low level gb engine commands",
	  TO "high level gb engine commands"
	  },
     PARA,
     "The Macaulay 2 engine provides fast polynomial and matrix operations,
     and Groebner bases, syzygies, Hilbert functions, resolutions and
     other operations that we feel need to be implemented directly for
     efficiency reasons.",
     }

document { "high level gb engine commands",
     "Sending commands to the engine:",
     MENU {
	  TO "callgg",
	  TO "gg",
	  TO "ggPush",
	  TO "handle",
	  TO "sendToEngine",
	  TO "sendgg"
	  },
     "This class provides an interface to rings implemented by the engine.",
     SHIELD MENU {
	  TO "EngineRing"
	  },
     "These routines take an element off the stack.",
     MENU {
	  TO "eePop",
	  TO "eePopBool",
	  TO "eePopInt",
	  TO "eePopIntarray",
	  TO "getMatrix"
	  },
     "These functions transfer ring elements to other rings.",
     MENU {
	  TO "eeLift",
	  TO "eePromote"
	  },
     "These functions are used mainly for debugging the engine.",
     MENU {
	  TO "look",
	  TO "engineMemory",
	  TO "engineStack",
	  TO "heap",
	  TO "see"
	  }
     }

document { "component example",
     "The following simple example illustrates the use of 
     ", TO "removeLowestDimension", ",", TO "top", ",", TO "radical",
     ", and ", TO "decompose", ".",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
      	  "I = monomialCurve(R,{1,3,4})",
      	  "J = ideal(a^3,b^3,c^3-d^3)",
      	  "I = intersect(I,J)",
      	  "removeLowestDimension I",
      	  "top I",
      	  "radical I",
      	  "decompose I"
	  },
     }

document { "top-method",
     "If M is a module in a polynomial ring R, then the implementations of 
     ", TO "top", " and ", TO "removeLowestDimension", " are based on 
     the following observations:",
     MENU {
	  "codim Ext^d(M,R) >= d, for all d (if the module is non-zero)",
	  "If P is an associated prime of M of codimension d := codim P > codim M,
	  then codim Ext^d(M,R) = d and the annihilator of Ext^d(M,R) is contained
	  in P",
	  "If codim Ext^d(M,R) = d, then there really is an associated prime 
	  of codimension d.",
	  "If M is R/I, then top(I) = ann Ext^c(R/I,R), where c = codim I"
	  }
     }

TEST "
    R = ZZ/32003[a..d]
    I = monomialCurve(R,{1,3,4})
    J = ideal(a^3,b^3,c^3-d^3)
    I = intersect(I,J)
    removeLowestDimension I
    top I
    radical I
    decompose I
"
TEST "
    -- test of removeLowestDimension
    R = ZZ/32003[a,b,c]
    I = ideal(a^2,b^2)
    J = ideal(a^3,b^3,c^3)
    I = intersect(I,J)
    time (I1 = removeLowestDimension I)
    time top I
    time radical I
"

     
TEST "
    -- examples of use of: radical, UnmixedRadical, 
    -- top, removeLowestDimension

    -- example 1: a simple monomial ideal
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,b^3,c^2))
    time (Irad = radical(I,Unmixed=>true))

    -- example 2: 
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,d^4), ideal(b^2,c^2,d^2))
    time (Itop = top I)
    time (I1 = removeLowestDimension I)
    time (Irad = radical I)
"

TEST "
R = ZZ/101[symbol a..symbol d]
I = monomialCurve(R,{1,2,3})
I^2
removeLowestDimension(I^2)
assert(I == 
     radical(I^2)
     )
assert(I == 
     radical(I^2, Unmixed=>true)
     )
assert(
     top (I^2) == I^2
     )
S = R/(a^3, b^3)
I = ideal(0_S)
J = I^2
J1 = top J
J1 == J   
time (radical I)

-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8)]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
I1 = ideal(I_0,I_1,I_2)
codim I1
radical(I, CompleteIntersection=>I1)
-- radical(I,Unmixed=>true)
-- I1 = removeLowestDimension I
-- I2 = removeLowestDimension I1
"

