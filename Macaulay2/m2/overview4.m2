--		Copyright 1993-2002 by Daniel R. Grayson


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

document { "emacs",
     "The best way to edit Macaulay 2 code or to run Macaulay 2 is with GNU 
     emacs, a versatile text editor written by Richard Stallman which
     runs well under most UNIX systems.  Its
     web page is ", HREF "http://www.gnu.org/software/emacs/emacs.html", "
     and the software can be obtained from one of the ftp sites listed
     at ", HREF "http://www.gnu.org/order/ftp.html", "; the primary ftp
     site is ", HREF "ftp://ftp.gnu.org/pub/gnu/", ".",
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
", TT "/emacs/M2-init.el", " to the value returned and incorporate it into 
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
frame, press ", TT "F11", " a few times to conduct the demo.  (If the font or frame 
is the wrong size, you may have to create a copy of the file ", TT "M2.el", "
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

document { "engine", 
     "The engine is the part of the program that is dedicated to
     performing the computation of Groebner bases with Buchberger's
     algorithm.  It is coded directly in C++ for speed.",
     PARA,
     "The Macaulay 2 engine provides fast polynomial and matrix operations,
     and Groebner bases, syzygies, Hilbert functions, resolutions and
     other operations that we feel need to be implemented directly for
     efficiency reasons.",
     }

document { "component example",
     "The following simple example illustrates the use of 
     ", TO "removeLowestDimension", ",", TO "top", ",", TO "radical",
     ", and ", TO "decompose", ".",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
      	  "I = monomialCurveIdeal(R,{1,3,4})",
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
    I = monomialCurveIdeal(R,{1,3,4})
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
I = monomialCurveIdeal(R,{1,2,3})
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

document { "computing Groebner bases",
     "In this section we give some more detail about how to control
     the computation of Groebner bases.",
     PARA,
     "We may stop the computation of a homogeneous Groebner basis
     after S-polynomials up to a certain degree have been handled with
     the ", TO "DegreeLimit", " option.  (This is meaningful only
     in homogeneous situations.)  Certains statistics about the suspended
     computation can be displayed with ", TO "summary", ".",     
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "I = ideal(x*y-z^2,y^2-w^2)",
      	  "g2 = gb(I,DegreeLimit => 2)",
	  "summary g2",
      	  "g3 = gb(I,DegreeLimit => 3)",
	  "summary g3"
	  },
     "The computation advanced further with the higher degree limit.",
     PARA,
     "The second computation advances the state of the Groebner
     basis object started by the first, and the two results are
     exactly the same Groebner basis object.",
     EXAMPLE {
	  "g2",
	  "g2 === g3"
	  },
     "The option ", TO "PairLimit", " can be used to stop after a certain
     number of S-polynomials have been reduced.  After being reduced, the
     S-polynomial is added to the basis, or a syzygy has been found.",
     EXAMPLE {
      	  "I = ideal(x*y-z^2,y^2-w^2)",
      	  "gb(I,PairLimit => 2)",
      	  "gb(I,PairLimit => 3)"
	  },
     "The option ", TO "BasisElementLimit", " can be used to stop after a
     certain number of basis elements have been found.",
     EXAMPLE {
      	  "I = ideal(x*y-z^2,y^2-w^2)",
      	  "gb(I,BasisElementLimit => 2)",
      	  "gb(I,BasisElementLimit => 3)"
	  },
     "The option ", TO "CodimensionLimit", " can be used to stop after the
     apparent codimension, as gauged by the leading terms of the basis
     elements found so far, reaches a certain number.",
     PARA,
     "The option ", TO "SubringLimit", " can be used to stop after a certain
     number of basis elements in a subring have been found.  The subring is
     determined by the monomial ordering in use.  For ", TT "Eliminate n", "
     the subring consists of those polynomials not involving any of the first
     ", TT "n", " variables.  For ", TT "Lex", " the subring consists of those
     polynomials not involving the first variable.  For
     ", TT "ProductOrder {m,n,p}", " the subring consists of those polynomials
     not involving the first ", TT "m", " variables.",
     PARA,
     "Here is an example where we are satisfied to find one relation
     from which the variable ", TT "t", " has been eliminated.",
     EXAMPLE {
	  "R = ZZ/101[t,F,G,MonomialOrder => Eliminate 1];",
	  "I = ideal(F - (t^3 + t^2 + 1), G - (t^4 - t))",
	  "transpose gens gb (I, SubringLimit => 1)",
	  },
     PARA,
     "Sometimes a Groebner basis computation can seem to last forever.  An ongoing     
     visual display of its progress can be obtained with ", TO "gbTrace", ".",
     EXAMPLE {
	  "gbTrace 3",
      	  "I = ideal(x*y-z^2,y^2-w^2)",
     	  "gb I",
	  },
     "Here is what the tracing symbols indicate.",
     PRE ///    {2}   ready to reduce S-polynomials of degree 2
    (0)   there are 0 more S-polynomials (the basis is empty)
     g    the generator yx-z2 has been added to the basis
     g    the generator y2-w2 has been added to the basis
    {3}   ready to reduce S-polynomials of degree 3
    (1)   there is 1 more S-polynomial
     m    the reduced S-polynomial yz2-xw2 has been added to the basis
    {4}   ready to reduce S-polynomials of degree 4
    (2)   there are 2 more S-polynomials
     m    the reduced S-polynomial z4-x2w2 has been added to the basis
     o    an S-polynomial reduced to zero and has been discarded
    {5}   ready to reduce S-polynomials of degree 5
    (1)   there is 1 more S-polynomial
     o    an S-polynomial reduced to zero and has been discarded
///,     
     PARA,
     "Let's turn off the tracing.",
     EXAMPLE {
	  "gbTrace 0"
	  },
     PARA,
     "Each of the operations dealing with Groebner bases may be
     interrupted or stopped (by typing CTRL-C).  The computation
     is continued by re-issuing the same command.  Alternatively, the
     command can be issued with the option ", TT "StopBeforeComputation => true", ".
     It will stop immediately, and return a Groebner basis object that can
     be inspected with ", TO "summary", ", ", TO "gens", " or ", TO "syz", ".
     The computation can be continued later.",
     PARA,
     "The function ", TO "forceGB", " can be used to create a Groebner
     basis object with a specified Groebner basis.  No computation is
     performed to check whether the specified basis is a Groebner
     basis.",
     PARA,
     "If the Poincare polynomial (or Hilbert function) for a homogeneous
     submodule ", TT "M", " is known, you can speed up the computation of a
     Groebner basis by informing the system.  This is done by storing
     the Poincare polynomial in ", TT "M.poincare", ".",
     PARA,
     "As an example, we compute the Groebner basis of a random ideal,
     which is almost certainly a complete intersection, in which
     case we know the Hilbert function already.",
     EXAMPLE {
	  "R = ZZ/101[a..e];",
      	  "T = (degreesRing R)_0",
      	  "f = random(R^1,R^{-3,-3,-5,-6});",
      	  "time betti gb f"
	  },
     "The matrix was randomly chosen, and we'd like to use the same one
     again, but this time with a hint about the Hilbert function, so first
     we must erase the memory of the Groebner basis computed above.",
     EXAMPLE {
	  "remove(f.cache,{false,0})",
	  },
     "Now we provide the hint and compute the Groebner basis anew.",
     EXAMPLE {
      	  "(cokernel f).poincare = (1-T^3)*(1-T^3)*(1-T^5)*(1-T^6)",
      	  "time betti gb f"
	  },
     "The computation turns out to be substantially faster."
     }

document { "diff and contract",
     "We may use the function ", TO "diff", " to differentiate polynomials:
     the first argument is the variable to differentiate with respect to,
     and the second argument is the polynomial to be differentiated.",
     EXAMPLE {
	  "R = QQ[a,b,t,x,y,z];",
	  "f = x^7 * y^11;",
	  "diff(x,f)",
	  "diff(y,f)",
	  },
     "We indicate higher derivatives by simply multiplying the variables
     to differentiate by.",
     EXAMPLE {
	  "diff(x^2,f)",
	  "diff(x*y,f)",
	  "diff(y^2,f)",
     	  },
     "The first argument can also be a sum, in which case the sum of
     the answers provided by each of its terms is returned.",
     EXAMPLE {
	  "diff(x+y,f)",
	  "diff(x^2+x*y+y^2,f)",
	  },
     "Remark: the operation ", TT "diff", " is useful, but it's not a 
     natural one: it's not invariant under linear coordinate changes;
     in effect, we've identified the a free module with its dual.",
     PARA,
     "The second argument can be a matrix, in which case each of
     its entries gets differentiated.",
     EXAMPLE {
	  "m = matrix {{x^3, x^4},{x^5,x^6}}",
	  "diff(x,m)",
	  "diff(x^2,m)",
	  },
     "The first argument can also be a matrix, in which case
     the matrices obtained from each of its entries, acting upon
     the second argument, are concatenated.  Thus the shape of
     the first matrix plays the major role.",
     EXAMPLE {
	  "diff(matrix {{x,x^2,x^3,x^4}}, m)",
	  "diff(matrix {{x,x^2},{x^3,x^4}}, m)",
	  },
     PARA,
     "Perhaps the most common usage of ", TO "diff", " is when one argument
     has a single column and the other column has a single row.  For example,
     the Jacobian matrix can be computed as follows.",
     EXAMPLE {
	  "diff(matrix {{x},{y}}, matrix {{x^2, x*y, y^2}})",
	  },
     HR,
     "We can also compute the Hessian matrix of a quadratic form using ", TO "diff", ",
     as follows.",
     EXAMPLE {
	  "v = matrix {{x,y}}",
	  "diff(v ** transpose v, 3*x^2 + 5*x*y + 11*y^2)"
	  },
     HR,
     "As another example, we show how to compute the Wronskian of a
     polynomial ", TT "f", ".",
     EXAMPLE {
      	  "f = x^3 + y^3 + z^3 - t*x*y*z",
      	  "v = matrix {{x,y,z}}",
      	  "det diff(transpose v * v, f)",
	  },
     HR,
     "The function ", TO "contract", " is the same as ", TO "diff", ",
     except the multiplication by integers that occurs during
     differentiation is omitted.",
     EXAMPLE {
	  "contract(x,m)",
	  "contract(x^2,m)",
	  "contract(matrix {{x,x^2,x^3,x^4}}, m)",
	  "contract(matrix {{x,x^2},{x^3,x^4}}, m)",
	  },
     "One use is for picking out coefficients of homogeneous polynomials.",
     EXAMPLE {
	  "f",
	  "v3 = symmetricPower(3,matrix{{x,y,z}})",
	  "contract(v3, f)",
	  },
     HR,
     "As an example, the Sylvester resultant between homogeneous polynomials
     ", TT "f(x,y)", " and ", TT "g(x,y)", " can be found in the following way.",
     EXAMPLE {
      	  "f = a * x^3 + b * x^2 * y + y^3",
      	  "g = b * x^3 + a * x * y^2 + y^3",
	  },
     "Multiply each of these by all quadrics, obtaining a set of elements in
     degree 5.",
     EXAMPLE {
	  "n = matrix {{f,g}} ** symmetricPower(2,matrix {{x,y}})",
	  },
     "Now create the matrix of coefficients by using contract against all
     monomials of degree 5 in ", TT "x", " and ", TT "y", ", and
     compute its determinant.",
     EXAMPLE {
	  "M = contract(transpose symmetricPower(5,matrix {{x,y}}), n)",
      	  "det M",
          --
          --                5    2 3    3     2 2       3    4    3     2        2    3
          --       ideal(- a  - a b  - a b - a b  + 2a*b  - b  + a  - 3a b + 3a*b  - b )
          --   
	  },
     HR,
     "The function ", TO "diff'", " is the same as ", TO "diff", ",
     except that the first argument is differentiated by the second;
     the shape of the first argument still plays the major role.",
     EXAMPLE {
	  "diff'(m, matrix {{x,x^2,x^3,x^4}})",
	  "diff'(m, matrix {{x,x^2},{x^3,x^4}})",
	  },
     "The function ", TO "contract'", " is the same as ", TO "contract", ",
     except that the first argument is contracted by the second;
     the shape of the first argument still plays the major role.",
     EXAMPLE {
	  "contract'(m, matrix {{x,x^2,x^3,x^4}})",
	  "contract'(m, matrix {{x,x^2},{x^3,x^4}})",
	  },
     HR,
     "All four of these operators are engineered so that the result is
     a homogeneous matrix if the arguments are.  The operations ", TO "diff", "
     and ", TO "contract", " are essentially partially defined division operations,
     so it should come as no surprise that the source and target of
     ", TT "diff(m,n)", " are the same as those we would get from
     the tensor product ", TT "transpose m^-1 ** n", ", if 
     only ", TT "m", " were invertible.",
     SEEALSO { (diff,Matrix,Matrix), (contract,Matrix,Matrix), "reshape", "adjoint" }
     }

document { "computing syzygies",
     "A syzygy among the columns of a matrix is, by definition, an
     element of the kernel of the corresponding map between free modules,
     and the easiest way to compute the syzygies applying the 
     function ", TO "kernel", ".",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "f = vars R",
	  "K = kernel f",
	  },
     "The answer is provided as a submodule of the source of ", TT "f", ".  The
     function ", TO "super", " can be used to produce the module that ", TT "K", " is
     a submodule of; indeed, this works for any module.",
     EXAMPLE {
	  "L = super K",
	  "L == source f",
	  },
     "The matrix whose columns are the generators of ", TT "K", ", lifted to
     the ambient free module of ", TT "L", " if necessary, can be obtained 
     with the function ", TO "generators", ", an abbreviation for which is
     ", TT "gens", ".",
     EXAMPLE {
	  "g = generators K",
	  },
     "We can check at least that the columns of ", TT "g", " are syzygies 
     of the columns of ", TT "f", " by checking that ", TT "f*g", " is zero.",
     EXAMPLE {
	  "f*g",
	  "f*g == 0",
	  },
     "Use the function ", TO "syz", " if you need detailed control over the
     extent of the computation."
     }

document { "computing resolutions",
     "Use the function ", TO "resolution", ", often abbreviated as ", TO "res", ",
     to compute a free resolution of a module.",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "M = cokernel vars R",
	  "C = res M",
	  },
     "See ", TO "chain complexes", " for further details about how to handle
     and examine the result.",
     PARA,
     "A reference to the result is stored within the module ", TT "M", ", so that
     requesting a computation of ", TT "res M", " a second time yields the formerly
     computed result immediately.",
     PARA,
     "If the computation is interrupted or discontinued after the skeleton 
     has been successfully computed, then the partially completed
     resolution is available as ", TT "M.resolution", ", and can be
     examined with ", TO "status", ".  The computation can be continued
     with ", TT "res M", ".  Here is an example, with an alarm interrupting
     the computation several times before it's complete.  (On my machine, 
     the computation takes a total of 14 seconds.)",
     PARA,
     EXAMPLE {
	  "R = ZZ/2[a..d];",
	  "M = coker random(R^4, R^{5:-3,6:-4});",
///while true do try (
     alarm 3;
     res M;
     << "-- computation complete" << endl;
     status M.resolution;
     << res M << endl << endl;
     break;
     ) else (
     << "-- computation interrupted" << endl;
     status M.resolution;
     << "-- continuing the computation" << endl;
     )///
	  },
     "If the user has a chain complex in hand which is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.resolution = C", ".",
     PARA,
     "There are various optional arguments associated with ", TO "res", "
     which allow detailed control over the progress of the computation."
     }
