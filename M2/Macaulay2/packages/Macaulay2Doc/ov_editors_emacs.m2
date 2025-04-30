doc ///
Node
  Key
    "using Macaulay2 with Emacs"
  Description
    Text
      Emacs is a free text editor which can be used as an ASCII-oriented interface to Macaulay2.
      The best way to edit Macaulay2 code or to run Macaulay2 is with Emacs or its MacOS variant Aquamacs.
      Emacs is available as part of many operating systems, as well as from @HREF "https://www.gnu.org/software/emacs/"@.
    Tree
      > "setting up the Macaulay2 Emacs interface"
      > "running Macaulay2 in Emacs"
      > "using Macaulay2 with Emacs after it has been set up"
      > "editing Macaulay2 code with Emacs"

Node
  Key
    "setting up the Macaulay2 Emacs interface"
  Description
    Text
      If you are a newcomer to Emacs, you should spend a few minutes going through the Emacs tutorial.
      Start up Emacs with the command @KBD "emacs"@ and then start up the Emacs tutorial with the
      keystrokes @KBD "C-h t"@.  (The notation @KBD "C-h"@ indicates that you should type @KBD "Ctrl-H"@,
      by holding down the @KBD "Ctrl"@ key, and pressing @KBD "H"@.)  The Emacs tutorial will introduce you to
      the basic keystrokes useful with Emacs.  After running through that you will want to examine the
      online Emacs manual that can be read with @KBD "info"@ mode; you may enter or re-enter that mode with
      the keystrokes @KBD "C-h i"@.
    Text
      The Macaulay2 Emacs interface consists of several files in the directory
      @TT "share/emacs/site-lisp/Macaulay2/"@ in the Macaulay2 distribution tree.
      In order to teach Emacs how to find these files, you have two options:

    Text
      @HEADER3 {"Method 1: using ", TO setupEmacs}@

      Start M2 in a terminal window and enter @TT "setupEmacs()"@.

    Text
      @HEADER3 {"Method 2: editing ", TT "~/.emacs"}@

      Place the following lines in the @TT ".emacs"@ file in your home directory,
      creating the file if necessary, and save the file.
    Pre
      ;; .emacs file in your home directory
      (load \"M2-init\")

      ; f12 starts M2 in Emacs
      ; comment out the following line with an initial semicolon
      ; if you want to use your f12 key for something else
      ; or change f12 to (for example) f8
      (global-set-key [ f12 ] 'M2)

      ; f11 sends commands to M2
      ; use C-u f11 to temporarily edit the M2 executable
      (global-set-key [ f11 ] 'M2-send-to-program)
    Text
      Now, try restarting Emacs and starting Macaulay2 by pressing the @KBD "F12"@ function key.
      If Macaulay2 doesn't start up (in a buffer named @TT "*M2*"@), check your @TT ".emacs"@ file
      to make sure the lines above have been saved correctly.
      If they look okay, then see the following pages for help.
    Tree
      :Debugging issues with Emacs
        > "teaching Emacs how to find M2-init.el"
	> "teaching Emacs how to find M2"
	* "teaching your shell how to find M2"
	* "getting help or reporting bugs"
    Text
      Note: on macOS systems, @KBD "F12"@ may be usurped by either DashBoard, SpotLight, or something else,
      so either you must change the @TT "f12"@ to some other key, e.g. @TT "f8"@, in your @TT ".emacs"@ file,
      or you should disable the systems use of the @KBD "F12"@ key.
  Subnodes
    "setupEmacs"

Node
  Key
    "teaching Emacs how to find M2-init.el"
  Description
    Text
      Files containing Emacs source code have names of the form @TT "*.el"@. Macaulay2 comes with a file
      called @TT "M2-init.el"@ that sets up Emacs for running M2 conveniently. It is important that Emacs
      be able to find that file and the three other files that come with it, by searching in the
      directories listed in the Emacs variable @TT "load-path"@.
    Text
      If the Macaulay2 directory tree has been installed with the same root as the Emacs directory tree,
      then Emacs already knows to look in that directory for source files. For example, if Emacs and
      Macaulay2 are both installed in @TT "/usr"@, then @TT "M2-init.el"@ is located at
      @TT "/usr/share/emacs/site-lisp/Macaulay2/M2-init.el"@.
    Text
      @HEADER3 {"Method 1: using ", TO setup}@

      The simplest way to teach Emacs how to find @TT "M2-init.el"@ is to let M2 do it for you.
      Start M2 in a terminal window and enter @TT "setup()"@.
      If that works, the next time you start Emacs, it should know how to find @TT "M2-init.el"@.
    Text
      @HEADER3 {"Method 2: editing ", TT "~/.emacs"}@

      To determine the precise path of the @TT "site-lisp"@ directory Emacs is looking in, so that you can
      install Macaulay2 properly, use the Emacs @TT "describe-variable"@ command, accessible with the key
      strokes @KBD "C-h v"@, and ask for the description of the variable @TT "load-path"@.

      Let's assume that you have located the Macaulay2 source code, and that @TT "M2-init.el"@ is located
      at @TT "/foo/bar/share/emacs/site-lisp/Macaulay2/M2-init.el"@, and that you want to tell Emacs to
      search that directory, too. Insert the following command into the file @TT ".emacs"@ in your home
      directory.
    Pre
      (add-to-list 'load-path "/foo/bar/share/emacs/site-lisp/Macaulay2")
    Text
      The next time you start Emacs, it will look also in that directory for files, and it should find
      @TT "M2-init.el"@.

Node
  Key
    "teaching Emacs how to find M2"
  Description
    Text
      If you teach your shell how to find M2, then you do not have to teach Emacs how to find M2.
      See @TO "teaching your shell how to find M2"@, and come back to this section only if you fail with that.
      Let's assume that you have found @TT "M2"@ (the program), and that is located in the directory
      @TT "/foo/bar/bin"@.
    Text
      Let's assume you have already set up the function key @KBD "F12"@ to call M2.
      That is done with the following command in the @TT ".emacs"@ file.
    Pre
      (global-set-key [ f12 ] 'M2)
    Text
      Then when you press @KBD "F12"@, M2 should start running.
    Text
      Here is what you will see on your screen in the minibuffer at the bottom of the screen when you
      press @KBD "F12"@ if Emacs doesn't know how to find the file @TT "M2-init.el"@.
    Pre
      Symbol's function definition is void: M2
    Text
      If you see that, you are not ready for this section: see @TO "teaching Emacs how to find M2-init.el"@.
    Text
      Here is what you will see on your screen in a buffer named @TT "*M2*"@ if Emacs knows how to find
      the file @TT "M2-init.el"@ but not how to find the program @TT "M2"@.
    Pre
      + M2 --no-readline --print-width 189
      /bin/sh: M2: command not found

      Process M2 exited abnormally with code 127
    Text
      @HEADER2{"Method 1: temporarily switching the path to M2"}@

      To teach Emacs temporarily where to find M2, press @KBD "C-u F12"@
      (i.e. hold @KBD "Ctrl"@ and press @KBD "u"@, then release and press @KBD "F12"@).
      You will get the @TT "M2"@ command line in the minibuffer at the bottom of the screen,
      and you can edit it. It will initially look something like this:
    Pre
      M2 --no-readline --print-width 189
    Text
      You can change it to the right thing:
    Pre
      /foo/bar/bin/M2 --no-readline --print-width 189
    Text
      Then press @KBD "Enter"@ and M2 should start running. That will stick for the rest of your Emacs
      session.  Later, to return to the @TT "*M2*"@ window from another, or to start up M2 again, just
      press @KBD "F12"@.

    Text
      @HEADER2{"Method 2: editing ", TT "~/.emacs"}@

      Every time Emacs starts up it reads commands from the file @TT ".emacs"@ in your home directory.
      Put the following command in your @TT ".emacs"@ file.
    Pre
      (setq M2-exe "/foo/bar/bin/M2")
    Text
      The next time you start Emacs it will know how to find M2.

Node
  Key
    "running Macaulay2 in Emacs"
  Description
    Text
      Because some answers can be very wide, it is a good idea to run Macaulay2 in a window that does not
      wrap output lines and allows the user to scroll horizontally to see the rest of the output.  We
      provide a package for @TO2("using Macaulay2 with Emacs", "Emacs")@ that implements this. It also
      provides for dynamic completion of symbols in the language.
    Text
      There is an ASCII version of this section of the documentation distributed in the file @TT
      "share/emacs/site-lisp/macaulay2/M2-emacs-help.txt"@. It might be useful for you to visit that file
      with Emacs now, thereby avoiding having to cut and paste bits of text into Emacs buffers for the
      demonstrations below.
    Text
      If you are a newcomer to Emacs, start up Emacs with the command @TT "emacs"@ and then start up the
      Emacs tutorial with the keystrokes @KBD "C-h t"@.  (The notation @KBD "C-h"@ indicates that you should
      type @KBD "Ctrl-H"@, by holding down the @KBD "Ctrl"@ key, and pressing @KBD "H"@.)  The Emacs tutorial
      will introduce you to the basic keystrokes useful with Emacs.  After running through that you will
      want to examine the online Emacs manual that can be read with @TT "info"@ mode; you may enter or
      re-enter that mode with the keystrokes @KBD "C-h i"@. You may also want to purchase (or print out)
      the Emacs manual.  It is cheap, comprehensive and informative.  Once you have spent an hour with the
      Emacs tutorial and manual, come back and continue from this point.
    Text
      We assume you have taught Emacs how to find Macaulay2's files, as described in the previous
      sections, and that Emacs is loading the file @TT "M2-init.el"@ successfully.  Loading that file will
      cause Emacs to enter a special mode for editing Macaulay2 code whenever a file whose name has the
      form @TT "*.m2"@ is encountered.  It will also provide a special mode for running Macaulay2 in an
      Emacs buffer.  It sets the variable @TT "transient-mark-mode"@ to have a different value in each
      buffer, and sets hooks so that @TT "transient-mark-mode"@ will be set to @TT "t"@ in M2 buffers.
      The effect of this is that the mark is only active occasionally, and then Emacs functions that act
      on a region of text will refuse to proceed unless the mark is active.  The @TT "set-mark"@ function
      or the @TT "exchange-point-and-mark"@ function will activate the mark, and it will remain active
      until some change occurs to the buffer.  The only reason we recommend the use of this mode is so the
      same key can be used to evaluate a line or a region of code, depending on whether the region is
      active.
    Text
      Exit and restart Emacs with your new initialization file. If you are reading this file with Emacs,
      then use the keystrokes @KBD "C-x 2"@ to divide the buffer containing this file into two windows.
      Then press the @KBD "M-x M2"@ to start up Macaulay2 in a buffer named @TT "*M2*"@.  (The command line
      used to start Macaulay2 may be edited before being run if you use a prefix argument with the above
      command: press @KBD "C-u"@ just before.)
    Text
      If this doesn't start up Macaulay2, one reason may be that your function keys are not operable.  In
      that case press @KBD "M-x M2"@ instead.  (The notation @KBD "M-x"@ is Emacs notation for pressing the
      @KBD "x"@ key while holding down the @KBD "Meta"@ or @KBD "Alt"@ key.)  If that doesn't work, please
      see @TO "teaching Emacs how to find M2-init.el"@ and @TO "teaching Emacs how to find M2"@.
    Text
      You may wish to bind the Emacs function @TT "M2-send-to-program"@ to a global keystroke for ease of
      use; this is done automatically in Macaulay2 buffers.  For example, the following Emacs code will
      bind it to the function key @KBD "F11"@.
    Pre
      (global-set-key [ f11 ] 'M2-send-to-program)
    Text
      You may use @KBD "C-x o"@ freely to switch from one window to the other. Verify that Macaulay2 is
      running by entering a command such as @KBD "2+2"@. Now paste the following text into a buffer, unless
      you have the ASCII version of this documentation in an Emacs buffer already, position the cursor on
      the first line of code, and press the @KBD "F11"@ function key repeatedly to present each line to
      Macaulay2.
    Code
      PRE M2CODE ////i1 : R = ZZ/101[x,y,z]
      i2 : f = symmetricPower(2,vars R)
      i3 : M = cokernel f
      i4 : C = resolution M
      i5 : betti C////
    Text
      Notice that the input prompts are not submitted to Macaulay2.
    Text
      Here is a way to conduct a demo of Macaulay2 in which the code to be submitted is not visible on the
      screen.  Visit a file called @TT "foo.m2"@ and paste the following text into it.
    Code
      PRE "20!\n4 + 5 * 2^20\n-- that's all folks!"
    Text
      Press @KBD "M-f11"@ with your cursor in this buffer to designate it as the source for the Macaulay2
      commands.  (The notation @KBD "M-f11"@ means that while holding the @KBD "Meta"@ key down, you should
      press the @KBD "F11"@ function key.  The @KBD "Meta"@ key is the @KBD "Alt"@ key on some keyboards,
      or it can be simulated by pressing Escape (just once) and following that with the key you wanted to
      press while the meta key was held down.)  Then position your cursor (and thus the Emacs point) within
      the line containing @TT "20!"@.  Now press @KBD "M-x M2-demo"@ to open up a new frame called @TT "DEMO"@
      for the @TT "*M2*"@ window with a large font suitable for use with a projector, and with your cursor in
      that frame, press @KBD "F11"@ a few times to conduct the demo.  (If the font or frame is the wrong size,
      you may have to create a copy of the file @TT "M2.el"@ with a version of the function @TT "M2-demo"@
      modified to fit your screen.)
    Text
      One press of @KBD "F11"@ brings the next line of code forward into the @TT "*M2*"@ buffer, and the
      next press executes it.  Use @KBD "C-x 5 0"@ when you want the demo frame to go away.
    Text
      There is a way to send a region of text to Macaulay2: simply select a region of text, making sure
      the mark is active (as described above) and press @KBD "F11"@. Try that on the list below; put it
      into an Emacs buffer, move your cursor to the start of the list, press @KBD "M-C-\@"@ or
      @KBD "M-C-space"@ to mark the list, and then press @KBD "F11"@ to send it to Macaulay2.
      (The notation @KBD "M-C-\@"@ means: while holding down the @KBD "Meta"@ key and the @KBD "Ctrl"@
      key press the @KBD "\@"@ key, for which you'll also need the shift key.)
    Pre
      {a,b,c,d,e,f,
           g,h,i,j,k,l,
           m,n}
    Text
      We have developed a system for incorporating Macaulay2 interactions into $\TeX$ files.
      Here is an example of how that looks. Paste the following text into an Emacs buffer.
    Pre
      The answer, 4, is displayed after the output label ``{\tt o1\ =}''.
           Multiplication is indicated with the traditional {\tt *}.
           <<<1*2*3*4>>>
           Powers are obtained as follows.
           <<<2^100>>>
    Text
      The bits in brackets can be submitted to Macaulay2 easily.  Position your cursor at the top of the
      buffer and press @KBD "F10"@.  The cursor will move just past the first @TT "<<<"@, and the Emacs
      mark will be positioned just before the @TT ">>>"@.  Thus @TT "1*2*3*4"@ is the region, and it will
      even be highlighted if you have set the Emacs variable @TT "transient-mark-mode"@ to @TT "t"@ for
      this buffer.  Pressing @KBD "F11"@ will send @TT "1*2*3*4"@ to Macaulay2 for execution: try it now.
      A sequence of such Macaulay2 commands can be executed by alternately pressing @KBD "F10"@ and @KBD "F11"@.
      You may also use @KBD "M-f10"@ to move backward to the previous bracketed expression.
    Text
      Now go to the very end of the @TT "*M2*"@ buffer with @KBD "M->"@ and experiment with keyword completion.
      Type @TT "reso"@ and then press the @KBD "TAB"@ key.  Notice how the word is completed to @TT "resolution"@
      for you.  Delete the word with @KBD "M-DEL"@, type @TT "res"@ and then press the @KBD "TAB"@ key.
      The possible completions are displayed in a window.  Switch to it with the @KBD "F8"@ key, move to the
      desired completion, press the @KBD "Enter"@ key, and then return to the @TT "*M2*"@ buffer with
      @KBD "C-x o"@.  Alternatively, if you have a mouse, use the middle button to select the desired completion.
    Text
      Experiment with command line history in the @TT "*M2*"@ buffer.  Position your cursor at the end of
      the buffer, and then use @KBD "M-p"@ and @KBD "M-n"@ to move to the previous and next line of input
      remembered in the history.  When you get to one you'd like to run again, simply press return to do
      so.  Or edit it slightly to change it before pressing return.

Node
  Key
    "using Macaulay2 with Emacs after it has been set up"
  Description
    Text
      In this section, we show by example how to use the Macaulay2 Emacs interface. We assume that you
      have already set up this interface, as described in @TO "setting up the Macaulay2 Emacs interface"@.
      After creating or changing the .emacs file mentioned there, you need to exit and restart Emacs.  For
      the rest of this section, we assume that you are running Emacs.
    Text
      The aspects of this interface that we describe include

      @UL{
	  LI{"Starting Macaulay2 with the ", KBD "F12", " key, or with ", KBD "M-x M2"},
	  LI{"Working with two buffers"},
	  LI{"Sending lines or selected text to Macaulay2 using the ", KBD "F11", " key"},
	  LI{"Command completion with ", KBD "TAB"},
	  LI{"Horizontal scrolling with ", KBD "F3", " and ", KBD "F4"}
      }@
    Text
      Before starting, note that when we say to type @KBD "M-x M2"@, what we really mean is:
      press the @KBD "x"@ key while holding down the @KBD "Meta"@ or @KBD "Alt"@ key (on Macs this is
      either the @KBD "Option"@ key or the @KBD "Apple"@ key, depending on how your Emacs is set up);
      type @TT "M2"@; and then press the @KBD "Enter"@ key after that.  Similarly, @KBD "C-c"@ means to press
      the @KBD "c"@ key while holding down the @KBD "Ctrl"@ key, and @KBD "C-x 2"@ means to press
      @KBD "x"@ while holding down the @KBD "Ctrl"@ key, then to press @KBD "2"@; this time do not press enter.
    Text
      Use the keystrokes @KBD "C-x 2"@ to divide the buffer containing this file into two windows.
      Then press the @KBD "F12"@ key or type @KBD "M-x M2"@ to start up Macaulay2 in a buffer named @TT "*M2*"@.
      (The command line used to start Macaulay2 may be edited before being run if you use a prefix
      argument with the above command: press @KBD "C-u"@ just before.)
    Text
      If @KBD "F12"@ doesn't start up Macaulay2, one reason may be that your function keys are not operable.
      In that case press @KBD "C-c m"@ instead. Another reason may be that you have not installed Macaulay2
      properly — the startup script (@TT "M2"@ or @TT "M2.bat"@) should be on your path.
    Text
      You may use @KBD "C-x o"@ freely to switch from one window to the other. Verify that Macaulay2 is
      running by entering a command such as @KBD "2+2"@. Now create (using @KBD "C-x C-f"@) a file,
      named something like @TT "foo.m2"@ (the final @TT ".m2"@ is important, as it informs Emacs to
      use the Macaulay2 mode). Paste the following text into a buffer. If you wish, save the file using @KBD "C-x C-s"@.
    Code
      PRE M2CODE ////R = QQ[x,y,z]
      f = symmetricPower(2, vars R)
      M = cokernel f
      C = resolution M
      betti C////
    Text
      Position the cursor on the first line of code, and press the @KBD "F11"@ function key (or @KBD "C-Enter"@)
      repeatedly to present each line to Macaulay2.  If you select several lines using the mouse,
      then pressing @KBD "F11"@ will present the entire selection to Macaulay2.  Try this on some of these lines.
    Text
      Here are some other useful keybindings:
    Code
      TABLE {
	  { KBD "C-c C-j",      " -- send the current line" },
	  { KBD "C-c C-r",      " -- send the current region" },
	  { KBD "C-c C-b",      " -- send the entire buffer" },
	  { KBD "C-c C-<up>",   " -- send everything before the point" },
	  { KBD "C-c C-<down>", " -- send everything after the point" },
	  { KBD "C-c C-p",      " -- send the current paragraph" }
      }
    Code
      HR()
    Text
      Now go to the very end of the @TT "*M2*"@ buffer with @KBD "M->"@ and experiment with keyword completion.
      Type @TT "reso"@ and then press the @KBD "TAB"@ key.  Notice how the word is completed to @TT "resolution"@
      for you.  Delete the word with @KBD "M-DEL"@, type @TT "res"@ and then press the @KBD "TAB"@ key.
      The possible completions are displayed in a window. Switch to it with @KBD "M-x switch-to-completions"@,
      move to the desired completion, press the @KBD "Enter"@ key, and then return to the @TT "*M2*"@
      buffer with @KBD "C-x o"@.  Alternatively, if you have a mouse, use the middle button to select the
      desired completion. (On the mac, hold down the option key while clicking the mouse.)
    Code
      HR()
    Text
      Experiment with command line history in the @TT "*M2*"@ buffer.  Position your cursor at the end of
      the buffer, and then use @KBD "M-p"@ and @KBD "M-n"@ to move to the previous and next line of input
      remembered in the history.  When you get to one you'd like to run again, simply press return to do
      so.  Or edit it slightly to change it before pressing return.
    Code
      HR()
    Text
      Now let's see how we can handle wide and tall Macaulay2 output.
      Execute the following line of code (put it in your @TT "foo.m2"@ buffer, and then press @KBD "F11"@).
    Code
      PRE M2CODE "printWidth = 0; random(R^20, R^{6:-2})"
    Text
      Setting printWidth to zero removes line wrapping in the buffer, sometimes useful to view large
      matrices.
    Text
      Notice that the long lines in the Macaulay2 window, instead of being wrapped around to the next
      line, simply disappear off the right side of the screen, as indicated by the dollar signs or little
      arrows in the rightmost column.  Switch to the other window and practice scrolling up and down with
      @KBD "M-v"@ and @KBD "C-v"@, and scrolling left and right with the function key @KBD "F3"@ (or @KBD "C-x <"@)
      and the function key @KBD "F4"@ (or @KBD "C-x >"@).  In modern Emacs implementations where mouse
      clicking works, click on the arrow to scroll in that direction.  In these versions of Emacs, typing
      @KBD "C-e"@, or @KBD "C-a"@ to get at the end or beginning of the line also horizontally scrolls the text to that
      position.  Older Emacs tend to need a bit more: Notice how the use of @KBD "C-e"@ to go to the end of
      the line sends the cursor to the dollar sign at the right hand side of the screen; that's where the
      cursor will appear whenever you go to a position off the screen to the right.  Then use the @KBD "F2"@
      function key (or @KBD "C-S-c ."@) to scroll the text so the cursor appears at the center of the
      screen.  Use @KBD "C-a"@ to move to the beginning of the line and then the @KBD "F2"@ function key (or
      @KBD "C-S-c ."@) to bring the left margin back into view.
    Text
      You may use @KBD "C-S-c SPC"@ to toggle whether long lines are truncated or wrapped; initially they
      are truncated.

Node
  Key
    "editing Macaulay2 code with Emacs"
  Description
    Text
      In this section we learn how to use Emacs to edit Macaulay2 code. Assuming you have set up your
      Emacs init file as described in @TO "setting up the Macaulay2 Emacs interface"@, when you visit a
      file whose name ends with @TT ".m2"@ you will see on the mode line the name Macaulay2 in
      parentheses, indicating that the file is being edited in Macaulay2 mode.
    Text
      To see how electric parentheses, electric semicolons, and indentation work, open a file
      whose name ends with @TT ".m2"@ and type the following text.
    Pre
      f = () -> (
	  a := 4;
	  b := {6,7};
	  a+b)
    Text
      Observe carefully how matching left parentheses are indicated briefly when a right parenthesis is typed.
    Text
      Now position your cursor in between the 6 and 7.  Notice how pressing @KBD "M-C-u"@ moves you up out
      of the list to its left.  Do it again.  Experiment with @KBD "M-C-f"@ and @KBD "M-C-b"@ to move
      forward and back over complete parenthesized expressions.  (In the Emacs manual a complete
      parenthesized expression is referred to as an sexp, which is an abbreviation for S-expression.)  Try
--      out @KBD ////C-u 2 M-C-@////@ as a way of marking the next two complete parenthesized expression, and see
      how to use @KBD "C-w"@ to kill them and @KBD "C-y"@ to yank them back.  Experiment with @KBD "M-C-k"@
      to kill the next complete parenthesized expression.
    Text
      Position your cursor on the 4 and observe how @KBD "M-;"@ will start a comment for you with two
      hyphens, and position the cursor at the point where commentary may be entered.
    Text
      Type @TT "res"@ somewhere and then press @KBD "C-c TAB"@ to bring up the possible completions of the
      word to documented Macaulay2 symbols.
    Text
      Notice how @KBD "C-h m"@ or @KBD "F1 m"@ will display the keystrokes peculiar to the mode in a help
      window.

Node
  Key
    "using Macaulay2 with TeXmacs"
  Description
    Text
      TeXmacs is a free visual text editor for mathematics that can be used to produce $\TeX$ output.
      It also supports interactive use with various symbolic algebra programs, such as Macaulay2.
      TeXmacs is available from @HREF{"https://www.texmacs.org", "TeXmacs.org"}@.
    Text
      Using TeXmacs as an interface to Macaulay2 is described in the TeXmacs
      @HREF{"http://www.texmacs.org/tmweb/manual/webman-interface.en.html", "online manual"}@.
      The basic procedure is to pull down the @KBD "Insert"@ menu, select @KBD "Session"@ from it,
      then select @KBD "Macaulay2"@ from the resulting submenu. Alternatively, you can click on
      the icon that looks like a computer and select @KBD "Macaulay2"@.
    Text
      The interface is implemented by a special top level mode in Macaulay2
      that causes the output to be converted to @TO mathML@, see @TO TeXmacs@.
      Note that MathML conversion methods have not been implemented yet for all data types.
///
