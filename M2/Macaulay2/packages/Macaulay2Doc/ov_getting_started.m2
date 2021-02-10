-- -*- coding: utf-8 -*-
-- Nodes for the getting started section of the overview

document {
     Key => "getting started",
     PARA {
	  "To get a good idea of what you will be able to do with Macaulay2, see
	  the package ", TO2{ "BeginningMacaulay2::BeginningMacaulay2", "BeginningMacaulay2" }, "."
	  },
     PARA {
	  "Macaulay2 is available from our web page ", 
	  HREF "http://www.math.uiuc.edu/Macaulay2/",
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
	  TO "checking your Macaulay2 installation",
	  TO "finding the Macaulay2 files",
	  TO "teaching M2 how to find its shared libraries",
	  TO "teaching your shell how to find M2",
	  TO "moving or copying the Macaulay2 files",
	  TO "running Macaulay2 in a terminal window",
	  TO "using Macaulay2 with emacs",
	  TO "using Macaulay2 with TeXmacs",
	  TO "a first Macaulay2 session",
	  TO "reading the documentation",
	  TO "getting help or reporting bugs",
	  -- Mike wanted this: TO "what to read next??"
	  }
     }

document {
     Key => "using Macaulay2 with TeXmacs",
     PARA {
	  "Texmacs is a free visual text editor for mathematics that can be used to produce TeX output.  It also supports
	  interactive use with various symbolic algebra programs, such as Macaulay2.  Texmacs is available
	  from ", HREF "http://www.texmacs.org/", "."
	  },
     PARA {
	  "Be sure to get TeXmacs version 1.0.6.12 or later.  There is bug in 1.0.6.10, the effect of
	  which is that no output from Macaulay2 will be displayed."
	  },
     PARA {
	  "Using TeXmacs as an interface to Macaulay2 is described in the TeXmacs online
	  manual: ", HREF "http://www.texmacs.org/tmweb/manual/webman-interface.en.html", ".
	  The basic procedure is to pull down the ", TT "Insert", " menu, select ", TT "Session", " from it,
	  and then to select ", TT "Macaulay2", " from the resulting submenu.  Alternatively, you 
	  can click on the icon that looks like a computer terminal and select ", TT "Macaulay2", " from the resulting submenu."
	  },
     PARA {
	  "The interface is implemented by a special top level mode in Macaulay2 that causes
	  the output to be converted to mathML, see ", TO "TeXmacs", ".  MathML conversion methods have
	  not been implemented yet for all data types."
	  }
     }

document {
     Key => "teaching M2 how to find its shared libraries",
     "Perhaps you know that your M2 executable is located at ", TT "/foo/bar/bin/M2", ", say, but when you run
     it, you get something like this:",
     PRE ///    /foo/bar/bin/M2
    M2: error while loading shared libraries: liblapack.so: cannot open shared object file: No such file or directory///,
     "What that means is that M2 hasn't been told where its shared libraries are.  Actually, it's the operating
     system that has to be told, since otherwise M2 can't even start up.  Hopefully, the missing shared libraries
     are located in ", TT "/foo/bar/lib", ", and all we have to do is to tell the operating system by
     setting the environment variable ", TT "LD_LIBRARY_PATH", ", if you are running a Unix operating system.
     If you have downloaded a version of Macaulay2 that comes with dynamically loaded libraries of its own,
     they will be in the directory ", TT concatenate("/foo/bar/",Layout#1#"libraries","lib/"), ".
     After setting ", TT "LD_LIBRARY_PATH", " temporarily you may use ", TO "setup", " to record the correct value in your 
     system start up files.",
     PARA {"Alternatively, you may be getting something like this:"},
     PRE ///    $ /foo/bar/bin/M2
    dyld: Library not loaded: /capybara/lib/libgmp.3.dylib
      Referenced from: /foo/bar/bin/M2
      Reason: image not found
    Trace/BPT trap///,
     "That would mean that you are running under Mac OS, and the suggestion above would not apply.  On such systems,
     Macaulay2 must be installed in the system application folder, or Macaulay2 will not be able to find its libraries."
     }

document {
     Key => {setup,1:setup},
     Usage => "setup()",
     Consequences => {
	  {"Initialization files for emacs and the standard command shells will have lines added to them to allow the 
	       Macaulay2 files to be found.
	       A missing initialization file will be created only if doing so will not prevent the command shell from
	       reading further initialization files.
	       The user is prompted before each file is modified and offered the opportunity to review or reject the changes." }
	  },
     PARA {
	  "Backup files are made carefully, and if a mangled initialization file is detected, it will not be modified.
	  The lines added are bracketed by comment lines containing the phrases ", TT "Macaulay 2 start", " and ", TT "Macaulay 2 end", "."
	  },
     PARA {
	  "The initialization files will contain added commands that add the appropriate directory to the front of the
	  environment variable ", TT "PATH", " so the program ", TT "M2", " can be found, unless that directory already occurs
	  in the path, in which case it does not check whether that directory is the first one on the path containing a program
	  whose name is ", TT "M2", ", for fear of escalating a competition with another init file.  (The expert user may prefer to remove
	  those commands and instead, to create a symbolic link from a directory already on their path to the program ", TT "M2", ".)"
	  },
     PARA {
	  "The other environment variables affected are ", TT "MANPATH", ", ", TT "INFOPATH", ", and ", TT "LD_LIBRARY_PATH", "."
	  },
     PARA {
	  "This function is intended to be run only by users, not by system administrators, nor on behalf of users by system
	  administrators, because system administrators can easily install Macaulay2 in such a way that the files can all be
	  found."
	  },
     Caveat => {
	  "Warning: there is at least one important situation where you will not want
	  to run the Macaulay2 ", TT "setup()", " command.  For example, suppose you have
	  installed M2, not in its own private location as described above, but in a
	  directory with many other executable files.  Suppose also that your account
	  is adapted so it will run programs correctly on hardware of more than one
	  architecture, or on differing operating systems, or even on various releases
	  of the same operating system.  Then ", TT "setup()", " will modify your command shell
	  init files so the directory containing M2 will the first one searched when
	  looking for a program.  If you then try to use your account on different
	  hardware, incorrect programs may be found, namely, those in the same
	  directory as M2.  Similarly, incorrect shareable libraries may also be found."
	  },
     SeeAlso => {
	  setupEmacs
	  }
     }

document {
     Key => {setupEmacs,1:setupEmacs},
     Usage => "setupEmacs()",
     Consequences => {
	  {"The initialization file for emacs (.emacs in your home directory) will have lines added to allow the Macaulay2 emacs mode to operate.
	       The user is prompted before the file is modified and offered the opportunity to review or reject the change." }
	  },
     PARA {
	  "A backup files is made carefully, and if a mangled initialization file is detected, it will not be modified.
	  The lines added are bracketed by comment lines containing the phrases ", TT "Macaulay 2 start", " and ", TT "Macaulay 2 end", ".
	  The function ", TO "setup", " does all this and more."
	  },
     SeeAlso => {
	  setup,
	  "setting up the Macaulay2 emacs interface",
	  "using Macaulay2 with emacs"	  
	  }
     }

document { Key => "moving or copying the Macaulay2 files",
     "It is important, if you want to move or copy the Macaulay2 files, to move or
     copy the entire directory tree, so their relative locations do not change.",
     PARA{},
     "When ", TT "M2", " starts up, it locates the files it needs (such as ", TT "setup.m2", "),
     by first locating ", TT "M2", " and then traversing the directory tree it expects to find ", TT "M2", " in.  If you copy
     or move just ", TT "M2", " (or ", TT "M2.exe", ", under windows) to some other directory, then 
     that traversal will not be successful.",
     PARA{},
     "There are other ways to arrange for ", TT "M2", " to be on your PATH: with a symbolic link, or
     by adding a directory name to the PATH; see ", TT "teaching your shell how to find M2", "."     
     }

document { Key => "finding the Macaulay2 files",
     "Often you will know where the Macaulay2 files are, because you have
     installed them yourself.  But it can happen that Macaulay2 was installed
     by your system administrator so you can run M2, but you don't know where
     its files are.  In that case, there are a couple of ways to locate the
     files.",
     PARA {},
     "One way, under Unix, is to use the following shell command.",
     PRE ///     type M2///,
     "The response will be of the following form.",
     PRE ///     M2 is /foo/bar/bin/M2///,
     "The Macaulay2 files come in a directory tree that mimics the directory
     tree found on unix systems in the directory /usr.  In particular, its
     top-level directory has subdirectories called ", TT "bin", ", ", TT "info", ", ", TT "lib", ", and
     ", TT "share", ", so now we know, from the output above, that the Macaulay
     2 files have been installed with a prefix ", TT "/foo/bar", " as the name
     of its root directory.  It will follow, for example, that the M2 emacs
     init file is located at ", TT "/foo/bar/share/emacs/site-lisp/Macaulay2/M2-init.el", ".",
     PARA {},
     "Another way to locate the files of Macaulay2 is to ask M2, assuming you
     can run it.  Start M2 and type the following expression.",
     PRE ///     prefixDirectory///,
     "The response will be of the following form, and will also tell you the
     prefix for the paths to the Macaulay2 files.",
     PRE ///     i1 : prefixDirectory

     o1 = /foo/bar/ ///,
     "If you are running M2 and emacs under cygwin on a Windows computer, then that
     can prevent special difficulties: see ", TO "finding your files under cygwin", "."
     }

document { Key => "finding your files under cygwin",
     "In Microsoft Windows, most people run programs by pulling down menus or by clicking on icons
     that have been carefully placed there by install programs.  Thus the idea of a
     ", EM "path", " along which to search for programs (such as a browser) is no longer useful in Windows.",
     PARA{},
     "On my computer the browser program ", TT "firefox.exe", " is in the following directory.",
     PRE ///    C:/Program Files/Mozilla Firefox///,
     "But M2 is a Cygwin program, and it lives in a different world, where paths to
     files don't ever start out with something like ", TT "C:", ".  In that world, firefox's
     directory is known instead as:",
     PRE ///     /cygdrive/c/Program Files/Mozilla Firefox///,
     "Conversely, the root directory, known in the Cygwin world as ", TT "/", ", could be located
     anywhere in the Windows world.  On my machine it is at",
     PRE ///     C:/cygwin///,
     "Use the ", TT "df", " command or the ", TT "mount", " command in a cygwin command shell window to determine
     that path: it is the file system on which ", TT "/", " is mounted."
     }

document { Key => "teaching your shell how to find M2",
     "Perhaps you have typed M2 into a shell window and gotten something like the
     following response:",
     PRE ///     % M2
     bash: M2: command not found///,
     "If so, then you have to teach your shell how to find M2.",
     PARA {},
     "Your shell will look for M2 in the directories listed in the value of the PATH environment variable.  You
     will want to arrange for that value to get set when you log in or when you start your shell.  The former
     is preferable, because environment variables are inherited by new processes only from their 
     parents, and your login shell is an ancestor of all of your processes.",
     PARA {},
     "If you teach your shell how to find M2, then emacs may be able to find M2, also.",
     PARA {},
     "The simplest way to teach your shell how to find M2 is to let M2 do it for you.  Assume that you have
     found M2, and it is located at the path ", TT "/foo/bar/bin/M2", ".  Run the command /foo/bar/bin/M2,
     and then, in response to Macaulay2's input prompt, enter ", TT "setup()", ".  If that works,
     the next time you log in or start a new shell, the shell should know how to find M2 (see
     ", TO "setup", ").  If that doesn't work, read onward.",
     PARA {},
     "Your goal is to add the directory containing M2 to the value of the PATH environment variable.
     For this, you must know where the Macaulay2 files are: see ", TO "finding the Macaulay2 files", ".  Your system administrator may have installed
     the Macaulay2 files under /usr, in which case you will see M2 in /usr/bin, and you can ignore the 
     rest of this section.  The files may also be installed under /usr/local, in which case you will see M2
     in /usr/local/bin, and you can ignore the rest of this section, provided you have /usr/local/bin
     on your PATH.  Or the files may be installed somewhere else, such as in /Applications/Macaulay2,
     in which case you will see M2 in /Applications/Macaulay2/bin, and you will want to add that
     directory to your path now.",
     PARA {},
     "The method for setting environment variables depends on which shell you are
     using.  Typical shells in use include ash, bash, csh, tcsh, ksh, sh, and zsh.  The command
     languages understood by these shells differ, but they fall into two main classes: the 
     Bourne shells sh, bash, ash, zsh, and ksh; and the C shells csh and tcsh.",
     SUBSECTION "Bourne shells",
     "A Bourne shell reads commands from the file .profile in your home directory when you log in,
     except perhaps under Mac OS X.
     The bash shell will also read commands from .bashrc each subsequent time it starts up,
     after the initial log in.  To add a directory called, say ", TT "/foo/bar/bin", ", to your
     PATH, put this command in your file .profile:",
     PRE "     export PATH=/foo/bar/bin:$PATH",
     "It will be acted upon the next time you log in, or the following shell command will
     run those commands in your current shell.",
     PRE "     source $HOME/.profile",
     SUBSECTION "C shells",
     "A C shell reads commands from the file .login in your home directory when you log in,
     except perhaps under Mac OS X.
     The shell will also read commands from .cshrc or perhaps .tcshrc.  Check the man page of your
     shell.",
     PARA {},
     "To add a directory called, say ", TT "/foo/bar/bin", ", to your PATH, put this command in your file .login:",
     PRE "     setenv PATH /foo/bar/bin:$PATH",
     "It will be acted upon the next time you log in, or the following shell command will
     run those commands in your current shell.",
     PRE "     source $HOME/.profile",
     SUBSECTION "making a link to M2",
     "Another way to proceed that sometimes works is this.  Look at the output from the shell command:",
     PRE ///	  printenv PATH///,
     "and see whether one of your own directories is already on the path.
     If so, say it's ~/bin, then you can make a symbolic link from M2 to that directory,
     and it will appear on your path.  First ensure the directory has been made with this command:",
     PRE ///      mkdir ~/bin///,
     "Ignore the error message if the directory already exists.  Then make the symbolic link with this command:",
     PRE ///	  ln -s /Applications/Macaulay2/bin/M2 ~/bin/M2///,
     "After that your shell will be able to find M2, and M2 will be able to find
     its files (because it knows about symbolic links).  (Don't use a hard link.)",
     SUBSECTION "what else to try",
     "If you fail to teach your shell how to find M2, then all is not lost.  We prefer
     to run M2 within emacs, and it is enough to teach emacs how find M2.  See ", TO "teaching emacs how to find M2", "."
     }

document { Key => "teaching emacs how to find M2-init.el",
     "Files containing emacs source code have names of the form ", TT "*.el", ".
     Macaulay2 comes with a file called ", TT "M2-init.el", " that sets up
     emacs for running M2 conveniently.  It is important that emacs be able to
     find that file and the three other files that come with it, by searching
     in the directories listed in the emacs variable ", TT "load-path", ".",
     PARA{},
     "If you are lucky, then the Macaulay2 directory tree has been installed
     with the same root as the emacs directory tree.  For example, if emacs
     and Macaulay2 are both installed in /usr, then ", TT "M2-init.el", " is located at
     ", TT "/usr/share/emacs/site-lisp/Macaulay2/M2-init.el", ", and emacs already knows
     to look in that directory for source files.",
     PARA {},
     "The simplest way to teach emacs how to find ", TT "M2-init.el", " is to let M2 do it for you.  Run M2,
     and then, in response to Macaulay2's input prompt, enter ", TT "setup()", ".  If that works,
     the next time you start emacs, it should know how to find ", TT "M2-init.el", " (see
     ", TO "setup", ").  If that doesn't work, read onward.",
     PARA {},
     "To determine the precise path of the site-lisp directory emacs is looking
     in, so that you can install Macaulay2 properly, use the emacs
     describe-variable command, accessible with the key strokes ", TT "C-h v",
     ", and ask for the description of the variable ", TT "load-path", ".",
     PARA {},
     "Let's assume that you have located the Macaulay2 source code, and that
     ", TT "M2-init.el", " is located at ", TT "/foo/bar/share/emacs/site-lisp/Macaulay2/M2-init.el", ",
     and that you want to tell emacs to search that directory, too.  
     Insert the following command into the file .emacs in your home directory.",
     PRE ///(add-to-list 'load-path "/foo/bar/share/emacs/site-lisp/Macaulay2")///,
     "The next time you start emacs, emacs will look also in that directory for 
     files, and it should find ", TT "M2-init.el", "."
     }


document { Key => "teaching emacs how to find M2",
     "If you teach your shell how to find M2, then you do not have to teach emacs how
     to find M2.  See ", TO "teaching your shell how to find M2", ", and come back to
     this section only if you fail with that.  Let's assume that you have found M2 (the 
     program), and that is located in the directory ", TT "/foo/bar/bin", ", say.",
     PARA {},
     "Let's assume you have already set up the function key f12 to call M2.  That is done with
     the following command in the .emacs file.",
     PRE ///     (global-set-key [ f12 ] 'M2)///,
     "Then when you press f12, M2 should start running.",
     PARA {},
     "Here is what you will see on your screen in the minibuffer at the bottom of the screen when you press f12 if
     emacs doesn't know how to find the file ", TT "M2-init.el", ".",
     PRE ///     Symbol's function definition is void: M2///,
     "If you see that, you are not ready for this section: see ", TO "teaching emacs how to find M2-init.el", ".",
     PARA {},
     "Here is what you will see on your screen in a buffer named ", TT "*M2*", " if emacs knows how
     to find the file ", TT "M2-init.el", " but not how to find the program ", TT "M2", ".",
     PARA {},
     PRE ///    + M2 --no-readline --print-width 189
    /bin/sh: M2: command not found

    Process M2 exited abnormally with code 127
///,
     SUBSECTION "teaching emacs temporarily",
     "To teach emacs temporarily where to find M2, press",
     PRE ///     C-u f12///,
     "(Recall that in emacs' notation for key-presses, C-u means to press u while holding down
     the control key.)  You will get the M2 command line in the minibuffer at the bottom of
     the screen, and you can edit it.  It will initially look something like this:",
     PRE ///     M2 --no-readline --print-width 189 ///,
     "You can change it to the right thing:",
     PRE ///     /foo/bar/bin/M2 --no-readline --print-width 189 ///,
     "Then press ", TT "enter", " and M2 should start running.
     That will stick for the rest of your emacs session.  Later, to return to the ", TT "*M2*", " window from another, or to start
     up M2 again, just press f12.",

     SUBSECTION "teaching emacs permanently",

     "Every time emacs starts up it reads commands from the file .emacs in your home
     directory.  Put the following command in your
     .emacs file.",
     PRE ///(setq M2-exe "/foo/bar/bin/M2")///,
     "The next time you start emacs it will know how to find M2."
     }

document {
     Key => "checking your Macaulay2 installation",
     Headline => "and bookmarking the Macaulay2 documentation",
     PARA {"At a command prompt in a terminal window, type ", TT "M2", 
	  ".  If Macaulay2 has been installed correctly on your machine, 
	  you should see a prompt such as this:"},
PRE///    indigo% M2
    Macaulay2, version 1.0
    with packages: Classic, Elimination, LLLBases, PrimaryDecomposition,
                   SchurRings, TangentCone

    i1 : ///,
     PARA {"If this is the first time that you are running Macaulay2, then Macaulay2
	  creates a directory called ", TT ".Macaulay2", 
	  " in your home directory (on unix machines),
	  or called ", TT "Library/Application Support/Macaulay2", " under MacOS X.  Inside this 
	  directory are several files of interest, including an ", 
	  TT "index.html", " file that contains links to your local copy of the Macaulay2 documentation."},
     PARA {"The ", TO viewHelp, " command in Macaulay2 starts up your web browser (if it is not
	  already running) and places you at this ", TT "index.html", " page.
	  "},
PRE///    viewHelp///,
     PARA {"This web page includes links to the main Macaulay2 documentation, as well as
	  documentation for any installed packages.   This is a good time to
	  bookmark the page in your browser."},
     PARA {"At this point you should try something simple in Macaulay2, such as"},
PRE///    printWidth = 60
    R = QQ[a..d]
    (a+b+c+d)^4///,
     PARA {"
	  To exit Macaulay2, type one of: ", TO "exit", ", ", TO "end", ", ", TO "quit", ", or your 
	  end of file character (which, under Unix, is often CONTROL-D)."},
	  PRE///    exit///,
     PARA {"Macaulay2 can be run in this way from the command line, but it is
	  generally much more convenient to run Macaulay2 from inside the emacs
	  text editor.  This is because you can more easily view larger objects,
	  do command completion, use cut and paste, search, save your session,
	  and so on.  There is a nice mode for running Macaulay2 inside
	  emacs."},
     }

document {
     Key => "setting up the Macaulay2 emacs interface",
     PARA {"If you are a newcomer to emacs, you should spend a few minutes
	  going through the emacs tutorial.  Start up emacs with the command 
	  ", TT "emacs", " and then start up the emacs tutorial with the keystrokes 
	  ", TT "C-h t", ".  (The notation ", TT "C-h", " indicates that you should type 
          ", TT "Control-H", ", by holding down the control key, 
	  and pressing ", TT "H", ".)  The emacs tutorial will introduce you to the
          basic keystrokes useful with emacs.  After running through that you will want
          to examine the online emacs manual that can be read with ", TT "info", "
          mode; you may enter or re-enter that mode with the keystrokes ", TT "C-h i", ".  "
          },
     PARA {"The Macaulay2 emacs interface consists of several files in the directory ", TT "share/emacs/site-lisp/Macaulay2/",
	  " in the Macaulay2 distribution tree.  If you're lucky, then your system administrator has installed Macaulay2 so
	  that directory ends up in the same place where emacs looks for its files, as listed by the emacs variable ", TT "loadpath", ".
	  If not, then in order for emacs to be able to find these files, place the following lines
	  in the ", TT ".emacs", " file in your home directory, creating the file if necessary.  Alternatively, 
	  and much simpler, run M2, and let the ", TO setupEmacs,  " function do it for you.",
	  },
PRE///    ;; .emacs file in your home directory
    (load "M2-init")

    ; comment out the following line with an initial semicolon 
    ; if you want to use your f12 key for something else
    ; or change f12 to (for example) f8
    (global-set-key [ f12 ] 'M2)///,
     PARA {"After saving your ", TT ".emacs", " file, try running emacs.  Start Macaulay2
	  by pressing the f12 function key.  On MacOS X systems, f12 is usurped
	  by either DashBoard, SpotLight, or something else, so either you must
	  change the f12 to some other key, e.g., f8, in the ", TT ".emacs", " file, or you should disable
	  the systems use of the f12 key."
	  },
     PARA {"If Macaulay2 doesn't start up (in a buffer named ", TT "*M2*", "), check that you
	  typed in the above lines correctly into ", TT ".emacs", ".  If they look okay, then
	  see ", TO "teaching emacs how to find M2-init.el", ", ", TO "teaching emacs how to find M2", ", and ",
	  TO "teaching your shell how to find M2", "."
	  }
     }

document {
     Key => "using Macaulay2 with emacs",
     PARA {
	  "Emacs is a free text editor written by Richard Stallman, which can be used as an ascii-oriented interface to Macaulay2.
	  The best way to edit Macaulay2 code or to run Macaulay2 is with emacs.
	  Emacs is available as part of many operating systems, as well as from ", HREF "http://www.gnu.org/software/emacs/", "."
	  },
     Subnodes => {
	  TO "setting up the Macaulay2 emacs interface",
	  TO "teaching emacs how to find M2-init.el",
	  TO "teaching emacs how to find M2",
	  TO "running Macaulay2 in emacs",
	  TO "using Macaulay2 with emacs after it has been set up",
	  TO "editing Macaulay2 code with emacs"
	  }
     }

document {
     Key => "using Macaulay2 with emacs after it has been set up",
     PARA {"In this section, we show by example how to use the Macaulay2 emacs interface.
	  We assume that you have already set up this interface, as described in 
	  ", TO "setting up the Macaulay2 emacs interface", ".  After creating or changing
	  the .emacs file mentioned there, you need to exit and restart emacs.  For the rest
	  of this section, we assume that you are running emacs."
	  },
     "The aspects of this interface that we describe include",
     UL {
	  "Starting Macaulay2 with the f12 key, or with M-x M2",
	  "Working with two buffers",
	  "Sending lines or selected text to Macaulay2 using the f11 key",
	  "Command completion with TAB",
	  "Horizontal scrolling with f3,f4,f5,f6,f7"
	  },
     PARA {"Before starting, note that when we say to type M-x M2, what we really mean is: press the x key while holding down the meta key (on Macs this is either
	  the option key or the apple key, depending on how your emacs is set up); type M2; and then press the return (or enter) key after that.  Similarly, C-c
	  means to press the c key while holding down the control key, and ", TT "C-x 2", " means to press x while holding down the control key, then to
	  press 2; this time do not press enter."},
     PARA {"Use the keystrokes ", TT "C-x 2", " to divide the buffer containing this file into two windows.
     	  Then press the ", TT "f12", " key or type ", TT "M-x M2", " to start up Macaulay2 in a buffer
     	  named ", TT "*M2*", ".  (The command line used to start Macaulay2 may
	  be edited before being run if you use a prefix argument with the above
	  command: press ", TT "C-u", " just before.)"
          },
     PARA {
          "If f12 doesn't start up Macaulay2, one reason may be that your function
	  keys are not operable.  In that case press ", TT "C-c m", " instead. 
       	  Another
          reason may be that you have not installed Macaulay2 properly - the startup
	  script (", TT "M2", " or ", TT "M2.bat", ") should be on your path.
	  A third reason may be that you are in Windows-98 and are using anti-virus 
	  software such as ", TT "Dr. Solomon's", ", which can interfere with emacs 
	  when it tries to run a subprocess."
	  },
     PARA {
     	  "You may use ", TT "C-x o", " freely to switch from one window to the other.
	  Verify that Macaulay2 is running by entering a command such as ", TT "2+2", ".  
	  Now create (using C-x C-f) a file, named something like foo.m2 (the final .m2 is
	  important, as it informs emacs to use the Macaulay2 mode).
	  Paste the following text into a buffer.  If you wish, save the file using C-x C-s."
	  },
     PRE ///    R = QQ[x,y,z]
    f = symmetricPower(2,vars R)
    M = cokernel f
    C = resolution M
    betti C///,
     PARA {     
     "Position the cursor on the first line of code, and press the ", TT "f11", " function 
	  key repeatedly to present each line to Macaulay2.  If you select several lines 
	  using the mouse, then pressing f11 will present the entire selection to
	  Macaulay2.  Try this on some of these lines."
	  },
     HR{},
"Now go to the very end of the ", TT "*M2*", " buffer with ", TT "M->", " and 
experiment with keyword completion.  Type ", TT "reso", " and then press the 
", TT "TAB", " key.  Notice how the word is completed to ", TT "resolution", "
for you.  Delete the word with ", TT "M-DEL", ", type ", TT "res", "
and then press the ", TT "TAB", " key.  The possible completions are displayed 
in a window.  Switch to it with the ", TT "F8", " key, move to the desired 
completion, select it with the ", TT "RETURN", " key, and then return to the 
", TT "*M2*", " buffer with ", TT "C-x o", ".  Alternatively, if you have a
mouse, use the middle button to select the desired completion. (On the mac, hold down the
     option key while clicking the mouse)",
HR{},
PARA{},
"Experiment with command line history in the ", TT "*M2*", " buffer.  Position 
your cursor at the end of the buffer, and then use ", TT "M-p", " and ", TT "M-n", " 
to move to the previous and next line of input remembered in the history.  When you 
get to one you'd like to run again, simply press return to do so.  Or edit it
slightly to change it before pressing return.",
     HR{},
     PARA{},
     "Now let's see how we can handle wide and tall Macaulay2 output.  Execute the
     following line of code (put it in your foo.m2 buffer, and then press f11)",
     PARA{},
     PRE ///printWidth=0; random(R^20,R^{6:-2})///,
     "Setting printWidth to zero removes line wrapping in the buffer, sometimes useful to 
     view large matrices.",
PARA{},
"Notice that the long lines in the Macaulay2 window, instead of being wrapped
around to the next line, simply disappear off the right side of the screen,
as indicated by the dollar signs or little arrows in the rightmost column.  Switch to the
other window and practice scrolling up and down with ", TT "M-v", " and ", TT "C-v", ", 
and scrolling left and right with the function key ", TT "F3", " (or ", TT "C-c <", ") 
and the function key ", TT "LinearAlgebra", " (or ", TT "C-c >", ").  In modern emacs implementations
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
PARA{},
"You may use the ", TT "f5", " function key or (or ", TT "C-c ?", ") to 
toggle whether long lines are truncated or wrapped; initially they are truncated."
     }

doc ///
  Key
    "reading the documentation"
  Description
    Text
      The documentation for Macaulay2 is available in several formats and can
      be searched and viewed using the following functions:

    Tree
      :Finding documentation nodes
        about
	apropos
      :Accessing documentation nodes
        help
	viewHelp
	infoHelp
	(symbol?, Symbol)
      :Getting other information from documentation
        examples

    Text
      To begin, the @TO "about"@ method allows for searching all documentation nodes
      whose title or content contains a given string:
    Example
      about "Horrocks-Mumford"
    Text
      The @TO "apropos"@ method is useful for finding all exported objects whose symbol
      matches the given regular expression:
    Example
      apropos "(H|h)ilbert"
    Text
      While in Macaulay2, type @TO "help"@ to get the documentation on a topic or function,
      or type @TO "viewHelp"@ to open @TT "~/.Macaulay2/index.html"@, which contains a list
      of all installed packages, in your default web browser.

      For help on a specific topic, e.g., the Jacobian function, use @TT "viewHelp jacobian"@
      or @TT "viewHelp \"jacobian\""@, or if you want the documentation for Jacobian of an
      Ideal, use @TT "viewHelp (jacobian, Ideal)"@ or @TT "viewHelp \"jacobian(Ideal)\""@.
      Using @TO "help"@ instead of @TO "viewHelp"@ results in the help text appearing
      in your Macaulay2 session.

      @BOLD "A useful tip:"@ within Emacs, if you place your cursor on one of the resulting
      output lines that starts with a @TT "*"@, and press the return key, Macaulay2 will
      display that documentation node.

      The documentation for most functions comes with example code.
      You can obtain the text of this example code using @TO "examples"@.
  Subnodes
    about
    apropos
    help
    viewHelp
    infoHelp
    (symbol?, Symbol)
    examples
///

document {
     Key => "getting help or reporting bugs",
     PARA {
	  "An easy way to get help or to report a bug is to use
	  our google group web page at ", HREF "http://groups.google.com/group/macaulay2", ".
	  These requests are automatically emailed to Dan Grayson and Mike Stillman, and they 
     	  try to handle these requests quickly."
	  }
     }

-*
-- Mike wanted this: 
document {
     Key => "what to read next??",
     }
*-

document {
     Key => "a first Macaulay2 session",
     PARA {
	  "Your first input prompt will be ", TT "i1 : ", ".  In response to the prompt,
	  type ", TT "2+2", " and press return.  The expression you entered will be
	  evaluated - no punctuation is required at the end of the line."},
     EXAMPLE "2+2",
     "The answer is displayed to the right of the output label ", TT "o1 =", ".",
     PARA{},
     "Here is some arithmetic with fractions.",
     EXAMPLE "3/5 + 7/11",
     PARA {"Notice the additional line of output labeled with ", TT "o2 :", ".  Output 
	  lines labeled with colons provide information about the type of output.  In 
	  this case, the symbol ", TO "QQ", " is our notation for the class of all 
	  rational numbers, and indicates that the answer on the previous line is a 
	  rational number."},
     PARA{},
     "Multiplication is indicated with ", TO "*", ".",
     EXAMPLE "1*2*3*4",
     "Powers are obtained with ", TO "^", ".",
     EXAMPLE "2^200",
     "Factorials are obtained with ", TO "!", ".",
     EXAMPLE "40!",			  -- this is o4 and is retrieved below
     "Because some answers can be very long, it is a good idea to run the
     program in a window that does not wrap output lines, and allows the
     user to scroll left horizontally to see the rest of the output.  (See
	  ", TO "using Macaulay2 with emacs", ".)",
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
     "The answer comes out as a module that is expressed as the image of
	  a homomorphism whose matrix is displayed.  Integers inside braces to
	  the left of the matrix give the degrees of the basis elements of the
	  target of the matrix; they are omitted if the degrees are all zero.
	  In case the matrix itself is desired, it can be obtained with ", TO "generators", ", as follows.",
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
     "Let's produce the equations generated by the equations that assert
     	  that m and n commute with each other.  (See ", TO "flatten", ".)",
     EXAMPLE "j = flatten(m*n - n*m)",
     "Let's compute a Gröbner basis for the image of ", TT "j", " with ", TO "gb", ".",
     EXAMPLE "gb j",
     "The resulting Gröbner basis contains a lot of information.
	  We can get the generators of the basis, and even though we call upon
	  ", TO "gb", " again, the computation will not be repeated.",
     EXAMPLE "generators gb j;",
     "The semicolon prevents the matrix of generators from appearing on the 
	  screen, but the class of the matrix appears -- we see that there are 26
	  generators.",
     PARA{},
     "We can use ", TO "betti", " to see the degrees involved in the Gröbner basis.",
     EXAMPLE "betti gb j"
     }


document {
     Key => "editing Macaulay2 code with emacs",
     PARA {
	  "In this section we learn how to use emacs to edit Macaulay2 code.
	  Assuming you have set up your emacs init file as described in 
	  ", TO "setting up the Macaulay2 emacs interface", " when you visit a file
	  whose name ends with ", TT ".m2", " you will see on the mode line the
	  name Macaulay2 in parentheses, indicating that the file is being
	  edited in Macaulay2 mode."
	  },
     PARA{
	  "To see how electric parentheses, electric semicolons, and indentation work,
	  move to a blank line of this file and type the following text.",
	  },
PRE ///f = () -> (
     a := 4;
     b := {6,7};
     a+b)///,
     PARA{
	  "Observe carefully how matching left parentheses are indicated briefly when a
	  right parenthesis is typed."
	  },
     PARA{
	  "Now position your cursor in between the 6 and 7.  Notice how
	  pressing ", TT "M-C-u", " moves you up out of the list to its left.  Do it 
	  again.  Experiment with ", TT "M-C-f", " and ", TT "M-C-b", " to move forward
	  and back over complete parenthesized
	  expressions.  (In the emacs manual a complete parenthesized expression is
	  referred to as an sexp, which is an abbreviation for S-expression.)  Try out
	   ", TT "C-u 2 M-C-@", " as a way of marking the next two complete parenthesized
	  expression, and see how to use ", TT "C-w", " to kill them and ", TT "C-y", " to yank 
	  them back.  Experiment with ", TT "M-C-k", " to kill the next complete parenthesized 
	  expression."
	  },
     PARA{
	  "Position your cursor on the 4 and observe how ", TT "M-;", " will start a comment 
	  for you with two hyphens, and position the cursor at the point where commentary
	  may be entered."
	  },
     PARA{
	  "Type ", TT "res", " somewhere and then press ", TT "C-c TAB", " to bring up the
	  possible completions of the word to documented Macaulay2 symbols."
	  },
     PARA{
	  "Notice how ", TT "C-h m", " or ", TT "F1 m", " will display the keystrokes peculiar to 
	  the mode in a help window.",
	  }
}

document {
     Key => "Invoking the program",
     "On systems with a command line interface, the following commands
     can be used to start the program.  When the program starts up,
     the ", TO "initialization file", ", ", TT "init.m2", ", will be loaded.",
     PARA{},
     TT "M2", " -- starts the program.",
     PARA{},
     TT "M2 file1 file2 ... ", " -- starts the program, reading and 
     executing the specified files.",
     PARA{},
     "The options that can be provided on the command line may be displayed by running ", TT "M2 --help", ".",
     EXAMPLE ///assert(0 == run (commandLine#0 | " --help"));///,
     -- In the long run, it would be better if we added the directory containing our M2 to PATH.
     PARA{},
     "To terminate the program, one may type ", TO "exit", ", ", TO "quit", ",
     ", TO "end", ", or the end of file character.",
     SeeAlso => {"prefixDirectory","prefixPath"}
     }

document {
     Key => "running Macaulay2 in a terminal window",
     PARA {
     	  "Some answers in Macaulay2 can be very wide, but many of them will be wrapped
	  appropriately to fit in the width of a terminal window.  Macaulay2 can successfully be
	  used in a terminal window if care is taken not to display very wide objects, but
	  most users will prefer to use the interface provided with emacs or with texmacs."
	  },
     PARA {
	  "Macaulay2 uses the readline library and the history library to handle user input
	  in a terminal window.  The readline user interface documentation is available
	  at ", HREF "http://tiswww.case.edu/php/chet/readline/rluserman.html", ".  It can also
	  be read with the command ", TT "info readline", ".  The history user interface
	  documentation is available at ", HREF "http://tiswww.case.edu/php/chet/readline/history.html#SEC1", ".  
	  It can also be read with the command ", TT "info history", "."
	  },
     PARA {
	  "Name completion is implemented.  Press the ", TT "TAB", " key to automatically complete
	  names of identifiers that have been only partially typed, if possible.  Press ", TT "TAB", " a second
	  time to get a list of the possible completions when multiple choices exist."
	  }
     }

document {
     Key => "running Macaulay2 in emacs",
     "Because some answers can be very wide, it is a good idea to run Macaulay2
     in a window that does not wrap output lines and allows the
     user to scroll horizontally to see the rest of the output.  We
     provide a package for ", TO2{"using Macaulay2 with emacs","emacs"}, " that implements this.
     It also provides for dynamic completion of symbols in the language.",
     PARA{},
     "There is an ASCII version of this section of the documentation distributed
     in the file ", TT (Layout#1#"emacs" | "M2-emacs-help.txt"), ". It might be useful for you to visit
     that file with emacs now, thereby avoiding having to cut and paste bits of
     text into emacs buffers for the deomonstrations below.",
     PARA{},
     "If you are a newcomer to emacs, start up emacs with the command 
     ", TT "emacs", " and then start up the emacs tutorial with the keystrokes 
     ", TT "C-h t", ".  (The notation ", TT "C-h", " indicates that you should type 
     ", TT "Control-H", ", by holding down the control key, 
     and pressing ", TT "H", ".)  The emacs tutorial will introduce you to the
     basic keystrokes useful with emacs.  After running through that you will want
     to examine the online emacs manual that can be read with ", TT "info", "
     mode; you may enter or re-enter that mode with the keystrokes ", TT "C-h i", ".  
     You may also want to purchase (or print out) the emacs manual.  It is cheap,
     comprehensive and informative.  Once you have spent an hour with the emacs
     tutorial and manual, come back and continue from this point.",
     PARA{},
     "We assume you have taught emacs how to find Macaulay2's files, as described in
     the previous sections, and that emacs is loading the file ", TT "M2-init.el", "
     successfully.  Loading that file will cause emacs to enter a special mode for editing
     Macaulay2 code whenever a file whose name has the form ", TT "*.m2", " is
     encountered.  It will also provide a special mode for running Macaulay2 in
     an emacs buffer.  It sets the variable ", TT "transient-mark-mode", " to have
     a different value in each buffer, and sets hooks so that 
     ", TT "transient-mark-mode", " will be set to ", TT "t", " in M2 buffers.  The
     effect of this is that the mark is only active occasionally, and then emacs
     functions that act on a region of text will refuse to proceed unless the
     mark is active.  The ", TT "set-mark", " function or the 
     ", TT "exchange-point-and-mark", " function will activate the mark, and it will
     remain active until some change occurs to the buffer.  The only reason we
     recommend the use of this mode is so the same key can be used to evaluate a
     line or a region of code, depending on whether the region is active.",
     PARA{},
     "Exit and restart emacs with your new initialization file.  
     If you are reading this file with emacs, then use the keystrokes
     ", TT "C-x 2", " to divide the buffer containing this file into two windows.
     Then press the ", TT "M-x M2", " to start up Macaulay2 in a buffer
     named ", TT "*M2*", ".  (The command line used to start Macaulay2 may
     be edited before being run if you use a prefix argument with the above
     command: press ", TT "C-u", " just before.)",
     PARA{},
     "If this doesn't start up Macaulay2, one reason may be that your function
     keys are not operable.  In that case press ", TT "M-x M2", " instead.  (The 
     notation ", TT "M-x", " is emacs notation for pressing the ", TT "x", "
     key while holding down the Meta or Alt key.)  If that doesn't work, please
     see ", TO "teaching emacs how to find M2-init.el", " and ",
     TO "teaching emacs how to find M2", ".",
     PARA{},
     "You may wish to bind the emacs function ", TT "M2-send-to-program", "
     to a global keystroke for ease of use; this is done automatically for
     in Macaulay2 buffers.  For example, the following emacs code
     will bind it to the function key ", TT "f11", ".",
     PARA{},
     PRE ///(global-set-key [ f11 ] 'M2-send-to-program)///,
     PARA{},
     "You may use ", TT "C-x o", " freely to switch from one window to the other.
     Verify that Macaulay2 is running by entering a command such as ", TT "2+2", ".  
     Now paste the following text into a buffer, unless you have the ASCII
     version of this documentation in an emacs buffer already, position
     the cursor on the first line of code, and press the ", TT "f11", " function 
     key repeatedly to present each line to Macaulay2.",
     PARA{},
     PRE ///i1 : R = ZZ/101[x,y,z]
     i2 : f = symmetricPower(2,vars R)
     i3 : M = cokernel f
     i4 : C = resolution M
     i5 : betti C///,
     PARA{},
     "Notice that the input prompts are not submitted to Macaulay2.",
     PARA{},
     "Here is a way to conduct a demo of Macaulay2 in which the code to be
     submitted is not visible on the screen.  Visit a file called ", 
     TT "foo.m2", " and paste the following text into it.",
     PARA{},
     PRE ///20!
     4 + 5 * 2^20
     -- that's all folks!///,
     PARA{},
     "Press ", TT "M-f11", " with your cursor in this buffer to designate it as
     the source for the Macaulay2 commands.  (The notation ", TT "M-f11", " means 
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
     PARA{},
     "One press of ", TT "f11", " brings the next line of code forward into the
     ", TT "*M2*", " buffer, and the next press executes it.  Use ", TT "C-x 5 0", " 
     when you want the demo frame to go away.",
     PARA{},
     "There is a way to send a region of text to Macaulay2: simply select a region
     of text, making sure the mark is active (as described above) and press ", TT "f11", ".
     Try that on the list below; put it into an emacs buffer, move your cursor to the 
     start of the list, press ", TT "M-C-@", " or ", TT "M-C-space", " to mark the list, 
     and then press ", TT "f11", " to send it to Macaulay2.  (The notation ", TT "M-C-@", " 
     means: while holding down the Meta key and the Control key press the ", TT "@", " key, 
     for which you'll also need the shift key.)",
     PARA{},
     PRE ///{a,b,c,d,e,f,
     g,h,i,j,k,l,
     m,n}///,
     PARA{},
     "We have developed a system for incorporating Macaulay2 interactions into TeX
     files.  Here is an example of how that looks.  Paste the following text
     into an emacs buffer.",
     PARA{},
     PRE ///The answer, 4, is displayed after the output label ``{\tt o1\ =}''.
     Multiplication is indicated with the traditional {\tt *}.
     <<<1*2*3*4>>>
     Powers are obtained as follows.
     <<<2^100>>>///,
     PARA{},
     "The bits in brackets can be submitted to Macaulay2 easily.  Position your
     cursor at the top of the buffer and press ", TT "F10.", "  The cursor will move 
     just past the first ", TT "<<<", ", and the emacs mark will be positioned just 
     before the ", TT ">>>", ".  Thus ", TT "1*2*3*4", " is the region, and it will
     even be highlighted if you have set the emacs variable ", TT "transient-mark-mode", "
     to ", TT "t", " for this buffer.  Pressing ", TT "f11", " will send ", TT "1*2*3*4", " 
     to Macaulay2 for execution: try it now.  A sequence of such Macaulay2 commands 
     can be executed by alternately pressing ", TT "F10", " and ", TT "f11", ".  You may
     also use ", TT "M-F10", " to move backward to the previous bracketed expression.",
     PARA{},
--     "Now let's see how we can handle wide and tall Macaulay2 output.  Execute the
--     following line of code.",
--     PARA{},
--     PRE ///random(R^20,R^{6:-2})///,
--     PARA{},
--     "Notice that the long lines in the Macaulay2 window, instead of being wrapped
--     around to the next line, simply disappear off the right side of the screen,
--     as indicated by the dollar signs in the rightmost column.  Switch to the
--     other window and practice scrolling up and down with ", TT "M-v", " and ", TT "C-v", ", 
--     and scrolling left and right with the function key ", TT "F3", " (or ", TT "C-c <", ") 
--     and the function key ", TT "LinearAlgebra", " (or ", TT "C-c >", ").  Notice how the use of
--     ", TT "C-e", " to go to the end of the line
--     sends the cursor to the dollar sign at the right hand side of the screen;
--     that's where the cursor will appear whenever you go to a position off the
--     screen to the right.  Then use the ", TT "F2", " function key (or ", TT "C-c .", ") to 
--     scroll the text so the cursor appears at the center of the screen.  Use ", TT "C-a", " to 
--     move to the beginning of the line and then the ", TT "F2", " function key 
--     (or ", TT "C-c .", ") to bring the left margin back into view.",
--     PARA{},
--     "You may use the ", TT "F5", " function key or (or ", TT "C-c ?", ") to 
--     toggle whether long lines are truncated or wrapped; initially they are truncated.",
--     PARA{},
     "Now go to the very end of the ", TT "*M2*", " buffer with ", TT "M->", " and 
     experiment with keyword completion.  Type ", TT "reso", " and then press the 
     ", TT "TAB", " key.  Notice how the word is completed to ", TT "resolution", "
     for you.  Delete the word with ", TT "M-DEL", ", type ", TT "res", "
     and then press the ", TT "TAB", " key.  The possible completions are displayed 
     in a window.  Switch to it with the ", TT "F8", " key, move to the desired 
     completion, select it with the ", TT "RETURN", " key, and then return to the 
     ", TT "*M2*", " buffer with ", TT "C-x o", ".  Alternatively, if you have a
     mouse, use the middle button to select the desired completion.",
     PARA{},
     "Experiment with command line history in the ", TT "*M2*", " buffer.  Position 
     your cursor at the end of the buffer, and then use ", TT "M-p", " and ", TT "M-n", " 
     to move to the previous and next line of input remembered in the history.  When you 
     get to one you'd like to run again, simply press return to do so.  Or edit it
     slightly to change it before pressing return.",
     }

document {
     Key => "how Macaulay2 finds its files",
     "When you run Macaulay2, it has to find and load a sequence of
     startup files containing code written in the Macaulay2 language.
     Here is the way it does that.",
     PARA{},
     "Its first task is to discover the path to the binary file ", TT "M2", " that is currently running.  On some systems, that
     information is available from the ", TT "/proc", " file system.  Otherwise, it examines the command name you used to run the
     program, which is provided to it as the argument in position number 0 on the command line.  If it's not an absolute path, it searches
     along the path of directories mentioned in the environment variable PATH until it finds a file with the same name.  If the
     result is a symbolic link, the link is followed.  The final
     result is assumed to be in a directory named \"", TT Layout#1#"bin", "\", and the
     startup files are located relative to that.  The path to the top level directory is stored in the variable
     ", TO "prefixDirectory", ", which you can examine to see whether it all worked out.
     For detailed information about the relative location of Macaulay2 files,
     see ", TO "Layout", ".",
     Subnodes => {
	  TO "Layout",
     	  TO "prefixDirectory"
	  }
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
