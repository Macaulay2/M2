-- -*- coding: utf-8 -*-
-- Nodes for the getting started section of the overview

doc ///
Node
  Key
    "setting up Macaulay2"
  Description
    Text
      -- TODO: move this somewhere else
      -- To get a good idea of what you will be able to do with Macaulay2,
      -- see @TO2("BeginningMacaulay2::BeginningMacaulay2", "Beginning Macaulay2")@.
      Macaulay2 is available from our web page @HREF "https://macaulay2.com"@.
      There you will find the online documentation, the source code, and precompiled versions
      for MacOSX, several Linux distributions, and instructions for various other systems.

      For developers, detailed information about building Macaulay2 from source is available
      on GitHub at @HREF "https://github.com/Macaulay2/M2/wiki"@.

      Once Macaulay2 is installed on your system, use the information here to set up
      the Emacs interface (the recommended way to run Macaulay2), bookmark the html documentation,
      and start your first Macaulay2 session.
    Tree
      > "invoking the Macaulay2 program"
      > "checking your Macaulay2 installation"
      > "finding the Macaulay2 files"
      > "teaching M2 how to find its shared libraries"
      > "teaching your shell how to find M2"
      > "running Macaulay2 in a terminal window"
      > "using Macaulay2 with Emacs"
      > "using Macaulay2 with TeXmacs"
      -- Mike wanted this: "what to read next??"
///

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
     of its root directory.  It will follow, for example, that the M2 Emacs
     init file is located at ", TT "/foo/bar/share/emacs/site-lisp/Macaulay2/M2-init.el", ".",
     PARA {},
     "Another way to locate the files of Macaulay2 is to ask M2, assuming you
     can run it.  Start M2 and type the following expression.",
     PRE ///     prefixDirectory///,
     "The response will be of the following form, and will also tell you the
     prefix for the paths to the Macaulay2 files.",
     PRE ///     i1 : prefixDirectory

     o1 = /foo/bar/ ///,
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
     "If you teach your shell how to find M2, then Emacs may be able to find M2, also.",
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
     to run M2 within Emacs, and it is enough to teach Emacs how find M2.  See ", TO "teaching Emacs how to find M2", ".",
     Subnodes => TO "setup"
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
	  generally much more convenient to run Macaulay2 from inside the Emacs
	  text editor.  This is because you can more easily view larger objects,
	  do command completion, use cut and paste, search, save your session,
	  and so on.  There is a nice mode for running Macaulay2 inside
	  Emacs."},
     }

needsPackage "Schubert2" -- make the example below work before Schubert2 is installed
doc ///
  Key
    "reading the documentation"
  Description
    Text
      The documentation for Macaulay2 is available in several formats and can
      be searched and viewed using the following functions:

    Tree
      :Finding documentation nodes
	> about
	> apropos
	> headlines
      :Accessing documentation nodes
	> help
	> viewHelp
	> infoHelp
	> (symbol ?, Symbol)
      :Getting other information from documentation
	> examples

    Text
      To begin, the @TO "about"@ method allows for searching all documentation nodes
      whose title or content contains a given string:
    Example
      -- FIXME
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
    "initial help"
///

document {
     Key => "getting help or reporting bugs",
     PARA {
	  "An easy way to get help or to report a bug is to use
	  our google group web page at ", HREF "http://groups.google.com/group/macaulay2", ".
	  These requests are automatically emailed to the developers, and they
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
     Key => "invoking the Macaulay2 program",
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
	  most users will prefer to use the interface provided with Emacs or with TeXmacs."
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
	  TO "prefixPath",
	  TO "prefixDirectory",
	  TO "applicationDirectory",
	  TO "applicationDirectorySuffix",
	  }
     }

document {
     Key => "prefixPath",
     Headline => "absolute locations of Macaulay2 files",
     PARA {
	  "The absolute location of a Macaulay2 file can be obtained by concatenating three components: (a) the
	  prefix, which is one of the members of the list ", TO "prefixPath", "; (b) the relative location of the directory
	  containing the file, as recorded in the hash table ", TO "Layout", "; and (c) the base name of the file.
	  The value of ", TO "prefixPath", " is used by ", TO "installPackage", " when determining how to direct
	  documentation hyperlinks from one package to another."
	  },
     PARA {
	  "The initial value of ", TO "prefixPath", " contains just the following two optional items.
	  If the variable ", TO "prefixDirectory", " was
	  given a non-null value initially or by a ", TT "-e", " command line argument,
	  then it will be the last element of ", TO "prefixPath", ".  If the ", TT "-q", " 
	  option was not given on the command line used to invoke Macaulay2, then the value of ", TT "applicationDirectory()|\"local/\"", "
	  will be the first element of ", TO "prefixPath", ".  No attempt is made to synchronize the value of ", TO "prefixPath", "
	  with the values of ", TO "prefixDirectory", " and of ", TT "applicationDirectory()", ", which may change."
	  },
     PARA {
	  "When running a newly compiled version of Macaulay2, adding something like ", TT "-E 'prefixDirectory=\"/usr/\"'", " to
	  the command line is a good way to direct hyperlinks created by ", TO "installPackage", " to the documentation provided by
	  an older copy of Macaulay2 installed with the prefix ", TT "/usr/", ", and that, in turn, is easily done within
	  Emacs by the keystroke sequence ", KBD "C-u f12", ", which offers you a chance to edit the command line."
	  },
     PARA {
	  "The initial value of ", TO "prefixPath", " described above can be overridden by the user's ", TO "initialization file", ")."
	  },
     PARA {
	  "The list ", TO "prefixPath", " should be distinguished from the list ", TO "path", ", which is used to locate files to be
	  loaded, by functions such as ", TO "searchPath", ", ", TO "load", ", ", TO "loadPackage", ", and ", TO "needsPackage", "."
	  },
     PARA {
	  "The following example shows the list of places where we might find the source code of a package called ", TT "Foo", "
	  after it has been installed by ", TO "installPackage", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | Layout#1#"packages" | "Foo.m2")///,
     PARA {
     	  "This example shows the list of places where we might reasonably find the html file documenting a
	  function named ", TT "bar", " in a package called ", TT "Foo", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | replace("PKG","Foo",Layout#1#"packagehtml") | "bar.html")///,
     PARA {
     	  "This example shows the list of places where we might reasonably find the info file documenting a
	  package called ", TT "Foo", "."
	  },
     EXAMPLE ///stack apply(prefixPath, p -> p | Layout#1#"info" | "Foo.info")///,
     SeeAlso => {"commandLine", "invoking the Macaulay2 program", applicationDirectory, "prefixDirectory", "path", searchPath, load, loadPackage, needsPackage}
     }

doc := new HashTable from {
     "bin" => "executable files (M2)",
     "common" => "architecture independent files",
     "data" => "architecture independent data files",
     "doc" => "documentation",
     "docdir" => "documentation for Macaulay2 packages",
     "emacs" => "Emacs source files (*.el, *.elc)",
     "exec" => "architecture dependent files",
     "factory gftables" => "directory for files containing addition tables in small finite fields used by the library 'factory'",
     "info" => "documentation in info form",
     "lib" => "architecture dependent data and executable files",
     "libraries" => "dynamically loadable libraries from third party packages linked with Macaulay2",
     "man" => "man pages",
     "package" => "additional source files for the Macaulay2 package PKG",
     "packagecache" => "cached data files for the Macaulay2 package PKG",
     "packagedoc" => "documentation for the Macaulay2 package PKG",
     "packageexampleoutput" => "example output files for the Macaulay2 package PKG",
     "packagehtml" => "html documentation for the Macaulay2 package PKG (*.html)",
     "packageimages" => "images for the Macaulay2 package PKG (*.jpg)",
     "packagelib" => "architecture dependent files for the Macaulay2 package PKG",
     "packages" => "source files for Macaulay2 packages; this directory appears on the path",
     "packagetests" => "test files for the Macaulay2 package PKG",
     "programs" => "programs to be run by Macaulay2",
     "program licenses" => "licenses for programs to be run by Macaulay2"
     }
assert( set keys Layout#1 === set keys Layout#2 )
assert( set keys Layout#1 === set keys doc )

document {
     Key => {"currentLayout", "Layout"},
     Headline => "relative locations of Macaulay2 files",
     PARA {
	  "Macaulay2 comes with a variety of types of files, and some of them are associated with a 
	  particular Macaulay2 package.  The hash table ", TT "currentLayout", " is a translation 
	  table from names, corresponding to the various types of files, to directory paths.  The
	  directory paths are to be interpreted relative to the path stored in ", TO "prefixDirectory", ".  Each
	  of the directories contained in the list ", TO "prefixPath", " has its own layout, which will be detected at runtime.
	  Some of the strings contain ", TT "PKG", " as a substring, which should be replaced
	  by the name of package whose files will be stored in that directory."
	  },
     PARA {
	  "The hash table ", TO "Layout", " contains the two possible values for ", TO "currentLayout", ";
	  corresponding to the two possible values for the ", TO [installPackage, SeparateExec], " option used with ", TO "installPackage", ".
	  The hash table ", TT "Layout#2", " is used if architecture dependent files are to be stored in
	  a directory tree separate from the one used for architecture independent files.  The hash table ", TT "Layout#1", "
	  is used otherwise."
	  },
     PARA {
	  "Basic Macaulay2 files are regarded as being associated
	  with a special package called ", TT{"Core"}, ", and the corresponding documentation files
	  are part of the package ", TT "Macaulay2Doc", "."
     	  },
     EXAMPLE {
	  "Layout"
	  },
     "Here are the meanings of the keys used in ", TO "currentLayout", ".",
     UL apply(sort pairs doc, (k,v) -> LI { TT format k, " : " | v}),
     SeeAlso => {[installPackage,SeparateExec]}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
