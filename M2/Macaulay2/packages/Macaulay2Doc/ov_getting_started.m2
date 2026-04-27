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
      Macaulay2 is available from our web page @HREF{"https://Macaulay2.com", "Macaulay2.com"}@.
      There you will find installation instructions for MacOSX and several Linux distributions,
      as well as precompiled versions for various other systems.

      For developers, detailed information about building Macaulay2 from source is available
      on our GitHub repository at @HREF "https://github.com/Macaulay2/M2/wiki"@.

      Once Macaulay2 is installed on your system, use the information here to set up
      the Emacs interface (the recommended way to run Macaulay2), bookmark the html documentation,
      and start your first Macaulay2 session.
    Text
      @HEADER2 "Checking your installation"@

      At a command prompt in a terminal window, type @KBD "M2"@.
      If Macaulay2 has been installed correctly on your machine,
      you should see a prompt such as this:
    Code
      EXAMPLE PRE {"$ M2\nMacaulay2, version " | version#"VERSION" | "\nwith packages: ...\n\ni1 :" }
    Text
      If this is the first time Macaulay2 runs on this machine, it creates an @TO "application directory"@
      under @TT ".Macaulay2"@ in your home directory on Unix-based systems, or
      under @TT "'Library/Application Support/Macaulay2'"@ under MacOS-based systems.
      Inside this directory there are several files of interest, including:
    Code
      UL {
	  LI { "an index of your local documentation in ", TT "index.html" }, -- TODO: make a node about history file?
	  LI { "your ", TO "initialization file", " ", TT "init.m2" },
	  LI { "your history file ", TT "history.m2" }, -- TODO: make a node about history file?
      }
    Text
      If instead you see an error, or if the version printed is too older than the version above,
      then see the following pages for help and come back to this page.
    Tree
      :Debugging issues with your installation
	> "finding the Macaulay2 files"
	> "teaching your shell how to find M2"
	> "teaching M2 how to find its shared libraries"
	* "getting help or reporting bugs"
    Text
      @HEADER2 "Bookmarking the documentation"@

      Macaulay2 stores links to a local copy of the Macaulay2 documentation,
      as well as any packages you have installed locally, in the file @TT "index.html"@.
      At the Macaulay2 prompt type @KBD "viewHelp"@ and press @KBD "Enter"@ to open @TT "index.html"@
      in your default web browser. This is a good time to bookmark this page in your browser.
      Note that there are several different ways of @TO "reading the documentation"@.
    Text
      @HEADER2 "Interacting with Macaulay2"@

      At this point you should try something simple in Macaulay2, such as
    Code
      EXAMPLE PRE {
	  "printWidth = 60",
	  "R = QQ[a..d]",
	  "(a+b+c+d)^4"
      }
    Text
      Macaulay2 can be run in this way from the command-line, but it is generally much more convenient
      to run Macaulay2 inside a program such as Emacs or TeXmacs. This is because you can more easily
      view larger objects, use tab-completion, cut and paste, search, save your session, and so on.
      There is a nice major mode for running Macaulay2 inside Emacs.
    Tree
      :Different ways of using Macaulay2
	> "invoking the Macaulay2 program"
	> "using Macaulay2 with Emacs"
	> "using Macaulay2 with TeXmacs"
	> "using Macaulay2 in a terminal"
    Text
      Regardless of how you use Macaulay2, you can start a new session by entering @TO restart@,
      or terminate it by entering @TO exit@, @TO quit@, @TO end@, or your end of file character
      (which is @KBD "Ctrl-D"@ under most systems). If Macaulay2 is reading and running a script,
      the end of file will terminate the program.

      See the @HREF{"https://github.com/Macaulay2/M2/wiki", "Macaulay2 wiki"}@ for links to extensions
      for other popular editors that at least support syntax highlighting for Macaulay2 files.

      Once you are set up, it is time for @TO "a first Macaulay2 session"@.

Node
  Key
    "using Macaulay2 in a terminal"
  Description
    Text
      Macaulay2 can be used in a terminal window if care is taken not to display very wide objects,
      Some answers in Macaulay2 can be very wide, but many of them will be wrapped appropriately to
      fit in the width of a terminal window. See @TO "invoking the Macaulay2 program"@ for optional
      arguments which you may use when starting Macaulay2 in a terminal.
    Text
      @HEADER3 "Using keyboard shortcuts"@
      -- TODO: insert gif demonstrating this?

      Macaulay2 uses the Readline and History libraries to handle user input in a terminal window.
      These libraries provide an array of keyboard shortcuts for command-line editing and accessing
      the history interactively, making the user interface consistent with other terminal-based programs.

      Here are a summary of most useful keyboard shortcuts.
    Code
      TABLE {
	  { KBD "Ctrl-a",	" -- move to the start of the current line" },
	  { KBD "Ctrl-e",	" -- move to the end of the line" },
	  { KBD "Ctrl-r",	" -- search backward in the history for matching commands" },
	  { KBD "Ctrl-k",	" -- kill (cut) the rest of the line" },
	  { KBD "Ctrl-y",	" -- yank (paste) into this position" },
      }
    Text
      For more information, refer to the documentation for the
      @HREF{"http://tiswww.case.edu/php/chet/readline/rluserman.html", "Readline user interface"}@,
      which can also be read from a terminal with the command @KBD "info readline"@.

      -- commented because we don't actually call history_expand to use any interface features
      -- @HREF{"http://tiswww.case.edu/php/chet/readline/history.html#SEC1"}@ or @TT "info history"@.
    Text
      @HEADER3 "Using tab-completion"@
      -- TODO: insert gif demonstrating this?

      Macaulay2 supports auto-completion in the terminal. For instance, type @KBD "hilb"@ and
      press the @KBD "TAB"@ key to automatically complete to @KBD "hilbert"@, then
      press @KBD "TAB"@ a second time to get a list of the possible completions:
    CannedExample
      i1 : hilbert<TAB><TAB>
      hilbertBasis       hilbertFunction    hilbertPolynomial  hilbertSeries

      i1 : hilbert

Node
  Key
    "invoking the Macaulay2 program"
  Description
    Text
      On systems with a command-line interface, the following commands can be used to start the program.
    Code
      TABLE {
	  { KBD "M2 [option ...]", " -- starts the program with optional arguments" },
	  { KBD "M2 file1.m2 ...", " -- reads and executes the specified files" },
      }
    Text
      -- TODO: say more about what happens when M2 starts, e.g. loading Core, packages, then init.m2
      When the program starts up the @TO "initialization file"@ @TT "init.m2"@ located in your
      @TO2(applicationDirectory, "application directory")@ will be loaded.
      You can modify this file to preload packages you use often or predefine your favorite field.
      Further, a log of your inputs to the program will be stored in @TT "history.m2"@.
      Both behaviors can be prevented by starting the program with @KBD "M2 -q"@ instead.

      Other options that can be provided on the command-line may be displayed by running @KBD "M2 --help"@.
    Code
      M2Usage := get("!" | commandLine#0 | " --help");
      assert match("usage:", M2Usage);
      EXAMPLE PRE M2Usage
///

doc ///
Node
  Key
    "finding the Macaulay2 files"
  Description
    Text
      Often you will know where the Macaulay2 files are, because you have installed them yourself.
      But it can happen that Macaulay2 was installed by your system administrator so you can run M2,
      but you don't know where its files are. In that case, there are a couple of ways to locate the files.

      The Macaulay2 files come in a directory tree that mimics the @TT "/usr"@ directory on
      Unix-based systems. In particular, its top-level prefix directory has subdirectories
      called @TT "bin"@, @TT "info"@, @TT "lib"@, and @TT "share"@
      (see @HREF{"https://www.gnu.org/prep/standards/html_node/Directory-Variables.html", "GNU Coding Standards"}@)

      One way to find the top-level prefix directory is to use the shell command @KBD "which M2"@ or @KBD "type M2"@.
      The response will be of the form @TT "/foo/bar/bin/M2"@. From this output, we learn that the
      Macaulay2 files have been installed in the top-level prefix directory @TT "/foo/bar"@.
      It will follow, for example, that the M2 Emacs init file is located at
      @TT "/foo/bar/share/emacs/site-lisp/Macaulay2/M2-init.el"@.

      In some systems, you can instead find the prefix directory by running @KBD "locate M2-binary"@,
      which lists all files of that name found on the system. Note that this will also find older versions
      of Macaulay2 if you have several versions installed.
    Text
      Another way to locate the files of Macaulay2 is to ask M2, assuming you can run it.
      Start M2 and type @KBD "prefixDirectory"@. The response will be of the following form,
      and will also tell you the prefix for the paths to the Macaulay2 files.
    CannedExample
      i1 : prefixDirectory

      o1 = /foo/bar/
  SeeAlso
    "how Macaulay2 finds its files"
    "currentLayout"
    "prefixDirectory"

Node
  Key
    "teaching your shell how to find M2"
  Description
    Text
      Perhaps you have typed M2 into a shell window and gotten something like the following response:
    Code
      EXAMPLE PRE "$ M2
      bash: M2: command not found"
    Text
      If so, then you have to teach your shell how to find M2.
      Once you do this, then Emacs may also be able to find M2.
    Text
      Your shell will look for M2 in the directories listed in the value of the @TT "PATH"@ environment variable.
      You will want to arrange for that value to get set when you log in or when you start your shell.
      The former is preferable, because environment variables are inherited by new processes only from
      their parents, and your login shell is an ancestor of all of your processes.
    Text
      First, you must know where the Macaulay2 files are: see @TO "finding the Macaulay2 files"@.
      Your goal is to add the directory containing M2 to the value of the @TT "PATH"@ environment variable.

      Your system administrator may have installed the Macaulay2 files under @TT "/usr"@, in which case you
      will see M2 in @TT "/usr/bin"@. The files may also be installed under @TT "/usr/local"@, in which case
      you will see M2 in @TT "/usr/local/bin"@. Or the files may be installed somewhere else, such as in
      @TT "/Applications/Macaulay2"@, in which case you will see M2 in @TT "/Applications/Macaulay2/bin"@,
      and you will want to add that directory to your path now.
    Text
      @HEADER3 {"Method 1: using ", TO setup}@

      The simplest way to teach your shell how to find M2 is to let M2 do it for you.
      Say you know that your M2 executable is located at @TT "/foo/bar/bin/M2"@.
      Run the command @TT "/foo/bar/bin/M2"@ and enter @TT "setup()"@.
      If you see an error about shared libraries, see @TO "teaching M2 how to find its shared libraries"@.
      If that works, the next time you log in or start a new shell,
      the shell should know how to find M2. If that doesn't work, read onward.
    Text
      @HEADER3 {"Method 2: editing your shell's profile file"}@

      The method for setting environment variables depends on which shell you are using.  Typical shells
      in use include @TT "ash"@, @TT "bash"@, @TT "csh"@, @TT "tcsh"@, @TT "ksh"@, @TT "sh"@, and @TT "zsh"@.
      The command languages understood by these shells differ, but they fall into two main classes:
      the Bourne shells @TT "sh"@, @TT "bash"@, @TT "ash"@, @TT "zsh"@, and @TT "ksh"@; and
      the C shells @TT "csh"@ and @TT "tcsh"@.
    Text
      @HEADER4{"Bourne shells"}@

      A Bourne shell reads commands from the file @TT ".profile"@ in your home directory when you log in.
      The bash shell will also read commands from @TT ".bashrc"@ each subsequent time it starts up,
      after the initial log in. To add a directory to your @TT "PATH"@, put this command in @TT ".profile"@:
    Code
      EXAMPLE PRE "export PATH=/foo/bar/bin:$PATH"
    Text
      @HEADER4{"C shells"}@

      A C shell reads commands from the file @TT ".login"@ in your home directory when you log in.
      The shell will also read commands from @TT ".cshrc"@ or perhaps @TT ".tcshrc"@.
      Check the manual page of your shell.
    Text
      To add a directory called to your @TT "PATH"@, put this command in @TT ".login"@:
    Code
      EXAMPLE PRE "setenv PATH /foo/bar/bin:$PATH"
    Text
      @HEADER3{"Method 3: making a link to M2"}@

      Another way to proceed that sometimes works is this. Look at the output from the shell command:
    Code
      EXAMPLE PRE "printenv PATH"
    Text
      and see whether one of your own directories is already on the path. If so, say it's @TT "~/bin"@,
      then you can make a symbolic link from M2 to that directory, and it will appear on your path.
      First ensure the directory has been made with this command:
    Code
      EXAMPLE PRE "mkdir ~/bin"
    Text
      Ignore the error message if the directory already exists.
      Then make the symbolic link with this command:
    Code
      EXAMPLE PRE "ln -s /foo/bar/bin/M2 ~/bin/M2"
    Text
      After that your shell will be able to find M2, and M2 will be able to find its files.
    Text
      @HEADER2 "What else to try"@

      If you fail to teach your shell how to find M2, then all is not lost.
      We prefer to run M2 within Emacs, and it is enough to teach Emacs how find M2.
      See @TO "teaching Emacs how to find M2"@.
  Subnodes
    "setup"
Node
  Key
    "teaching M2 how to find its shared libraries"
  Description
    Text
      Perhaps you know that your M2 executable is located at @TT "/foo/bar/bin/M2"@, but when you run it,
      you get something like this:
    Code
      EXAMPLE PRE "$ /foo/bar/bin/M2
      M2: error while loading shared libraries: liblapack.so: cannot open shared object file: No such file or directory"
    Text
      Or something like this:
    Code
      EXAMPLE PRE "$ /foo/bar/bin/M2
      dyld: Library not loaded: libgmp.3.dylib
        Referenced from: /foo/bar/bin/M2
        Reason: image not found
      Trace/BPT trap"
    Text
      What that means is that M2 cannot locate its shared libraries. Hopefully, the missing shared
      libraries are located in @TT "/foo/bar/lib"@, and all we have to do is to tell the operating
      system by setting the environment variable @TT "LD_LIBRARY_PATH"@ on Unix-based systems and
      @TT "DYLD_LIBRARY_PATH"@ on MacOS-based systems.

      If you have downloaded a version of Macaulay2 that comes with dynamically loaded libraries
      of its own, they will be in the directory @TT "/foo/bar/lib64/Macaulay2/lib"@.
    Code
      EXAMPLE PRE "LD_LIBRARY_PATH=/foo/bar/lib64/Macaulay2/lib /foo/bar/bin/M2"
    Text
      After setting the environment variable temporarily, you can use @TO setup@ to record the
      correct value in your system start up files.
///

needsPackage "Schubert2" -- make the example below work before Schubert2 is installed
doc ///
Node
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
      about "Horrocks-Mumford"
    Text
      The @TO "apropos"@ method is useful for finding all exported objects whose symbol
      matches the given regular expression, and can be combined with the @TO "headlines"@
      method to display a table with each symbol and its documentation headline:
    Example
      headlines apropos "(H|h)ilbert"
    Text
      While in Macaulay2, type @TO "help"@ to get the documentation on a topic or function,
      or type @TO "viewHelp"@ to open @TT "~/.Macaulay2/index.html"@, which contains a list
      of all installed packages, in your default web browser.

      For help on a specific topic, e.g., the Jacobian function, use @KBD "viewHelp jacobian"@
      or @KBD "viewHelp \"jacobian\""@, or if you want the documentation for Jacobian of an
      Ideal, use @KBD "viewHelp (jacobian, Ideal)"@ or @KBD "viewHelp \"jacobian(Ideal)\""@.
      Using @TO "help"@ instead of @TO "viewHelp"@ results in the help text appearing
      in your Macaulay2 session.

      @BOLD "A useful tip:"@ within Emacs, if you place your cursor on one of the resulting
      output lines that starts with a @TT "*"@, and press the return key, Macaulay2 will
      display that documentation node.

      The documentation for most functions comes with example code.
      You can obtain the text of this example code using @TO "examples"@.
  SeeAlso
    "writing documentation"
  Subnodes
    "initial help"

Node
  Key
    "getting help or reporting bugs"
  Description
    Text
      If you notice something that seems like a bug or a mistake in the documentation,
      please reach out through one of the following channels. You can also ask for help
      with using Macaulay2 in your research or find ways to contribute to Macaulay2.
    Code
      UL {
	  LI { HREF{"https://github.com/Macaulay2/M2/issues", "GitHub Issues"},
	      " -- for reporting bugs and requesting features" },
	  LI { HREF{"https://macaulay2.zulipchat.com", "Zulip server"},
	      " -- for online collaboration and discussions" },
	  LI { HREF{"https://groups.google.com/group/macaulay2", "Google Group"},
	      " -- for announcements and general questions" },
      }
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
