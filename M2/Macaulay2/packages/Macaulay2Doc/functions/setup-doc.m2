doc ///
Node
  Key
    setup
    1:setup
  Usage
    setup()
  Consequences
    Item
      Initialization files for Emacs and the standard command shells will
      have lines added to them to allow the Macaulay2 files to be found.
      A missing initialization file will be created only if doing so will not
      prevent the command shell from reading further initialization files.
      The user is prompted before each file is modified and offered the
      opportunity to review or reject the changes.
  Description
    Text
      Backup files are made carefully, and if a mangled initialization file is detected,
      it will not be modified. The lines added are bracketed by comment lines containing
      the phrases @TT "Macaulay 2 start"@ and @TT "Macaulay 2 end"@.
    Text
      The initialization files will contain added commands that add the appropriate directory to the front
      of the environment variable @TT "PATH"@ so the program @TT "M2"@ can be found, unless that directory
      already occurs in the path, in which case it does not check whether that directory is the first one
      on the path containing a program whose name is @TT "M2"@, for fear of escalating a competition with
      another init file.  (The expert user may prefer to remove those commands and instead, to create a
      symbolic link from a directory already on their path, such as @TT "~/bin"@ or @TT "~/.bin"@ to @TT "M2"@.)
    Text
      The other environment variables affected are @TT "MANPATH"@, @TT "INFOPATH"@, and @TT "LD_LIBRARY_PATH"@.
    Text
      This function is intended to be run only by users, not by system administrators, nor on behalf of
      users by system administrators, because system administrators can easily install Macaulay2 in such a
      way that the files can all be found.
  Caveat
    Warning: there is at least one important situation where you will not want to run the Macaulay2
    @TT "setup()"@ command. For example, suppose you have installed M2, not in its own private location
    as described above, but in a directory with many other executable files.  Suppose also that your
    account is adapted so it will run programs correctly on hardware of more than one architecture, or
    on differing operating systems, or even on various releases of the same operating system.
    Then @TT "setup()"@ will modify your command shell init files so the directory containing M2 will
    be the first one searched when looking for a program. If you then try to use your account on
    different hardware, incorrect programs may be found, namely, those in the same directory as M2.
    Similarly, incorrect shareable libraries may also be found.
  SeeAlso
    setupEmacs

Node
  Key
    setupEmacs
    1:setupEmacs
  Usage
    setupEmacs()
  Consequences
    Item
      The initialization file for Emacs (@TT ".emacs"@ in your home directory) will have lines
      added to allow the Macaulay2 Emacs mode to operate. The user is prompted before the file
      is modified and offered the opportunity to review or reject the change.
  Description
    Text
      A backup files is made carefully, and if a mangled initialization file is detected,
      it will not be modified. The lines added are bracketed by comment lines containing
      the phrases @TT "Macaulay 2 start"@ and @TT "Macaulay 2 end"@.

      The function @TO setup@ does all this and more.
  SeeAlso
    setup
///
