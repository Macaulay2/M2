doc ///
  Key
    Program
  Headline
    external program object
  Description
    Text
      A hash table returned by @TO findProgram@ with the following strings
      as keys:

      @UL {
	  {TT "\"name\"", ", the name of the loaded program.  ",
	      "This comes from the first argument passed to ",
	      TO "findProgram", ".  It is also what is displayed when ",
	      "printing a Program."},
	  {TT "\"path\"", ", the path to the program as determined by ",
	      TO "findProgram", "."},
	  {TT "\"prefix\"", ", a sequence of two strings identifying the ",
	      "prefix prepended to the binary executables.  See ",
	      TO "findProgram", ", specifically the description of the ",
	      TT "Prefix", " option, for more."},
	  {TT "\"version\"", ", a string containing the version number ",
	      "of the program.  Only present if ", TO "findProgram",
	      " was called with the ", TT "MinimumVersion", " option."}}@
  SeeAlso
    symbol programPaths
    findProgram
///

doc ///
  Key
    symbol programPaths
  Headline
    user-defined external program paths
  Description
    Text
      A mutable hash table containing user-defined paths to external
      programs used by Macaulay2.  Its keys are strings containing the
      names of programs and must coincide with the first argument of
      @TO findProgram@.

      It is only necessary to define a path in this way if a program
      is installed in a non-standard location.  In particular,
      @TO findProgram@ already checks
      @TT "prefixDirectory | currentLayout#\"programs\""@,
      (where the programs shipped with Macaulay2 are installed)
      and all of the directories in the user's @TT "PATH"@
      environment variable.

      If you use a particular program frequently and it is installed
      in a non-standard location, then it may be useful to add a line to
      your @TO "initialization file"@ defining its path in this way.
  SeeAlso
    Program
    findProgram
///

doc ///
  Key
    RaiseError
  Headline
    whether to raise an error
///

doc ///
  Key
    AdditionalPaths
  Headline
    list of non-standard paths to search for a program
///

doc ///
    Key
      MinimumVersion
    Headline
      the minimum required version of a program
///

doc ///
  Key
    findProgram
    (findProgram, String)
    (findProgram, String, String)
    (findProgram, String, List)
    [findProgram, AdditionalPaths]
    [findProgram, MinimumVersion]
    [findProgram, Prefix]
    [findProgram, RaiseError]
    [findProgram, Verbose]
  Headline
    load external program
  Usage
    findProgram name
    findProgram(name, cmd)
    findProgram(name, cmds)
  Inputs
    name:String
      the name of the program to load.  This should match the
      corresponding key in @TO symbol programPaths@.
    cmd:String
      a command to run that should return 0 if the program is present.
    cmds:List
      a list of commands to run that should all return 0 if the
      program is present.
    RaiseError => Boolean
      whether to raise an error if the program is not found.
    Verbose => Boolean
      whether to inform the user of each path that is checked.
    Prefix => List
      a list of sequences containing two strings identifying a prefix
      that is added to the executable binaries belonging to the
      program by some distributions.  These sequences should be of the
      form @TT "(regex, prefix)"@, where @TT "regex"@ is a
      @TO2("regular expressions", "regular expression")@ that should
      match all binary executables that need the prefix and @TT
      "prefix"@ is the prefix itself.
    AdditionalPaths => List
      a list of strings containing any paths to check for the program
      in addition to the default ones.
    MinimumVersion => Sequence
      containing two strings the form @TT "(minVersion,
      versionCommand)"@ where @TT "minVersion"@ is the minimum
      required version of the program and @TT "versionCommand"@ is a
      shell command to obtain the version number of an installed
      program.
  Outputs
    :Program
      the program that was loaded.  If the program is not found and
      @TT "RaiseError"@ is set to @TO false@, then @TO null@ is returned.
  Description
    Text
      This function checks for the existence of an external program by
      running @TT "cmd "@ (or every element of @TT "cmds"@),
      prepended with various paths in the following order:

      @UL {
	  {"The user-defined path specified by ", TT "programPaths#name",
	      ", if it exists.",},
	  {"The path specified by ",
	      TT "prefixDirectory | currentLayout#\"programs\"",
	      ", where the programs shipped with Macaulay2 are installed."},
	  {"Each path specified by the ", TT "AdditionalPaths", " option."},
	  {"Each path specified by the user's ", TT "PATH",
	      " environment variable."},
	  {"The path to ", TT "M2-binary", "."}
	  }@

      For each path, any prefixes specified by the @TT "Prefix"@
      option are checked.

      Once this is successful (i.e., @TT "cmd"@ or each element of
      @TT "cmds"@ returns a value of 0) then a @TO Program@ object is
      returned.  If it is unsuccessful, then either an error is raised
      or @TO null@ is returned, depending on the value of @TT "RaiseError"@.

      Note that if a program consists of a single executable binary
      file, then @TT "name"@ should coincide with the name of this
      file.
    Example
      -* no-capture-flag *-
      programPaths#"gfan" = "/path/to/gfan/"
      gfan = findProgram("gfan", "gfan _version --help", Verbose => true)
    Text
      If @TT "cmd"@ is not provided, then @TT "cmd"@ is run with the common
      @TT "--version"@ command line option.
    Example
      findProgram "normaliz"
    Text
      One program that is shipped with a variety of prefixes in
      different distributions and for which the @TT "Prefix"@ option
      is useful is TOPCOM:
    Example
      findProgram("topcom", "cube 3", Verbose => true, Prefix => {
        (".*", "topcom-"),
        ("^(cross|cube|cyclic|hypersimplex|lattice)$", "TOPCOM-"),
        ("^cube$", "topcom_")})
    Text
      Note that when using the @TT "MinimumVersion"@ option, the
      command used to obtain the current version number must remove
      everything except the version number itself and any leading or
      trailing whitespace.  Piping with standard UNIX utilities such as
      @TT "sed"@, @TT "head"@, @TT "tail"@, @TT "cut"@, and @TT "tr"@
      may be useful.
    Example
      findProgram("gfan", "gfan _version --help", Verbose => true,
        MinimumVersion => ("0.5",
       "gfan _version | head -2 | tail -1 | sed 's/gfan//'"))
  SeeAlso
    Program
    symbol programPaths
    runProgram
///
