document {
    Key => {Program, (net, Program)},
    Headline => "external program object",
    PARA {"A hash table returned by ", TO "loadProgram",
	" with the following strings as keys:"},
    UL {
	{TT "\"name\"", ", the name of the loaded program.  ",
	    "This comes from the first argument passed to ",
	    TO "loadProgram", ".  It is also what is displayed when ",
	    "printing a Program."},
	{TT "\"path\"", ", the path to the program as determined by ",
	    TO "loadProgram", "."},
	{TT "\"prefix\"", ", a sequence of three strings identifying the ",
	    "prefix prepended to the binary executables.  See ",
	    TO "loadProgram", ", specifically the description of the ",
	    TT "Prefix", " option, for more."}},
    SeeAlso => {"programPaths", "loadProgram"}
}

document {
    Key => symbol programPaths,
    Headline => "user-defined external program paths",
    PARA {"A mutable hash table containing user-defined paths to external ",
	"programs used by Macaulay2.  Its keys are strings containing the ",
	"names of programs and must coincide with the first argument of ",
	TO "loadProgram", "."},
    PARA {"It is only necessary to define a path in this way if a program ",
	"is installed in a non-standard location.  In particular, ",
	TO "loadProgram", " already checks ",
	TT "prefixDirectory | currentLayout#\"programs\"",
	" (where the programs shipped with Macaulay2 are installed) ",
	"and all of the directories in the user's ", TT "PATH",
	" environment variable."},
    PARA {"If you use a particular program frequently and it is installed ",
	"in a non-standard location, then it may be useful to add a line to ",
	"your ", TO "initialization file", " defining its path in this way."},
    SeeAlso => {"Program", "loadProgram"}
}

document {
    Key => RaiseError,
    Headline => "whether to raise an error"
}

document {
    Key => {loadProgram,
	(loadProgram, String, String),
	(loadProgram, String, List),
	[loadProgram, Prefix],
	[loadProgram, RaiseError],
	[loadProgram, Verbose]},
    Headline => "load external program",
    Usage => "loadProgram(name, cmd)\nloadProgram(name, cmds)",
    Inputs => {
	"name" => String => {"the name of the program to load.  ",
	    "This should match the corresponding key in ",
	    TO "programPaths", "."},
	"cmd" => String =>
	    "a command to run that should return 0 if the program is present.",
	"cmds" => List => {"a list of commands to run that should all return ",
	    "0 if the program is present."},
	RaiseError => Boolean =>
	    "whether to raise an error if the program is not found.",
	Verbose => Boolean =>
	    "whether to inform the user of each path that is checked.",
	Prefix => List => {
	    "a list of sequences containing three strings identifying a ",
	    "prefix that is added to the executable binaries belonging to the ",
	    "program by some distributions.  These sequences should be of the ",
	    "form ", TT "(distribution, regex, prefix)", " where: ",
		UL {
		    {TT "distribution", " identifies the name of the ",
			"distribution that uses the prefix, e.g., ",
			"\"debian\".  It is only used for printing messages ",
			"when ", TT "Verbose", " is set to ", TO "true", "."},
		    {TT "regex", " is a ",
			TO2("regular expressions", "regular expression"),
			" that should match all binary executables that ",
			"need the prefix."},
		    {TT "prefix", " is the prefix itself."}
		}
	}
    },
    Outputs => {Program => { "the program that was loaded.  ",
	"If the program is not found and ", TT "RaiseError", " is set to ",
	TO "false", " then ", TO "null", " is returned."}},
    PARA {"This function checks for the existence of an external program by ",
	"running ", TT "cmd", " (or every element of ", TT "cmds", ") ",
	"prepended with various paths in the following order:"},
    UL {
	{"The user-defined path specified by ", TT "programPaths#name",
	    ", if it exists.",},
	{"The path specified by ",
	    TT "prefixDirectory | currentLayout#\"programs\"",
	    ", where the programs shipped with Macaulay2 are installed."},
	{"Each path specified by the user's ", TT "PATH",
	    " environment variable."}
    },
    PARA {"For each path, any prefixes specified by the ", TT "Prefix",
	" option are checked."},
    PARA {"Once this is successful (i.e., ", TT "cmd", " or each element of ",
	TT "cmds", " returns a value of 0), then a ", TO "Program",
	" object is returned.  If it is unsuccessful, then either an error is ",
	"raised or ", TO "null", " is returned, depending on the value of ",
	TT "RaiseError", "."},
    PARA {"Note that if a program consists of a single executable binary ",
	"file, then ", TT "name", " should coincide with the name of this ",
	"file."},
    EXAMPLE lines ///
	programPaths#"gfan" = "/path/to/gfan/"
	gfan = loadProgram("gfan", "gfan --help", Verbose => true)///,
    PARA {"One program that is shipped with a variety of prefixes in ",
	"different distributions and for which the ", TT "Prefix",
	" option is useful is TOPCOM:"},
    EXAMPLE {///loadProgram("topcom", "cube 3", Verbose => true, Prefix => {
    ("debian", ".*", "topcom-"),
    ("fedora", "^(cross|cube|cyclic|hypersimplex|lattice)$", "TOPCOM-"),
    ("gentoo", "^cube$", "topcom_")})///},
    SeeAlso => {"Program", "programPaths", "runProgram"}
}
