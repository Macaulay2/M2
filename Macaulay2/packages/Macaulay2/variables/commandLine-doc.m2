document {
     Key => "commandLine",
     Headline => "the command line arguments",
     Usage => "commandLine",
     "A constant whose value is the list of arguments passed to the interpreter,
     including argument 0, the name of the program.",
     SeeAlso => {"scriptCommandLine"}
     }
document {
     Key => "scriptCommandLine",
     Headline => "the command line arguments to be used when running a script",
     Usage => "scriptCommandLine",
     "A constant whose value is the list of arguments passed to the interpreter when a script
     is started, excluding argument 0, the name of the program, and excluding argument 1, the
     option \"--script\".",
     SeeAlso => {"commandLine"}
     }
