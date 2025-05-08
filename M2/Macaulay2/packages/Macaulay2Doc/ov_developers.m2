document { Key => Core,
    Headline => "the core of Macaulay2",
    PARA {
	"This package contains the core functionality of Macaulay2, without the documentation,
	which is in the package ", TO "Macaulay2Doc", "."
    },
    Subnodes => {
	TO "how Macaulay2 finds its files",
    }
}

-- TODO: merge this with the node above.
doc ///
Node
  Key
    "how Macaulay2 finds its files"
  Description
    Text
      When you run Macaulay2, it has to find and load a sequence of startup files containing code written
      in the Macaulay2 language. Here is the way it does that.
    Text
      Its first task is to discover the path to the binary file @TT "M2"@ that is currently running.  On
      some systems, that information is available from the @TT "/proc"@ file system.  Otherwise, it
      examines the command name you used to run the program, which is provided to it as the argument in
      position number 0 on the command line.  If it's not an absolute path, it searches along the path of
      directories mentioned in the environment variable PATH until it finds a file with the same name.  If
      the result is a symbolic link, the link is followed.  The final result is assumed to be in a
      directory named @TT "'bin/'"@, and the startup files are located relative to that.  The path to the
      top level directory is stored in the variable @TO "prefixDirectory"@, which you can examine to see
      whether it all worked out. For detailed information about the relative location of Macaulay2 files,
      see @TO "Layout"@.
  Subnodes
    "currentLayout"
    "prefixPath"
    "prefixDirectory"
    "applicationDirectory"
    "applicationDirectorySuffix"
///

document {
    Key => "the interpreter of Macaulay2",
    PARA {
	"This part is in charge of interpreting the top-level language of Macaulay2 ",
	"and calling the requested functions in ", TO "the engine of Macaulay2", "."
    }
}

document {
    Key => "the engine of Macaulay2",
    "The engine is the part of the program that is dedicated to
    performing the computation of Gröbner bases with Buchberger's
    algorithm.  It is coded directly in C++ for speed.",
    PARA{},
    "The Macaulay2 engine provides fast polynomial and matrix operations,
    and Gröbner bases, syzygies, Hilbert functions, resolutions and
    other operations that we feel need to be implemented directly for
    efficiency reasons.",
    Subnodes => {
	TO "parallelism in engine computations",
    },
}
