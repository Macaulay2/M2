document { Key => Core,
    Headline => "the core of Macaulay2",
    PARA {
	"This package contains the core functionality of Macaulay2, without the documentation,
	which is in the package ", TO "Macaulay2Doc", "."
    }
}

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
