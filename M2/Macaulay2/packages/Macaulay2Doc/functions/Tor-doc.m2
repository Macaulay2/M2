undocumented {
    (Tor, ZZ, Matrix, Module),
    (Tor, ZZ, Matrix, Ideal),
    (Tor, ZZ, Matrix, Ring),
    (Tor, ZZ, Module, Matrix),
    (Tor, ZZ, Ideal,  Matrix),
    (Tor, ZZ, Ring,   Matrix),
}

document {
    Key => Tor,
    Headline => "Tor module",
    SeeAlso => {
	tensor,
	"Complexes :: Tor_ZZ(Matrix,Module)",
	"Complexes :: Tor_ZZ(Module,Matrix)",
	Hom, Ext },
    Subnodes => {
	TO (Tor, ZZ, Module, Module),
    }
}

document {
    Key => {
	(Tor, ZZ, Module, Module),
	(Tor, ZZ, Module, Ideal),
	(Tor, ZZ, Module, Ring),
	(Tor, ZZ, Ideal, Module),
	(Tor, ZZ, Ideal, Ideal),
	(Tor, ZZ, Ideal, Ring),
	(Tor, ZZ, Ring, Module),
	(Tor, ZZ, Ring, Ideal),
	(Tor, ZZ, Ring, Ring),
    },
    Headline => "compute a Tor module",
    Usage => "Tor_i(M,N)",
    Inputs => { "i", "M", "N" },
    Outputs => {
	{ "the ", TT "i", "-th ", TT "Tor", " module of ", TT "M", " and ", TT "N" }
	},
    PARA {
	"If ", TT "N", " is a ring (instead of a module), then the free module of rank 1 over it is used instead.  If
	", TT "M", " or ", TT "N", " is an ideal of ring, then the underlying module is used instead."
	}
    }
