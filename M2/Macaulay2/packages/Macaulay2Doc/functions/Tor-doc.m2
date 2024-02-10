document {
    Key => Tor,
    Headline => "Tor module"
    }

document {
    Key => {(Tor,ZZ,Module,Module),(Tor,ZZ,Module,Ring),(Tor,ZZ,Ideal,Ideal),(Tor,ZZ,Ideal,Module),(Tor,ZZ,Module,Ideal),(Tor,ZZ,Ideal,Ring)},
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
