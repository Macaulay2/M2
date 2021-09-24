-- here we document the stub definitions in m2/shared.m2

methodstr := PARA { "This function is a method function, defined in the core so multiple packages can add methods to it." }

document { Key => decompose,    methodstr, SeeAlso => { "MinimalPrimes::MinimalPrimes" } }
document { Key => truncate,     methodstr, SeeAlso => { "Truncations::Truncations" } }
document { Key => chi,          methodstr }
document { Key => isEmpty,      methodstr, SeeAlso => { "Polyhedra::Polyhedra",(isEmpty, RRi)} }

document {
    Key => { intersect, (intersect, List), (intersect, Sequence), intersection },
    Headline => "compute an intersection",
    PARA {
	"This function calculates the intersection of a list or sequence of compatible objects."
	},
    SeeAlso => {
	-- add references to intersect methods installed in packages _other than Core_ here
	"M0nbar::M0nbar",
	"NAGtypes::NAGtypes",
	"Polyhedra::Polyhedra"
	}
    }

document {
     Key => Jacobian,
     PARA {
	  "This symbol is defined in the core so it can be used as the name of an optional argument by multiple packages."
	  },
     SeeAlso => { "ReesAlgebra::ReesAlgebra" }
     }
