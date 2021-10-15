-- here we document the stub definitions in m2/shared.m2

methodstr := PARA { "This function is a method function, defined in the core so multiple packages can add methods to it." }

document { Key => decompose,    methodstr, SeeAlso => { "MinimalPrimes::MinimalPrimes" } }
document { Key => truncate,     methodstr, SeeAlso => { "Truncations::Truncations" } }
document { Key => chi,          methodstr }
document { Key => isEmpty,      methodstr, SeeAlso => { "Polyhedra::Polyhedra",(isEmpty, RRi)} }

-- also see functions/intersect-doc.m2
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

-- also see functions/tensor-doc.m2
document {
    Key => { tensor, (tensor, List), (tensor, Sequence) },
    Headline => "tensor product",
    PARA {
	"This function calculates the tensor product of a list or sequence of compatible objects."
	},
    PARA{
	"This method is declared as a ", TO MethodFunctionBinary, ", so for two or more argument
	the product is computed iteratively from the binary tensor products, working from left to right.
	For two arguments, this is the same as ", TT "A ** B", " except that options are allowed."
	},
    EXAMPLE ///tensor(ZZ^2, ZZ^3, ZZ^4)///,
    PARA{
	"When a more efficient algorithm is available for computing the intersection of all inputs
	simultaneously rather than iteratively, for instance for intersecting ", TO Module, "s,
	a specialized function that takes a list or sequence may be installed on ", TT "(symbol intersect, Type)", "."
	},
    EXAMPLE ///code lookup(symbol intersect, Module)///,
    SeeAlso => {
	symbol**
	-- add references to tensor methods installed in packages _other than Core_ here
	}
    }

document {
     Key => Jacobian,
     PARA {
	  "This symbol is defined in the core so it can be used as the name of an optional argument by multiple packages."
	  },
     SeeAlso => { "ReesAlgebra::ReesAlgebra" }
     }
