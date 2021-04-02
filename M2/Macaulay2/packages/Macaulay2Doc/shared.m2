-- here we document the stub definitions in m2/shared.m2

methodstr := PARA { "This function is a method function, defined in the core so multiple packages can add methods to it." }

document { Key => decompose,    methodstr, SeeAlso => { "MinimalPrimes::MinimalPrimes" } }
document { Key => truncate,     methodstr, SeeAlso => { "Truncations::Truncations" } }
document { Key => chi,          methodstr }

document {
     Key => Jacobian,
     PARA {
	  "This symbol is defined in the core so it can be used as the name of an optional argument by multiple packages."
	  },
     SeeAlso => { "ReesAlgebra::ReesAlgebra" }
     }
