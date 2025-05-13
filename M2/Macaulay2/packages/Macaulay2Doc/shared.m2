-- here we document the stub definitions in m2/shared.m2

methodstr := PARA { "This function is a method function, defined in the core so multiple packages can add methods to it." }

document { Key => height, Headline => "height of an object", methodstr,
    SeeAlso => { "Posets::height(Poset)" } }
document { Key => depth,  Headline => "depth of an object",  methodstr,
    SeeAlso => { "Depth::Depth", "SLPexpressions::SLPexpressions" } }
document { Key => width,  Headline => "width of an object",  methodstr }
document { Key => length, Headline => "length of an object", methodstr,
    SeeAlso => { "Complexes::length(Complex)", "Permutations::length(Permutation)" } }
document { Key => extend, Headline => "extend an object",    methodstr,
    SeeAlso => {
	"Permutations :: extend(Permutation,ZZ)",
	"Complexes :: extend(Complex,Complex,Matrix)",
	"OldChainComplexes :: extend(ChainComplex,ChainComplex,Matrix)",
    },
}

document { Key => complete,     methodstr, SeeAlso => { "OldChainComplexes::complete(ChainComplex)" } }
document { Key => minimize,     methodstr, SeeAlso => { "Complexes::minimize(Complex)" } }
document { Key => decompose,    methodstr, SeeAlso => { "MinimalPrimes::MinimalPrimes" } }
document { Key => truncate,     methodstr, SeeAlso => { "Truncations::Truncations" } }
document { Key => chi,          methodstr, SeeAlso => {
	"Schubert2::chi(AbstractSheaf)", "NormalToricVarieties::chi(CoherentSheaf)" } }
document { Key => euler,        methodstr, SeeAlso => {
	"HyperplaneArrangements::HyperplaneArrangements",
	"MultiprojectiveVarieties::euler(MultiprojectiveVariety)",
	"Schubert2::euler(AbstractVariety)", "Varieties::Varieties" } }
document { Key => eulers,       methodstr }
document { Key => genera,       methodstr }
document { Key => genus,        methodstr }
document { Key => isExact,      methodstr, SeeAlso => {
	"ChainComplexExtras :: isExact(ChainComplex)",
	"Complexes :: isExact(Complex)" } }
document { Key => isSmooth,     methodstr, SeeAlso => {
	"Divisor::isSmooth(Ideal)", "LatticePolytopes::isSmooth(Polyhedron)",
	"Varieties::isSmooth(Variety)", "SpaceCurves::isSmooth(Curve)",
	"Polyhedra::isSmooth(Cone)", "NormalToricVarieties::isSmooth(NormalToricVariety)",
	"SpecialFanoFourfolds::isSmooth(EmbeddedProjectiveVariety)" } }
document { Key => isVeryAmple,  methodstr, SeeAlso => {
	"Divisor::isVeryAmple(WeilDivisor)", "Polyhedra::isVeryAmple(Polyhedron)",
	"PositivityToricBundles::isVeryAmple(ToricVectorBundleKlyachko)",
	"NormalToricVarieties::isVeryAmple(ToricDivisor)" } }
document { Key => isNormal,     methodstr, SeeAlso => {
	"Polyhedra::isNormal(Polyhedron)",
	"IntegralClosure::isNormal(Ring)",
	"AssociativeAlgebras::isNormal(RingElement)",
	} }
document { Key => normalCone,   methodstr, SeeAlso => {
	"Polyhedra::normalCone(Polyhedron,Polyhedron)",
	"ReesAlgebra::normalCone(Ideal)",
    } }

document { Key => { isEmpty, (isEmpty, Thing) },
    Headline => "whether an object is empty",
    SeeAlso => { "Polyhedra::Polyhedra" } }

document { Key => { isIsomorphism, (isIsomorphism, Matrix) },
    Headline => "whether a map is an isomorphism",
    Usage => "isIsomorphism f",
    "Whether the map $f$ is an isomorphism.",
    SeeAlso => {
	"Cremona::isIsomorphism(RationalMap)",
	"GradedLieAlgebras::isIsomorphism(LieAlgebraMap)",
	"MultiprojectiveVarieties::isIsomorphism(MultirationalMap)",
	"Varieties::isIsomorphism(SheafMap)",
    } }

document { Key => cone,
    Headline => "mapping cone or polyhedral cone",
    SeeAlso => {
	"Complexes::cone(ComplexMap)",
	"Polyhedra::cone(Polyhedron)",
	}
    }
document { Key => rays,
    Headline => "1-dimensional cones of a fan or polyhedral object",
    SeeAlso => {
	"FourTiTwo::rays(Matrix)",
	"NormalToricVarieties::rays(NormalToricVariety)",
	"Polyhedra::rays(PolyhedralObject)",
	}
    }

document {
     Key => pullback,
     Headline => "compute the pullback morphism",
     SeeAlso => { pushout }
     }

document {
     Key => pushout,
     Headline => "compute the pushout morphism",
     SeeAlso => { pullback }
     }

-- also see functions/intersect-doc.m2
document {
    Key => { intersect, (intersect, List), (intersect, Sequence) },
    Headline => "compute an intersection",
    PARA {     
	"This function calculates the intersection of a list or sequence of compatible objects."
	},
    PARA{
	"When a more efficient algorithm is available for computing the intersection of all inputs
	simultaneously rather than iteratively, for instance for intersecting ", TO Module, "s,
	a specialized function that takes a list or sequence may be installed on ", TT "(symbol intersect, Type)", "."
	},
    EXAMPLE ///code lookup(symbol intersect, Module)///,
    PARA {
	"This function may also be called using the synonym ", M2CODE "intersection", "."},
    SeeAlso => {
	-- add references to intersect methods installed in packages _other than Core_ here
	"M0nbar::intersect",
	"NAGtypes::intersect(PolySpace,PolySpace)",
	"Polyhedra::intersect"
	}
    }

undocumented { 1:union }
document {
    Key => { union, (union, List), (union, Sequence) },
    Headline => "compute the union",
    PARA { "This function returns the union of a list or sequence of compatible sets or varieties." },
    SeeAlso => {
	"NumericalAlgebraicGeometry::union(NumericalVariety,NumericalVariety)",
	"Posets::union(Poset,Poset)",
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
    SeeAlso => {
	symbol**
	-- add references to tensor methods installed in packages _other than Core_ here
	}
    }

doc ///
  Key
     directProduct
    (directProduct, List)
    (directProduct, Sequence)
  Headline
    direct product
  Description
    Text
      This function calculates the direct product of a list or sequence of
      compatible objects.

      This is a binary method, meaning it is defined for two inputs but can
      accept more. When three or more arguments are given, the direct product is
      evaluated from left to right: the direct product of the first two inputs
      is computed, then the result is used to compute the direct product with
      the third input, and so on.
  SeeAlso
    "Polyhedra::directProduct(Cone,Cone)"
    "Polyhedra::directProduct(Fan,Fan)"
    "Graphs::directProduct(Graph,Graph)"
///

document {
    Key => status,
    Headline => "get the status of a computation",
    SeeAlso => {
	(status, GroebnerBasis),
	"OldChainComplexes :: status(ChainComplex)",
	"SubalgebraBases :: status(SAGBIBasis)",
	"SumsOfSquares :: status(SDPResult)",
	-- this one is used in a different sense
	-- "NAGtypes :: status(AbstractPoint)",
    },
}
