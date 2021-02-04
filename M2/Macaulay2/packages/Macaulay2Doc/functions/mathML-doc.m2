--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
     (mathML,VerticalList),
     (mathML, Adjacent),
     (mathML, Array),
     (mathML, Boolean),
     (mathML, CacheFunction),
     (mathML, ChainComplex),
     (mathML, ChainComplexMap),
     (mathML, CoherentSheaf),
     (mathML, Divide),
     (mathML, Expression),
     (mathML, FunctionApplication),
     (mathML, GradedModule),
     (mathML, GradedModuleMap),
     (mathML, GroebnerBasis),
     (mathML, HashTable),
     (mathML, Holder),
     (mathML, Hypertext),
     (mathML, Ideal),
     (mathML, ImmutableType),
     (mathML, IndexedVariableTable),
     (mathML, InfiniteNumber),
     (mathML, List),
     (mathML, LITERAL),
     (mathML, MatrixExpression),
     (mathML, Minus),
     (mathML, Monoid),
     (mathML, MonoidElement),
     (mathML, MutableHashTable),
     (mathML, MutableMatrix),
     (mathML, Net),
     (mathML, Nothing),
     (mathML, OneExpression),
     (mathML, Option),
     (mathML, OptionTable),
     (mathML, Package),
     (mathML, Power),
     (mathML, Product),
     (mathML, ProjectiveHilbertPolynomial),
     (mathML, QQ),
     (mathML, BettiTally),
     (mathML, RingMap),
     (mathML, RR),
     (mathML, ScriptedFunctor),
     (mathML, Sequence),
     (mathML, Set),
     (mathML, SheafOfRings),
     (mathML, String),
     (mathML, Subscript),
     (mathML, Sum),
     (mathML, Superscript),
     (mathML, Symbol),
     (mathML, VirtualTally),
     (mathML, Thing),
     (mathML, Type),
     (mathML, Variety),
     (mathML, ZeroExpression),
     (mathML, ZZ)
     }

document { 
     Key => mathML,
     Headline => "convert to MathML format",
     Usage => "mathML x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to ML format"}
	  },
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "mathML (x+2*y-1)^2"
	  },
     EXAMPLE {
	  "mathML matrix {{x,y},{x^2+2,0}}"
	  },
     SeeAlso => {tex, texMath}
     }

