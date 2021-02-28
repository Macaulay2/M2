--- status: DRAFT
--- author(s): from before
--- notes: 

undocumented {
	(texMath, InfiniteNumber),
	(texMath, SparseVectorExpression),
	(texMath, Divide),
	(texMath, FunctionApplication),
	(texMath, Thing),
	(texMath, Power),
	(texMath, SparseMonomialVectorExpression),
	(texMath, Subscript),
	(texMath, STRONG),
	(texMath, Superscript),
	(texMath, Expression),
	(texMath, Sum),
	(texMath, Symbol),
	(texMath, Hypertext),
	(texMath, SUB),
	(texMath, SUP),
	(texMath, Net),
	(texMath, String),
	(texMath, ITALIC),
	(texMath, MatrixExpression),
	(texMath, RowExpression),
	(texMath, TEX),
	(texMath, TT),
	(texMath, Product),
	(texMath, ZZ),
	(texMath, Adjacent),
	(texMath, Holder),
	(texMath, RR),
	(texMath, ChainComplex),
	(texMath, Minus),
	(texMath, Nothing),
	(texMath, Bag),
	(texMath, BasicList),
	(texMath, BinaryOperation),
	(texMath, CC),
	(texMath, CoherentSheaf),
	(texMath, Constant),
	(texMath, Descent),
	(texMath, EngineRing),
	(texMath, Function),
	(texMath, GeneralOrderedMonoid),
	(texMath, GradedModule),
	(texMath, GradedModuleMap),
	(texMath, GroebnerBasis),
	(texMath, HashTable),
	(texMath, Ideal),
	(texMath, IndeterminateNumber),
	(texMath, IndexedVariable),
	(texMath, IndexedVariableTable),
	(texMath, MapExpression),
	(texMath, Matrix),
	(texMath, Module),
        (texMath, MutableList),
	(texMath, MutableMatrix),
        (texMath, NumberedVerticalList),
	(texMath, Option),
	(texMath, Package),
	(texMath, Parenthesize),
	(texMath, ProjectiveHilbertPolynomial),
	(texMath, QQ),
	(texMath, RingElement),
	(texMath, RingMap),
	(texMath, Set),
	(texMath, SheafExpression),
	(texMath, SheafOfRings),
	(texMath, SumOfTwists),
	(texMath, Table),
	(texMath, Variety),
	(texMath, Vector),
	(texMath, VectorExpression),
	(texMath, VerticalList)
	     }

document { 
     Key => texMath,
     Headline => "convert to TeX math format",
     Usage => "texMath x",
     Inputs => {
	  "x" => "any Macaulay2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to TeX math format"}
	  },
     "The main difference between this and ", TO tex, " is that the
     surrounding dollar signs aren't there.",
     EXAMPLE {
	  "R = ZZ[x];",
      	  "texMath (x-1)^6",
	  },
     Caveat => {
	  "No attempt is made to wrap large matrices or equations."
	  },
     SeeAlso => {tex, mathML, showTex}
     }
