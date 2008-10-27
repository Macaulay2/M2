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
	(texMath, ZeroExpression),
	(texMath, SparseMonomialVectorExpression),
	(texMath, Subscript),
	(texMath, OneExpression),
	(texMath, STRONG),
	(texMath, Superscript),
	(texMath, Function),
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
	(texMath, Boolean),
	(texMath, RowExpression),
	(texMath, PolynomialRing),
	(texMath, List),
	(texMath, TEX),
	(texMath, Sequence),
	(texMath, Array),
	(texMath, TT),
	(texMath, Product),
	(texMath, ZZ),
	(texMath, Adjacent),
	(texMath, Holder2),
	(texMath, QuotientRing),
	(texMath, RR),
	(texMath, ChainComplex),
	(texMath, TABLE),
	(texMath, Minus),
	(texMath, Nothing)
     }

document { 
     Key => texMath,
     Headline => "convert to TeX math format",
     Usage => "texMath x",
     Inputs => {
	  "x" => "any Macaulay 2 object"
	  },
     Outputs => {
	  String => {TT "x", " converted to TeX math format"}
	  },
     "The main difference between this and ", TO tex, " is that the
     surrouding dollar signs aren't there.",
     EXAMPLE {
	  "R = ZZ[x];",
      	  "texMath (x-1)^6",
	  },
     Caveat => {
	  "No attempt is made to wrap large matrices or equations."
	  },
     SeeAlso => {tex, mathML, showTex}
     }
