--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => {HeaderType,
	  (symbol SPACE, HeaderType, List),
	  (symbol SPACE, HeaderType, Sequence)
	  },
     Headline => "a class of lists with abbreviated constructors",
     "These are the types ", TT "X", " of lists that can be constructed
     by expressions of the form ", TT "X {a,b,c,...}", ".  They also
     act on sequences.",
     PARA{},
     EXAMPLE {
	  "X = new HeaderType of BasicList",
	  "X {a,b,c}"
	  },
     SeeAlso => {"WrapperType", "SelfInitializingType"}
     }

document {
     Key => {WrapperType,
	  (symbol SPACE, WrapperType, List),
	  (symbol SPACE, WrapperType, Sequence),
	  (symbol SPACE, WrapperType, Thing)
	  },
     Headline => "a class of lists with abbreviated constructors",
     "These are the types ", TT "X", " of lists that can be constructed
     by expressions of the form ", TT "X {a,b,c,...}", ", or, for lists of
     length one, by an expression of the form ", TT "X a", ".  They also act
     on sequences.",
     PARA{},
     EXAMPLE {
	  "X = new WrapperType of BasicList",
	  "X {a,b,c}",
	  "X a"
	  },
     SeeAlso => {"HeaderType", "SelfInitializingType"}
     }

document {
     Key => AssociativeExpression,
     Headline => "the class of associative expressions",
     Subnodes => TO \ { Sum, DirectSum, Product, TensorProduct, Equation },
     }

document {
     Key => Holder,
     Headline => "the class of all holder expressions",
     PARA{},
     "This type of expression is a container for a single, arbitrary, thing that
     is basic enough that the correct method for printing does not depend
     on its neighbors in the containing expression.  A negative number would
     not be basic enough for this purpose, since as a member of a sum, it would
     require special treatment.",
     Subnodes => TO \ { Describe, OneExpression, Parenthesize, ZeroExpression },
     }

document {
     Key => Describe,
     Headline => "the class of the output of describe",
     PARA{},
     "This is a type of ", TO "Holder", " that contains the ",
     TO "Expression", " produced by the method ", TO "Describe","."
     }

document {
     Key => ZeroExpression,
     Headline => "the class of all zero expressions",
     TT "ZeroExpression", " a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 0."
     }

document {
     Key => OneExpression,
     Headline => "the class all one expressions",
     TT "OneExpression", " a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 1."
     }

document {
     Key => Divide,
     Headline => "the class of all divide expressions",
     TT "Divide", " is a type of ", TO "Expression", " representing a quotient."
     }

document {
     Key => Table,
     Headline => "the class of all table expressions",
     TT "Table", " -- a type of ", TO "Expression", " representing
     a table, i.e., a list of lists of the same length.",
     PARA{},
     EXAMPLE {
	  ///Table {{a,b,c},{a,bb,ccc}}///,
	  ///value oo///,
	  },
     SeeAlso => {"MatrixExpression"}
     }

document {
     Key => MatrixExpression,
     Headline => "the class of all matrix expressions",
     TT "MatrixExpression", " is a type of ", TO "Expression", " representing
     a matrix.",
     PARA{},
     EXAMPLE {
	 ///MatrixExpression {{a,b,c},{a,bb,ccc}}///,
	 ///R=QQ[x,y];///,
         ///MatrixExpression append(applyTable({{x^2-y^2,x^3-y^3},{x^2-4*y^2,x^3+y^3}},factor),Degrees=>{{{-2},{-3}},{{0},{0}}})///,
	 ///value oo///
	 },
     SeeAlso => {"Table"}
     }

document {
     Key => VectorExpression,
     Headline => "the class of all vector expressions",
     TT "VectorExpression", " is a type of ", TO "Expression", " representing
     a vector.",
     PARA{},
     EXAMPLE ///VectorExpression {a,b,c}///,
     SeeAlso => {"MatrixExpression"}
     }

document {
     Key => MapExpression,
     Headline => "the class of map expressions",
     TT "MapExpression", " is a type of ", TO "Expression", " representing
     a map.",
     PARA{},
     EXAMPLE ///MapExpression {a,b,c}///,
     }

document {
     Key => RowExpression,
     Headline => "the class of all matrix expressions",
     TT "RowExpression", " is a type of ", TO "Expression", " representing
     a horizontal sequence of expressions."
     }

document {
     Key => Minus,
     Headline => "the class of all minus expressions",
     TT "Minus", " is a type of ", TO "Expression", " representing negation.",
     PARA{},
     "This is a unary operator."
     }

-*
document {
     Key => NonAssociativeProduct,
     Headline => "the class of all nonassociative product expressions",
     TT "NonAssociativeProduct", " is a type of ", TO "Expression", " representing
     a nonassociative product."
     }
*-

document {
     Key => Power,
     Headline => "the class of all power expressions",
     TT "Power", " is a type of ", TO "Expression", " representing a power.",
     PARA{},
     "Normally power expressions with an exponent equal to 1 will not be
     produced.  But it is desirable for ", TO "factor", " to return 
     a product of powers, and some of them will have 1 as exponent.  The
     routines for printing of expressions will take this into account,
     suppress exponents equal to 1, and arrange for parenthesization
     correctly."
     }

document {
     Key => Product,
     Headline => "the class of all product expressions",
     TT "Product", " is a type of ", TO "Expression", " representing a product."
     }

document {
     Key => TensorProduct,
     Headline => "the class of all tensor product expressions",
     TT "TensorProduct", " is a type of ", TO "Expression", " representing a tensor product."
     }

document {
     Key => SparseVectorExpression,
     Headline => "the class of all sparse vector expressions",
     TT "SparseVectorExpression", " is a type of ", TO "Expression", "
     representing a sparse vector."
     }

document {
     Key => SparseMonomialVectorExpression,
     Headline => "the class of all sparse monomial vector expressions",
     TT "SparseMonomialVectorExpression", " is a type of ", TO "Expression", "
     representing a sparse monomial vector.",
     PARA{},
     "The difference between this and ", TO "SparseVectorExpression", " is that
     the basis vectors are treated like variables for printing purposes."
     }

document {
     Key => BinaryOperation,
     Headline => "the class of all binary operation expressions",
     TT "BinaryOperation", " is a type of ", TO "Expression", " representing
     the result of a binary operation."
     }

document {
     Key => Subscript,
     Headline => "the class of all subscript expressions",
     TT "Subscript", " is a type of ", TO "Expression", " representing a
     subscripted expression."
     }

document {
     Key => Adjacent,
     Headline => "the class of all adjacent expression pairs",
     TT "Adjacent", " is a type of ", TO "Expression", " representing a pair
     of adjacent expressions, separated only by white space."
     }

document {
     Key => FunctionApplication,
     Headline => "the class of all function application expressions",
     TT "FunctionApplication", " is a type of ", TO "Expression", " representing an
     application of a function."
     }

document {
     Key => Superscript,
     Headline => "the class of all superscript expressions",
     TT "Superscript", " is a type of ", TO "Expression", " representing a
     superscripted expression."
     }

document {
     Key => Equation,
     Headline => "the class of all equation expressions",
     TT "Equation", " is a type of ", TO "Expression", " representing an
     equation."
     }

document {
     Key => Sum,
     Headline => "the class of all sum expressions",
     TT "Sum", " is a type of ", TO "Expression", " representing a sum."
     }

document {
     Key => DirectSum,
     Headline => "the class of all direct sum expressions",
     TT "DirectSum", " is a type of ", TO "Expression", " representing a direct sum."
     }

document {
     Key => {ScriptedFunctor,
	  (symbol^, ScriptedFunctor, Thing),
	  (symbol_, ScriptedFunctor, Thing),
	  (symbol SPACE, ScriptedFunctor, Thing)},
     Headline => "the class of all scripted functors",
     "A scripted functor accepts a subscript or a superscript:
     the primary example is ", TO "HH", ".",
     Subnodes => TO \ { "subscript", "superscript", "argument" },
     }

document {
     Key => argument,
     Headline => "specify the function in a scripted functor for an argument",
     TT "argument", " -- a key used in scripted functors under which is
     stored the function that accepts the arguments.",
     SeeAlso => "ScriptedFunctor"
     }

document {
     Key => subscript,
     Headline => "specify the function in a scripted functor for a subscript",
     TT "subscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the subscript and
     returns a scripted functor that accepts the arguments.",
     SeeAlso => "ScriptedFunctor"
     }

document {
     Key => superscript,
     Headline => "specify the function in a scripted functor for a superscript",
     TT "superscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the superscript and
     returns a scripted functor that accepts the arguments.",
     SeeAlso => "ScriptedFunctor"
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
