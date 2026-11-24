--		Copyright 1993-1999 by Daniel R. Grayson

doc ///
  Key
    HeaderType
    (symbol SPACE, HeaderType, List)
    (symbol SPACE, HeaderType, Sequence)
  Headline
    a class of lists with abbreviated constructors
  Description
    Text
      These are the types @TT "X"@ of lists that can be constructed by
      expressions of the form @TT "X {a,b,c,...}"@. They also act on sequences.
    Example
      X = new HeaderType of BasicList
      X {a,b,c}
  SeeAlso
    "WrapperType"
    "SelfInitializingType"
///

doc ///
  Key
    WrapperType
    (symbol SPACE, WrapperType, List)
    (symbol SPACE, WrapperType, Sequence)
    (symbol SPACE, WrapperType, Thing)
  Headline
    a class of lists with abbreviated constructors
  Description
    Text
      These are the types @TT "X"@ of lists that can be constructed by
      expressions of the form @TT "X {a,b,c,...}"@, or, for lists of length
      one, by an expression of the form @TT "X a"@. They also act on sequences.
    Example
      X = new WrapperType of BasicList
      X {a,b,c}
      X a
  SeeAlso
    "HeaderType"
    "SelfInitializingType"
///

doc ///
  Key
    AssociativeExpression
  Headline
    the class of associative expressions
  Subnodes
    Sum
    DirectSum
    Product
    TensorProduct
    Equation
///

doc ///
  Key
    Holder
  Headline
    the class of all holder expressions
  Description
    Text
      This type of expression is a container for a single, arbitrary, thing
      that is basic enough that the correct method for printing does not depend
      on its neighbors in the containing expression. A negative number would
      not be basic enough for this purpose, since as a member of a sum, it
      would require special treatment.
  Subnodes
    Describe
    OneExpression
    Parenthesize
    ZeroExpression
///

doc ///
  Key
    Describe
  Headline
    the class of the output of describe
  Description
    Text
      This is a type of @TO "Holder"@ that contains the @TO "Expression"@
      produced by the method @TO "Describe"@.
///

doc ///
  Key
    ZeroExpression
  Headline
    the class of all zero expressions
  Description
    Text
      @TT "ZeroExpression"@ a type of @TO "Expression"@ of which there is just
      one instance, an expression representing the number 0.
///

doc ///
  Key
    OneExpression
  Headline
    the class all one expressions
  Description
    Text
      @TT "OneExpression"@ a type of @TO "Expression"@ of which there is just
      one instance, an expression representing the number 1.
///

doc ///
  Key
    Divide
  Headline
    the class of all divide expressions
  Description
    Text
      @TT "Divide"@ is a type of @TO "Expression"@ representing a quotient.
///

doc ///
  Key
    Table
  Headline
    the class of all table expressions
  Description
    Text
      @TT "Table"@ -- a type of @TO "Expression"@ representing a table, i.e., a
      list of lists of the same length.
    Example
      Table {{a,b,c},{a,bb,ccc}}
      value oo
  SeeAlso
    "MatrixExpression"
///

doc ///
  Key
    MatrixExpression
  Headline
    the class of all matrix expressions
  Description
    Text
      @TT "MatrixExpression"@ is a type of @TO "Expression"@ representing a
      matrix.
    Example
      MatrixExpression {{a,b,c},{a,bb,ccc}}
      R=QQ[x,y];
      MatrixExpression append(applyTable({{x^2-y^2,x^3-y^3},{x^2-4*y^2,x^3+y^3}},factor),Degrees=>{{{-2},{-3}},{{0},{0}}})
      value oo
  SeeAlso
    "Table"
///

doc ///
  Key
    VectorExpression
  Headline
    the class of all vector expressions
  Description
    Text
      @TT "VectorExpression"@ is a type of @TO "Expression"@ representing a
      vector.
    Example
      VectorExpression {a,b,c}
  SeeAlso
    "MatrixExpression"
///

doc ///
  Key
    MapExpression
  Headline
    the class of map expressions
  Description
    Text
      @TT "MapExpression"@ is a type of @TO "Expression"@ representing a map.
    Example
      MapExpression {a,b,c}
///

doc ///
  Key
    RowExpression
  Headline
    the class of all matrix expressions
  Description
    Text
      @TT "RowExpression"@ is a type of @TO "Expression"@ representing a
      horizontal sequence of expressions.
///

doc ///
  Key
    Minus
  Headline
    the class of all minus expressions
  Description
    Text
      @TT "Minus"@ is a type of @TO "Expression"@ representing negation.

      This is a unary operator.
///

-*
document {
     Key => NonAssociativeProduct,
     Headline => "the class of all nonassociative product expressions",
     TT "NonAssociativeProduct", " is a type of ", TO "Expression", " representing
     a nonassociative product."
     }
*-

doc ///
  Key
    Power
  Headline
    the class of all power expressions
  Description
    Text
      @TT "Power"@ is a type of @TO "Expression"@ representing a power.

      Normally power expressions with an exponent equal to 1 will not be
      produced. But it is desirable for @TO "factor"@ to return a product of
      powers, and some of them will have 1 as exponent. The routines for
      printing of expressions will take this into account, suppress exponents
      equal to 1, and arrange for parenthesization correctly.
///

doc ///
  Key
    Product
  Headline
    the class of all product expressions
  Description
    Text
      @TT "Product"@ is a type of @TO "Expression"@ representing a product.
///

doc ///
  Key
    TensorProduct
  Headline
    the class of all tensor product expressions
  Description
    Text
      @TT "TensorProduct"@ is a type of @TO "Expression"@ representing a tensor
      product.
///

doc ///
  Key
    SparseVectorExpression
  Headline
    the class of all sparse vector expressions
  Description
    Text
      @TT "SparseVectorExpression"@ is a type of @TO "Expression"@ representing
      a sparse vector.
///

doc ///
  Key
    SparseMonomialVectorExpression
  Headline
    the class of all sparse monomial vector expressions
  Description
    Text
      @TT "SparseMonomialVectorExpression"@ is a type of @TO "Expression"@
      representing a sparse monomial vector.

      The difference between this and @TO "SparseVectorExpression"@ is that the
      basis vectors are treated like variables for printing purposes.
///

doc ///
  Key
    BinaryOperation
  Headline
    the class of all binary operation expressions
  Description
    Text
      @TT "BinaryOperation"@ is a type of @TO "Expression"@ representing the
      result of a binary operation.
///

doc ///
  Key
    Subscript
  Headline
    the class of all subscript expressions
  Description
    Text
      @TT "Subscript"@ is a type of @TO "Expression"@ representing a
      subscripted expression.
///

doc ///
  Key
    Adjacent
  Headline
    the class of all adjacent expression pairs
  Description
    Text
      @TT "Adjacent"@ is a type of @TO "Expression"@ representing a pair of
      adjacent expressions, separated only by white space.
///

doc ///
  Key
    FunctionApplication
  Headline
    the class of all function application expressions
  Description
    Text
      @TT "FunctionApplication"@ is a type of @TO "Expression"@ representing an
      application of a function.
///

doc ///
  Key
    Superscript
  Headline
    the class of all superscript expressions
  Description
    Text
      @TT "Superscript"@ is a type of @TO "Expression"@ representing a
      superscripted expression.
///

doc ///
  Key
    Equation
  Headline
    the class of all equation expressions
  Description
    Text
      @TT "Equation"@ is a type of @TO "Expression"@ representing an equation.
///

doc ///
  Key
    Sum
  Headline
    the class of all sum expressions
  Description
    Text
      @TT "Sum"@ is a type of @TO "Expression"@ representing a sum.
///

doc ///
  Key
    DirectSum
  Headline
    the class of all direct sum expressions
  Description
    Text
      @TT "DirectSum"@ is a type of @TO "Expression"@ representing a direct
      sum.
///

doc ///
  Key
    ScriptedFunctor
    (symbol^, ScriptedFunctor, Thing)
    (symbol_, ScriptedFunctor, Thing)
    (symbol SPACE, ScriptedFunctor, Thing)
  Headline
    the class of all scripted functors
  Description
    Text
      A scripted functor accepts a subscript or a superscript: the primary
      example is @TO "HH"@.
  Subnodes
    "subscript"
    "superscript"
    "argument"
///

doc ///
  Key
    argument
  Headline
    specify the function in a scripted functor for an argument
  Description
    Text
      @TT "argument"@ -- a key used in scripted functors under which is stored
      the function that accepts the arguments.
  SeeAlso
    "ScriptedFunctor"
///

doc ///
  Key
    subscript
  Headline
    specify the function in a scripted functor for a subscript
  Description
    Text
      @TT "subscript"@ -- a key used in scripted functors under which is stored
      the function of one variable that accepts the subscript and returns a
      scripted functor that accepts the arguments.
  SeeAlso
    "ScriptedFunctor"
///

doc ///
  Key
    superscript
  Headline
    specify the function in a scripted functor for a superscript
  Description
    Text
      @TT "superscript"@ -- a key used in scripted functors under which is
      stored the function of one variable that accepts the superscript and
      returns a scripted functor that accepts the arguments.
  SeeAlso
    "ScriptedFunctor"
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
