undocumented methods hold
undocumented methods expression

document {
     Key => { Expression },
     Headline => "the class of all expressions",
     "An ", EM "expression", " is a symbolic representation of a mathematical expression.  It retains some of the semantics of the mathematical expression,
     as well as enough information to print the expression nicely.  In Macaulay2 expressions have two main functions: they are an intermediate phase in
     the conversion of a mathematical object to a ", TO "net", " that can be printed; and they are a way of holding and displaying a mathematical expression
     in an unevaluated form that can be both printed and evaluated.",
     PARA {},
     "Internally, each expression is a basic list whose elements may also be expressions.  The elements that are not expressions are interpreted as themselves,
     and may be strings, symbols, numbers, etc.  There are several types of expression that correspond to various sorts of mathematical entities, such as
     sums of class ", TO "Sum", ", products, of class ", TO "Product", ", fractions of class ", TO "Divide", ", etc.",
     PARA {},
     "Expressions are produced with the function ", TO "expression", ".  The various methods installed for it try to bring as much of the semantic structure of
     the mathematical object to light.  The following examples illustrate that, using ", TO "peek", " and ", TO "peek'", " to display the internal structure.",
     EXAMPLE lines ///
     	  expression 4
	  peek oo
	  d = expression (-4)
	  peek oo
	  QQ[x];
	  f = (x+1)^5
     	  peek f
     	  e = expression f
     	  peek e
	  peek'_2 e
	  peek'_11 e
     ///,
     "The function ", TO "factor", " returns an expression.",
     EXAMPLE lines ///
     	  c = factor f
	  peek'_2 c
	  factor 240012
     ///,
     "Expressions can be evaluated using ", TO "value", ".",
     EXAMPLE lines ///
	  value e
	  value e == f
     	  value c
     ///,
     "The following operators can be applied to expressions: ", TO "SPACE", ", ", TO "*", ", ", TO "**", ", ", TO "+", ", ", TO "-", ", ", 
     TO "/", ", ", TO "==", ", ", TO "^", ", and ", TO "_", ".  They are contagious, in the sense that when applied to an expression and a non-expression,
     the non-expression will be converted to an expression and the operator will be applied.  Only the most trivial algebraic simplifications are applied.",
     EXAMPLE lines ///
     	  d + e
	  d + 4
	  d / 4
	  d / 1
	  d == e
     ///,
     Subnodes => {
	 TO (value, Expression),
	 TO hold,
	 TO Table,
	 TO Holder,
	 TO Adjacent,
	 TO MapExpression,
	 TO FunctionApplication,
	 TO AssociativeExpression,
	 TO MatrixExpression,
	 TO VectorExpression,
	 TO RowExpression,
	 TO SparseVectorExpression,
	 TO SparseMonomialVectorExpression,
	 TO BinaryOperation,
	 TO Divide,
	 TO Minus,
	 TO Power,
	 TO Subscript,
	 TO Superscript,
         }
     }

document {
    Key => expression,
    Headline => "convert to an expression",
    Usage => "expression x",
    Inputs => { "x" },
    Outputs => { Expression => { " constructed from ", TT "x", " using the mathematical structure of ", TT "x", ", if present" }},
    "Here is example of a function that expresses small rational numbers as Egyptian fractions using ", TO "expression", ".",
    EXAMPLE {
	"egyptian = method();",
	///egyptian QQ := x -> if x == 0 then 0 else (
	    n := ceiling(1/x);
	    expression(1/n) + egyptian(x - 1/n));///,
	"egyptian(30/31)"
    }
}

document {
    Key => hold,
    Headline => "hold something in a holder expression",
    Usage => "hold x",
    Inputs => { "x" },
    Outputs => {
	Expression => { " of class ", TO "Holder", " containing ", TT "x", " as its single element, unless ", TT "x",
	    " is already an expression, in which case ", TT "x", " is returned." }},
    EXAMPLE "(hold 2)^5 * (hold 3)^3",
}
