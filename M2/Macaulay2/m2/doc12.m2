--		Copyright 1993-1999 by Daniel R. Grayson

document { (accumulate, Function, Thing, VisibleList),
     TT "accumulate(f,x0,{x1,...,xn})", " -- computes the list ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     EXAMPLE {
	  "accumulate(identity, a, {b,c,d,e})",
	  "accumulate (times, 1, 1 .. 10)"
	  }
     }
document { (accumulate, Function, VisibleList),
     TT "accumulate(f,{x0,x1,...,xn})", " -- computes the list ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     EXAMPLE {
	  "accumulate(identity, {a,b,c,d,e})",
	  "accumulate(plus, 0 .. 10)"
	  }
     }
document { (accumulate, VisibleList, Thing, Function),
     TT "accumulate({xn,...,x1},x0,f)", " -- computes the list ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     EXAMPLE "accumulate({a,b,c,d}, e, identity)"
     }
document { (accumulate, VisibleList, Function),
     TT "accumulate({xn,...,x1,x0},f)", " -- computes the list ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     EXAMPLE "accumulate({a,b,c,d,e}, identity)"
     }
document { accumulate,
     HEADLINE "apply binary operator repeatedly",
     SEEALSO {"fold"}
     }

TEST ///
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
///     

document { (fold, Function, Thing, VisibleList),
     TT "fold(f,x0,{x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     EXAMPLE "fold(identity, a, {b,c,d,e})"
     }
document { (fold, Function, VisibleList),
     TT "fold(f,{x0,x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     EXAMPLE "fold(identity, {a,b,c,d,e})"
     }
document { (fold, VisibleList, Thing, Function),
     TT "fold({xn,...,x1},x0,f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE "fold({a,b,c,d}, e, identity)"
     }
document { (fold, VisibleList, Function),
     TT "fold({xn,...,x1,x0},f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE "fold({a,b,c,d,e}, identity)"
     }
document { fold,
     HEADLINE "apply binary operator repeatedly",
     SEEALSO {"accumulate"}
     }

TEST ///
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
///

document { demark,
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA,
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }

document { InfiniteNumber, HEADLINE "the class of all infinite numbers" }

document { infinity, HEADLINE "infinity" }

document { IndeterminateNumber,
     HEADLINE "the class of all indeterminate numbers",
     "Indeterminate numbers result, for exmaple, from multiplying 0 by infinity.
     There is only one instance of this class."
     }

document { indeterminate,
     TT "indeterminate", " -- a representation of an indeterminate number,
     such as might result from multiplying 0 by infinity.",
     }

document { max,
     TT "max x", " -- yields the maximum of the elements in the list or sequence x."
     }

document { min,
     TT "min x", " -- yields the minimum of the elements in the list or sequence x."
     }

TEST ///
assert(max{4,5,6} === 6)
assert(min{4,5,6} === 4)
assert(max(4,5,6) === 6)
assert(min(4,5,6) === 4)
///

document { sort,
     TT "sort v", " -- produces a sorted version of the list v.",
     PARA,
     "The sort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "sort {c,e,a,f,b,f}",
	  "sort {4,2,6,3,8,2}"
	  },
     SEEALSO { "rsort", "<=", "?" }
     }

document { rsort,
     TT "rsort v", " -- produces a reverse sorted version of the list v.",
     PARA,
     "The rsort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "rsort {g,d,w,s,c,a,r}",
	  "rsort {4,2,3,1}",
	  },
     SEEALSO { "sort", "<=", "?" }
     }


document { pack,
     TT "pack(n,v)", " -- packs the elements of the list or sequence
     ", TT "v", " into a table ", TT "n", " at a time.",
     PARA,
     "It produces, from a list ", TT "v", ", a list of lists formed 
     by packing the elements of ", TT "v", " into lists ", TT "n", " 
     at a time.  The last of the lists produced may have fewer 
     than ", TT "n", " elements.",
     EXAMPLE "pack(3,{a,b,c,d,e,f,g,h,i,j,k})",
     }

document { join,
     TT "join(u,v,...)", " -- joins the elements of the lists or
     sequences u, v, ... into a single list.",
     PARA,
     "The class of the result is the same as the class of the first argument.
     If there is just one argument, and it's mutable, a copy is returned.",
     EXAMPLE "join({1,2,3},{a,b,c},{7,8,9})",
     PARA,
     "The operator ", TO (symbol |, List, List), " can be used as a synonym."
     }

document { take,
     TT "take(v,n)    ", " -- yields a list containing the first n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,-n)", "    -- yields a list containing the last n elements of the list v.",
     BR,NOINDENT,
     TT "take(v,{m,n})", " -- yields a list containing the elements of the list v 
     in positions m through n.",
     PARA,
     EXAMPLE {
	  "take({a,b,c,d,e,f},3)",
	  "take({a,b,c,d,e,f},-3)",
	  "take({a,b,c,d,e,f},{2,4})",
	  },
     SEEALSO "drop"
     }

document { first,
     TT "first v", " -- yields the first element of the list v.",
     PARA,
     "See also ", TO "last", "."
     }

document { last,
     TT "last v", " -- yields the last element of the list v.",
     PARA,
     "See also ", TO "first", "."
     }

document { positions,
     TT "positions(v,f)", " -- yields a list of integers giving the positions of the
     elements of the list v which yield the value true when
     the function f is applied."
     }

TEST "
assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
"

document { position,
     TT "position(v,f)", " -- returns the index of the first element of v satisfying 
     the condition f, or null if there is none."
     }

document { delete,
     TT "delete(x,v)", " -- removes any occurrences of the expression ", TT "x", "
     from the list ", TT "v", ".",
     PARA,
     "Equality is determined with ", TO "==", " which may do extensive
     calculations in certain cases.",
     EXAMPLE {
	  "delete(c,{a,b,c,d,e,a,b,c,d,e})",
	  },
     SEEALSO "member"
     }

TEST "
stream = (action,state) -> () -> stream(action, action state)
fib = stream( (i,j) -> (j,i+j), (0,1))
scan(1 .. 22, i -> fib = fib())
"

document { ultimate,
     HEADLINE " -- ultimate value for an iteration",
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA,
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported.",
     PARA,
     "It's a bad idea to use this function, because unexpected errors will
     produce unexpected results silently."
     }

document { tmpname,
     TT "tmpname x", " -- create a temporary file name based on the string x
     unique to this process.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     "The routine doesn't actually check to see whether file exists."
     }

document { subsets,
     TT "subsets", " -- a function for computing a list of subsets
     of a set or list.",
     PARA,
     MENU {
	  TO (subsets,ZZ,ZZ),
	  TO (subsets,Set,ZZ),
	  TO (subsets,List),
	  TO (subsets,Set)
	  }
     }

document { (subsets,ZZ,ZZ),
     TT "subsets(n,j)", " -- for an integer ", TT "n", ", yields a list of those
     subsets of ", TT "{0, ..., n-1}", " which have ", TT "j", " elements.",
     PARA,
     EXAMPLE "subsets(3,2)",
     SEEALSO "subsets"
     }
document { (subsets,Set,ZZ),
     TT "subsets(s,j)", " -- yields a list of those subsets of the list or 
     set ", TT "s", " which have ", TT "j", " elements.",
     PARA,
     EXAMPLE "subsets(set {a,b,c},2)",
     SEEALSO "subsets"
     }
document { (subsets,List),
     TT "subsets s", " -- yields a list of the subsets of the list ", TT "s", ".",
     PARA,
     "The subsets are returned as lists whose elements are in the same order.",
     EXAMPLE "subsets {1,2,3}",
     SEEALSO "subsets"
     }
document { (subsets,Set),
     TT "subsets s", " -- yields a list of the subsets of the set ", TT "s", ".",
     PARA,
     EXAMPLE "subsets set {a,b,c}",
     SEEALSO "subsets"
     }

TEST "
assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
assert( 
     set subsets(set {a,b,c,d},2) === 
     set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )
assert( partitions 4 === {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} )
assert( partitions(5,3) === {{3,2},{3,1,1},{2,2,1},{2,1,1,1},{1,1,1,1,1}} )
"


document { partitions,
     TT "partitions n", " -- returns a list of the partitions of the integer n.",
     BR, NOINDENT,
     TT "partitions(n,k)", " -- returns a list of the partitions of the integer n
     into terms each of which does not exceed k.",
     PARA,
     EXAMPLE {
	  "partitions 4",
      	  "partitions(4,2)",
	  },
     }

document { examples,
     -- this should come after the documentation for partitions, because of the example
     TT "examples f", " -- returns a list of strings containing examples
     of code using the function ", TT "f", " provided in the documentation
     of ", TT "f", ".",
     SEEALSO {"document", "printExamples"}
     }

TEST ///
     assert( class examples MutableList === List )
     assert( # examples MutableList > 0 )
///

document { (symbol +, List, List),
     TT "v + w", " -- the sum of two vectors represented as lists."
     }

document { (symbol _, List, List),
     HEADLINE "get a list of entries",
     TT "w_{i,j,...}", " -- selects entries from a list or sequence ", TT "w", ".",
     PARA,
     EXAMPLE {
	  "w = {a,b,c,d,e,f,g,h};",
      	  "w_{1,3,4}",
	  },
     "We can use this operation to compute composition of permutations
     represented as lists.",
     EXAMPLE "{4,2,3,1,0} _ {2,1,3,4,0}"
     }

document { number,
     TT "number(x,f)", " -- the number of elements e of the list x for which f(e) is true.",
     PARA,
     SEEALSO { "positions", "select" }
     }

document { all,
     TT "all(v,f)", " -- whether each element x of a list or hash table
     v has f(x) true.",
     PARA,
     "Returns the value true if all elements v#i of the list v yield 
     the value true when the function f is applied, otherwise returns 
     false.  For hash tables, the function is applied to all its key/value
     pairs (k,v), just as with ", TO "any", ".",
     PARA,
     SEEALSO { "scan", "apply", "select", "any", "member" }
     }

document { same,
     TT "same v", " -- whether every element of the list v is the same.
     The comparison is done with ", TO "==", "."
     }

document { member, HEADLINE "whether something is a member",
     TT "member(e,x)", " -- whether ", TT "e", " is an element of the list, set, or 
     sequence ", TT "x", ".",
     PARA,
     EXAMPLE {
	  "x = {a,b,c,d,e};",
      	  "member(c,x)",
      	  "member(f,x)"
	  },
     SEEALSO {"positions"}
     }

document { sum,
     TT "sum", " -- provides the sum of the members of a list, set, 
     or chain complex, optionally with a function applied to each one."
     }
document { (sum, List),
     HEADLINE "sum the elements of a list",
     TT "sum v", " -- yields the sum of the elements in the list ", TT "v", ".",
     PARA,
     EXAMPLE "sum {1,2,3,4,5}",
     SEEALSO "sum"
     }
document { (sum, List, List, Function),
     HEADLINE "sum results of applying a function pairwise",
     TT "sum(v,w,f)", " -- yields the sum of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  "sum({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "sum"
     }
document { (sum, List, Function),
     HEADLINE "sum results of applying a function",
     TT "sum(v,f)", " -- yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, ZZ, Function),
     HEADLINE "sum consecutive values of a function",
     TT "sum(n,f)", " -- compute the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA,
     EXAMPLE "sum(10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, Tally),
     HEADLINE "sum of elements",
     TT "sum v", " -- yields the sum of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }
document { (sum, Set),
     HEADLINE "sum of elements",
     TT "sum v", " -- yields the sum of the elements in the set ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = set{1,100,10000}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }

document { product,
     TT "product", " -- provides the product of the members of a list or set,
     optionally with a function applied to each one."
     }
document { (product, List),
     HEADLINE "product of elements",
     TT "product v", " -- yields the product of the elements in the list v.",
     PARA,
     EXAMPLE "product {1,2,3,4,5}"
     }
document { (product, List, List, Function),
     HEADLINE "product of results of applying a function pairwise",
     TT "product(v,w,f)", " -- yields the product of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists ", TT "v", " and ", TT "w", ", which should be of the same length.",
     PARA,
     EXAMPLE {
	  "M = monoid [x,y,z];",
      	  "product({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SEEALSO "product"
     }
document { (product, List, Function),
     HEADLINE "product of values of a function",
     TT "product(v,f)", " -- yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SEEALSO "product"
     }
document { (product, ZZ, Function),
     HEADLINE "product of consecutive values of a function",
     TT "product(n,f)", " -- compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA,
     EXAMPLE "product(5, i -> 2*i+1)",
     SEEALSO "product"
     }
document { (product, Tally),
     HEADLINE "product of elements",
     TT "product v", " -- yields the product of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SEEALSO "product"
     }
document { (product, Set),
     HEADLINE "product of elements",
     TT "product v", " -- yields the product of the elements in the set ", TT "v", ".",
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SEEALSO "product"
     }

document { toString,
     TT "toString x", " -- converts ", TT "x", " to a string.",
     PARA,
     "See also ", TO "toExternalString", " which will try to convert ", TT "x", "
     to a string which can be read back into the program later."     
     }

document { toExternalString,
     TT "toExternalString x", " -- converts ", TT "x", " to a string, in such a way
     that it can be read back into the program later.",
     PARA,
     "See also ", TO "toString", " which simply converts ", TT "x", "
     to a string which can be displayed meaningfully.",     
     PARA,
     "Not everything can be converted to a string in such a way that it
     can be read back into the program later, because circular data structures
     are common."
     }

document { HeaderType,
     HEADLINE "a class of lists with abbreviated constructors",
     "These are the types ", TT "X", " of lists which can be constructed
     by expressions of the form ", TT "X {a,b,c,...}", ".  They also
     act on sequences.",
     PARA,
     EXAMPLE {
	  "X = new HeaderType of BasicList",
	  "X {a,b,c}"
	  },
     SEEALSO {"WrapperType", "SelfInitializingType"}
     }

document { WrapperType,
     HEADLINE "a class of lists with abbreviated constructors",
     "These are the types ", TT "X", " of lists which can be constructed
     by expressions of the form ", TT "X {a,b,c,...}", ", or, for lists of
     length one, by an expression of the form ", TT "X a", ".  They also act
     on sequences.",
     PARA,
     EXAMPLE {
	  "X = new WrapperType of BasicList",
	  "X {a,b,c}",
	  "X a"
	  },
     SEEALSO {"HeaderType", "SelfInitializingType"}
     }

document { AssociativeExpression }

document { Holder,
     TT "Holder", " -- a type of ", TO "Expression", ".",
     PARA,
     "This type of expresssion is a container for a single, arbitrary, thing which
     is basic enough that the correct method for printing does not depend
     on its neighbors in the containing expression.  A negative number would
     not be basic enough for this purpose, since as a member of a sum, it would
     require special treatment."
     }

document { ZeroExpression,
     TT "ZeroExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 0."
     }
document { OneExpression,
     TT "OneExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 1."
     }

document { DoubleArrow,
     TT "DoubleArrow", " -- a type of ", TO "Expression", " which represents
     something of the form ", TT "a => b", ".",
     PARA,
     "This is experimental, and intended for internal use only."
     }

document { Expression, HEADLINE "the class of all expressions",
     "These expressions are symbolic representations of algebraic
     expressions, mainly useful in printing.  The method for 
     producing them is ", TO "expression", ".  The usual algebraic
     operations are available for them, but most simplifications do not
     occur.",
     PARA,
     "The parts of expressions are not always expressions.  For example,
     ", TO "factor", " returns such an expression.",
     PARA,
     EXAMPLE "(expression 2)^5 * (expression 3)^3 / ((expression 5) * (expression 11)^2)^6",
     PARA,
     "Functions which act on expressions:",
     MENU {
	  TO "value",
	  TO "precedence"
	  }
     }

document { expression,
     TT "expression x", " -- make an ", TO "Expression", " from x."
     }

document { Divide,
     TT "Divide", " -- a type of ", TO "Expression", " representing a quotient."
     }
document { Table,
     TT "Table", " -- a type of ", TO "Expression", " representing
     a table, i.e., a list of lists of the same length.",
     PARA,
     EXAMPLE {
	  ///Table {{a,b,c},{a,bb,ccc}}///,
	  ///value oo///,
	  },
     SEEALSO {"MatrixExpression"}
     }
document { MatrixExpression,
     TT "MatrixExpression", " -- a type of ", TO "Expression", " representing
     a matrix.",
     PARA,
     EXAMPLE ///MatrixExpression {{a,b,c},{a,bb,ccc}}///,
     SEEALSO {"Table"}
     }
document { RowExpression,
     TT "RowExpression", " -- a type of ", TO "Expression", " representing
     a horizontal sequence of expressions."
     }
document { Minus,
     TT "Minus", " -- a type of ", TO "Expression", " representing negation.",
     PARA,
     "This is a unary operator."
     }
document { NonAssociativeProduct,
     TT "NonAssociativeProduct", " -- a type of ", TO "Expression", " representing
     a nonassociative product."
     }
document { Power,
     TT "Power", " -- a type of ", TO "Expression", " representing a power.",
     PARA,
     "Normally power expressions with an exponent equal to 1 will not be
     produced.  But it is desirable for ", TO "factor", " to return 
     a product of powers, and some of them will have 1 as exponent.  The
     routines for printing of expressions will take this into account,
     suppress exponents equal to 1, and arrange for parenthesization
     correctly."
     }
document { Product,
     TT "Product", " -- a type of ", TO "Expression", " representing a product."
     }
document { SparseVectorExpression,
     TT "SparseVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse vector."
     }
document { SparseMonomialVectorExpression,
     TT "SparseMonomialVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse monomial vector.",
     PARA,
     "The difference between this and ", TO "SparseVectorExpression", " is that
     the basis vectors are treated like variables for printing purposes."
     }
document { BinaryOperation,
     TT "BinaryOperation", " -- a type of ", TO "Expression", " representing
     the result of a binary operation."
     }
document { Subscript,
     TT "Subscript", " -- a type of ", TO "Expression", " representing a
     subscripted expression."
     }
document { Adjacent,
     TT "Adjacent", " -- a type of ", TO "Expression", " representing a pair
     of adjacent expressions, separated only by white space."
     }
document { FunctionApplication,
     TT "FunctionApplication", " -- a type of ", TO "Expression", " representing an
     application of a function."
     }
document { Superscript,
     TT "Superscript", " -- a type of ", TO "Expression", " representing a
     superscripted expression."
     }
document { Equation,
     TT "Equation", " -- a type of ", TO "Expression", " representing an
     equation."
     }
document { Sum,
     TT "Sum", " -- a type of ", TO "Expression", " representing a sum."
     }

document { tex,
     TT "tex x", " -- convert ", TT "x", " to TeX format.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..f]",
      	  "tex matrix {{a^2+2,b,c},{d,e,f^3-a}}",
	  },
     SEEALSO {"TeX", "texMath"}
     }

document { texMath,
     TT "texMath x", " -- convert ", TT "x", " to TeX format
     for use in TeX math mode.",
     PARA,
     "The main difference between this and ", TO "tex", " is that the
     surrouding dollar signs aren't there.",
     PARA,
     EXAMPLE {
	  "R = ZZ[x]",
      	  "texMath (x-1)^6",
	  },
     SEEALSO {"TeX", "tex"}
     }

document { TeX,
     TT "TeX x", " -- convert ", TT "x", " to TeX format, and display it on the screen.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     SEEALSO "tex"
     }

document { print,
     TT "print x", " -- prints ", TT "x", " on the standard output followed by a new line",
     PARA,
     "The return value is ", TO "null", "."
     }
document { (symbol <<, File, Thing),
     TT "f << x", " -- prints the expression x on the output file f.",
     PARA,
     "Returns f as its value.  Parsing associates leftward, so that 
     several expressions may be displayed with something like ", TT "f<<x<<y<<z", ".
     If ", TT "f", " is a string, then a new file with name ", TT "f", " is created,
     the expression ", TT "x", " is printed into ", TT "f", ", and the file ", TT "f", " is closed.",
     PARA,
     EXAMPLE {
	  "x = 5",
      	  ///<< "the value of x is " << x << endl///,
	  },
     SEEALSO {"<<"}
     }     

document { hold,
     TT "hold x", " -- embeds it argument x in a list of class ", TO "Holder", ".",
     PARA,
     "It might be useful for displaying an integer in factored form,
     for example, because the usual algebraic operations are available
     for ", TO "Expression", "s, but no simplification occurs.",
     PARA,
     EXAMPLE "(hold 2)^5 * (hold 3)^3 * (hold 5) * (hold 11)^2",
     PARA,
     "Here is example of a little function that expresses rational numbers
     as Egyptian fractions using ", TT "hold", ".",
     EXAMPLE {
	  "egyptian = method();",
	  ///egyptian QQ := x -> (
    if x == 0 then 0
    else (
         n := ceiling(1/x);
         hold(1/n) + egyptian(x - 1/n) 
         ));///,
     	  "egyptian(30/31)"
     	  }
     }

document { RightArrow,
     TT "RightArrow", " -- an entity used in hypertext to represent an
     rightward pointing arrow."
     }

document { DownArrow,
     TT "DownArrow", " -- an entity used in hypertext to represent an
     downward pointing arrow."
     }

document { peek,
     TT "peek s", " -- displays contents of ", TT "s", " to depth 1, bypassing
     installed methods.",
     PARA,
     EXAMPLE {
	  "t = set {1,2,3}",
      	  "peek t",
      	  "new MutableHashTable from {a=>3, b=>44}",
      	  "peek oo"
	  },
     SEEALSO "peek2"
     }

document { peek2,
     TT "peek2(s,n)", " -- displays contents of ", TT "s", " to depth ", TT "n", ", 
     bypassing installed methods.",
     PARA,
     "It applies the default output method to the object ", TT "s", ",
     bypassing the installed method for objects of its class.",
     EXAMPLE {
	  "s = factor 112",
      	  "peek s",
      	  "peek2(s,2)"
	  },
     SEEALSO "peek"
     }

document { pad,
     TT "pad(s,n)", " -- pads the string s to length n with spaces on the right.",
     BR,
     NOINDENT, 
     TT "pad(n,s)", " -- pads the string s to length n with spaces on the left."
     }

document { columnate,
     TT "columnate(s,w)", " -- arranges the strings in the list s in columns, returning
     a ", TO "Net", " suitable for output to a terminal with a linewidth of w.",
     PARA,
     EXAMPLE {
	  "columnate(characters ascii (65 .. 90) , 12)",
	  }
     }

document { ScriptedFunctor, HEADLINE "the class of all scripted functors",
     "A scripted functor accepts a subscript or a superscript:
     the primary example is ", TO "HH", ".",
     SEEALSO {"subscript", "superscript", "argument"}
     }

document { argument,
     TT "argument", " -- a key used in scripted functors under which is
     stored the function that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { subscript,
     TT "subscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the subscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { superscript,
     TT "superscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the superscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { HH, HEADLINE "general homology and cohomology functor",
     TT "HH", " is a ", TO "ScriptedFunctor", " which serves as an interface
     to the methods for ",  TO "homology", " and ", TO "cohomology", ", in
     the sense that, ", TT "HH_i(M)", " is an abbreviation for ", TT "homology(i,M)", "
     and ", TT "HH^i(M)", " is an abbreviation for ", TT "cohomology(i,M)", ".
     A second argument and optional arguments may be added."
     }

TEST ("
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )
     ")

document { cohomology,
     TT "cohomology", " -- a method name available for computing expressions
     of the forms ", TT "HH^i(X)", " and ", TT "HH^i(M,N)", ".",
     PARA,
     "If it is intended that ", TT "i", " be of class ", TT "ZZ", ", ", TT "M", " be of
     class ", TT "A", ", and ", TT "N", " be of 
     class ", TT "B", ", then the method can be installed with ",
     PRE "     cohomology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SEEALSO {"homology", "HH", "ScriptedFunctor"}
     }

document { homology,
     TT "homology(f,g)", " -- computes the homology module ", TT "(kernel f)/(image g)", ".",
     BR, NOINDENT,
     TT "homology", " -- a method name available for computing expressions
     of the forms ", TT "HH_i(X) and", " ", TT "HH_i(M,N).",
     PARA,
     "If it is intended that ", TT "i", " be of class ", TT "ZZ", ", 
     ", TT "M", " be of class ", TT "A", ", and ", TT "N", " be of
     class ", TT "B", ", then the method can be installed with ",
     PRE "     homology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SEEALSO {"HH", "cohomology", "ScriptedFunctor"}
     }
