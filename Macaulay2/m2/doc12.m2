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
     Headline => "apply binary operator repeatedly",
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
     EXAMPLE {
	  "fold(identity, {a,b,c,d,e})",
	  "fold(plus, {1,2,3,4,5})"
	  }
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
     Headline => "apply binary operator repeatedly",
     SEEALSO {"accumulate"}
     }

TEST ///
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
///

document { demark,
     Headline => "insert a string between elements of a list of strings",
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA,
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }

document { InfiniteNumber,
     Headline => "the class of all infinite numbers"
     }

document { infinity,
     Headline => "infinity"
     }

document { IndeterminateNumber,
     Headline => "the class of all indeterminate numbers",
     "Indeterminate numbers result, for exmaple, from multiplying 0 by infinity.
     There is only one instance of this class."
     }

document { indeterminate,
     Headline => "an indeterminate number",
     TT "indeterminate", " -- a representation of an indeterminate number,
     such as might result from multiplying 0 by infinity.",
     }

document { max,
     Headline => "maximum of elements of a list",
     TT "max x", " -- yields the maximum of the elements in the list or sequence x."
     }

document { min,
     Headline => "minimum of elements of a list",
     TT "min x", " -- yields the minimum of the elements in the list or sequence x."
     }

TEST ///
assert(max{4,5,6} === 6)
assert(min{4,5,6} === 4)
assert(max(4,5,6) === 6)
assert(min(4,5,6) === 4)
///

document { sort,
     Headline => "sort a list",
     TT "sort v", " -- produces a sorted version of the list v.",
     PARA,
     "The sort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "sort {c,e,a,f,b,f}",
	  "sort {4,2,6,3,8,2}"
	  },
     "Note: we have modified the sort order for strings so that symbols come
     before alphanumeric characters, and upper and lower case characters are
     sorted together.",
     SEEALSO { "rsort", "<=", "?" }
     }

document { rsort,
     Headline => "sort a list in reverse order",
     TT "rsort v", " -- produces a reverse sorted version of the list v.",
     PARA,
     "The rsort function uses ", TO "<=", " to compare elements of the
     list, which in turn calls upon ", TO "?", ".",
     EXAMPLE {
	  "rsort {g,d,w,s,c,a,r}",
	  "rsort {4,2,3,1}",
	  },
     "Note: we have modified the sort order for strings so that symbols come
     before alphanumeric characters, and upper and lower case characters are
     sorted together.",
     SEEALSO { "sort", "<=", "?" }
     }

document { pack,
     Headline => "pack elements of a list into shorter ones",
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
     Headline => "join lists",
     TT "join(u,v,...)", " -- joins the elements of the lists or
     sequences u, v, ... into a single list.",
     PARA,
     "The class of the result is the same as the class of the first argument.
     If there is just one argument, and it's mutable, a copy is returned.",
     EXAMPLE "join({1,2,3},{a,b,c},{7,8,9})",
     PARA,
     "The operator ", TO (symbol |, List, List), " can be used as a synonym."
     }

document { (take,BasicList,ZZ),
     Synopsis => {
	  "w = take(v,n)",
	  "v" => null,
	  "n" => null,
	  "w" => {"a list containing the first ", TT "n", " elements of 
	       the list ", TT "v", " if ", TT "n", " is positive, or
	       the last ", TT "-n", " elements if ", TT "n", " is negative."}
	  },
     EXAMPLE { "take({a,b,c,d,e,f},3)", "take({a,b,c,d,e,f},-2)" }
     }

document { (take,BasicList,List),
     Synopsis => {
	  "w = take(v,{m,n})",
	  "v" => null,
	  "{m,n}" => "a pair of natural numbers",
	  "w" => {"a list containing those elements of the list
	       ", TT "v", " in positions ", TT "m", " through ", TT "n", "." }
	  },
     EXAMPLE "take({a,b,c,d,e,f},{2,4})"
     }

document { take,
     Headline => "take some elements from a list",
     SEEALSO "drop"
     }

document { first,
     Headline => "first element of a list",
     TT "first v", " -- yields the first element of the list v.",
     PARA,
     "See also ", TO "last", "."
     }

document { last,
     Headline => "last element of a list",
     TT "last v", " -- yields the last element of the list v.",
     PARA,
     "See also ", TO "first", "."
     }

document { positions,
     Headline => "which elements of a list satisfy a condition",
     TT "positions(v,f)", " -- yields a list of integers giving the positions of the
     elements of the list ", TT "v", " which yield the value ", TT "true", " when
     the function ", TT "f", " is applied.",
     SEEALSO "position"
     }

TEST "
assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
"

document { position,
     Headline => "find first element of a list satisfying a condition",
     TT "position(v,f)", " -- returns the index of the first element of ", TT "v", " satisfying 
     the condition ", TT "f", ", or ", TO "null", " if there is none.",
     SEEALSO "positions"
     }

document { delete,
     Headline => "delete elements of a list",
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
     Headline => "ultimate value for an iteration",
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA,
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported.",
     PARA,
     "It's a bad idea to use this function, because unexpected errors will
     produce unexpected results silently."
     }

document { temporaryFileName,
     Headline => "make a temporary file name",
     Synopsis => {
	  "y = temporaryFileName()",
	  "y" => "a unique temporary file name."
	  },
     "The file name is so unique that even with various suffixes
     appended, no collision with existing files should occur.  But
     no check is done to see whether such files are present.",
     EXAMPLE {
	  ///temporaryFileName () | ".tex"///,
     	  ///temporaryFileName () | ".html"///,
	  },
     PARA,
     "This function will work under Unix, and also under Windows
     if you have a directory on the same drive called ", TT "tmp", "."
     }

document { subsets,
     Headline => "produce all the subsets",
     TT "subsets", " -- a function for computing a list of subsets
     of a set or list."
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
     Headline => "list the partitions of an integer",
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
     Headline => "list the examples in documentation",
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

document { (symbol -, List), Headline => "negation of a vector",
     Synopsis => {
	  "w = -v",
	  "v" => "a list interpreted as a vector",
	  "w" => {"the negation of ", TT "v"}}}

document { (symbol +, List, List), Headline => "sum of two vectors",
     Synopsis => {
	  "x = v+w",
	  "v" => "a list interpreted as a vector",
	  "w" => "a list interpreted as a vector",
	  "x" => "the sum of the two vectors"}}

document { (symbol -, List, List), Headline => "sum of two vectors",
     Synopsis => {
	  "x = v-w",
	  "v" => "a list interpreted as a vector",
	  "w" => "a list interpreted as a vector",
	  "x" => "the difference of the two vectors"}}

document { (symbol _, VisibleList, List),
     Headline => "get some entries of a list",
     Synopsis => {
	  "v = w_{i,j,...}",
	  "w" => "a list",
	  "{i,j,...}" => "the list of subscripts",
	  "v" => "the list of entries ", TT "{w_i, w_j, ...}", "."
	  },
     EXAMPLE {
	  "w = {a,b,c,d,e,f,g,h};",
      	  "w_{1,3,4}",
	  },
     "We can use this operation to compute composition of permutations
     represented as lists.",
     EXAMPLE "{4,2,3,1,0} _ {2,1,3,4,0}",
     "Remark: any subscripts which are sequences will have their elements
     spliced into the rest of the list.",
     EXAMPLE "{a,b,c,d,e}_{2..4}"
     }

document { number,
     Headline => "count how many elements of a list satisfy a condition",
     TT "number(x,f)", " -- the number of elements ", TT "e", " of the list ", TT "x", " 
     for which ", TT "f(e)", " is true.",
     PARA,
     SEEALSO { "positions", "select" }
     }

document { all,
     Headline => "whether all elements satisfy a condition",
     TT "all(v,f)", " -- whether each element ", TT "x", " of a list or hash table
     ", TT "v", " has ", TT "f(x)", " true.",
     PARA,
     "Returns the value true if all elements v#i of the list v yield 
     the value true when the function f is applied, otherwise returns 
     false.  For hash tables, the function is applied to all its key/value
     pairs (k,v), just as with ", TO "any", ".",
     PARA,
     SEEALSO { "scan", "apply", "select", "any", "member" }
     }

document { same,
     Headline => "whether everything in a list is the same",
     TT "same v", " -- whether every element of the list ", TT "v", " is the same.
     The comparison is done with ", TO "==", "."
     }

document { member,
     Headline => "test membership in a list",
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
     Headline => "compute the sum",
     TT "sum", " -- provides the sum of the members of a list, set, 
     or chain complex, optionally with a function applied to each one."
     }

document { (sum, List),
     Headline => "sum the elements of a list",
     TT "sum v", " -- yields the sum of the elements in the list ", TT "v", ".",
     PARA,
     EXAMPLE "sum {1,2,3,4,5}",
     SEEALSO "sum"
     }
document { (sum, List, List, Function),
     Headline => "sum results of applying a function pairwise",
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
     Headline => "sum results of applying a function",
     TT "sum(v,f)", " -- yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, ZZ, Function),
     Headline => "sum consecutive values of a function",
     TT "sum(n,f)", " -- compute the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA,
     EXAMPLE "sum(10, i -> i^2)",
     SEEALSO "sum"
     }
document { (sum, Tally),
     Headline => "sum of elements",
     TT "sum v", " -- yields the sum of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SEEALSO "sum"
     }
document { (sum, Set),
     Headline => "sum of elements",
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
     Headline => "product of elements",
     TT "product v", " -- yields the product of the elements in the list v.",
     PARA,
     EXAMPLE "product {1,2,3,4,5}"
     }
document { (product, List, List, Function),
     Headline => "product of results of applying a function pairwise",
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
     Headline => "product of values of a function",
     TT "product(v,f)", " -- yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA,
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SEEALSO "product"
     }
document { (product, ZZ, Function),
     Headline => "product of consecutive values of a function",
     TT "product(n,f)", " -- compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA,
     EXAMPLE "product(5, i -> 2*i+1)",
     SEEALSO "product"
     }
document { (product, Tally),
     Headline => "product of elements",
     TT "product v", " -- yields the product of the elements in the tally ", TT "v", ".",
     PARA,
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SEEALSO "product"
     }
document { (product, Set),
     Headline => "product of elements",
     TT "product v", " -- yields the product of the elements in the set ", TT "v", ".",
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SEEALSO "product"
     }

document { toString,
     Headline => "convert to a string",
     TT "toString x", " -- converts ", TT "x", " to a string.",
     PARA,
     "See also ", TO "toExternalString", " which will try to convert ", TT "x", "
     to a string which can be read back into the program later."     
     }

document { toExternalString,
     Headline => "convert to a readable string",
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
     Headline => "a class of lists with abbreviated constructors",
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
     Headline => "a class of lists with abbreviated constructors",
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

document { AssociativeExpression,
     Headline => "the class of associative expressions"
     }

document { Holder,
     Headline => "the class of all holder expressions",
     TT "Holder", " -- a type of ", TO "Expression", ".",
     PARA,
     "This type of expresssion is a container for a single, arbitrary, thing which
     is basic enough that the correct method for printing does not depend
     on its neighbors in the containing expression.  A negative number would
     not be basic enough for this purpose, since as a member of a sum, it would
     require special treatment."
     }

document { ZeroExpression,
     Headline => "the class of all zero expressions",
     TT "ZeroExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 0."
     }

document { OneExpression,
     Headline => "the class all one expressions",
     TT "OneExpression", " -- a type of ", TO "Expression", " of which
     there is just one instance, an expression representing the number 1."
     }

document { DoubleArrow,
     TT "DoubleArrow", " -- a type of ", TO "Expression", " which represents
     something of the form ", TT "a => b", ".",
     PARA,
     "This is experimental, and intended for internal use only."
     }

document { Expression,
     Headline => "the class of all expressions",
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
     Headline => "convert to an expression",
     TT "expression x", " -- make an ", TO "Expression", " from x."
     }

document { Divide,
     Headline => "the class of all divide expressions",
     TT "Divide", " -- a type of ", TO "Expression", " representing a quotient."
     }

document { Table,
     Headline => "the class of all table expressions",
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
     Headline => "the class of all matrix expressions",
     TT "MatrixExpression", " -- a type of ", TO "Expression", " representing
     a matrix.",
     PARA,
     EXAMPLE ///MatrixExpression {{a,b,c},{a,bb,ccc}}///,
     SEEALSO {"Table"}
     }

document { RowExpression,
     Headline => "the class of all matrix expressions",
     TT "RowExpression", " -- a type of ", TO "Expression", " representing
     a horizontal sequence of expressions."
     }

document { Minus,
     Headline => "the class of all minus expressions",
     TT "Minus", " -- a type of ", TO "Expression", " representing negation.",
     PARA,
     "This is a unary operator."
     }

document { NonAssociativeProduct,
     Headline => "the class of all nonassociative product expressions",
     TT "NonAssociativeProduct", " -- a type of ", TO "Expression", " representing
     a nonassociative product."
     }

document { Power,
     Headline => "the class of all power expressions",
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
     Headline => "the class of all product expressions",
     TT "Product", " -- a type of ", TO "Expression", " representing a product."
     }

document { SparseVectorExpression,
     Headline => "the class of all sparse vector expressions",
     TT "SparseVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse vector."
     }

document { SparseMonomialVectorExpression,
     Headline => "the class of all sparse monomial vector expressions",
     TT "SparseMonomialVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse monomial vector.",
     PARA,
     "The difference between this and ", TO "SparseVectorExpression", " is that
     the basis vectors are treated like variables for printing purposes."
     }

document { BinaryOperation,
     Headline => "the class of all binary operation expressions",
     TT "BinaryOperation", " -- a type of ", TO "Expression", " representing
     the result of a binary operation."
     }

document { Subscript,
     Headline => "the class of all subscript expressions",
     TT "Subscript", " -- a type of ", TO "Expression", " representing a
     subscripted expression."
     }

document { Adjacent,
     Headline => "the class of all adjacent expression pairs",
     TT "Adjacent", " -- a type of ", TO "Expression", " representing a pair
     of adjacent expressions, separated only by white space."
     }

document { FunctionApplication,
     Headline => "the class of all function application expressions",
     TT "FunctionApplication", " -- a type of ", TO "Expression", " representing an
     application of a function."
     }

document { Superscript,
     Headline => "the class of all superscript expressions",
     TT "Superscript", " -- a type of ", TO "Expression", " representing a
     superscripted expression."
     }

document { Equation,
     Headline => "the class of all equation expressions",
     TT "Equation", " -- a type of ", TO "Expression", " representing an
     equation."
     }

document { Sum,
     Headline => "the class of all sum expressions",
     TT "Sum", " -- a type of ", TO "Expression", " representing a sum."
     }

document { tex,
     Headline => "convert to TeX",
     TT "tex x", " -- convert ", TT "x", " to TeX format.",
     PARA,
     EXAMPLE {
	  "R = ZZ[a..f]",
      	  "tex matrix {{a^2+2,b,c},{d,e,f^3-a}}",
	  },
     SEEALSO {"TeX", "texMath"}
     }

document { texMath,
     Headline => "convert to TeX math",
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

document { showTex,
     Headline => "convert to TeX and display on screen",
     TT "showTex x", " -- convert ", TT "x", " to TeX format, and display it on the screen.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     SEEALSO "tex"
     }

document { print,
     Headline => "print something",
     TT "print x", " -- prints ", TT "x", " on the standard output followed by a 
     new line",
     PARA,
     "The return value is ", TO "null", "."
     }

document { (symbol <<, File, Thing),
     Headline => "print something to a file",
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
     Headline => "hold something in a holder expression",
     TT "hold x", " -- embeds it argument ", TT "x", " in a list of class ", TO "Holder", ".",
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
     Headline => "examine contents of an object",
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
     Headline => "examine contents of an object",
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
     Headline => "pad a string with spaces",
     TT "pad(s,n)", " -- pads the string ", TT "s", " to length ", TT "n", " with
     spaces on the right.",
     BR,
     NOINDENT, 
     TT "pad(n,s)", " -- pads the string ", TT "s", " to length ", TT "n", " with
     spaces on the left."
     }

document { columnate,
     Headline => "arrange strings in columns",
     TT "columnate(s,w)", " -- arranges the strings in the list ", TT "s", " in
     columns, returning a ", TO "Net", " suitable for output to a terminal 
     with a linewidth of ", TT "w", ".",
     PARA,
     EXAMPLE {
	  "columnate(characters ascii (65 .. 90) , 12)",
	  }
     }

document { ScriptedFunctor,
     Headline => "the class of all scripted functors",
     "A scripted functor accepts a subscript or a superscript:
     the primary example is ", TO "HH", ".",
     SEEALSO {"subscript", "superscript", "argument"}
     }

document { argument,
     Headline => "specify the function in a scripted functor for an argument",
     TT "argument", " -- a key used in scripted functors under which is
     stored the function that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { subscript,
     Headline => "specify the function in a scripted functor for a subscript",
     TT "subscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the subscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { superscript,
     Headline => "specify the function in a scripted functor for a superscript",
     TT "superscript", " -- a key used in scripted functors under which is
     stored the function of one variable that accepts the superscript and
     returns a scripted functor that accepts the arguments.",
     SEEALSO "ScriptedFunctor"
     }

document { HH,
     Headline => "general homology and cohomology functor",
     TT "HH", " is a ", TO "ScriptedFunctor", " which serves as an interface
     to the methods for ",  TO "homology", " and ", TO "cohomology", ", in
     the sense that, ", TT "HH_i(M)", " is an abbreviation for ", TT "homology(i,M)", "
     ", TT "HH^i(M)", " is an abbreviation for ", TT "cohomology(i,M)", ",
     and ", TT "HH(M)", " is an abbreviation for ", TT "homology(,M)", ".
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
     Headline => "general cohomology functor",
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
     Headline => "general homology functor",
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

document { showHtml,
     TT "showHtml x", "convert ", TT "x", " to html format, contact a
     netscape process currently running on the same host, and have it 
     display it.",
     PARA,
     "Try this example: ", TT ///showHtml documentation "loops"///, "."
     }

document { sheafExt,
     Headline => "sheaf Ext of coherent sheaves"
     }

document { (sheafExt,ZZ,CoherentSheaf,CoherentSheaf),
     Synopsis => {
	  "E = sheafExt^n(F,G)",
	  "F" => null,
	  "G" => null,
	  "E" => { "the n-th sheaf Ext of ", TT "F", " and ", TT "G", "." }
	  }
     }
