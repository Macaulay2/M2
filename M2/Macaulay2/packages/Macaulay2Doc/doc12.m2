--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => (accumulate, Function, Thing, VisibleList),
     TT "accumulate(f,x0,{x1,...,xn})", " -- computes the list ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     EXAMPLE {
	  "accumulate(identity, a, {b,c,d,e})",
	  "accumulate (times, 1, 1 .. 10)"
	  }
     }

document {
     Key => (accumulate, Function, VisibleList),
     TT "accumulate(f,{x0,x1,...,xn})", " -- computes the list ", TT "{f(x0,x1),f(f(x0,x1),x2),...}", ".",
     EXAMPLE {
	  "accumulate(identity, {a,b,c,d,e})",
	  "accumulate(plus, 0 .. 10)"
	  }
     }

document {
     Key => (accumulate, VisibleList, Thing, Function),
     TT "accumulate({xn,...,x1},x0,f)", " -- computes the list ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     EXAMPLE "accumulate({a,b,c,d}, e, identity)"
     }

document {
     Key => (accumulate, VisibleList, Function),
     TT "accumulate({xn,...,x1,x0},f)", " -- computes the list ", TT "{...,f(x2,f(x1,x0)),f(x1,x0)}", ".",
     EXAMPLE "accumulate({a,b,c,d,e}, identity)"
     }

document {
     Key => accumulate,
     Headline => "apply binary operator repeatedly",
     SeeAlso => {"fold"}
     }

TEST ///
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
///     

document {
     Key => (fold, Function, Thing, VisibleList),
     TT "fold(f,x0,{x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     EXAMPLE "fold(identity, a, {b,c,d,e})"
     }
document {
     Key => (fold, Function, VisibleList),
     TT "fold(f,{x0,x1,...,xn})", " -- computes ", TT "f(...f(f(x0,x1),x2)...)}", ".",
     EXAMPLE {
	  "fold(identity, {a,b,c,d,e})",
	  "fold(plus, {1,2,3,4,5})"
	  }
     }
document {
     Key => (fold, VisibleList, Thing, Function),
     TT "fold({xn,...,x1},x0,f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE "fold({a,b,c,d}, e, identity)"
     }
document {
     Key => (fold, VisibleList, Function),
     TT "fold({xn,...,x1,x0},f)", " -- computes ", TT "f(...f(x2,f(x1,x0))...)}", ".",
     EXAMPLE "fold({a,b,c,d,e}, identity)"
     }
document {
     Key => fold,
     Headline => "apply binary operator repeatedly",
     SeeAlso => {"accumulate"}
     }

TEST ///
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )
///

document {
     Key => demark,
     Headline => "insert a string between elements of a list of strings",
     TT "demark(s,x)", " -- given a list of strings ", TT "x", " and
     a string ", TT "s", " provides the string obtained by concatenating
     the elements of ", TT "x", " with a copy of ", TT "x", " inserted
     between each successive pair.",
     PARA{},
     EXAMPLE "demark(\"+\",{\"a\",\"b\",\"c\"})"
     }

document {
     Key => InfiniteNumber,
     Headline => "the class of all infinite numbers"
     }

document {
     Key => infinity,
     Headline => "infinity"
     }

document {
     Key => IndeterminateNumber,
     Headline => "the class of all indeterminate numbers",
     "Indeterminate numbers result, for exmaple, from multiplying 0 by infinity.
     There is only one instance of this class."
     }

document {
     Key => indeterminate,
     Headline => "an indeterminate number",
     TT "indeterminate", " -- a representation of an indeterminate number,
     such as might result from multiplying 0 by infinity.",
     }

document {
     Key => {max,(max, VisibleList)},
     Headline => "maximum of elements of a list",
     TT "max x", " -- yields the maximum of the elements in the list or sequence x."
     }

document {
     Key => {min,(min, VisibleList)},
     Headline => "minimum of elements of a list",
     TT "min x", " -- yields the minimum of the elements in the list or sequence x."
     }

TEST ///
assert(max{4,5,6} === 6)
assert(min{4,5,6} === 4)
assert(max(4,5,6) === 6)
assert(min(4,5,6) === 4)
///

document {
     Key => {pack,(pack, BasicList, ZZ),(pack, ZZ, BasicList)},
     Headline => "pack elements of a list into shorter ones",
     TT "pack(n,v)", " -- packs the elements of the list or sequence
     ", TT "v", " into a table ", TT "n", " at a time.",
     PARA{},
     "It produces, from a list ", TT "v", ", a list of lists formed 
     by packing the elements of ", TT "v", " into lists ", TT "n", " 
     at a time.  The last of the lists produced may have fewer 
     than ", TT "n", " elements.",
     EXAMPLE "pack(3,{a,b,c,d,e,f,g,h,i,j,k})",
     }

document {
     Key => join,
     Headline => "join lists",
     TT "join(u,v,...)", " -- joins the elements of the lists or
     sequences u, v, ... into a single list.",
     PARA{},
     "The class of the result is the same as the class of the first argument.
     If there is just one argument, and it's mutable, a copy is returned.",
     EXAMPLE "join({1,2,3},{a,b,c},{7,8,9})",
     PARA{},
     "The operator ", TO (symbol |, List, List), " can be used as a synonym."
     }

document {
     Key => (take,BasicList,ZZ),
     Usage => "take(v,n)",
     Inputs => { "v", "n" },
     Outputs => {
	  {"a list containing the first ", TT "n", " elements of the list ", TT "v", " if ", TT "n", " is positive, or
	       the last ", TT "-n", " elements if ", TT "n", " is negative."}
	  },
     EXAMPLE { "take({a,b,c,d,e,f},3)", "take({a,b,c,d,e,f},-2)" }
     }

document {
     Key => (take,BasicList,List),
     Usage => "take(v,{m,n})",
     Inputs => { "v", Nothing => { TT "{m,n}", ", a pair of natural numbers" } },
     Outputs => { {"a list containing those elements of the list ", TT "v", " in positions ", TT "m", " through ", TT "n" } },
     EXAMPLE "take({a,b,c,d,e,f},{2,4})"
     }

document {
     Key => take,
     Headline => "take some elements from a list",
     SeeAlso => "drop"
     }

document {
     Key => first,
     Headline => "first element of a list",
     TT "first v", " -- yields the first element of the list v.",
     PARA{},
     "See also ", TO "last", "."
     }

document {
     Key => last,
     Headline => "last element of a list",
     TT "last v", " -- yields the last element of the list v.",
     PARA{},
     "See also ", TO "first", "."
     }

document {
     Key => {(positions, VisibleList, Function),positions},
     Headline => "which elements of a list satisfy a condition",
     Usage => "positions(v,f)",
     Inputs => { "v", "f" => { "which, when applied to an element of ", TT "v", ", yields ", TO "true", " or ", TO "false", "" }},
     Outputs => {
	  List => { "the list of integers ", TT "k", ", in ascending order, such that ", TT "f v#k", " evaluates to ", TO "true" }
	  },
     EXAMPLE lines ///
     positions(100..110,isPrime)
     ///,
     SeeAlso => {position,select}
     }

TEST "
assert( 3 === position({a,b,c,d,e,f},i->i===d ) )
"

document {
     Key => [position, Reverse],
     Headline => "search the list in reverse order",
     Usage => "position(...,Reverse=>true)",
     Consequences => {"the search is conducted in the reverse direction, starting at the end"}
     }

document {
     Key => {position,(position, VisibleList, Function)},
     Headline => "find first element of a list satisfying a condition",
     TT "position(v,f)", " -- returns the index of the first element of ", TT "v", " satisfying 
     the condition ", TT "f", ", or ", TO "null", " if there is none.",
     SeeAlso => "positions"
     }

document { Key => (position, VisibleList, VisibleList, Function),
     Usage => "position(v,w,f)",
     Inputs => {"v","w","f"},
     Outputs => {{"the smallest index ", TT "i", " such that ", TT "f(v#i,w#i)", " is true"}},
     EXAMPLE lines ///
     	  position((0,1,2,3,4), (0,0,0,9,9), (i,j)->i<j)
     ///}

document {
     Key => delete,
     Headline => "delete elements of a list",
     TT "delete(x,v)", " -- removes any occurrences of the expression ", TT "x", "
     from the list ", TT "v", ".",
     PARA{},
     "Equality is determined with ", TO "===", ", which is quick.",
     EXAMPLE {
	  "delete(c,{a,b,c,d,e,a,b,c,d,e})",
	  },
     SeeAlso => "member"
     }

TEST "
stream = (action,state) -> () -> stream(action, action state)
fib = stream( (i,j) -> (j,i+j), (0,1))
scan(1 .. 22, i -> fib = fib())
"

document {
     Key => {ultimate,(ultimate, Function, Thing)},
     Headline => "ultimate value for an iteration",
     TT "ultimate(f,x)", " -- yields the value ultimately obtained by
     applying the function ", TT "f", " to ", TT "x", ".",
     PARA{},
     "Iteration ceases when an error occurs during application of the
     function, or the result is the same.  Errors are not reported.",
     PARA{},
     "It's a bad idea to use this function, because unexpected errors will
     produce unexpected results silently."
     }

document {
     Key => subsets,
     Headline => "produce all the subsets",
     TT "subsets", " -- a function for computing a list of subsets
     of a set or list."
     }

document {
     Key => (subsets,ZZ,ZZ),
     Usage => "subsets(n,j)",
     Inputs => { "n", "j" },
     Outputs => { {"a list of the subsets of ", TT "{0, ..., n-1}", " that have ", TT "j", " elements; each subset is provided as a list"} },
     EXAMPLE "subsets(7,3)",
     "If each of the sets is reversed, then the resulting list is in lexicographic order.",
     EXAMPLE lines ///
     	  x = reverse \ subsets(6,3)
	  sort x === x
     ///
     }

document {
     Key => (subsets,Set,ZZ),
     TT "subsets(s,j)", " -- yields a list of those subsets of the set ", TT "s", " that have ", TT "j", " elements.",
     EXAMPLE "subsets(set {a,b,c},2)"
     }

document {
     Key => {(subsets,List,ZZ),(subsets, Sequence, ZZ)},
     TT "subsets(s,j)", " -- yields a list of those subsets of the list ", TT "s", " that have ", TT "j", " elements.",
     EXAMPLE "subsets({a,b,c},2)"
     }

document {
     Key => (subsets,ZZ),
     Usage => "subsets n",
     Inputs => { "n" },
     Outputs => {{"a list of the subsets of the {0,1,...,n-1}"}},
     EXAMPLE lines ///
     	  subsets 4
     ///
     }

document {
     Key => {(subsets,List)},
     TT "subsets s", " -- yields a list of the subsets of the list ", TT "s", ".",
     PARA{},
     "The subsets are returned as lists whose elements are in the same order.",
     EXAMPLE "subsets {1,2,3}"
     }

document {
     Key => (subsets,Set),
     TT "subsets s", " yields a list of the subsets of the set ", TT "s", ".",
     PARA{},
     EXAMPLE "subsets set {a,b,c}"
     }

TEST "
assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
assert( 
     set subsets(set {a,b,c,d},2) === 
     set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )
assert( partitions 4 === {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} / (x -> new Partition from x) )
assert( partitions(5,3) === {{3,2},{3,1,1},{2,2,1},{2,1,1,1},{1,1,1,1,1}} / (x -> new Partition from x) )
"

document {
     Key => (symbol -, List),
     Headline => "negation of a vector",
     Usage => "-v",
     Inputs => { "v" => "a list interpreted as a vector" },
     Outputs => {{"the negation of ", TT "v"}},
     EXAMPLE "- {1,5,6}"
     }

document {
     Key => (symbol +, List, List),
     Headline => "sum of two vectors",
     Usage => "v+w",
     Inputs => { "v" => "a list interpreted as a vector", "w" => "a list interpreted as a vector" },
     Outputs => {"the sum of the two vectors"},
     EXAMPLE "{1,2,3} + {1,5,6}"
     }

document {
     Key => (symbol -, List, List),
     Headline => "difference of two vectors",
     Usage => "v-w",
     Inputs => { "v" => "a list interpreted as a vector", "w" => "a list interpreted as a vector" },
     Outputs => {"the difference of the two vectors"},
     EXAMPLE "{1,2,3} - {1,5,6}"
     }

document {
     Key => number,
     Headline => "count how many elements of a list satisfy a condition",
     TT "number(x,f)", " the number of elements ", TT "e", " of the list ", TT "x", " 
     for which ", TT "f(e)", " is true.",
     PARA{},
     SeeAlso => { "positions", "select" }
     }

document {
     Key => same,
     Headline => "whether everything in a list is the same",
     TT "same v", " whether every element of the list ", TT "v", " is the same.
     The comparison is done with ", TO "==", "."
     }

document {
     Key => sum,
     Headline => "compute the sum",
     TT "sum", " provides the sum of the members of a list, set, 
     or chain complex, optionally with a function applied to each one."
     }

document {
     Key => (sum, List),
     Headline => "sum the elements of a list",
     TT "sum v", " yields the sum of the elements in the list ", TT "v", ".",
     PARA{},
     EXAMPLE "sum {1,2,3,4,5}",
     PARA {
	  "The sum of an empty list is the integer 0."
	  },
     EXAMPLE lines ///
     sum {}
     class oo
     ///,
     PARA {
	  "When summing a possibly empty list of elements from a ring, one may
	  use ", TO "promote", " to ensure the result is always in the same ring."
	  },
     EXAMPLE lines ///
     R = QQ[x_1 .. x_10];
     f = n -> sum for i from 1 to n list x_i;
     f 4
     f 0
     class oo
     g = n -> promote(sum for i from 1 to n list x_i, R);
     g 10
     g 0
     class oo
     ///,     
     SeeAlso => "sum"
     }
document {
     Key => (sum, VisibleList, VisibleList, Function),
     Headline => "sum results of applying a function pairwise",
     TT "sum(v,w,f)", " yields the sum of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists or sequences ", TT "v", " and ", TT "w", ", which should be of 
     the same length.",
     PARA{},
     EXAMPLE {
	  "R = ZZ[x,y,z];",
      	  "sum({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SeeAlso => "sum"
     }
document {
     Key => (sum, VisibleList, Function),
     Headline => "sum results of applying a function",
     TT "sum(v,f)", " yields the sum of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA{},
     EXAMPLE "sum(1 .. 10, i -> i^2)",
     SeeAlso => "sum"
     }
document {
     Key => (sum, ZZ, Function),
     Headline => "sum consecutive values of a function",
     TT "sum(n,f)", " computes the sum ", TT "f(0) + f(1) + ... + f(n-1)", ".",
     PARA{},
     EXAMPLE "sum(10, i -> i^2)",
     SeeAlso => {"product", "plus", "times"}
     }
document {
     Key => (sum, VirtualTally),
     Headline => "sum of elements",
     TT "sum v", " yields the sum of the elements in the tally ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = tally{1,1,1,1,1,10,10,10,100,100}",
      	  "sum a",
	  },
     SeeAlso => "product"
     }
document {
     Key => (sum, Set),
     Headline => "sum of elements",
     TT "sum v", " yields the sum of the elements in the set ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = set{1,100,10000}",
      	  "sum a",
	  },
     SeeAlso => "sum"
     }

document {
     Key => product,
     TT "product", " provides the product of the members of a list or set,
     optionally with a function applied to each one."
     }
document {
     Key => (product, List),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the list v.",
     PARA{},
     EXAMPLE "product {1,2,3,4,5}"
     }
document {
     Key => (product, VisibleList, VisibleList, Function),
     Headline => "product of results of applying a function pairwise",
     TT "product(v,w,f)", " yields the product of the results obtained by
     applying ", TT "f", " to each of the pairs ", TT "(i,j)", " of elements from 
     the lists ", TT "v", " and ", TT "w", ", which should be of the same length.",
     PARA{},
     EXAMPLE {
	  "M = monoid [x,y,z];",
      	  "product({2,3,4},{x,y,z},(i,j)->j^i)",
	  },
     SeeAlso => "product"
     }
document {
     Key => (product, VisibleList, Function),
     Headline => "product of values of a function",
     TT "product(v,f)", " yields the product of the expressions obtained by
     applying ", TT "f", " to each of the elements of the list or sequence ", TT "v", ".",
     PARA{},
     EXAMPLE "product(1 .. 5, i -> i^2)",
     SeeAlso => "product"
     }
document {
     Key => (product, ZZ, Function),
     Headline => "product of consecutive values of a function",
     TT "product(n,f)", " compute the product ", TT "f(0) * f(1) * ... * f(n-1)", ".",
     PARA{},
     EXAMPLE "product(5, i -> 2*i+1)",
     SeeAlso => "product"
     }
document {
     Key => (product, VirtualTally),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the tally ", TT "v", ".",
     PARA{},
     EXAMPLE {
	  "a = tally{2,2,2,2,2,3,3,3,5,5}",
      	  "product a",
	  },
     SeeAlso => "product"
     }
document {
     Key => (product, Set),
     Headline => "product of elements",
     TT "product v", " yields the product of the elements in the set ", TT "v", ".",
     EXAMPLE {
	  "a = set select(1..50, isPrime)",
      	  "product a",
	  },
     SeeAlso => "product"
     }

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
     Headline => "the class of associative expressions"
     }

document {
     Key => Holder,
     Headline => "the class of all holder expressions",
     PARA{},
     "This type of expresssion is a container for a single, arbitrary, thing that
     is basic enough that the correct method for printing does not depend
     on its neighbors in the containing expression.  A negative number would
     not be basic enough for this purpose, since as a member of a sum, it would
     require special treatment."
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

undocumented {
     (symbol SPACE,Expression,Thing), (symbol SPACE,Thing,Expression),
     (symbol SPACE,Holder,Holder),
     (symbol *,Expression,Thing), (symbol *,Thing,Expression),
     (symbol **,Expression,Thing), (symbol **,Thing,Expression),
     (symbol +,Expression,Thing), (symbol +,Thing,Expression),
     (symbol -,Expression,Thing), (symbol -,Thing,Expression),
     (symbol /,Expression,Thing), (symbol /,Thing,Expression),
     (symbol ==,Expression,Thing), (symbol ==,Thing,Expression),
     (symbol ^,Thing,Expression),
     (symbol _,Expression,Thing), 
     (symbol _,Thing,Expression),
     (symbol :,Expression,Expression),
     (symbol :,Expression,Holder),
     (symbol :,Expression,Thing),
     (symbol :,Holder,Expression),
     (symbol :,Holder,Holder),
     (symbol :,Thing,Expression),
     (symbol ..,Expression,Expression),
     (symbol ..,Expression,Holder),
     (symbol ..,Expression,Thing),
     (symbol ..,Holder,Expression),
     (symbol ..,Holder,Holder),
     (symbol ..,Thing,Expression)
     }

undocumented {(value, RingElement),(value, Nothing), (value, IndexedVariableTable)}

document {
     Key => {Expression,
	  (symbol SPACE,Expression,Expression),
	  (symbol *,Expression,Expression),
	  (symbol **,Expression,Expression),
	  (symbol +,Expression,Expression),
	  (symbol -,Expression),
	  (symbol -,Expression,Expression),
	  (symbol /,Expression,Expression),
	  (symbol ==,Expression,Expression),
	  (symbol ^,Expression,Expression),
	  (symbol _,Expression,Expression),
          (value,Expression)
	  },
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
     the non-expression will be converted to an expression and the operator will be applied.  Only the most trivial algebraic simplications are applied.",
     EXAMPLE lines ///
     	  d + e
	  d + 4
	  d / 4
	  d / 1
	  d == e
     ///
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
     EXAMPLE ///MatrixExpression {{a,b,c},{a,bb,ccc}}///,
     SeeAlso => {"Table"}
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

document {
     Key => NonAssociativeProduct,
     Headline => "the class of all nonassociative product expressions",
     TT "NonAssociativeProduct", " is a type of ", TO "Expression", " representing
     a nonassociative product."
     }

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
     Key => print,
     Headline => "print something",
	Usage => "print x",
     TT "print x", " prints ", TT "x", " on the standard output followed by a 
     new line.",
	EXAMPLE {
		///print "Hello world!"///
		},
     "The return value is ", TO "null", "."
     }

document {
     Key => hold,
     Headline => "hold something in a holder expression",
     Usage => "hold x",
     Inputs => { "x" },
     Outputs => { Expression => { " of class ", TO "Holder", " containing ", TT "x", " as its single element, unless ", TT "x", " is 
	       already an expression, in which case ", TT "x", " is returned." }},
     EXAMPLE "(hold 2)^5 * (hold 3)^3",
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
     Key => {pad,(pad, String, ZZ),(pad, ZZ, String)},
     Headline => "pad a string with spaces",
     TT "pad(s,n)", " -- pads the string ", TT "s", " to length ", TT "n", " with
     spaces on the right.",
     BR{},
 
     TT "pad(n,s)", " -- pads the string ", TT "s", " to length ", TT "n", " with
     spaces on the left."
     }

document {
     Key => columnate,
     Headline => "arrange strings in columns",
     TT "columnate(w,s)", " -- arranges the strings in the list ", TT "s", " in
     columns, returning a ", TO "Net", " suitable for output to a terminal 
     with a linewidth of ", TT "w", ".",
     PARA{},
     EXAMPLE {
	  "columnate(12, characters ascii (65 .. 90))",
	  }
     }

document {
     Key => {ScriptedFunctor,
	  (symbol^, ScriptedFunctor, Thing),
	  (symbol_, ScriptedFunctor, Thing),
	  (symbol SPACE, ScriptedFunctor, Thing)},
     Headline => "the class of all scripted functors",
     "A scripted functor accepts a subscript or a superscript:
     the primary example is ", TO "HH", ".",
     SeeAlso => {"subscript", "superscript", "argument"}
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

document {
     Key => {(sheafExt,ZZ,CoherentSheaf,CoherentSheaf),
	  sheafExt,
       	  (sheafExt, ZZ, SheafOfRings, CoherentSheaf),
       	  (sheafExt, ZZ, CoherentSheaf, SheafOfRings),
	  (sheafExt, ZZ, SheafOfRings, SheafOfRings)},
     Headline => "sheaf Ext of coherent sheaves",
     Usage => "sheafExt^n(F,G)",
     Inputs => { "n", "F", "G" },
     Outputs => { CoherentSheaf => { "the n-th sheaf Ext of ", TT "F", " and ", TT "G" } },
     "If ", TT "F", " or ", TT "G", " is a sheaf of rings, it is regarded as a sheaf of modules in the evident way.",
     PARA{},
     TT "F", " and ", TT "G", " must be coherent sheaves on the same projective variety or scheme ", TT "X", ".",
     PARA{},
     "The result is the sheaf associated to the graded module ", TT "Ext^n(module M, module N).",
     EXAMPLE lines ///
     	  X = Proj(QQ[x,y])
	  sheafExt^1(OO_X^1(2),OO_X(-11)^1)
     ///,
     SeeAlso => {OO, sheafHom, Hom, Ext, HH, (Ext, ZZ, CoherentSheaf, CoherentSheaf)}
     
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
