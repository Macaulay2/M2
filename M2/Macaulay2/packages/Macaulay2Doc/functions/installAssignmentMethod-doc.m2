document { 
     Key => {installAssignmentMethod,
	  (installAssignmentMethod,Symbol,HashTable,Option),
	  (installAssignmentMethod,Symbol,HashTable,HashTable,Option),
	  (installAssignmentMethod,Symbol,HashTable,Function),
	  (installAssignmentMethod,Symbol,HashTable,HashTable,Function)},
     Headline => "install methods assignment to the value of an operator",
     Usage => "installAssignmentMethod(op,X,Y,f)",
     Inputs => {
	  "op" => Keyword => "the symbol for a binary operator",
	  "X" => Type => "the type of the left-hand operand",
	  "Y" => Type => "the type of the right-hand operand",
	  "f" => Function => "the method function that will perform the assignment"
	  },
     Consequences => { { "If the operator to be used is ", TT "**", ", say, then ", TT "op", " should be ", TT "symbol **", ".
	       After installing the assignment method, assignments of the form ", TT "x**y=v", " will be
	       handled by evaluating ", TT "f(x,y,v)", "." }},
     PARA { "If the operator is a unary operator, just omit ", TT "X", " and its comma." },
     PARA { "We illustrate this function by defining a type of mutable list whose elements can be both extracted and replaced 
	  using the operator ", TT "_", ".  We also install a method for ", TO "net", " so objects of the class can be printed."
	  },
     EXAMPLE lines ///
	  M = new Type of MutableList
	  net M := m -> peek m
	  M _ ZZ := (x,i) -> x#i
	  installAssignmentMethod(symbol _, M, ZZ, (x,i,v) -> x#i = v);
	  y = new M from (a..z)
	  y_12
	  y_12 = foo
	  y
	  y_12
     ///,
     "For sample applications of this facility, see ", TO ((symbol _,symbol =),Symbol,Thing), " and 
     ", TO ((symbol _,symbol =),MutableMatrix,Sequence), ".",
     SeeAlso => {MutableList, (symbol #,BasicList,ZZ)},
     Caveat => { "The syntax involved here is not pleasant, so we may change it to something briefer, something involving yet another
	  assignment operator, distinct from ", TT "=", " and ", TT ":=", "." }
     }

document {
     Key => ((symbol _,symbol =),Symbol,Thing),
     -- this node documents the assignment method for x_a=v where x is a symbol
     Usage => "x_a = v",
     Headline => "assignment to an indexed variable",
     Inputs => { "x", "a", "v" },
     Outputs => { "v" },
     Consequences => { 
	  { "the indexed variable ", TT "x_a", " is assigned the value ", TT "v", "." }
	  },
     EXAMPLE lines ///
     	  x_a
          x_a = b
	  x_a
	  x_c
	  x_c = d
	  x_c
     ///,
     "The source code explains how it works, behind the scenes.",
     PRE ("    " | code {(symbol _,Symbol,Thing),((symbol _,symbol =),Symbol,Thing)}),
     EXAMPLE lines ///
	  peek x
     ///,
     SeeAlso => { IndexedVariable },
     }

document {
     Key => ((symbol _,symbol =),MutableMatrix,Sequence),
     Headline => "assignment to an element of a mutable matrix",
     Usage => "f_(i,j) = r",
     Inputs => { "f", Nothing => { "a pair ", TT "(i,j)", " of integers" }, "r" },
     Consequences => {
	  { "the ", TT "(i,j)", " entry of ", TT "f", " is replaced by the ring element ", TT "r" }
	  },
     EXAMPLE lines ///
          R = QQ[a..d]
	  f = mutableMatrix vars R
	  f_(0,2)
     	  f_(0,2) = 7*c^2
     	  f
     ///
     }
