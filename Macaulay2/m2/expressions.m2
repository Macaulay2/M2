--		Copyright 1994 by Daniel R. Grayson

precedence = method(SingleArgumentDispatch=>true)

-- local variables
local PowerPrecedence
expandFunction := quote expandFunction
EmptyName := quote EmptyName
unit := quote unit
operator := quote operator
letters := set characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
digits := set characters "0123456789"
endsWithIdentifier := s -> (
     c := "";
     n := # s - 1;
     while ( 
	  c = s#n;
	  n > 0 and digits#?c
	  ) do n = n - 1;
     letters#?c)

Expression = new Type of BasicList
expression Expression := identity
Expression#operator = ""
Expression#expandFunction = first

AssociativeExpression = new Type of Expression
--new AssociativeExpression from Sequence := 
--new AssociativeExpression from List := (type,v) -> (
--     elements splice apply(v, 
--	  term -> if class term === type then unlist term else term
--	  )
--     )

document { quote AssociativeExpression,
     TT "AssociativeExpression", " -- a type of ", TO "Expression", ".",
     PARA,
     "Types of associative expression:",
     MENU {
	  TO "Equation",
	  TO "Product",
	  TO "Sum"
	  },
     SEEALSO "Expression"
     }

lookupi := x -> (
     r := lookup x;
     if r === null then error "encountered null or missing value";
     r)

name Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(v,term -> (
	       if precedence term <= p
	       then "(" | name term | ")"
	       else name term
	       )
	  );
     if # v === 0 then op#EmptyName
     else concatenate between(op#operator,names)
     )

Held = new Type of Expression
document { quote Held,
     TT "Held", " -- a type of ", TO "Expression", ".",
     PARA,
     "A held expression is a container for a single, arbitrary, thing which
     is basic enough that the correct method for printing it is always to
     apply ", TO "name", " rather than ", TO "net", ".  Such basic things
     include positive numbers, functions, boolean values, symbols, ...,
     but not negative numbers or rational numbers, for they require special
     treatment when printed which depends on their neighbors in the containing
     expression, and on whether two-dimensional printing is being done."
     }

Held#expandFunction = first
tex Held := v -> "{" | name v#0 | "}"

HeldString = new Type of Expression
HeldString#expandFunction = first
net HeldString := v -> v#0
name HeldString := v -> v#0
new HeldString from String := (HeldString,s) -> new HeldString from {s}
document { quote HeldString,
     TT "HeldString", " -- a type of ", TO "Expression", ".",
     PARA,
     "Used for holding a string in an expression so that it will print
     without quotation marks."
     }

remove(Sequence,expression)

Minus = new Type of Expression		  -- unary minus
Minus#operator = "-"
Minus#expandFunction = minus
name Minus := v -> (
     term := v#0;
     if precedence term <= precedence v
     then "-(" | name term | ")"
     else "-" | name term
     )

Equation = new Type of AssociativeExpression
Equation#operator = "=="
Equation#expandFunction = (v) -> (
     if # v === 2
     then v#0 == v#1
     else if # v <= 1
     then true
     else (
     	  x := v#0;
     	  w := drop(v,1);
     	  all(w,y->x==y)
     	  )
     )
net Equation := v -> (
     n := # v;
     if n === 0 then "Equation{}"
     else if n === 1 then "Equation{" | net v#0 | "}"
     else (
	  p := precedence v;
	  horizontalJoin elements between(" == ", 
	       apply(v, e -> if precedence e <= p then "(" | net e | ")" else net e))))
name Equation := v -> (
     n := # v;
     if n === 0 then "Equation{}"
     else if n === 1 then "Equation{" | name v#0 | "}"
     else (
	  p := precedence v;
	  concatenate between(" == ", 
	       apply(v, e -> if precedence e <= p then ("(", name e, ")") else name e))))

ZeroExpression := new Type of Expression
ZeroExpression.name = quote ZeroExpression
ZERO := new ZeroExpression
name ZeroExpression := net ZeroExpression := x -> "0"

OneExpression := new Type of Expression
OneExpression.name = quote OneExpression
ONE := new OneExpression
name OneExpression := net OneExpression := x -> "1"

Sum = new Type of AssociativeExpression
Sum#unit = ZERO
Sum#EmptyName = "0"
Sum#operator = "+"
Sum#expandFunction = plus

name Sum := v -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"+"));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus 
		    then ( seps#i = "-"; v#i#0 )
		    else v#i ));
	  names := apply(n, i -> (
		    if precedence v#i <= p 
		    then "(" | name v#i | ")"
		    else name v#i ));
	  concatenate mingle ( seps, names )))

Product = new Type of AssociativeExpression
Product#unit = ONE
Product#EmptyName = "1"
Product#operator = "*"
Product#expandFunction = times

name Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"*"));
	  seps#0 = seps#n = "";
     	  names := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then (
--			 seps#i = seps#(i+1) = "";
			 "(" | name term | ")"
			 )
	       	    else name term
	       	    )
	       );
--	  scan(# v - 1,
--	       i -> (
--		    if seps#(i+1)!=""
--		    and names#(i+1)#?0
--		    and letters#?(names#(i+1)#0)
--		    and not endsWithIdentifier names#i 
--		    then seps#(i+1)=""
--		    )
--	       );
	  concatenate mingle ( seps, names )
	  )
     )

NonAssociativeProduct = new Type of Expression
NonAssociativeProduct#unit = ONE
NonAssociativeProduct#EmptyName = "1"
NonAssociativeProduct#operator = "**"
NonAssociativeProduct#expandFunction = times

name NonAssociativeProduct := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"**"));
	  seps#0 = seps#n = "";
     	  names := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then "(" | name term | ")"
	       	    else name term
	       	    )
	       );
	  scan(# v - 1,
	       i -> (
		    if seps#(i+1)!=""
		    and names#(i+1)#?0
		    and letters#?(names#(i+1)#0)
		    and not endsWithIdentifier names#i 
		    then seps#(i+1)=""
		    )
	       );
	  concatenate mingle ( seps, names )
	  )
     )

Divide = new Type of Expression
Divide#operator = "/"
Divide#expandFunction = (x,y) -> x/y
numerator Divide := x -> x#0
denominator Divide := x -> x#1

Power = new Type of Expression
Power#operator = "^"
Power#expandFunction = (x,y) -> x^y

Subscript = new Type of Expression
Subscript#operator = "_"
Subscript#expandFunction = (x,n) -> x_n

Superscript = new Type of Expression
Superscript#operator = "^"
Superscript#expandFunction = (x,n) -> x_n

name Power := name Subscript := name Superscript := v -> (
     x := v#0;
     y := v#1;
     if y === 1 then name x else (
	  x = name x;
	  y = name y;
	  if precedence v#0 <  PowerPrecedence then x = "(" | x | ")";
	  if precedence v#1 <= PowerPrecedence then y = "(" | y | ")";
	  concatenate(x,(class v)#operator,y)))

-----------------------------------------------------------------------------
Equation == Equation        := join
Equation == Expression      := append
Expression == Equation      := prepend
Expression == Expression    := (x,y) -> new Equation from {x,y}
Expression == Thing         := (x,y) -> x == expression y
Thing == Expression         := (x,y) -> expression x == y
ZeroExpression + Expression := (x,y) -> y
Expression + ZeroExpression := (x,y) -> x
Sum + Sum                   := join
Sum + Expression            := append
Expression + Sum            := prepend
Expression + Expression := (x,y) -> new Sum from {x,y}
Expression + Thing      := (x,y) -> x + expression y
     Thing + Expression := (x,y) -> expression x + y
       - ZeroExpression := identity
	   - Minus      := x -> x#0
           - Expression := x -> new Minus from {x}
Expression - Expression := (x,y) -> x + -y
Expression - Thing      := (x,y) -> x - expression y
     Thing - Expression := (x,y) -> expression x - y
Expression * OneExpression := (x,y) -> x
OneExpression * Expression := (x,y) -> y
Expression * ZeroExpression := (x,y) -> y
ZeroExpression * Expression := (x,y) -> x
Product * Product       := join
Product * Expression    := append
Expression * Product    := prepend
Expression * Expression := (x,y) -> new Product from {x,y}
Expression * Minus := (x,y) -> -(x * y#0)
Minus * Expression := (x,y) -> -(x#0 * y)
Minus * Minus := (x,y) -> x#0 * y#0
Expression * Thing      := (x,y) -> x * (expression y)
     Thing * Expression := (x,y) -> (expression x) * y
Expression ** OneExpression := (x,y) -> x
OneExpression ** Expression := (x,y) -> y
NonAssociativeProduct ** NonAssociativeProduct := join
NonAssociativeProduct ** Expression := append
Expression ** NonAssociativeProduct := prepend
Expression ** Expression := (x,y) -> new NonAssociativeProduct from {x,y}
Expression ** Thing      := (x,y) -> x ** (expression y)
     Thing ** Expression := (x,y) -> (expression x) ** y
Expression / OneExpression := (x,y) -> x
Expression / Expression := (x,y) -> new Divide from {x,y}
Expression / Thing      := (x,y) -> x / (expression y)
     Thing / Expression := (x,y) -> (expression x) / y
expression ZZ := i -> (
     if i === 0 then ZERO
     else if i === 1 then ONE
     else if i === -1 then new Minus from { ONE }
     else if i < 0 then new Minus from { new Held from {-i} }
     else new Held from {i}
     )
Expression ^ OneExpression := (x,y) -> x
Expression ^ ZeroExpression := (x,y) -> ONE
ZeroExpression ^ Expression := (x,y) -> ZERO
ZeroExpression ^ ZeroExpression := (x,y) -> ONE
Expression ^ Expression := (x,y) -> new Power from {x,y}
Expression ^ Thing      := (x,y) -> x ^ (expression y)
     Thing ^ Expression := (x,y) -> (expression x) ^ y

Thing#expandFunction = identity

document { quote expand,
     TT "expand x", " -- expand the ", TO "Expression", " x, hopefully recovering
     something semantically equivalent.",
     PARA,
     EXAMPLE "p = (expression 2)^5 * (expression 3)^3",
     EXAMPLE "expand p"
     } 

expand Expression := v -> (class v)#expandFunction apply(unlist v,expand)
expand Thing := identity

-----------------------------------------------------------------------------
SparseVectorExpression = new Type of Expression
SparseVectorExpression#expandFunction = x -> notImplemented()
name SparseVectorExpression := v -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=name x);
     concatenate("{",between(",",w),"}")
     )
-----------------------------------------------------------------------------
SparseMonomialVectorExpression = new Type of Expression
-- in these, the basis vectors are treated as variables for printing purposes
SparseMonomialVectorExpression#expandFunction = x -> notImplemented()
name SparseMonomialVectorExpression := v -> name (
     sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  new HeldString from concatenate("<",name i,">"))
     )
-----------------------------------------------------------------------------
MatrixExpression = new Type of Expression
MatrixExpression#expandFunction = x -> matrix applyTable(elements x,expand)
name MatrixExpression := m -> concatenate(
     "{",
     between(",",apply(m,row->("{", between(",",apply(row,name)), "}"))),
     "}" )
-----------------------------------------------------------------------------
X := new Type of HashTable
X X := identity
SPACE := first first keys X

binary := new HashTable from {
     quote * => ((x,y) -> x*y),		  -- 
     quote + => ((x,y) -> x+y),		  -- 
     quote - => ((x,y) -> x-y),		  -- 
     quote / => ((x,y) -> x/y),		  -- 
     quote // => ((x,y) -> x//y),	  -- 
     quote ^ => ((x,y) -> x^y),		  -- 
     quote == => ((x,y) -> x==y),	  -- 
     quote ~ => ((x,y) -> x~y),		  -- 
     quote .. => ((x,y) -> x..y),	  -- 
     quote % => ((x,y) -> x%y),
     quote @ => ((x,y) -> x@y),
     quote \ => ((x,y) -> x\y),
     quote @@ => ((x,y) -> x@@y),
     quote & => ((x,y) -> x&y),
     quote ? => ((x,y) -> x?y),
     quote | => ((x,y) -> x|y),
     quote => => ((x,y) -> x=>y),
     quote || => ((x,y) -> x||y),
     quote << => ((x,y) -> x<<y),
     quote >> => ((x,y) -> x>>y),
     quote : => ((x,y) -> x:y),
     -- quote :: => ((x,y) -> x::y),
     quote ++ => ((x,y) -> x++y),
     quote ** => ((x,y) -> x**y),
     quote /^ => ((x,y) -> x/^y),
     quote _ => ((x,y) -> x_y),
     SPACE => ((x,y) -> x y)
     }
BinaryOperation = new Type of Expression -- {op,left,right}
BinaryOperation#expandFunction = m -> (
     if binary#?(m#0) then binary#(m#0) (m#1,m#2) else m
     )
net BinaryOperation := m -> (
     -- must put precedences here eventually
     horizontalJoin( "(", net m#1, string m#0, net m#2, ")" )
     )
name BinaryOperation := m -> (
     horizontalJoin( "(", name m#1, string m#0, name m#2, ")" )
     )
-----------------------------------------------------------------------------
FunctionApplication = new Type of Expression -- {fun,args}
FunctionApplication#expandFunction = m -> (expand m#0) (expand m#1)
name FunctionApplication := m -> (
     fun := m#0;
     args := m#1;
     if class args === Sequence
     then concatenate(name fun, name args)
     else if precedence args >= precedence m
     then concatenate(name fun, " ", name args)
     else concatenate(name fun, "(", name args, ")"))
net FunctionApplication := m -> (
     fun := m#0;
     args := m#1;
     if precedence args > precedence m
     then horizontalJoin (net fun, " ", net args)
     else horizontalJoin (net fun, "(", net args, ")"))
-----------------------------------------------------------------------------
     	       	      precedence Sequence := x -> (
			   if #x === 0 then 70
			   else if #x === 1 then 40
			   else 5
			   )
-----------------------------------------------------------------------------
     	       	      precedence Equation := x -> 10
-----------------------------------------------------------------------------
     	       	     precedence HashTable := x -> (
			  if x.?name then precedence x.name else 20
			  )
			 precedence Thing := x -> 20
			       	    	      	  MinusPrecedence := 20
			 precedence Minus := x -> 20
			   precedence Sum := x -> 20
-----------------------------------------------------------------------------
		       precedence Product := x -> 30
	 precedence NonAssociativeProduct := x -> 30
-----------------------------------------------------------------------------
	   precedence FunctionApplication := x -> 40
-----------------------------------------------------------------------------
			precedence Divide := x -> 50
-----------------------------------------------------------------------------
		     precedence Subscript := x -> 60
		   precedence Superscript := x -> 60
		                		  PowerPrecedence = 60
			 precedence Power := x -> (
			      if x#1 === 1 then precedence x#0 else 60
			      )
-----------------------------------------------------------------------------
			     precedence ZZ := x -> (
				  if x>=0 
				  then 70
				  else MinusPrecedence)
			  precedence RR := x -> 70
		      precedence Function := x -> 70
		          precedence List := x -> 70
			precedence Symbol := x -> 70
			precedence String := x -> 70
         precedence AssociativeExpression := x -> 70
precedence SparseMonomialVectorExpression := x -> 70
	precedence SparseVectorExpression := x -> 70
	        precedence ZeroExpression := x -> 70
	         precedence OneExpression := x -> 70
		     precedence Subscript := x -> 70
		   precedence Superscript := x -> 70
	      precedence MatrixExpression := x -> 70
		    precedence HeldString := x -> 70
-----------------------------------------------------------------------------
		          precedence Held := x -> precedence x#0
-----------------------------------------------------------------------------

document { quote Expression,
     TT "Expression", " -- the class of all expressions.",
     PARA,
     "These expressions are symbolic representations of algebraic
     expressions, mainly useful in printing.  The method for 
     producing them is ", TO "expression", ".  The usual algebraic
     operations are available for them, but most simplifications do not
     occur.",
     PARA,
     "An expression whose class is ", TT "Expression", " has just one part
     and serves as a simple container for it; such containers are produced
     by the function ", TO "hold", ".",
     PARA,
     "The parts of expressions are not always expressions.  For example,
     ", TO "factor", " returns such an expression.",
     PARA,
     EXAMPLE "(expression 2)^5 * (expression 3)^3 / ((expression 5) * (expression 11)^2)^6",
     PARA,
     "Types of expressions:",
     MENU {
	  TO "AssociativeExpression",
	  TO "BinaryOperation",
	  TO "Divide",
     	  TO "FunctionApplication",
	  TO "Held",
	  TO "HeldString",
	  TO "MatrixExpression",
	  TO "Minus",
	  TO "NonAssociativeProduct",
	  TO "Power",
	  TO "Product",
	  TO "SparseMonomialVectorExpression",
	  TO "SparseVectorExpression",
	  TO "Subscript",
	  TO "Superscript",
	  TO "Sum"
	  },
     "Functions which act on expressions:",
     MENU {
	  TO "expand",
	  TO "precedence"
	  }
     }
document { quote expression,
     TT "expression x", " -- make an ", TO "Expression", " from x."
     }
document { quote Divide,
     TT "Divide", " -- a type of ", TO "Expression", " representing a quotient."
     }
document { quote MatrixExpression,
     TT "MatrixExpression", " -- a type of ", TO "Expression", " representing
     a matrix."
     }
document { quote Minus,
     TT "Minus", " -- a type of ", TO "Expression", " represting negation."
     }
document { quote NonAssociativeProduct,
     TT "NonAssociativeProduct", " -- a type of ", TO "Expression", " representing
     a nonassociative product."
     }
document { quote Power,
     TT "Power", " -- a type of ", TO "Expression", " representing a power.",
     PARA,
     "Normally power expressions with an exponent equal to 1 will not be
     produced.  But it is desirable for ", TO "factor", " to return 
     a product of powers, and some of them will have 1 as exponent.  The
     routines for printing of expressions will take this into account,
     suppress exponents equal to 1, and arrange for parenthesization
     correctly."
     }
document { quote Product,
     TT "Product", " -- a type of ", TO "Expression", " representing a product."
     }
document { quote SparseVectorExpression,
     TT "SparseVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse vector."
     }
document { quote SparseMonomialVectorExpression,
     TT "SparseMonomialVectorExpression", " -- a type of ", TO "Expression", "
     representing a sparse monomial vector.",
     PARA,
     "The difference between this and ", TO "SparseVectorExpression", " is that
     the basis vectors are treated like variables for printing purposes."
     }
document { quote BinaryOperation,
     TT "BinaryOperation", " -- a type of ", TO "Expression", " representing
     the result of a binary operation."
     }
document { quote Subscript,
     TT "Subscript", " -- a type of ", TO "Expression", " representing a
     subscripted expression."
     }
document { quote FunctionApplication,
     TT "FunctionApplication", " -- a type of ", TO "Expression", " representing an
     application of a function."
     }
document { quote Superscript,
     TT "Superscript", " -- a type of ", TO "Expression", " representing a
     superscripted expression."
     }
document { quote Equation,
     TT "Equation", " -- a type of ", TO "Expression", " representing an
     equation."
     }
document { quote Sum,
     TT "Sum", " -- a type of ", TO "Expression", " representing a sum."
     }

-----------------------------------------------------------------------------
-- printing two dimensional ascii output

nobracket := identity

padto := (s,n) -> (
     k := n - stringlen s;
     if k === 0 then s else (s, k))

-- document { quote stringlen,
--      TT "stringlen s", " -- returns the length of the string s.  The argument
--      may also be a sequence or list of strings and symbols, and so
--      on, recursively, in which case the lengths of the various elements
--      are summed.  Additionally, an integer may be used to represent a
--      number of spaces.",
--      PARA,
--      SEEALSO ("String", "concatenate", "#" )
--      }

erase quote stringlen

net Subscript := x -> (
     n := net x#1;
     if precedence x#0 <= PowerPrecedence
     then horizontalJoin( "(", net x#0, ")", n^-(height n) )
     else net x#0 | n^-(height n)
     )
net Superscript := x -> (
     n := net x#1;
     if precedence x#0 <= PowerPrecedence
     then horizontalJoin( "(", net x#0, ")", n^(1+depth n))
     else net x#0 | n^(1+depth n)
     )
net Power := v -> (
     x := v#0;
     netx := net x;
     y := v#1;
     if y === 1 then netx 
     else (
     	  nety := net y;
     	  nety = nety ^ (1 + depth nety);
     	  horizontalJoin (
	       if precedence x <= PowerPrecedence
     	       then ( "(" , netx , ")" , nety)
	       else (       netx ,       nety)
	       )
	  )
     )
net Sum := v -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->" + "));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus 
		    then (
			 seps#i = if i == 0 then "- " else " - "; 
			 v#i#0)
		    else v#i));
	  horizontalJoin splice mingle(seps, 
	       apply(n, i -> 
		    if precedence v#i <= p 
		    then ("(", net v#i, ")")
		    else net v#i))))
net Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i -> " "));
	  seps#0 = seps#n = "";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
		    nterm := net term;
	       	    if precedence term <= p then (
			 -- seps# i = seps#(i+1) = "";
			 nterm = ("(", nterm, ")");
			 );
	       	    nterm
	       	    )
	       );
	  horizontalJoin splice mingle (seps, boxes)
	  )
     )
net NonAssociativeProduct := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i -> "**"));
	  seps#0 = seps#n = "";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then ("(", net term, ")")
	       	    else net term
	       	    )
	       );
	  horizontalJoin splice mingle (seps, boxes)
	  )
     )
net Minus := x -> (
     term := x#0;
     horizontalJoin
     if precedence term <= precedence x 
     then ("-", "(", net term, ")")
     else ("-", net term))

dashes1 := n -> concatenate (n:"-")
dashes2 := memoize dashes1
dashes  := n -> if n <= 0 then "" else if n < 80 then dashes2 n else dashes1 n

spaces1 := n -> concatenate n
spaces2 := memoize spaces1
spaces  := n -> if n <= 0 then "" else if n < 80 then spaces2 n else spaces1 n

net Divide := x -> (
     top := net x#0;
     bot := net x#1;
     wtop := width top;
     wbot := width bot;
     w := max(wtop,wbot);
     itop := (w-wtop+1)//2;
     if itop != 0 then top = spaces itop | top;
     ibot := (w-wbot+1)//2;
     if ibot != 0 then bot = spaces ibot | bot;
     top = top || dashes w;
     top = top ^ (depth top);
     top || bot)
net SparseVectorExpression := v -> (
     if # v === 0
     then "0"
     else net sum(v#1,(i,r) -> expression r * new HeldString from concatenate("<",name i,">"))
     )
net SparseMonomialVectorExpression := v -> (
     if # v === 0
     then "0"
     else (
	  net sum(v#1,(i,m,a) -> 
	       expression a * 
	       expression m * 
	       new HeldString from concatenate("<",name i,">"))
	  )
     )

center := (s,wid) -> (
     n := width s;
     if n === wid then s
     else (
     	  w := (wid-n+1)//2;
     	  horizontalJoin(spaces w,s,spaces(wid-w-n))))

net MatrixExpression := x -> (
     x = elements x;
     if # x === 0 or # (x#0) === 0 then "|  |"
     else (
     	  x = applyTable(x,net);
     	  w := apply(transpose x, col -> 3 + max apply(col, i -> width i));
	  m := verticalJoin between("",
	       apply(#x,
		    i -> (
	       	    	 row := x#i;
	       	    	 horizontalJoin apply(#row,
		    	      j -> center(row#j,w#j)))));
	  side := verticalJoin apply(height m + depth m, i -> "|");
	  side = side^(height m - height side);
	  horizontalJoin(side,m,side)))

-----------------------------------------------------------------------------
-- tex stuff
document { quote tex,
     TT "tex x", " -- convert x to TeX format.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f]",
     EXAMPLE "p = matrix {{a^2+2,b,c},{d,e,f^3-a}}",
     EXAMPLE "tex p",
     SEEALSO ("TeX", "hypertex")
     }
tex Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(v,term -> (
	       if precedence term <= p
	       then ("{(", tex term, ")}")
	       else ("{", tex term, "}") ) );
     if # v === 0 then op#EmptyName 
     else concatenate between(op#operator,names)
     )
tex HeldString := v -> "{" | name v#0 | "}"
tex Minus := v -> (
     term := v#0;
     if precedence term <= precedence v
     then "{-(" | tex term | ")}"
     else "{-" | tex term | "}"
     )
tex Divide := x -> "{" | tex x#0 | " \\over " | tex x#1 | "}"
tex OneExpression := tex ZeroExpression := name
tex Sum := v -> (
     n := # v;
     if n === 0 then "0"
     else (
	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"+"));
	  seps#0 = seps#n = "";
	  v = apply(n, i -> (
		    if class v#i === Minus 
		    then ( seps#i = "-"; v#i#0 )
		    else v#i ));
	  names := apply(n, i -> (
		    if precedence v#i <= p 
		    then "(" | tex v#i | ")"
		    else tex v#i ));
	  concatenate mingle ( seps, names )))
tex Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
     	  concatenate apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p 
		    then "(" | tex term | ")"
	       	    else tex term
	       	    )
	       )
	  )
     )
tex Power := tex Subscript := tex Superscript := v -> (
     p := precedence v;
     x := tex v#0;
     y := tex v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     -- if precedence v#1 <= p then y = "(" | y | ")";
     concatenate("{",x,"}",(class v)#operator,"{",y,"}"))

tex SparseVectorExpression := v -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=tex x);
     concatenate("\\pmatrix{", between("\\cr ",w),"\\cr}")
     )
tex SparseMonomialVectorExpression := v -> (
     tex sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  new HeldString from concatenate("<",name i,">"))
     )
tex MatrixExpression := m -> concatenate(
     "\\pmatrix{",
     apply(m,
	  row->(
	       between("&"|newline,apply(row,tex)),
 	       "\\cr"|newline)),
     "}"
     )

ctr := 0
TeX = x -> (
     ctr = ctr + 1;
     f := tmpname "tx" | string ctr;
     f | ".tex" 
     << "\\magnification = \\magstep 0" << endl
     << tex x << endl
     << "\\end" << endl << close;
     if 0 === run("cd /tmp; tex " | f)
     then run("(xdvi -s 4 "|f|".dvi; rm -f "|f|".tex "|f|".dvi "|f|".log)&")
     else run("rm -f "|f|".tex "|f|".dvi "|f|".log");
     )
document { quote TeX,
     TT "TeX x", " -- convert x to TeX format, and display it on the screen.",
     PARA,
     "The code for this function is Unix dependent at the moment.",
     PARA,
     SEEALSO "tex"
     }
-----------------------------------------------------------------------------
-- netscape stuff

netscape = x -> (
     fn := tmpname "netscape" | ".html";
     f := openOut fn;
     f << "<TITLE>Macaulay 2 Output</TITLE>" << endl;
     r := lookup(html,class x);
     if r =!= null
     then f << html x
     else f << "<PRE>" << endl << net x << "<PRE>" << endl;
     f << close;
     run ( "netscape -remote 'openFile(" | fn | ")'; rm " | fn );
     )

document { quote netscape,
     TT "netscape x", " -- convert x to html format, contact a netscape
     process currently running on the same host, and have it display
     it.",
     PARA,
     "Try this example: ", TT "netscape doc netscape", "."
     }

-----------------------------------------------------------------------------
print = x -> (<< net x << endl; null)

document { quote print,
     TT "print x", " -- prints x on the standard output followed by a new line",
     }
-----------------------------------------------------------------------------

tex Thing := x -> "$" | tex expression x | "$"

File << Thing := (o,x) -> printString(o,net x)

AfterPrint Thing := x -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class x;
     << endl;
     d := doc x;
     if d =!= null then (
     	  i := 0;
     	  while i < #d and d#i =!= PARA do i = i+1;
     	  if i > 0 then << endl << text take(d,i) << endl;
	  );
     )
AfterPrint Expression := x -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class x
     << endl;
     )
AfterPrint ZZ := identity
AfterPrint String := identity
AfterPrint Boolean := identity

-----------------------------------
expression List := expression Sequence := v -> apply(v,expression)
-----------------------------------
-- these interact ... prevent an infinite loop!

expression Thing := x -> new FunctionApplication from { expression, x }
expression RR := x -> (
     if x < 0 then -new Held from {-x} else new Held from {x}
     )

net String := format
net Function := name
net Boolean := net Symbol := net File := net ZZ :=
     net Handle := net Database := string

net Nothing := null -> "" -- we need a way to put blank spots in matrix expressions

hold = x -> new Held from {x}

expression Boolean := expression Symbol := expression File := 
     expression Handle := expression Nothing := expression Database := 
     expression Function := x -> new Held from {x}

net Held := v -> net v#0
name Held := v -> name v#0
-----------------------------------

document { quote hold,
     TT "hold x", " -- embeds it argument x in a list of class ", TO "Held", ".",
     PARA,
     "It might be useful for displaying an integer in factored form,
     for example, because the usual algebraic operations are available
     for ", TO "Expression", "s, but no simplification occurs.",
     PARA,
     EXAMPLE "(hold 2)^5 * (hold 3)^3 * (hold 5) * (hold 11)^2"
     }

Position = new Type of BasicList
name Position := net Position := i -> concatenate(i#0,":",string i#1,":",string i#2)

backtrace = () -> apply(elements deepSplice report, 
     i -> (
	  if class i#0 === String then new Position from i
	  else if class i#0 === Function then new FunctionApplication from i
	  else if class i#0 === Symbol and #i === 3 then new BinaryOperation from i
	  else i
	  )
     )
erase quote report


---------------------------------

SelfNamer = new Type of MutableHashTable

document { quote SelfNamer,
     TT "SelfNamer", " -- the class of mutable hash tables which acquire
     the name of the first global variable they get assigned to.",
     PARA,
     EXAMPLE "X = new Type of MutableHashTable",
     EXAMPLE "x = new X",
     EXAMPLE "Y = new Type of SelfNamer",
     EXAMPLE "y = new Y"
     }

GlobalAssignHook SelfNamer := (X,x) -> (
     if not x#?(quote name) then x.name = X
     )

GlobalReleaseHook SelfNamer := (X,x) -> (
     if x#?(quote name) and X === x.name
     then remove(x,quote name)
     )

expression SelfNamer := (x) -> hold x.name
