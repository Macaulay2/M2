--		Copyright 1994 by Daniel R. Grayson

precedence = method(SingleArgumentDispatch=>true)

-- local variables
local PowerPrecedence
EmptyName := symbol EmptyName
unit := symbol unit
operator := symbol operator
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

-----------------------------------------------------------------------------

HeaderType = new Type of Type
HeaderType List := (T,z) -> new T from z
HeaderType Sequence := (T,z) -> new T from toList z

WrapperType = new Type of Type
WrapperType List := (T,z) -> new T from z
WrapperType Sequence := (T,z) -> new T from toList z
WrapperType Thing := (T,z) -> new T from {z}

-----------------------------------------------------------------------------

Expression = new Type of BasicList
expression Expression := identity
Expression#operator = ""

AssociativeExpression = new Type of Expression
--new AssociativeExpression from Sequence := 
--new AssociativeExpression from List := (type,v) -> (
--     toList splice apply(v, 
--	  term -> if class term === type then toSequence term else term
--	  )
--     )

lookupi := x -> (
     r := lookup x;
     if r === null then error "encountered null or missing value";
     r)

toString Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(v,term -> (
	       if precedence term <= p
	       then "(" | toString term | ")"
	       else toString term
	       )
	  );
     if # v === 0 then op#EmptyName
     else concatenate between(op#operator,names)
     )

Holder = new WrapperType of Expression

texMath Holder := v -> "{" | texMath v#0 | "}"
mathML Holder := v -> mathML v#0
html Holder := v -> html v#0
net Holder := v -> net v#0
toString Holder := v -> toString v#0

remove(Sequence,expression)

Minus = new WrapperType of Expression		  -- unary minus

Minus#operator = "-"
value Minus := v -> minus apply(toSequence v,value)
toString Minus := v -> (
     term := v#0;
     if precedence term <= precedence v
     then "-(" | toString term | ")"
     else "-" | toString term
     )

Equation = new HeaderType of AssociativeExpression
Equation#operator = "=="
value Equation := (v) -> (
     v = apply(toSequence v,value);
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
	  horizontalJoin toList between(" == ", 
	       apply(v, e -> if precedence e <= p then "(" | net e | ")" else net e))))
toString Equation := v -> (
     n := # v;
     if n === 0 then "Equation{}"
     else if n === 1 then "Equation{" | toString v#0 | "}"
     else (
	  p := precedence v;
	  concatenate between(" == ", 
	       apply(v, e -> if precedence e <= p then ("(", toString e, ")") else toString e))))
-----------------------------------------------------------------------------
ZeroExpression = new Type of Holder
ZeroExpression.name = "ZeroExpression"
ZERO := new ZeroExpression from {0}
-----------------------------------------------------------------------------
OneExpression = new Type of Holder
OneExpression.name = "OneExpression"
ONE := new OneExpression from {1}
-----------------------------------------------------------------------------
Sum = new WrapperType of AssociativeExpression

Sum#unit = ZERO
Sum#EmptyName = "0"
Sum#operator = "+"
value Sum := v -> plus apply(toSequence v,value)

toString Sum := v -> (
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
		    then "(" | toString v#i | ")"
		    else toString v#i ));
	  concatenate mingle ( seps, names )))

DoubleArrow = new HeaderType of Expression
DoubleArrow#operator = "=>"
value DoubleArrow := (v) -> value v#0 => value v#1
expression Option := v -> new DoubleArrow from apply(v,expression)
net DoubleArrow := v -> (
     p := precedence v;
     v0 := net v#0;
     v1 := net v#1;
     if precedence v#0 <= p then v0 = horizontalJoin("(",net v0,")");
     if precedence v#1 <  p then v1 = horizontalJoin("(",net v1,")");
     horizontalJoin(v0," => ",v1))

Product = new WrapperType of AssociativeExpression

Product#unit = ONE
Product#EmptyName = "1"
Product#operator = "*"
value Product := v -> times apply(toSequence v,value)

toString Product := v -> (
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
			 "(" | toString term | ")"
			 )
	       	    else toString term
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

NonAssociativeProduct = new WrapperType of Expression

NonAssociativeProduct#unit = ONE
NonAssociativeProduct#EmptyName = "1"
NonAssociativeProduct#operator = "**"
value NonAssociativeProduct := v -> times apply(toSequence v,value)

toString NonAssociativeProduct := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, apply(n+1, i->"**"));
	  seps#0 = seps#n = "";
     	  names := apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p then "(" | toString term | ")"
	       	    else toString term
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

Divide = new HeaderType of Expression
Divide#operator = "/"
value Divide := (x) -> (value x#0) / (value x#1)
numerator Divide := x -> x#0
denominator Divide := x -> x#1

Power = new HeaderType of Expression
Power#operator = "^"
value Power := (x) -> (value x#0) ^ (value x#1)

Subscript = new HeaderType of Expression
Subscript#operator = "_"
value Subscript := (x) -> (value x#0)_(value x#1)

Superscript = new HeaderType of Expression
Superscript#operator = "^"
value Superscript := (x) -> (value x#0)^(value x#1)

toString Power := toString Subscript := toString Superscript := v -> (
     x := v#0;
     y := v#1;
     if y === 1 then toString x else (
	  x = toString x;
	  y = toString y;
	  if precedence v#0 <  PowerPrecedence then x = "(" | x | ")";
	  if precedence v#1 <= PowerPrecedence then y = "(" | y | ")";
	  concatenate(x,(class v)#operator,y)))

-----------------------------------------------------------------------------
RowExpression = new HeaderType of Expression
net RowExpression := w -> horizontalJoin apply(toList w,net)
html RowExpression := w -> concatenate apply(w,html)
texMath RowExpression := w -> concatenate apply(w,texMath)
toString RowExpression := w -> concatenate apply(w,toString)
-----------------------------------------------------------------------------
Adjacent = new HeaderType of Expression
value Adjacent := x -> (value x#0) (value x#1)
-----------------------------------------------------------------------------
prepend0 := (e,x) -> prepend(e#0, x)
append0 := (x,e) -> append(x, e#0)
Equation == Equation        := join
Equation == Expression      := append
Equation == Holder          := append0
Expression == Equation      := prepend
Holder     == Equation      := prepend0
Expression == Expression    := (x,y) -> new Equation from {x,y}
Holder     == Holder        := (x,y) -> new Equation from {x#0,y#0}
Expression == Thing         := (x,y) -> x == expression y
Thing == Expression         := (x,y) -> expression x == y
ZeroExpression + Holder     :=
ZeroExpression + Expression := (x,y) -> y
Expression + ZeroExpression := (x,y) -> x
Sum + Sum                   := join
Sum + Expression            := append
Sum + Holder                := append0
Expression + Sum            := prepend
Holder     + Sum            := prepend0
Expression + Expression     := (x,y) -> new Sum from {x,y}
Holder     + Holder         := (x,y) -> new Sum from {x#0,y#0}
Expression + Thing          := (x,y) -> x + expression y
     Thing + Expression     := (x,y) -> expression x + y
       - ZeroExpression     := identity
	   - Minus          := x -> expression x#0
           - Expression     := x -> new Minus from {x}
           - Holder         := x -> new Minus from {x#0}
Expression - Expression     := (x,y) -> x + -y
Expression - Thing          := (x,y) -> x - expression y
     Thing - Expression     := (x,y) -> expression x - y
Expression * OneExpression  :=
Holder     * OneExpression  := (x,y) -> x
OneExpression * Expression  :=
OneExpression * Holder      := (x,y) -> y
Holder     * ZeroExpression :=
Expression * ZeroExpression := (x,y) -> y
ZeroExpression * Holder     :=
ZeroExpression * Expression := (x,y) -> x
Product * Product           := join
Product * Expression        := append
Product * Holder            := append0
Expression * Product        := prepend
Holder     * Product        := prepend0
Expression * Expression := (x,y) -> new Product from {x,y}
Holder     * Expression := (x,y) -> new Product from {x#0,y}
Expression * Holder     := (x,y) -> new Product from {x,y#0}
Holder     * Holder     := (x,y) -> new Product from {x#0,y#0}
Expression * Minus := (x,y) -> -(x * y#0)
Minus * Expression := (x,y) -> -(x#0 * y)
Minus * Minus := (x,y) -> expression x#0 * expression y#0
Expression * Thing      := (x,y) -> x * (expression y)
     Thing * Expression := (x,y) -> (expression x) * y
Holder     ** OneExpression :=
Expression ** OneExpression := (x,y) -> x
OneExpression ** Holder     :=
OneExpression ** Expression := (x,y) -> y
NonAssociativeProduct ** NonAssociativeProduct := join
NonAssociativeProduct ** Expression := append
NonAssociativeProduct ** Holder     := append0
Expression Expression := (x,y) -> new Adjacent from {x,y}
Holder     Expression := (x,y) -> new Adjacent from {x#0,y}
Expression Holder     := (x,y) -> new Adjacent from {x,y#0}
Holder     Holder     := (x,y) -> new Adjacent from {x#0,y#0}
Expression Array      := (x,y) -> new Adjacent from {x,y}
Holder     Array      := (x,y) -> new Adjacent from {x#0,y}
     -- are lists expressions, too???
Expression Thing      := (x,y) -> x (expression y)
     Thing Expression := (x,y) -> (expression x) y
Expression ** NonAssociativeProduct := prepend
Holder     ** NonAssociativeProduct := prepend0
Expression ** Expression := (x,y) -> new NonAssociativeProduct from {x,y}
Holder     ** Expression := (x,y) -> new NonAssociativeProduct from {x#0,y}
Expression ** Holder     := (x,y) -> new NonAssociativeProduct from {x,y#0}
Holder     ** Holder     := (x,y) -> new NonAssociativeProduct from {x#0,y#0}
Expression ** Thing      := (x,y) -> x ** (expression y)
     Thing ** Expression := (x,y) -> (expression x) ** y
Holder     / OneExpression :=
Expression / OneExpression := (x,y) -> x
Expression / Expression := (x,y) -> new Divide from {x,y}
Holder     / Expression := (x,y) -> new Divide from {x#0,y}
Expression / Holder     := (x,y) -> new Divide from {x,y#0}
Holder     / Holder     := (x,y) -> new Divide from {x#0,y#0}
Expression / Thing      := (x,y) -> x / (expression y)
     Thing / Expression := (x,y) -> (expression x) / y
expression ZZ := i -> (
     if i === 0 then ZERO
     else if i === 1 then ONE
     else if i === -1 then new Minus from { ONE }
     else if i < 0 then new Minus from { new Holder from {-i} }
     else new Holder from {i}
     )
Holder     ^ OneExpression :=
Expression ^ OneExpression := (x,y) -> x
Holder     ^ ZeroExpression :=
Expression ^ ZeroExpression := (x,y) -> ONE
ZeroExpression ^ Holder     :=
ZeroExpression ^ Expression := (x,y) -> ZERO
ZeroExpression ^ ZeroExpression := (x,y) -> ONE
Expression ^ Expression := (x,y) -> Power{x,y}
Holder     ^ Expression := (x,y) -> Power{x#0,y}
Expression ^ Holder     := (x,y) -> Power{x,y#0}
Holder     ^ Holder     := (x,y) -> Power{x#0,y#0}
Expression ^ Thing      := (x,y) -> x ^ (expression y)
     Thing ^ Expression := (x,y) -> (expression x) ^ y
-----------------------------------------------------------------------------
value Holder := x -> value x#0
value OneExpression := v -> 1
value ZeroExpression := v -> 0
value Thing := identity
-----------------------------------------------------------------------------
SparseVectorExpression = new HeaderType of Expression
value SparseVectorExpression := x -> notImplemented()
toString SparseVectorExpression := v -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=toString x);
     concatenate("{",between(",",w),"}")
     )
-----------------------------------------------------------------------------
SparseMonomialVectorExpression = new HeaderType of Expression
-- in these, the basis vectors are treated as variables for printing purposes
value SparseMonomialVectorExpression := x -> notImplemented()
toString SparseMonomialVectorExpression := v -> toString (
     sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  hold concatenate("<",toString i,">"))
     )
-----------------------------------------------------------------------------
MatrixExpression = new HeaderType of Expression
value MatrixExpression := x -> matrix applyTable(toList x,value)
toString MatrixExpression := m -> concatenate(
     "MatrixExpression {",		  -- ????
     between(",",apply(m,row->("{", between(",",apply(row,toString)), "}"))),
     "}" )
-----------------------------------------------------------------------------
Table = new HeaderType of Expression
value Table := x -> applyTable(toList x,value)
toString Table := m -> concatenate(
     "Table {",
     between(",",apply(m,row->("{", between(",",apply(row,toString)), "}"))),
     "}" )
-----------------------------------------------------------------------------

binary := new HashTable from {
     symbol * => ((x,y) -> x*y),		  -- 
     symbol + => ((x,y) -> x+y),		  -- 
     symbol - => ((x,y) -> x-y),		  -- 
     symbol / => ((x,y) -> x/y),		  -- 
     symbol // => ((x,y) -> x//y),	  -- 
     symbol ^ => ((x,y) -> x^y),		  -- 
     symbol == => ((x,y) -> x==y),	  -- 
     symbol .. => ((x,y) -> x..y),	  -- 
     symbol % => ((x,y) -> x%y),
     symbol @ => ((x,y) -> x@y),
     symbol \ => ((x,y) -> x\y),
     symbol @@ => ((x,y) -> x@@y),
     symbol & => ((x,y) -> x&y),
     symbol ? => ((x,y) -> x?y),
     symbol | => ((x,y) -> x|y),
     symbol => => ((x,y) -> x=>y),
     symbol || => ((x,y) -> x||y),
     symbol << => ((x,y) -> x<<y),
     symbol >> => ((x,y) -> x>>y),
     symbol : => ((x,y) -> x:y),
     -- symbol :: => ((x,y) -> x::y),
     symbol ++ => ((x,y) -> x++y),
     symbol ** => ((x,y) -> x**y),
     symbol /^ => ((x,y) -> x/^y),
     symbol _ => ((x,y) -> x_y),
     symbol " " => ((x,y) -> x y)
     }
BinaryOperation = new HeaderType of Expression -- {op,left,right}
value BinaryOperation := (m) -> (
     if binary#?(m#0) then binary#(m#0) (value m#1,value m#2) else m
     )
net BinaryOperation := m -> (
     -- must put precedences here eventually
     horizontalJoin( "(", net m#1, toString m#0, net m#2, ")" )
     )
toString BinaryOperation := m -> (
     horizontalJoin( "(", toString m#1, toString m#0, toString m#2, ")" )
     )
-----------------------------------------------------------------------------
FunctionApplication = new HeaderType of Expression -- {fun,args}
value FunctionApplication := (m) -> (value m#0) (value m#1)
toString Adjacent := toString FunctionApplication := m -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     if class args === Sequence
     then if #args === 1
     then concatenate(toString fun, " ", toString args)  -- f singleton x
     else concatenate(toString fun, toString args)       -- f(x,y) or f(), ...
     else if precedence args >= p
     then if precedence fun > p
     then concatenate(toString fun, " ", toString args)
     else concatenate("(", toString fun, ")", toString args)
     else if precedence fun > p
     then concatenate(toString fun, "(", toString args, ")")
     else concatenate("(", toString fun, ")(", toString args, ")")
     )
net Adjacent := net FunctionApplication := m -> (
     p := precedence m;
     fun := m#0;
     args := m#1;
     if precedence args >= p
     then if precedence fun > p
     then horizontalJoin (net fun, " ", net args)
     else horizontalJoin ("(", net fun, ")", net args)
     else if precedence fun > p
     then horizontalJoin (net fun, "(", net args, ")")
     else horizontalJoin ("(",net fun,")(", net args, ")")
     )
-----------------------------------------------------------------------------
	      precedence Sequence := x -> if #x === 0 then 70 else if #x === 1 then 40 else 70
	   precedence DoubleArrow := x -> 5
	      precedence Equation := x -> 10
	     precedence HashTable := x -> if x.?name then precedence x.name else 20
		 precedence Thing := x -> 20
		   precedence Sum := x -> 20
	       precedence Product := x -> 30
 precedence NonAssociativeProduct := x -> 30
					  MinusPrecedence := 30
		 precedence Minus := x -> MinusPrecedence
   precedence FunctionApplication := x -> 40
              precedence Adjacent := x -> 40
		precedence Divide := x -> 50
	     precedence Subscript := x -> 60
	   precedence Superscript := x -> 60
					  PowerPrecedence = 60
		 precedence Power := x -> if x#1 === 1 then precedence x#0 else PowerPrecedence
		    precedence ZZ := x -> if x>=0 then 70 else MinusPrecedence
		    precedence RR := x -> 70
	      precedence Function := x -> 70
		  precedence List := x -> 70
		 precedence Array := x -> 70
		precedence Symbol := x -> 70
		precedence String := x -> 70
	    precedence Expression := x -> 70    -- the default
		precedence Holder := x -> 70    -- used to be precedence x#0
-----------------------------------------------------------------------------
-- printing two dimensional ascii output

nobracket := identity

padto := (s,n) -> (
     k := n - stringlen s;
     if k === 0 then s else (s, k))

-- document { stringlen,
--      TT "stringlen s", " -- returns the length of the string s.  The argument
--      may also be a sequence or list of strings and symbols, and so
--      on, recursively, in which case the lengths of the various elements
--      are summed.  Additionally, an integer may be used to represent a
--      number of spaces.",
--      PARA,
--      SEEALSO {"String", "concatenate", "#" }
--      }

erase symbol stringlen

nopar := x -> (
     -- this is like net Sequence except we omit the parentheses.
     horizontalJoin deepSplice (
	  if #x === 0 then "()"
	  else if #x === 1 then ("singleton ", net x#0)
	  else (toSequence between(",",apply(x,net)))))

nopars := x -> if class x === Sequence then nopar x else net x

net Subscript := x -> (
     n := nopars x#1;
     if precedence x#0 < PowerPrecedence
     then horizontalJoin( "(", net x#0, ")", n^-(height n) )
     else net x#0 | n^-(height n)
     )
net Superscript := x -> (
     n := net x#1;
     if precedence x#0 < PowerPrecedence
     then horizontalJoin( "(", net x#0, ")", n^(1+depth n))
     else net x#0 | n^(1+depth n)
     )

net Power := v -> (
     x := v#0;
     y := v#1;
     if y === 1 then net x
     else (
     	  nety := net y;
	  nety = nety ^ (1 + depth nety);
	  if class x === Subscript then (
	       t := stack(nety,"",nopars x#1);
	       horizontalJoin (
		    if precedence x < PowerPrecedence
		    then ( "(" , net x#0 , ")" , t)
		    else (       net x#0 ,       t)
		    )
	       )
	  else (
	       horizontalJoin (
		    if precedence x < PowerPrecedence
		    then ( "(" , net x , ")" , nety)
		    else (       net x ,       nety)
		    )
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

isNumber := method(TypicalValue => Boolean)
isNumber Thing := i -> false
isNumber RR :=
isNumber QQ :=
isNumber ZZ := i -> true
isNumber Holder := i -> isNumber i#0

startsWithVariable := method(TypicalValue => Boolean)
startsWithVariable Thing := i -> false
startsWithVariable Symbol := i -> true
startsWithVariable Product := 
startsWithVariable Subscript := 
startsWithVariable Power := 
startsWithVariable Holder := i -> startsWithVariable i#0

net Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
	  seps := newClass(MutableList, splice {"", n-1 : "*", ""});
	  if n>1 and isNumber v#0 and startsWithVariable v#1 then seps#1 = "";
     	  boxes := apply(#v,
	       i -> (
		    term := v#i;
		    nterm := net term;
	       	    if precedence term <= p then (
			 seps#i = seps#(i+1) = "";
			 nterm = ("(", nterm, ")");
			 );
		    if class term === Power
		    and not (term#1 === 1 or term#1 === ONE)
		    or class term === Subscript then (
			 seps#(i+1) = "";
			 );
	       	    nterm));
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
     if precedence term < precedence x 
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
     else net sum(v#1,(i,r) -> (
	       expression r * 
	       hold concatenate("<",toString i,">")
	       )
	  )
     )
net SparseMonomialVectorExpression := v -> (
     if # v === 0
     then "0"
     else (
	  net sum(v#1,(i,m,a) -> 
	       expression a * 
	       expression m * 
	       hold concatenate("<",toString i,">"))
	  )
     )

center := (s,wid) -> (
     n := width s;
     if n === wid then s
     else (
     	  w := (wid-n+1)//2;
     	  horizontalJoin(spaces w,s,spaces(wid-w-n))))

net Table := x -> (
     x = applyTable(toList x, net);
     w := apply(transpose x, col -> 2 + max apply(col, i -> width i));
     stack between("",
	  apply(x, row -> horizontalJoin apply(#row, j -> center(row#j,w#j)))))

net MatrixExpression := x -> (
     if # x === 0 or # (x#0) === 0 then "|  |"
     else (
	  m := net Table toList x;
	  side := "|" ^ (height m, depth m);
	  horizontalJoin(side,m,side)))

html MatrixExpression := x -> html TABLE toList x

mathML MatrixExpression := x -> concatenate(
     "<matrix>",
     newline,
     apply(x, row -> (
	       "<matrixrow>",
	       apply(row, mathML),
	       "</matrixrow>",
     	       newline
	       )
	  ),
     "</matrix>",
     newline
     )

-----------------------------------------------------------------------------
-- tex stuff

texMath Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(v,term -> (
	       if precedence term <= p
	       then ("{(", texMath term, ")}")
	       else ("{", texMath term, "}") ) );
     if # v === 0 then (
	  if op#?EmptyName then op#EmptyName
	  else error("no method for texMath ", op)
	  )
     else (
	  if op#?operator then concatenate between(op#operator,names)
	  else error("no method for texMath ", op)
	  )
     )

html Expression := v -> (
     op := class v;
     p := precedence v;
     names := apply(v,term -> (
	       if precedence term <= p
	       then ("(", html term, ")")
	       else html term));
     if # v === 0 
     then (
	  if op#?EmptyName then op#EmptyName
	  else error("no method for html ", op)
	  )
     else (
	  if op#?operator then concatenate between(op#operator,names)
	  else error("no method for html ", op)
	  )
     )

texMath Minus := v -> (
     term := v#0;
     if precedence term <= precedence v
     then "{-(" | texMath term | ")}"
     else "{-" | texMath term | "}"
     )

mathML Minus := v -> concatenate( "<apply><minus/>", mathML v#0, "</apply>" )

html Minus := v -> (
     term := v#0;
     if precedence term <= precedence v
     then "-(" | html term | ")"
     else "-" | html term
     )

texMath Divide := x -> "{" | texMath x#0 | " \\over " | texMath x#1 | "}"
html Divide := x -> (
     p := precedence x;
     a := html x#0;
     b := html x#1;
     if precedence x#0 <= p then a = "(" | a | ")";
     if precedence x#1 <= p then b = "(" | b | ")";
     a | " / " | b)

mathML Divide := x -> concatenate("<apply><divide/>", mathML x#0, mathML x#1, "</apply>")

mathML OneExpression := x -> "<cn>1</cn>"
mathML ZeroExpression := x -> "<cn>0</cn>"

html OneExpression := html ZeroExpression :=
texMath OneExpression := texMath ZeroExpression := toString

texMath Sum := v -> (
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
		    then "(" | texMath v#i | ")"
		    else texMath v#i ));
	  concatenate mingle ( seps, names )))

html Sum := v -> (
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
		    then "(" | html v#i | ")"
		    else html v#i ));
	  concatenate mingle ( seps, names )))

mathML Sum := v -> (
     n := # v;
     if n === 0 then "<ci>0</ci>"
     else concatenate (
	  "<apply><plus/>",
	  apply( v, mathML ),
	  "</apply>",
	  newline
	  )
     )

mathML Product := v -> (
     n := # v;
     if n === 0 then "<ci>0</ci>"
     else concatenate (
	  "<apply><times/>",
	  apply( v, mathML ),
	  "</apply>",
	  newline
	  )
     )

texMath Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
     	  concatenate between( " ",	  -- could also try \cdot in case there are integers together.
	        apply(#v,
		    i -> (
			 term := v#i;
			 if precedence term <= p 
			 then "(" | texMath term | ")"
			 else texMath term
			 )
		    )
	       )
	  )
     )
html Product := v -> (
     n := # v;
     if n === 0 then "1"
     else (
     	  p := precedence v;
     	  concatenate apply(#v,
	       i -> (
		    term := v#i;
	       	    if precedence term <= p 
		    then "(" | html term | ")"
	       	    else html term
	       	    )
	       )
	  )
     )

mathML Power := v -> if v#1 === 1 then mathML v#0 else concatenate (
     "<apply><power/>",
     mathML v#0,
     mathML v#1,
     "</apply>",
     newline
     )     

texMath Power := v -> (
     if v#1 === 1 then texMath v#0
     else (
	  p := precedence v;
	  x := texMath v#0;
	  y := texMath v#1;
	  if precedence v#0 <  p then x = "(" | x | ")";
	  concatenate("{",x,"}",(class v)#operator,"{",y,"}")))

texMath Subscript := texMath Superscript := v -> (
     p := precedence v;
     x := texMath v#0;
     y := texMath v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     concatenate("{",x,"}",(class v)#operator,"{",y,"}"))

html Superscript := v -> (
     p := precedence v;
     x := html v#0;
     y := html v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     concatenate(x,"<sup>",y,"</sup>"))

html Power := v -> (
     if v#1 === 1 then html v#0
     else (
	  p := precedence v;
	  x := html v#0;
	  y := html v#1;
	  if precedence v#0 <  p then x = "(" | x | ")";
	  concatenate(x,"<sup>",y,"</sup>")))

html Subscript := v -> (
     p := precedence v;
     x := html v#0;
     y := html v#1;
     if precedence v#0 <  p then x = "(" | x | ")";
     concatenate(x,"<sub>",y,"</sub>"))

texMath SparseVectorExpression := v -> (
     n := v#0;
     w := newClass(MutableList, apply(n,i->"0"));
     scan(v#1,(i,x)->w#i=texMath x);
     concatenate("\\pmatrix{", between("\\cr ",w),"\\cr}")
     )

texMath SparseMonomialVectorExpression := v -> (
     texMath sum(v#1,(i,m,a) -> 
	  expression a * 
	  expression m * 
	  hold concatenate("<",toString i,">"))
     )

texMath MatrixExpression := m -> concatenate(
     "\\pmatrix{",
     apply(m,
	  row->(
	       between("&"|newline,apply(row,texMath)),
 	       "\\cr"|newline)),
     "}"
     )

ctr := 0
TeX = x -> (
     ctr = ctr + 1;
     f := tmpname "tx" | toString ctr;
     f | ".tex" 
     << "\\magnification = \\magstep 0" << endl
     << tex x << endl
     << "\\end" << endl << close;
     if 0 === run("cd /tmp; tex " | f)
     then run("(xdvi -s 4 "|f|".dvi; rm -f "|f|".tex "|f|".dvi "|f|".log)&")
     else run("rm -f "|f|".tex "|f|".dvi "|f|".log");
     )

-----------------------------------------------------------------------------
-- netscape stuff

--netscape = x -> (
--     fn := tmpname "netscape" | ".html";
--     f := openOut fn;
--     f << "<TITLE>Macaulay 2 Output</TITLE>" << endl;
--     r := lookup(html,class x);
--     if r =!= null
--     then f << html x
--     else f << "<PRE>" << endl << net x << "<PRE>" << endl;
--     f << close;
--     run ( "netscape -remote 'openFile(" | fn | ")'; rm " | fn );
--     )
--
--document { netscape,
--     TT "netscape x", " -- convert x to html format, contact a netscape
--     process currently running on the same host, and have it display
--     it.",
--     PARA,
--     "Try this example: ", TT "netscape documentation netscape", "."
--     }

-----------------------------------------------------------------------------
print = x -> (<< net x << endl; null)

-----------------------------------------------------------------------------

texMath RR := toString
texMath ZZ := toString
texMath Thing := x -> texMath expression x
texMath Symbol := toString
texMath String := identity

tex Expression := x -> concatenate("$",texMath x,"$")
tex Thing := x -> tex expression x

html ZZ := html RR := toString
html Thing := x -> html expression x

mathML Boolean := i -> if i then "<ci type='constant'>&true;</ci>" else "<ci type='constant'>&false;</ci>"

mathML ZZ := i -> concatenate("<cn type='integer'>",toString i, "</cn>")
mathML RR := i -> concatenate("<cn>",toString i, "</cn>")
mathML QQ := i -> concatenate(
     "<cn type='rational'>",toString numerator i, "<sep/>", toString denominator i, "</cn>"
     )
mathML Thing := x -> mathML expression x

File << Thing := (o,x) -> printString(o,net x)
List << Thing := (o,x) -> (x = net x; scan(o, o -> printString(o,x)); o)

AfterPrint Thing := x -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class x;
     << endl;
     briefDocumentation x;
     )

AfterPrint Expression := x -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class x
     << endl;
     briefDocumentation x;
     )

AfterPrint Holder := x -> (
     << endl;				  -- double space
     << "o" << lineNumber() << " : " << class x << " " << class x#0
     << endl;
     briefDocumentation x;
     )

AfterPrint ZZ := identity
AfterPrint Boolean := identity
-- AfterPrint String := identity
-----------------------------------
expression Array :=
expression List :=
expression Sequence := v -> apply(v,expression)
-----------------------------------
-- these interact ... prevent an infinite loop!

-- maybe this isn't a good idea
-- expression Thing := x -> new FunctionApplication from { expression, x }

expression HashTable := x -> (
     if x.?name then new Holder from {x.name}
     else new FunctionApplication from { 
	  expression class x, 
	  apply(pairs x, (k,v) -> expression(k=>v)) 
	  }
     )
expression BasicList := x -> (
     new FunctionApplication from { 
	  expression class x, 
	  apply(toList x, expression) 
	  }
     )
expression RR := x -> (
     if x < 0 then -new Holder from {-x} else new Holder from {x}
     )
-----------------------------------------------------------------------------
hold = x -> new Holder from {x}

expression Boolean := expression Symbol := expression File := 
     expression Handle := expression Nothing := expression Database := 
     expression Function := x -> new Holder from {x}

-----------------------------------

Position = new Type of BasicList
toString Position := net Position := i -> concatenate(i#0,":",toString i#1,":",toString i#2)

backtrace = () -> apply(toList deepSplice report, 
     i -> (
	  if class i#0 === String then new Position from i
	  else if class i#0 === Function then new FunctionApplication from i
	  else if class i#0 === Symbol and #i === 3 then new BinaryOperation from i
	  else i
	  )
     )
erase symbol report

-----------------------------------------------------------------------------

Entity = new HeaderType of HashTable
tex Entity := x -> if x.?tex then x.tex else "$" | texMath x | "$"
texMath Entity := x -> if x.?texMath then x.texMath else x.name
html Entity := x -> if x.?html then x.html else x.name
toString Entity := x -> x.name
net Entity := x -> if x.?net then x.net else x.name
value Entity := x -> if x.?value then x.value else x
use Entity := x -> if x.?use then x.use x else x

RightArrow = Entity {
     symbol texMath => ///\rightarrow{}///,
     symbol html    => ///<IMG SRC="RightArrow.gif">///,
     symbol name    => "RightArrow",
     symbol net     => "--->",
     symbol symbol  => symbol RightArrow
     }

DownArrow = Entity {
     symbol texMath => ///\downarrow{}///,
     symbol html    => ///<IMG SRC="DownArrow.gif">///,
     symbol name    => "DownArrow",
     symbol net     => "|" || "|" || "V",
     symbol symbol  => symbol DownArrow
     }

