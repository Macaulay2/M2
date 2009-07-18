--This code, on the one hand, allows for operations (e.g. addition) on OpenMath expressions.
--On the other hand, it provides toOpenMath on some of these constructs that can exist in Macaulay
 
--     
-- toOM  Expr
--             Adjacent
-- OK    OK    AssociativeExpression : Equation
-- OK    OK                            Product
-- OK    OK                            Sum
--             BinaryOperation
-- OK    OK    Divide
-- OK          FunctionApplication
--             Holder : OneExpression
--                      ZeroExpression
--             MatrixExpression
-- OK    OK    Minus (note! Minus (x,y) === -x !)
--             NonAssociativeProduct
--             Parenthesize
-- OK    OK    Power
--             RowExpression
--             SparseMonomialVectorExpression
--             SparseVectorExpression
--             Subscript
--             Superscript
--             Table

--------------------------
-- Building Expressions --
--------------------------
XMLnode + XMLnode := (a,b) -> OMA("arith1", "plus", {a,b})
XMLnode + Thing   := (a,b) -> OMA("arith1", "plus", {a,openMath b})
Thing   + XMLnode := (a,b) -> OMA("arith1", "plus", {openMath a,b})

XMLnode - XMLnode := (a,b) -> OMA("arith1", "minus", {a,b})
XMLnode - Thing   := (a,b) -> OMA("arith1", "minus", {a,openMath b})
Thing   - XMLnode := (a,b) -> OMA("arith1", "minus", {openMath a,b})

XMLnode * XMLnode := (a,b) -> OMA("arith1", "times", {a,b})
XMLnode * Thing   := (a,b) -> OMA("arith1", "times", {a,openMath b})
Thing   * XMLnode := (a,b) -> OMA("arith1", "times", {openMath a,b})

XMLnode / XMLnode := (a,b) -> OMA("arith1", "divide", {a,b})
XMLnode / Thing   := (a,b) -> OMA("arith1", "divide", {a,openMath b})
Thing   / XMLnode := (a,b) -> OMA("arith1", "divide", {openMath a,b})

XMLnode ^ XMLnode := (a,b) -> OMA("arith1", "power", {a,b})
XMLnode ^ Thing   := (a,b) -> OMA("arith1", "power", {a,openMath b})
Thing   ^ XMLnode := (a,b) -> OMA("arith1", "power", {openMath a,b})

XMLnode == XMLnode := (a,b) -> OMA("relation1", "eq", {a,b})
XMLnode == Thing   := (a,b) -> OMA("relation1", "eq", {a,openMath b})
Thing   == XMLnode := (a,b) -> OMA("relation1", "eq", {openMath a,b})

XMLnode and XMLnode := (a,b) -> OMA("logic1", "and", {a,b})
XMLnode and Thing   := (a,b) -> OMA("logic1", "and", {a,openMath b})
Thing   and XMLnode := (a,b) -> OMA("logic1", "and", {openMath a,b})

XMLnode or XMLnode := (a,b) -> OMA("logic1", "or", {a,b})
XMLnode or Thing   := (a,b) -> OMA("logic1", "or", {a,openMath b})
Thing   or XMLnode := (a,b) -> OMA("logic1", "or", {openMath a,b})

size XMLnode := x -> ( OMA("set1", "size", { x }) )


-----------------------
----- To OpenMath -----
-----------------------
toOpenMath Equation := x -> (
	OMA("relation1", "eq", apply(toList(x), toOpenMath))
)
toOpenMath Product := x -> (
	OMA("arith1", "times", apply(toList(x), toOpenMath))
)
toOpenMath Sum := x -> (
	OMA("arith1", "plus", apply(toList(x), toOpenMath))
)
toOpenMath Power := x -> (
	OMA("arith1", "power", apply(toList(x), toOpenMath))
)
toOpenMath Divide := x -> (
	OMA("arith1", "divide", apply(toList(x), toOpenMath))
)
toOpenMath Minus := x -> (
	OMA("arith1", "unary_minus", apply(toList(x), toOpenMath))
)
toOpenMath FunctionApplication := a -> (
	if #a === 0 then
		error "FunctionApplication of 0 arguments?!"
	else if #a === 1 then
		OMA(toOpenMath a#0, {})
	else if #a === 2 and class(a#1) === Sequence then
		OMA(toOpenMath a#0, apply(toList(a#1), toOpenMath))
	else if #a === 2 then
		OMA(toOpenMath a#0, { toOpenMath a#1} )
	else
		error "FunctionApplication of more than 2 arguments?!"
		
)
toOpenMath BinaryOperation := a -> (
	symb := a#0;
	if      symb === (symbol +)  then OMA("arith1", "plus",   { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol -)  then OMA("arith1", "minus",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol *)  then OMA("arith1", "times",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol /)  then OMA("arith1", "divide", { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol ^)  then OMA("arith1", "power",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol ==) then OMA("relation1", "eq",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol <)  then OMA("relation1", "lt",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol >)  then OMA("relation1", "gt",  { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol <=) then OMA("relation1", "leq", { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol >=) then OMA("relation1", "geq", { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol !=) then OMA("relation1", "neq", { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol and) then OMA("logi1", "and",    { toOpenMath a#1, toOpenMath a#2 })
	else if symb === (symbol or) then OMA("logic1", "or",     { toOpenMath a#1, toOpenMath a#2 })
	else
		error concatenate("Cannot handle binary operator '", toString symb, "'")
)

--Dan Grayson may implement this some day:
-- toOpenMath UnaryOperation := a -> (
-- 	symb := a#0;
-- 	if      symb === (symbol -)   then OMA("arith1", "unary_minus",  { toOpenMath a#1 })
-- 	else if symb === (symbol not) then OMA("logic1", "not",  { toOpenMath a#1 })
-- 	else
-- 		error concatenate("Cannot handle unary operator '", toString symb, "'")
-- )


