Gate = new Type of HashTable

InputGate = new Type of Gate -- "abstract" unit of input  
inputGate = method()
inputGate Symbol := a -> new InputGate from {
    Name => a
    } 
net InputGate := g -> net g.Name

SumGate = new Type of Gate
net SumGate := g -> (
    n := net "(";
    scan(drop(g.Inputs,-1), i -> n = n | net i | " + ");
    n | net last g.Inputs | ")"
    )
Gate + Gate := (a,b) -> new SumGate from {
    Name => a,
    Inputs => {a,b}
    } 

 

{* Expressions in M2 
   *** should have this as a Gate 
   ??? maybe?
                            Expression : Adjacent
                                      AssociativeExpression : Equation
                                                              Product  ***
                                                              Sum      ***
                                      BinaryOperation
                                      Divide                           ???
                                      FunctionApplication
                                      Holder : OneExpression           ???
                                               ZeroExpression          ???
                                      MatrixExpression                 ***
                                      Minus                            ***
                                      NonAssociativeProduct            ???
                                      Parenthesize                     
                                      Power                            ???
                                      RowExpression                    
                                      SparseMonomialVectorExpression
                                      SparseVectorExpression
                                      Subscript
                                      Superscript
                                      Table
   In addition: 
       Det     	   ***
       Submatrix   ???
       Minor       ???     
*}

end -------------------------------------------

restart
load "SLP-expressions.m2"
A = inputGate X
B = inputGate Y
A+B

------------------------------------------------------------
-- BELOW is the "expression" stuff thay used to be in SLP.m2


---------------------------------------------------------------
-- EXPRESSIONS (think: gates of curcuits used for evaluation)
-- 
-- Already exist:
--    Sum: {E1,E2,...,En}
--    Product: {E1,E2,...,En}
--    Power: {E1,E2}
--    MatrixExpression: 
--
-- New:
--    PolyExpression: {f} where f is a RingElement
--    DetExpression: {M} where M is a MatrixExpression
--
---------------------------------------------------------------
PolyExpression = new Type of Expression
polyExpression = method()
polyExpression RingElement := f -> new PolyExpression from {f}

submatrix (MatrixExpression,BasicList,BasicList) := (M,rows,cols) -> 
    MatrixExpression apply((toList M)_(toList rows), r->r_(toList cols))
submatrix (MatrixExpression,BasicList,Nothing) := (M,rows,cols) -> 
    submatrix(M,rows,0..<numcols M) 
submatrix (MatrixExpression,Nothing,BasicList) := (M,rows,cols) -> 
    submatrix(M,0..<numrows M,cols) 
numrows MatrixExpression := M -> #M
numcols MatrixExpression := M -> if numrows M > 0 then #(M#0) else 0

MatrixExpression | MatrixExpression := (A,B) -> (
    if #A == 0 then B
    else if #B == 0 then A
    else if numrows A != numrows B
    then error "numbers of rows should match"
    else (
	a := toList A;
	b := toList B;
	MatrixExpression apply(#a, r->a#r|b#r)
       	)
    )
MatrixExpression || MatrixExpression :=  (A,B) -> (
    if #A == 0 then B
    else if #B == 0 then A
    else if numcols A != numcols B 
    then error "numbers of columns should match"
    else MatrixExpression(toList A | toList B)
    )
    
DetExpression = new Type of Expression
det MatrixExpression := o -> M -> new DetExpression from M
value DetExpression := e -> det value e 

diff'Thing'Expression = (x,e) -> (
    if class e === Sum then Sum apply(toList e, t->diff(x,t))
    else if class e === MatrixExpression then 
        MatrixExpression apply(toList e, row -> apply(row, a->diff(x,a)))
    else if class e === DetExpression then (
	M := MatrixExpression toList e;
	m := numrows M;
	sum(m, r-> det(
		submatrix(M,0..r-1,) || 
		diff(x,submatrix(M,{r},)) || 
		submatrix(M,r+1..<m,)
		))
	)  
    else if class e === PolyExpression then polyExpression diff(x,e#0)
    else (
	<< "for " << e << endl; 
	<< " of type " << class e <<endl;
	error "diff is not emplemented"
	)
    )
diff (RingElement,Expression) := memoize diff'Thing'Expression

jacobian (List,MatrixExpression) := (xx,F) -> 
    MatrixExpression apply(flatten toList F, f->apply(xx,x->diff(x,f)))

-- oldish...
expression2preSLP = method()
expression2preSLP Expression := e -> (
    if class e === Sum then addPreSLPs apply(toList e,expression2preSLP)
    else if class e === MatrixExpression then 
    stackPreSLPs apply(toList e, row -> concatPreSLPs row)
    else if class e === DetExpression then 
    detPreSLP stackPreSLPs apply(toList e, row -> concatPreSLPs apply(row,expression2preSLP)) 
    else if class e === PolyExpression then poly2preSLP e#0
    else (
	<< "for " << e << " of type " << class e <<endl;
	error "not emplemented"
	)
    )

end 

restart
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y]
f = new PolyExpression from {x^2 + x*y^3 + 1}
a = diff(x,f)
M = MatrixExpression{
    {Sum(polyExpression (x^2+1), polyExpression x^5), f},
    {polyExpression 2_R, polyExpression 3_R}
    }
submatrix(M,{1},{0,1})    	
diff(x,M)
jacobian ({x,y},M)
e = det M
diff(x,det M)

value (MatrixExpression {{Sum(x,y)}} + MatrixExpression {{Sum(x,y)}})

e = det M
value e 
diff(x,e)
expression2preSLP e
