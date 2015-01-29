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

TO DO:

-- decide what types of non-terminal gates we need (see above)
-- terminal gates: InputGate, ConstantGate???
-- what does MatrixGate do?
-- gatesToPreSLP List (rewrite existing preSLP code?)
-- diff(InputGate,Gate), jacobian
-- preSLPtoEngine
-- evaluator
-- evaluator in the engine
-- sub(Gate,{Gate=>Gate,...,}) 

*}

concatenateNets = method()
concatenateNets List := L -> (
    result := net "";
    for a in L do result = result | net a;
    result
    )

Gate = new Type of HashTable

InputGate = new Type of Gate -- "abstract" unit of input  
inputGate = method()
inputGate Thing := a -> new InputGate from {
    Name => a
    } 
net InputGate := g -> net g.Name
oneGate = inputGate 1
zeroGate = inputGate 0

SumGate = new Type of Gate
net SumGate := g -> concatenateNets( {"("} | between(" + ", g.Inputs) | {")"} )
Gate + Gate := (a,b) -> if a===zeroGate then b else 
                        if b===zeroGate then a else 
			new SumGate from {
    			    Inputs => {a,b}
    			    } 
sumGate = method()
sumGate List := L -> (
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new SumGate from {Inputs=>L}
    )
 
ProductGate = new Type of Gate
net ProductGate := g -> concatenateNets( {"("} | between(" * ", g.Inputs) | {")"} )
Gate * Gate := (a,b) -> if a===zeroGate or b===zeroGate then zeroGate else 
                        if a===oneGate then b else 
			if b===oneGate then a else 
			new ProductGate from {
    			    Inputs => {a,b}
    			    } 
productGate = method()
productGate List := L -> (
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new ProductGate from {Inputs=>L}
    )
value (InputGate,HashTable) := (g,h) -> h#g
value (SumGate,HashTable) := memoize ((g,h) -> sum apply(g.Inputs, a->value(a,h)))
value (ProductGate,HashTable) := memoize ((g,h) -> product apply(g.Inputs, a->value(a,h)))

support InputGate := g -> g
support SumGate := memoize (g -> g.Inputs/support//flatten//unique)
support ProductGate := memoize (g -> g.Inputs/support//flatten//unique)

diff (InputGate, InputGate) := (x,y) -> if y === x then oneGate else zeroGate
diff (InputGate, SumGate) := (x,g) -> g.Inputs/(s->diff(x,s))//sum
diff (InputGate, ProductGate) := (x,g) -> sum apply(#g.Inputs, i->(
	dgi := diff(x,g.Inputs#i);
	product(drop(g.Inputs,{i,i}))*dgi -- commutativity assumed
	))


compile = method()

end -------------------------------------------

restart
load "SLP-expressions.m2"
A = inputGate X
B = inputGate Y
C = sumGate {A+B,B,A}
D = productGate {A*B,B,C}
h = new HashTable from {A=>1,B=>ii}
assert (value(D,h) == product{value(A*B,h),value(B,h),value(C,h)})
support (A*A)
support (D+C)
s = new MutableHashTable from {A+B=>C}
peek s
s#(A+B)

E = inputGate 2 -- one way to handel constants
f = product {E*(A*A+E*B)+oneGate, E, oneGate}
diff(A,f)

debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y]
f = random(3,R)
poly2preSLP f
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
