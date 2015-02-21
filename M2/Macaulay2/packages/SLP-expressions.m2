debug needsPackage "NumericalAlgebraicGeometry"

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
minusOneGate = inputGate(-1)
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

ZZ + Gate := (a,b) -> inputGate a + b
Gate + ZZ := (a,b) -> a + inputGate b 
ZZ * Gate := (a,b) -> inputGate a * b			
Gate * ZZ := (a,b) -> a * inputGate b
RR + Gate := (a,b) -> inputGate a + b
Gate + RR := (a,b) -> a + inputGate b 
RR * Gate := (a,b) -> inputGate a * b			
Gate * RR := (a,b) -> a * inputGate b
CC + Gate := (a,b) -> inputGate a + b
Gate + CC := (a,b) -> a + inputGate b 
CC * Gate := (a,b) -> inputGate a * b			
Gate * CC := (a,b) -> a * inputGate b
QQ + Gate := (a,b) -> inputGate a + b
Gate + QQ := (a,b) -> a + inputGate b 
QQ * Gate := (a,b) -> inputGate a * b			
Gate * QQ := (a,b) -> a * inputGate b

ZZ - Gate := (a,b) -> inputGate a - b
Gate - ZZ := (a,b) -> a - inputGate b 
RR - Gate := (a,b) -> inputGate a - b
Gate - RR := (a,b) -> a - inputGate b 
CC - Gate := (a,b) -> inputGate a - b
Gate - CC := (a,b) -> a - inputGate b 
QQ - Gate := (a,b) -> inputGate a - b
Gate - QQ := (a,b) -> a - inputGate b 

productGate = method()
productGate List := L -> (
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new ProductGate from {Inputs=>L}
    )

- Gate := g -> minusOneGate*g
Gate - Gate := (a,b) -> a+(-b)

DetGate = new Type of Gate
net DetGate := g -> concatenateNets {"det", MatrixExpression applyTable(g.Inputs,net)}
detGate = method()
detGate List := L {*doubly nested list*} -> (
    n := #L;
    if not all(L, a->instance(a,List) and #a==n and all(a,b->instance(b,Gate)))
    then error "expected a square matrix (a doubly nested list) of gates";
    new DetGate from {Inputs=>L}
    )

DivideGate = new Type of Gate
net DivideGate := g -> net Divide(first g.Inputs,last g.Inputs) 
Gate / Gate := (a,b) -> if b===zeroGate then error "division by zero"  else 
                        if a===zeroGate then zeroGate else 
			new DivideGate from {
    			    Inputs => {a,b}
    			    } 
			
value (InputGate,HashTable) := (g,h) -> if isConstant g then g.Name else h#g
value (SumGate,HashTable) := memoize ((g,h) -> sum apply(g.Inputs, a->value(a,h)))
value (ProductGate,HashTable) := memoize ((g,h) -> product apply(g.Inputs, a->value(a,h)))
value (DetGate,HashTable) := memoize ((g,h) -> det matrix applyTable(g.Inputs, a->value(a,h)))
value (DivideGate,HashTable) := memoize ((g,h) -> value(first g.Inputs,h)/value(last g.Inputs,h))

support InputGate := g -> g
support SumGate := memoize (g -> g.Inputs/support//flatten//unique)
support ProductGate := memoize (g -> g.Inputs/support//flatten//unique)
support DivideGate := memoize (g -> g.Inputs/support//flatten//unique)
support DetGate := memoize (g -> g.Inputs//flatten/support//flatten//unique)

diff (InputGate, InputGate) := (x,y) -> if y === x then oneGate else zeroGate
diff (InputGate, SumGate) := (x,g) -> g.Inputs/(s->diff(x,s))//sum
diff (InputGate, ProductGate) := (x,g) -> sum apply(#g.Inputs, i->(
	dgi := diff(x,g.Inputs#i);
	product(drop(g.Inputs,{i,i}))*dgi -- commutativity assumed
	))
diff (InputGate, DetGate) := (x,g) -> sum apply(#g.Inputs, i->(
	dgi := apply(g.Inputs#i, a->diff(x,a));
	detGate replace(i,dgi,g.Inputs)
	))
diff (InputGate, DivideGate) := (x,g) -> (
    a := first g.Inputs;
    b := last g.Inputs;	
    da := diff(x,a);
    db := diff(x,b);
    if db===zeroGate then da/b else (da*b-a*db)/(b*b)
    )

subSanityCheck = method()
subSanityCheck Option := ab -> (
    if not instance(first ab, InputGate) then error "only an InputGate can be substituted";
    if not instance(last ab, Gate) then error "can substitute with a Gate only";
    )
sub (InputGate, Option) := (g,ab) -> (
    subSanityCheck ab;
    (a,b) := toSequence ab; 
    if a===g then b else g 
    )
sub (SumGate, Option) := (g,ab) -> (
    subSanityCheck ab;
    sumGate apply(g.Inputs, i->sub(i,ab))
    )
sub (ProductGate, Option) := (g,ab) -> (
    subSanityCheck ab;
    productGate apply(g.Inputs, i->sub(i,ab))
    )

isConstant InputGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement))
compress Gate := g -> g
compress SumGate := g -> (
    L := g.Inputs/compress;
    nums := positions(L, a -> instance(a,InputGate) and isConstant a);
    not'nums := toList(0..<#L) - set nums;
    s := L_nums/(a->a.Name)//sum;
    c := (if s != 0 then {inputGate s} else {}) | L_not'nums;
    if #c == 0 then zeroGate else
    if #c == 1 then first c else 
    sum c
    )
compress ProductGate := g -> (
    L := g.Inputs/compress;
    nums := positions(L, a -> instance(a,InputGate) and isConstant a);
    not'nums := toList(0..<#L) - set nums;
    p := L_nums/(a->a.Name)//product;
    if p==0 then return zeroGate;
    c := (if p != 1 then {inputGate p} else {}) | L_not'nums; -- assumes commutativity
    if #c == 0 then oneGate else
    if #c == 1 then first c else 
    product c
    )

-- returns (consts,program), modifies pos
appendToProgram = method()
appendToProgram (Gate,List,List,MutableHashTable) := (g,consts,program,pos)->(    
    )
appendToProgram (InputGate,List,List,MutableHashTable) := (g,consts,program,pos) -> (
    if pos#?g then return (consts,program); -- do nothing
    if isConstant g then (
	pos#g = #consts;
	(append(consts,g.Name),program)
	) else (
	if not pos#?g then error "a variable is not specified as input";
	(consts,program)
	)
    )
appendToProgram (SumGate,List,List,MutableHashTable) := (g,consts,program,pos)->( 
    if pos#?g then return (consts,program); -- do nothing
    scan(g.Inputs,f->(consts,program)=appendToProgram(f,consts,program,pos));
    abs'pos := #program;
    pos#g = abs'pos;
    (
	consts,
    	append(program, {slpMULTIsum} | {#g.Inputs} | apply(g.Inputs,f->
	    if instance(f,InputGate) then (
	    	if isConstant f then CONST=>pos#f
		else INPUT=>pos#f
		)
	    else pos#f-abs'pos))
        )
    )
appendToProgram (ProductGate,List,List,MutableHashTable) := (g,consts,program,pos)->(    
    if pos#?g then return (consts,program); -- do nothing
    if #g.Inputs!=2 then error "cannot convert products of more than 2 numbers to preSLP";
    scan(g.Inputs,f->(consts,program)=appendToProgram(f,consts,program,pos));
    abs'pos := #program;
    pos#g = abs'pos;
    (
	consts,
    	append(program, {slpPRODUCT} | apply(g.Inputs,f->
	    if instance(f,InputGate) then (
	    	if isConstant f then CONST=>pos#f
		else INPUT=>pos#f
		)
	    else pos#f-abs'pos))
        )
    )

-- assembles a preSLP (see NumericalAlgebraicGeometry/SLP.m2) 
-- that takes a list of InputGates and a list of Gates that produce the output
toPreSLP = method()
toPreSLP (List,List) := (inputs,outputs) -> (
    consts := {};
    program := {};
    pos := new MutableHashTable from apply(#inputs,i->inputs#i=>i);
    scan(outputs,o->(consts,program)=appendToProgram(o,consts,program,pos));
    (consts, program, matrix{outputs/(o->pos#o)})
    )  

end -------------------------------------------

restart
load "SLP-expressions.m2"

--InputGate
X = inputGate symbol X
Y = inputGate symbol Y

--SumGate and ProductGate
C = sumGate {X+Y,Y,X}
D = productGate {X*Y,Y,C}
h = new HashTable from {X=>1,Y=>ii}
assert (value(D,h) == product{value(X*Y,h),value(Y,h),value(C,h)})
support (X*X)
support (D+C)

-- one way to handle constants
E = inputGate 2
F = product{E*(X*X+E*Y)+oneGate, oneGate}

-- sub
G = sub(sub(F,X=>X+Y),Y=>X*Y) 
-- sub and compress = evaluate over a ring
R = CC[x,y] 
H = sub(sub(G,X=>E),Y=>inputGate(x+2*y))
I = compress H 

-- DetGate
J = detGate {{X,C},{D,Y}}

-- diff
diff(X,F)
diff(X,J)
h = new HashTable from {X=>x,Y=>y}
assert(
    value(diff(X,J),h) 
    ==
    value(detGate{{oneGate,C},{diff(X,D),Y}}+detGate{{X,diff(X,C)},{D,zeroGate}},h)
    )

-- DivideGate
F/H
diff(X,X/Y)
diff(Y,X/Y)
compress diff(Y,F/H)
 
-- evaluate toPreSLP == compress 
output = {F,diff(X,F),G}
preSLP = toPreSLP({X,Y},output)
out'eval = evaluatePreSLP(preSLP, gens R)
out'comp = matrix{ output/(o->sub(sub(o,X=>inputGate x),Y=>inputGate y))/compress/(g->g.Name) }
assert(out'eval == out'comp)
out'value = matrix {output/(o->value(o,h))}
assert(out'eval == out'value)


-------------------------------------------------------
-- trackHomotopy 

restart
load "SLP-expressions.m2"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T

K = CC
R = K[x,y,t] 
one = oneGate
F = {X*X-one, Y*Y-one}
G = {X*X+Y*Y-one, minusOne*X*X+Y}
H = (one - T) * F + T * G

preH = toPreSLP({X,Y,T},H)
evaluatePreSLP(preH, {1,1,0})
preHx = transposePreSLP jacobianPreSLP(preH,toList(0..1));
evaluatePreSLP(preHx, {1,1,0})
s = coordinates first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2)
s = coordinates first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2enginePrecookedSLPs)
assert (norm evaluatePreSLP(preH, s|{1}) < 1e-6)
