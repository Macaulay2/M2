debug needsPackage "NumericalAlgebraicGeometry"
debug Core

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
    Name => a,
    cache => new CacheTable
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
    			    Inputs => {a,b}, 
			    cache => new CacheTable
    			    } 
sumGate = method()
sumGate List := L -> (
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new SumGate from {Inputs=>L, cache => new CacheTable}
    )
 
ProductGate = new Type of Gate
net ProductGate := g -> concatenateNets( {"("} | between(" * ", g.Inputs) | {")"} )
Gate * Gate := (a,b) -> if a===zeroGate or b===zeroGate then zeroGate else 
                        if a===oneGate then b else 
			if b===oneGate then a else 
			new ProductGate from {
    			    Inputs => {a,b},
			    cache => new CacheTable
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
    new ProductGate from {Inputs=>L, cache => new CacheTable}
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

ValueHashTable = new Type of HashTable
valueHashTable = method()
valueHashTable (List,List) := (a,b) -> hashTable (apply(a,b,identity) | {(cache,new CacheTable)})
			
value (InputGate,HashTable) := (g,h)-> if h.cache#?g then h.cache#g else h.cache#g = (if isConstant g then g.Name else h#g)
value (SumGate,HashTable) :=  (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (sum apply(g.Inputs, a->value(a,h)))
value (ProductGate,HashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (product apply(g.Inputs, a->value(a,h)))
value (DetGate,HashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (det matrix applyTable(g.Inputs, a->value(a,h)))
value (DivideGate,HashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (value(first g.Inputs,h)/value(last g.Inputs,h))

support InputGate := g -> if isConstant g then {} else g
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
subSanityCheck Option := memoize (ab -> (
    if not instance(first ab, InputGate) then error "only an InputGate can be substituted";
    if not instance(last ab, Gate) then error "can substitute with a Gate only";
    ))
sub (InputGate, Option) := memoize((g,ab) -> (
    subSanityCheck ab;
    (a,b) := toSequence ab; 
    if a===g then b else g 
    ))
sub (SumGate, Option) := memoize((g,ab) -> (
    subSanityCheck ab;
    sumGate apply(g.Inputs, i->sub(i,ab))
    ))
sub (ProductGate, Option) := memoize((g,ab) -> (
    subSanityCheck ab;
    productGate apply(g.Inputs, i->sub(i,ab))
    ))
sub (DetGate, Option) := memoize((g,ab) -> detGate applyTable(g.Inputs, i->sub(i,ab)))

sub (Gate, List) := (g,L) -> (
    g' := g;
    for ab in L do g' = sub(g',ab);
    g'
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

appendToSLProgram = method()
appendToSLProgram (RawSLProgram, InputGate) := (slp, g) -> 
    g.cache#slp = rawSLPInputGate(slp)
appendToSLProgram (RawSLProgram, SumGate) := (slp, g) -> 
    g.cache#slp = rawSLPSumGate(slp,g.Inputs/(a->a.cache#slp))
appendToSLProgram (RawSLProgram, ProductGate) := (slp, g) -> 
    g.cache#slp = rawSLPProductGate(slp,g.Inputs/(a->a.cache#slp))
///
restart
load "SLP-expressions.m2"
s = rawSLProgram(1)
X = inputGate symbol X
C = inputGate symbol C
n0 = appendToSLProgram(s,C)
n1 = appendToSLProgram(s,X)
n2 = appendToSLProgram(s,X+C)
n3 = appendToSLProgram(s,productGate{X,X,C})
rawSLPsetOutputPositions(s,{n0,n1,n2,n3}) 
e = rawSLEvaluator(s,{n0},{n1},raw matrix{{3_QQ}})
rawSLEvaluatorEvaluate(e, raw matrix{{2_QQ}})
///

-- GateMatrix is NOT A GATE
GateMatrix = new Type of List

old'matrix'List = lookup(matrix,List)
gateMatrix = method()
gateMatrix List := L -> (
    if not isTable L then error "a table is expected";
    new GateMatrix from L
    )
gateMatrix Matrix := M -> gateMatrix entries M
matrix List := o -> L -> (
    fL := flatten L;
    if #fL>0 and any(fL, g->instance(g,Gate)) 
    then gateMatrix L
    else (old'matrix'List o) L
    )

GateMatrix_Sequence := (M,ab) -> ( (a,b):=ab; M#a#b )
GateMatrix_List := (M,cols) -> gateMatrix transpose (transpose entries M)_cols

submatrix(GateMatrix,List,List) := (M,a,b) -> gateMatrix apply(a,i->(M#i)_b)

entries GateMatrix := M -> toList M  
transpose GateMatrix := M -> gateMatrix transpose entries M

numcols GateMatrix := M -> # first M
numrows GateMatrix := M -> # M

GateMatrix | GateMatrix := (A,B) -> (
    if numrows A != numrows B then error "need the same number of rows to join";
    gateMatrix transpose (transpose entries A | transpose entries B)      
    )
GateMatrix || GateMatrix := (A,B) -> (
    if numcols A != numcols B then error "need the same number of columns to stack";
    gateMatrix (entries A | entries B)      
    )
GateMatrix * GateMatrix := (A,B) -> ( -- two tables
    B' := transpose B;
    matrix table(#A,#B',(i,j)->sum apply(A#i,B'#j,(a,b)->a*b))
    )
Matrix * GateMatrix := (A,B) -> gateMatrix A * B
GateMatrix * Matrix := (A,B) -> A * gateMatrix B

GateMatrix + GateMatrix := (A,B) -> gateMatrix (entries A + entries B)
Matrix + GateMatrix := (A,B) -> gateMatrix A + B
GateMatrix + Matrix := (A,B) -> A + gateMatrix B

GateMatrix - GateMatrix := (A,B) -> gateMatrix (entries A - entries B)
Matrix - GateMatrix := (A,B) -> gateMatrix A - B
GateMatrix - Matrix := (A,B) -> A - gateMatrix B

det GateMatrix := o -> M -> detGate applyTable(M, a->if instance(a,Gate) then a else inputGate a)

compress GateMatrix := M -> gateMatrix applyTable(M,compress)

value(GateMatrix, HashTable) := (M,H) -> matrix applyTable(M,g->value(g,H))

sub (GateMatrix, List) := (M,L) -> matrix applyTable(M,g->sub(g,L))

support GateMatrix := memoize (M -> flatten entries M/support//flatten//unique)

joinHorizontal = method()
joinHorizontal List := L->(
    if #L==0 then error "empty list";
    r := first L;
    scan(drop(L,1), x->r=r|x);
    r
    )
joinVertical = method()
joinVertical List := L->(
    if #L==0 then error "empty list";
    r := first L;
    scan(drop(L,1), x->r=r||x);
    r
    )
diff (InputGate, GateMatrix) := (x,M) -> gateMatrix applyTable(entries M, g->diff(x,g))
diff (GateMatrix, GateMatrix) := (xx,M) -> joinVertical apply(
    applyTable(entries xx, x->diff(x,M)), 
    row-> joinHorizontal row
    )
PrintTable = new Type of MutableHashTable
newPrintTable := method()
newPrintTable List := o -> (h := new PrintTable; h.numConsts=h.numVars=h.numGates=h.numLines=0; h.Outputs=o; h)
addLine := method()
addLine (PrintTable, Thing) := (h,t) -> ( h#(h#numLines) = t; h#numLines = h#numLines + 1; )    
printName = method()
printName (Gate, HashTable) := (g,h) -> error "not implemented"
printName (InputGate, HashTable) := (g,h) -> if h#?g then h#g else (
    if isConstant g then (
	h#g = "C"|toString h#numConsts;
	addLine(h,h#g | " = " | toString g.Name);
    	h#numConsts = h#numConsts + 1;
	)
    else (
	h#g = "X"|toString h#numVars;
    	h#numVars = h#numVars + 1;
	);
    h#g
    )
printName (SumGate, HashTable) := (g,h) -> if h#?g then h#g else (
    s := between(" + ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#numGates;
    addLine(h, h#g | " = " | concatenateNets s);  
    h#numGates = h#numGates + 1;
    h#g 
    )
printName (ProductGate, HashTable) := (g,h) -> if h#?g then h#g else (
    s := between(" * ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#numGates;
    addLine(h, h#g | " = " | concatenateNets s);  
    h#numGates = h#numGates + 1;
    h#g 
    )
printName (DetGate, HashTable) := (g,h) -> if h#?g then h#g else (
    h#g = "G"|toString h#numGates;
    addLine(h, h#g | " = det " | toString applyTable(g.Inputs, gg->printName(gg,h)));  
    h#numGates = h#numGates + 1;
    h#g 
    )

printAsSLP = method()
printAsSLP List := outputs -> (
    h := newPrintTable outputs;
    scan(outputs, g->printName(g,h));
    scan(h#numLines, i->print h#i);
    print "output:";
    scan(h#Outputs, g->print printName(g,h));     
    )
printAsSLP GateMatrix := M -> printAsSLP flatten entries M

GateHomotopySystem = new Type of HomotopySystem    
gateHomotopySystem = method()
gateHomotopySystem (GateMatrix, GateMatrix, InputGate) := (H,X,T) -> (
    GH := new GateHomotopySystem;
    GH.H = H;
    GH.X = X;
    GH.T = T;    
    GH.Hx = diff(X,H);
    GH.Ht = diff(T,H);
    GH
    ) 
evaluateH (GateHomotopySystem,Matrix,Number) := (H,x,t) -> value(H.H, 
    hashTable(apply(flatten entries H.X, flatten entries x,identity) | {(H.T,t), (cache,new CacheTable)})
    )
evaluateHt (GateHomotopySystem,Matrix,Number) := (H,x,t) -> value(H.Ht, 
    hashTable(apply(flatten entries H.X, flatten entries x,identity) | {(H.T,t), (cache,new CacheTable)})
    )
evaluateHx (GateHomotopySystem,Matrix,Number) := (H,x,t) -> value(H.Hx, 
    hashTable(apply(flatten entries H.X, flatten entries x,identity) | {(H.T,t), (cache,new CacheTable)})
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
h = new HashTable from {X=>1,Y=>ii,cache=>new CacheTable}
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
h = new HashTable from {X=>x,Y=>y,cache=>new CacheTable}
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
printSLP preSLP
printAsSLP output

-------------------------------------------------------
-- trackHomotopy 

restart
load "SLP-expressions.m2"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T

K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G

-- preSLP way
preH = toPreSLP({X,Y,T},H)
evaluatePreSLP(preH, {1,1,0})
preHx = transposePreSLP jacobianPreSLP(preH,toList(0..1));
evaluatePreSLP(preHx, {1,1,0})
s = coordinates first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2)
s = coordinates first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2enginePrecookedSLPs)
assert (norm evaluatePreSLP(preH, s|{1}) < 1e-6)

-- HomotopySystem
Rvars = hashTable{X=>x,Y=>y,T=>t,cache=>new CacheTable} 
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gHx = diff(gV,gH)
gHt = diff(T,gH)
value(gH, Rvars)
value(gHt, Rvars)
value(gHx, Rvars)


HS = gateHomotopySystem(gH,gV,T)
s = coordinates first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)

F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-1}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
HS = gateHomotopySystem(gH,gV,T)
trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)

