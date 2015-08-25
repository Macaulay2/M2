-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

newPackage select((
     "SLPexpressions",
     Version => "1.8",
     Date => "August 2015",
     Headline => "Straight Line Programs and Algebraic Circuits",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => false,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Configuration =>  {},	
     PackageExports => {"NumericalAlgebraicGeometry"},
     PackageImports => {},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     --DebuggingMode => true
     DebuggingMode => false
     ), x -> x =!= null)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
    "Gate", "GateMatrix", "InputGate", 
    "oneGate", "minusOneGate", "zeroGate",
    "SumGate", "ProductGate", "DetGate", "DivideGate",
    "inputGate", "sumGate", "productGate", "detGate", 
    "constants",  
    "printAsSLP",
    "GateHomotopySystem", "gateHomotopySystem" 
    }
exportMutable {
    }
debug NumericalAlgebraicGeometry
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
GateMatrix = new Type of List

gateCatalog = new MutableHashTable -- records all gates
gateCatalogCount = new MutableHashTable -- count for each gate
add2GC := g -> if gateCatalog#?g then (
    gateCatalogCount#g = gateCatalogCount#g + 1;
    gateCatalog#g -- value = stored identical gate (may differ in "cache")
    ) else (
    gateCatalogCount#g= 1;
    gateCatalog#g = g
    ) 

InputGate = new Type of Gate -- "abstract" unit of input  
inputGate = method()
inputGate Thing := a -> add2GC new InputGate from {
    Name => a,
    cache => new CacheTable
    } 
net InputGate := g -> net g.Name

oneGate = inputGate 1
minusOneGate = inputGate(-1)
zeroGate = inputGate 0

SumGate = new Type of Gate
net SumGate := g -> concatenateNets( {"("} | between(" + ", g.Inputs) | {")"} )
Gate + Gate := (a,b) -> add2GC (
    if a===zeroGate then b else 
    if b===zeroGate then a else 
    new SumGate from {
      	Inputs => {a,b}, 
      	cache => new CacheTable
      	} 
    )
sumGate = method()
sumGate List := L -> add2GC(
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new SumGate from {Inputs=>L, cache => new CacheTable}
    )
 
ProductGate = new Type of Gate
net ProductGate := g -> concatenateNets( {"("} | between(" * ", g.Inputs) | {")"} )
Gate * Gate := (a,b) -> add2GC (
    if a===zeroGate or b===zeroGate then zeroGate else 
    if a===oneGate then b else 
    if b===oneGate then a else 
    new ProductGate from {
      	Inputs => {a,b},
      	cache => new CacheTable
      	}	   
    )

productGate = method()
productGate List := L -> add2GC(
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new ProductGate from {Inputs=>L, cache => new CacheTable}
    )
Gate ^ ZZ := (g,n) -> if n == 0 then oneGate else 
    if n > 0 then productGate toList(n:g) else -- inefficient!!!
    {*(n < 0)*}   oneGate / productGate toList(n:g) 

- Gate := g -> minusOneGate*g
Gate - Gate := (a,b) -> a+(-b)

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

DetGate = new Type of Gate
net DetGate := g -> concatenateNets {"det", MatrixExpression applyTable(g.Inputs,net)}
detGate = method()
detGate List := L {*doubly nested list*} -> add2GC(
    n := #L;
    if not all(L, a->instance(a,List) and #a==n and all(a,b->instance(b,Gate)))
    then error "expected a square matrix (a doubly nested list) of gates";
    new DetGate from {Inputs=>L,cache => new CacheTable}
    )

DivideGate = new Type of Gate
net DivideGate := g -> net Divide(first g.Inputs,last g.Inputs) 
Gate / Gate := (a,b) -> add2GC(
    if b===zeroGate then error "division by zero"  else 
    if a===zeroGate then zeroGate else 
    new DivideGate from {
      	Inputs => {a,b}, 
      	cache => new CacheTable
      	}
    ) 
    
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
support List := memoize (L -> L/support//flatten//unique)
support GateMatrix := memoize (M -> support flatten entries M)

constants = method()
constants InputGate := g -> if isConstant g then g else {}
constants SumGate := memoize (g -> g.Inputs/constants//flatten//unique)
constants ProductGate := memoize (g -> g.Inputs/constants//flatten//unique)
constants DivideGate := memoize (g -> g.Inputs/constants//flatten//unique)
constants DetGate := memoize (g -> g.Inputs//flatten/constants//flatten//unique)
constants List := memoize (L -> L/constants//flatten//unique)
constants GateMatrix := memoize (M -> constants flatten entries M)

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
sub (DivideGate, Option) := memoize((g,ab) -> (
    subSanityCheck ab;
    (x,y) := toSequence apply(g.Inputs, i->sub(i,ab));
    x/y
    ))

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

-----------------------------------------------
-- OLD!!! preSLP routines

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

TEST ///
needsPackage "SLPexpressions"

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
J = detGate {{X,C,F},{D,Y,E},{G,F,X}}

-- diff
diff(X,F)
diff(X,J)
h = new HashTable from {X=>x,Y=>y,cache=>new CacheTable}
assert(
    value(diff(X,J),h) 
    ==
    diff(x, det matrix applyTable(J.Inputs, i->value(i,h)))
    )

-- DivideGate
G/F
diff(X,X/Y)
diff(Y,X/Y)
h = new HashTable from {X=>2,Y=>3,cache=>new CacheTable}
GY = value(diff(Y,G),h)
FY = value(diff(Y,F),h)
assert ( value(compress diff(Y,G/F), h) == (GY*value(F,h) - value(G,h)*FY)/(value(F,h))^2 )

debug SLPexpressions
debug NumericalAlgebraicGeometry
-- evaluate toPreSLP == compress 
output = {F,diff(X,F),G}
preSLP = toPreSLP({X,Y},output)
out'eval = evaluatePreSLP(preSLP, gens R)
out'comp = matrix{ output/(o->sub(sub(o,X=>inputGate x),Y=>inputGate y))/compress/(g->g.Name) }
assert(out'eval == out'comp)
printSLP preSLP
printAsSLP output
///

--------------------------------------------------
-- RawSLProgram routines
--------------------------------------------------

makeSLProgram = method()
makeSLProgram (List,List) := (inL,outL) -> (
    s := rawSLProgram(1); -- 1 means nothing anymore
    scan(inL,g->appendToSLProgram(s,g)); 
    out := apply(outL, g->appendToSLProgram(s,g));
    rawSLPsetOutputPositions(s,out);
    s
    )
makeSLProgram (GateMatrix,GateMatrix) := (inM,outM) -> makeSLProgram(flatten entries inM, flatten entries outM)

positions(List, RawSLProgram) := (L,s) -> apply(L, g->g.cache#s)

appendToSLProgram = method()
appendToSLProgram (RawSLProgram, InputGate) := (slp, g) -> 
    if g.cache#?slp then g.cache#slp else g.cache#slp = rawSLPInputGate(slp)
appendToSLProgram (RawSLProgram, SumGate) := (slp, g) -> 
    if g.cache#?slp then g.cache#slp else (
	scan(g.Inputs, a->appendToSLProgram (slp,a));
	g.cache#slp = rawSLPSumGate(slp, g.Inputs/(a->a.cache#slp))
	)
appendToSLProgram (RawSLProgram, ProductGate) := (slp, g) -> 
    if g.cache#?slp then g.cache#slp else (
	scan(g.Inputs, a->appendToSLProgram (slp,a));
    	g.cache#slp = rawSLPProductGate(slp, g.Inputs/(a->a.cache#slp))
	)
appendToSLProgram (RawSLProgram, DetGate) := (slp, g) -> 
    if g.cache#?slp then g.cache#slp else (
	scan(flatten g.Inputs, a->appendToSLProgram (slp,a));
    	g.cache#slp = rawSLPDetGate(slp, flatten g.Inputs/(a->a.cache#slp))
	)
appendToSLProgram (RawSLProgram, DivideGate) := (slp, g) -> 
    if g.cache#?slp then g.cache#slp else (
	scan(g.Inputs, a->appendToSLProgram (slp,a));
	g.cache#slp = rawSLPDivideGate(slp, g.Inputs/(a->a.cache#slp))
	)
TEST ///
needsPackage "SLPexpressions"
debug SLPexpressions
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
s = makeSLProgram({C,X},{XXC,detXCCX,XoC,XpC+XoC}) 

debug Core
(consts,indets):=(positions({C},s), positions({X},s))
eQQ = rawSLEvaluator(s,consts,indets,raw matrix{{3_QQ}})
rawSLEvaluatorEvaluate(eQQ, raw matrix{{7_QQ}}) 
eCC = rawSLEvaluator(s,consts,indets,raw matrix{{3_CC}})
rawSLEvaluatorEvaluate(eCC, raw matrix{{7_CC}}) 
R = CC_1000
eCC = rawSLEvaluator(s,consts,indets,raw matrix{{3_R}})
rawM = rawSLEvaluatorEvaluate(eCC, raw matrix{{7_R}}) 
assert (abs(last flatten entries map(R,rawM) - 37/3) < 2^(-999))
///

-------------------------------------
-- GateMatrix is NOT A GATE
-- GateMatrix = new Type of List (actually a table) 
-------------------------------------

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
newPrintTable List := o -> (h := new PrintTable; h#"#consts"=h#"#vars"=h#"#gates"=h#"#lines"=0; h.Outputs=o; h)
addLine := method()
addLine (PrintTable, Thing) := (h,t) -> ( h#(h#"#lines") = t; h#"#lines" = h#"#lines" + 1; )    
printName = method()
printName (Gate, HashTable) := (g,h) -> error "not implemented"
printName (InputGate, HashTable) := (g,h) -> if h#?g then h#g else (
    if isConstant g then (
	h#g = "C"|toString h#"#consts";
	addLine(h,h#g | " = " | toString g.Name);
    	h#"#consts" = h#"#consts" + 1;
	)
    else (
	h#g = "X"|toString h#"#vars";
    	h#"#vars" = h#"#vars" + 1;
	);
    h#g
    )
printName (SumGate, HashTable) := (g,h) -> if h#?g then h#g else (
    s := between(" + ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | " = " | concatenateNets s);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (ProductGate, HashTable) := (g,h) -> if h#?g then h#g else (
    s := between(" * ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | " = " | concatenateNets s);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (DivideGate, HashTable) := (g,h) -> if h#?g then h#g else (
    (x,y) := toSequence apply(g.Inputs, gg->printName(gg,h));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | " = " | x | " / " | y);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (DetGate, HashTable) := (g,h) -> if h#?g then h#g else (
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | " = det " | toString applyTable(g.Inputs, gg->printName(gg,h)));  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )

printAsSLP = method()
printAsSLP List := outputs -> (
    h := newPrintTable outputs;
    scan(outputs, g->printName(g,h));
    scan(h#"#lines", i->print h#i);
    print "output:";
    scan(h#Outputs, g->print printName(g,h));     
    )
printAsSLP GateMatrix := M -> printAsSLP flatten entries M

----------------------------------
-- GateHomotopySystem

GateHomotopySystem := new Type of HomotopySystem    
GateParameterHomotopySystem := new Type of ParameterHomotopySystem

gateHomotopySystem = method(Options=>{Parameters=>null,Software=>M2engine})
gateHomotopySystem (GateMatrix, GateMatrix, InputGate) := o->(H,X,T) -> (
    para := o.Parameters=!=null;
    GH := new GateHomotopySystem;
    GH#"X" = X;
    if para then GH#"X" = o.Parameters | GH#"X";
    GH#"T" = T;    
    GH#"H" = H;
    GH#"Hx" = diff(X,H);
    GH#"Ht" = diff(T,H);
    if (GH.Software = o.Software) === M2 then (
	)
    else if (GH.Software = o.Software) === M2engine then (
	varMat := X | matrix{{T}};
	if para then varMat = o.Parameters | varMat;
    	GH#"H core" = makeSLProgram (varMat,H);
	GH#"H consts" = constants H;
    	GH#"Hx core" = makeSLProgram (varMat,GH#"Hx");
	GH#"Hx consts" = constants GH#"Hx";
    	GH#"Ht core" = makeSLProgram (varMat,GH#"Ht");
	GH#"Ht consts" = constants GH#"Ht";
	)
    else error "uknown Software option value";
    if para then (
	GPH := new GateParameterHomotopySystem;
	GPH.GateHomotopySystem = GH;
	GPH
	) 
    else GH
    ) 

matrix (Matrix,ZZ,ZZ) := o -> (M,m,n) -> (
    R := ring M;
    e := flatten entries M;  
    map(R^m,R^n,(i,j)->e#(n*i+j)) 
    )
matrix (Ring,RawMatrix,ZZ,ZZ) := o -> (R,M,m,n) -> (
    e := flatten entries M;  
    map(R^m,R^n,(i,j)->e#(n*i+j)) 
    )
evaluateH (GateHomotopySystem,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"H", 
    hashTable(apply(flatten entries H#"X", flatten entries x,identity) | {(H#"T",t), (cache,new CacheTable)})
    ) else if H.Software===M2engine then (
    K := ring x;
    if not H#?(H#"H",K) then (
	s := H#"H core"; -- core SLP
	consts := H#"H consts"; -- constants of SLP
	H#(H#"H",K) = rawSLEvaluator(s, positions(consts,s), positions(flatten entries H#"X" | {H#"T"},s),
	    raw matrix{apply(consts,c->c.Name_K)});
	);
    matrix(K, rawSLEvaluatorEvaluate(H#(H#"H",K), raw (transpose x | matrix{{t}})), numrows H#"H", numcols H#"H")
    )
evaluateHt (GateHomotopySystem,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Ht", 
    hashTable(apply(flatten entries H#"X", flatten entries x,identity) | {(H#"T",t), (cache,new CacheTable)})
    ) else if H.Software===M2engine then (
    K := ring x;
    if not H#?(H#"Ht",K) then (
	s := H#"Ht core"; -- core SLP
	consts := H#"Ht consts"; -- constants of SLP
	H#(H#"Ht",K) = rawSLEvaluator(s, positions(consts,s), positions(flatten entries H#"X" | {H#"T"},s),
	    raw matrix{apply(consts,c->c.Name_K)});
	);
    matrix(K, rawSLEvaluatorEvaluate(H#(H#"Ht",K), raw (transpose x | matrix{{t}})), numrows H#"Ht", numcols H#"Ht")
    )
evaluateHx (GateHomotopySystem,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Hx", 
    hashTable(apply(flatten entries H#"X", flatten entries x,identity) | {(H#"T",t), (cache,new CacheTable)})
    ) else if H.Software===M2engine then (
    K := ring x;
    if not H#?(H#"Hx",K) then (
	s := H#"Hx core"; -- core SLP
	consts := H#"Hx consts"; -- constants of SLP
	H#(H#"Hx",K) = rawSLEvaluator(s, positions(consts,s), positions(flatten entries H#"X" | {H#"T"},s),
	    raw matrix{apply(consts,c->c.Name_K)});
	);
    matrix(K,rawSLEvaluatorEvaluate(H#(H#"Hx",K), raw (transpose x | matrix{{t}})), numrows H#"Hx", numcols H#"Hx")
    )
evaluateH (GateParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateH(H.GateHomotopySystem,parameters||x,t)
evaluateHt (GateParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHt(H.GateHomotopySystem,parameters||x,t)
evaluateHx (GateParameterHomotopySystem,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHx(H.GateHomotopySystem,parameters||x,t)



-------------------------------------------------------
-- trackHomotopy tests
TEST ///
needsPackage "SLPexpressions"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T

K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G

debug SLPexpressions
debug NumericalAlgebraicGeometry
-- preSLP way
preH = toPreSLP({X,Y,T},H)
evaluatePreSLP(preH, {1,1,0})
preHx = transposePreSLP jacobianPreSLP(preH,toList(0..1));
evaluatePreSLP(preHx, {1,1,0})
s = first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2)
peek s
s = first trackHomotopy((R,preH),{matrix{{1},{1}}},Software=>M2enginePrecookedSLPs)
peek s
assert (norm evaluatePreSLP(preH, coordinates s|{1}) < 1e-6)
///

TEST /// -- HomotopySystem
needsPackage "SLPexpressions"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G
Rvars = hashTable{X=>x,Y=>y,T=>t,cache=>new CacheTable} 
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gHx = diff(gV,gH)
gHt = diff(T,gH)
value(gH, Rvars)
value(gHt, Rvars)
value(gHx, Rvars)

debug NumericalAlgebraicGeometry
HS = gateHomotopySystem(gH,gV,T)
x0 = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{x0},Software=>M2)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
value(HS#"H", Rvars)
evaluateH(HS,x0,0.1_CC)
value(HS#"Ht", Rvars)
evaluateHt(HS,x0,0.1_CC)
evaluateHx(HS,x0,0.1_CC)

F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-1}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
HS = gateHomotopySystem(gH,gV,T)
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)
peek s
///

TEST /// -- ParameterHomotopySystem
needsPackage "SLPexpressions"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
P = inputGate symbol P
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-P, -X*X+Y}
H = (1 - T) * F + T * G
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gP = matrix{{P}}

debug NumericalAlgebraicGeometry
PHS = gateHomotopySystem(gH,gV,T,Parameters=>gP)
HS = specialize(PHS,matrix{{1_CC}})
x0 = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{x0},Software=>M2)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
///

beginDocumentation()
{* run

restart
installPackage "SLPexpressions"

to see missing docs.
 
-------- TEMPLATE ------------------
document {
    Key => {,},
    Headline => "",
    Usage => "",
    Inputs => { ""=>"" },
    Outputs => { "" },
    "", 
    EXAMPLE lines ///
    ///,
    Caveat => {"" },
    SeeAlso=>{()}
    }
*}

document {
    Key => {Gate,InputGate,SumGate,ProductGate},
    "Some basic gates:",
    UL{
	{TO InputGate, " is constructed with ", TO inputGate, TT " name", ", if ", TT "name", 
	    " is a number then this gate is assumed to be constant"},
	{TO SumGate, " is constructed with ", TO sumGate, TT " {list of gates}", " or ", TO (symbol +,Gate,Gate)},
	{TO ProductGate, " is constructed with ", TO productGate, TT " {list of gates}", " or ", TO (symbol *,Gate,Gate)}	 
	},  
    Headline => "an expression that is a part of an evaluation circuit (abstract type)"
    }
document {
    Key => {GateMatrix},
    Headline => "a matrix of Gates",
    "An object of this type is a matrix with Gates as entries. 
    Some algebraic operations (matrix multiplication, determinant, etc.) are defined for this type. 
    It is provided, in part, for convenience of setting up involved evaluation circuits.",
    SeeAlso=>{Gate, Matrix}
    }
