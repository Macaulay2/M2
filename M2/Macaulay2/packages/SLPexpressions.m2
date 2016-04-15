-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

newPackage select((
     "SLPexpressions",
     Version => "1.9",
     Date => "Apr 2016",
     Headline => "Straight Line Programs and Algebraic Circuits",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => false,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"}
	  },
     Configuration =>  {},	
     PackageExports => {"NAGtypes"},
     PackageImports => {},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     --DebuggingMode => true
     DebuggingMode => false
     ), x -> x =!= null)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
    "Gate", 
    "GateMatrix", 
    "InputGate", 
    "oneGate", "minusOneGate", "zeroGate",
    "SumGate", "ProductGate", "DetGate", "DivideGate",
    "inputGate", "sumGate", "productGate", "detGate", 
    "constants",  
    "printAsSLP",
    "ValueHashTable","valueHashTable"
    }
exportMutable {
    }
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

getGateCatalogCount = () -> gateCatalogCount  

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
valueHashTable (List,List) := (a,b) -> new ValueHashTable from hashTable (apply(a,b,identity) | {(cache,new CacheTable)})
			
value (InputGate,ValueHashTable) := (g,h)-> if h.cache#?g then h.cache#g else h.cache#g = (if isConstant g then g.Name else h#g)
value (SumGate,ValueHashTable) :=  (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (sum apply(g.Inputs, a->value(a,h)))
value (ProductGate,ValueHashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (product apply(g.Inputs, a->value(a,h)))
value (DetGate,ValueHashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (det matrix applyTable(g.Inputs, a->value(a,h)))
value (DivideGate,ValueHashTable) := (g,h) -> if h.cache#?g then h.cache#g else h.cache#g = (value(first g.Inputs,h)/value(last g.Inputs,h))

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

depth InputGate := g -> 0
depth SumGate := memoize (g -> g.Inputs//depth + 1)
depth ProductGate := memoize (g -> g.Inputs//depth + 1)
depth DivideGate := memoize (g -> g.Inputs//depth + 1)
depth DetGate := memoize (g -> g.Inputs//flatten//depth + 1)
depth GateMatrix := memoize (M -> depth flatten entries M)
depth List := memoize (L -> L/depth//max)

flattenGates = method()
flattenGates InputGate := g -> {}
flattenGates SumGate := memoize (g -> g.Inputs//unique)
flattenGates ProductGate := memoize (g -> g.Inputs//unique)
flattenGates DivideGate := memoize (g -> g.Inputs//unique)
flattenGates DetGate := memoize (g -> g.Inputs//flatten//unique)
flattenGates GateMatrix := memoize (M -> flattenGates flatten entries M)
flattenGates List := memoize (L -> L/flattenGates//flatten//unique)


diff (InputGate, InputGate) := (x,y) -> if y === x then oneGate else zeroGate
diff (InputGate, SumGate) := (x,g) -> g.Inputs/(s->diff(x,s))//sumGate
diff (InputGate, ProductGate) := (x,g) -> sumGate apply(#g.Inputs, i->(
	dgi := diff(x,g.Inputs#i);
	productGate drop(g.Inputs,{i,i}) * dgi -- commutativity assumed
	))
diff (InputGate, DetGate) := (x,g) -> sumGate apply(#g.Inputs, i->(
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
sub (Gate, GateMatrix, GateMatrix) := (g,A,B) -> (
    if numcols A != numcols B or numrows A != numrows B 
    then error "matrices are of different shape";
    sub(g, apply(flatten entries A, flatten entries B, (a,b)->a=>b))
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
    sumGate c
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
    productGate c
    )


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

positionsOfInputGates = method()
positionsOfInputGates(List, RawSLProgram) := (L,s) -> apply(L, g->g.cache#s)

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
(consts,indets):=(positionsOfInputGates({C},s), positionsOfInputGates({X},s))
eQQ = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_QQ}})
output = mutableMatrix(QQ,1,4)
rawSLEvaluatorEvaluate(eQQ, raw mutableMatrix{{7_QQ}}, raw output) 
output
eCC = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_CC}})
output = mutableMatrix(CC,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_CC}}, raw output) 
output
R = CC_1000
eCC = rawSLEvaluator(s,consts,indets,raw mutableMatrix{{3_R}})
rawM = mutableMatrix(R,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_R}}, raw rawM) 
assert (abs(last flatten entries rawM - 37/3) < 2^(-999))
///

-------------------------------------
-- GateMatrix is NOT A GATE
-- GateMatrix = new Type of List (actually a table) 
-------------------------------------

old'matrix'List = lookup(matrix,List)
gateMatrix = method()
gateMatrix GateMatrix := M -> M
gateMatrix List := L -> (
    if not isTable L then error "a table is expected";
    new GateMatrix from applyTable(L,x->if instance(x,Gate) then x else inputGate x) 
    )
gateMatrix Matrix := M -> if numcols M == 0 then gateMatrix toList (numrows M:{}) else gateMatrix entries M

matrix List := o -> L -> (
    fL := flatten L;
    if #fL>0 and any(fL, g->instance(g,Gate)) 
    then gateMatrix L
    else (old'matrix'List o) L
    )

GateMatrix_Sequence := (M,ab) -> ( (a,b):=ab; M#a#b )
GateMatrix_List := (M,cols) -> gateMatrix transpose (transpose entries M)_cols
GateMatrix^List := (M,rows) -> gateMatrix (entries M)_rows

submatrix(GateMatrix,List,List) := (M,a,b) -> gateMatrix apply(a,i->(M#i)_b)

entries GateMatrix := M -> toList M  
transpose GateMatrix := M -> gateMatrix transpose entries M

numcols GateMatrix := M -> # first M
numrows GateMatrix := M -> # M

GateMatrix | GateMatrix := (A,B) -> (
    if numrows A != numrows B then error "need the same number of rows to join";
    gateMatrix transpose (transpose entries A | transpose entries B)      
    )
Matrix | GateMatrix := (A,B) -> gateMatrix A | B
GateMatrix | Matrix := (A,B) -> A | gateMatrix B

GateMatrix || GateMatrix := (A,B) -> (
    if numcols A != numcols B then error "need the same number of columns to stack";
    gateMatrix (entries A | entries B)      
    )
Matrix || GateMatrix := (A,B) -> gateMatrix A || B
GateMatrix || Matrix := (A,B) -> A || gateMatrix B

GateMatrix * GateMatrix := (A,B) -> ( -- two tables
    B' := transpose B;
    matrix table(#A,#B',(i,j)->sumGate apply(A#i,B'#j,(a,b)->a*b))
    )
Matrix * GateMatrix := (A,B) -> gateMatrix A * B
GateMatrix * Matrix := (A,B) -> A * gateMatrix B

RingElement * GateMatrix := (a,B) -> matrix applyTable(entries B, x->a*x) 
GateMatrix * RingElement := (A,b) -> matrix applyTable(entries A, x->x*b) 
Gate * Matrix := (a,B) -> matrix applyTable(entries B, x->a*x) 
Matrix * Gate := (A,b) -> matrix applyTable(entries A, x->x*b) 

GateMatrix + GateMatrix := (A,B) -> gateMatrix (entries A + entries B)
Matrix + GateMatrix := (A,B) -> gateMatrix A + B
GateMatrix + Matrix := (A,B) -> A + gateMatrix B

GateMatrix - GateMatrix := (A,B) -> gateMatrix (entries A - entries B)
Matrix - GateMatrix := (A,B) -> gateMatrix A - B
GateMatrix - Matrix := (A,B) -> A - gateMatrix B

det GateMatrix := o -> M -> detGate applyTable(M, a->if instance(a,Gate) then a else inputGate a)

compress GateMatrix := M -> gateMatrix applyTable(M,compress)

value(GateMatrix, ValueHashTable) := (M,H) -> matrix applyTable(M,g->value(g,H))

sub (GateMatrix, List) := (M,L) -> matrix applyTable(M,g->sub(g,L))
sub (GateMatrix, GateMatrix, GateMatrix) := (M,A,B) -> matrix applyTable(M,g->sub(g,A,B))

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


--fill m x n matrix with values from another matrix
matrix (Matrix,ZZ,ZZ) := o -> (M,m,n) -> (
    R := ring M;
    e := flatten entries M;  
    map(R^m,R^n,(i,j)->e#(n*i+j)) 
    )
matrix (Ring,RawMatrix,ZZ,ZZ) := o -> (R,M,m,n) -> (
    e := flatten entries M;  
    map(R^m,R^n,(i,j)->e#(n*i+j)) 
    )

Evaluator = new Type of MutableHashTable
makeEvaluator = method()
makeEvaluator(GateMatrix,GateMatrix) := (M,I) -> (
    consts := constants M;
    E := new Evaluator from {
    	"rawSLP"=>makeSLProgram(I,M),
    	};
    E#"constant positions" = positionsOfInputGates(consts,E#"rawSLP");
    E#"input positions" = positionsOfInputGates(flatten entries I,E#"rawSLP");
    E#"constants" = matrix{consts/(c->c.Name)}; -- conceptually: constants should be anything that can be evaluated to any precision
    E
    )

rawSLEvaluatorK = method()
rawSLEvaluatorK Evaluator := E -> rawSLEvaluatorK(E,ring E#"constants")
rawSLEvaluatorK (Evaluator,Ring) := (E,K) -> if E#?K then E#K else E#K = rawSLEvaluator(
    E#"rawSLP", E#"constant positions", E#"input positions",
    raw mutableMatrix promote(E#"constants",K)
    );
  
evaluate(Evaluator, MutableMatrix, MutableMatrix) := (E,I,O) -> (
    K := ring I; 
    assert(ring O === K);
    rawSLEvaluatorEvaluate(rawSLEvaluatorK(E,K), raw I, raw O);
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
E = makeEvaluator(matrix{{XXC,detXCCX,XoC,XpC+2}},matrix{{C,X}}) 
inp = mutableMatrix{{1.2,-1}}
out = mutableMatrix(ring inp,1,4)
evaluate(E,inp,out)
assert(clean_0.001(out - mutableMatrix {{1.2, -.44, -.833333, 2.2}})==0)  
inp = mutableMatrix{{1.2,ii+2}}
out = mutableMatrix(ring inp,1,4)
evaluate(E,inp,out)
assert(clean_0.001(out - mutableMatrix {{3.6+4.8*ii, 1.56+4*ii, 1.66667+.833333*ii, 5.2+ii}})==0)  
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
    Key => {SLPexpressions},
    Headline => "Straight Line Programs and expressions for evaluation circuits",
    "", 
    EXAMPLE lines ///
    X = inputGate x
    f = X + 1
    n = 12;
    for i from 1 to n do f = f*f -- f = (x+1)^(2^n)
    time A = value(f,valueHashTable({X},{1}))  
    ZZ[y];
    time B = sub((y+1)^(2^n),{y=>1})    
    A == B
    ///,
    SeeAlso=>{"NumericalAlgebraicGeometry",NAGtypes}
    }

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
    PARA {
	"The package ", TO SLPexpressions, " overrides ", TO matrix, " to allow a table (a nested list) of ", TO Gate,"s as an argument." 
	},    
    EXAMPLE lines ///
    X = inputGate x; Y = inputGate y; 
    A = matrix { apply(5,i->i*X) }
    B = matrix { apply(4,i->Y^i) }
    C = transpose A * B    
    numrows C, numcols C
    ///,
    SeeAlso=>{Gate, Matrix}
    }

undocumented {
"zeroGate",
inputGate,
(inputGate,Thing),
sumGate,
(sumGate,List),
printAsSLP,
(printAsSLP,GateMatrix),
(printAsSLP,List),
productGate,
(productGate,List),
detGate,
(detGate,List),
DetGate,
"minusOneGate",
DivideGate,
valueHashTable,
(valueHashTable,List,List),
ValueHashTable,
constants,
(constants,DetGate),
(constants,DivideGate),
(constants,GateMatrix),
(constants,InputGate),
(constants,List),
(constants,ProductGate),
(constants,SumGate),
"oneGate",
(symbol -,Gate),
(symbol *,CC,Gate),
(symbol +,CC,Gate),
(symbol -,CC,Gate),
(compress,Gate),
(compress,GateMatrix),
(compress,ProductGate),
(compress,SumGate),
(depth,DetGate),
(depth,DivideGate),
(depth,GateMatrix),
(depth,InputGate),
(depth,ProductGate),
(depth,SumGate),
(determinant,GateMatrix),
(diff,GateMatrix,GateMatrix),
(diff,InputGate,DetGate),
(diff,InputGate,DivideGate),
(diff,InputGate,GateMatrix),
(diff,InputGate,InputGate),
(diff,InputGate,ProductGate),
(diff,InputGate,SumGate),
(entries,GateMatrix),
(symbol *,Gate,CC),
(symbol *,Gate,Gate),
(symbol *,Gate,Matrix),
(symbol *,Gate,QQ),
(symbol *,Gate,RR),
(symbol *,Gate,ZZ),
(symbol +,Gate,CC),
(symbol +,Gate,Gate),
(symbol +,Gate,QQ),
(symbol +,Gate,RR),
(symbol +,Gate,ZZ),
(symbol -,Gate,CC),
(symbol -,Gate,Gate),
(symbol -,Gate,QQ),
(symbol -,Gate,RR),
(symbol -,Gate,ZZ),
(symbol /,Gate,Gate),
(symbol ^,Gate,ZZ),
(symbol *,GateMatrix,GateMatrix),
(symbol *,GateMatrix,Matrix),
(symbol *,GateMatrix,RingElement),
(symbol +,GateMatrix,GateMatrix),
(symbol +,GateMatrix,Matrix),
(symbol -,GateMatrix,GateMatrix),
(symbol -,GateMatrix,Matrix),
(symbol ^,GateMatrix,List),
(symbol _,GateMatrix,List),
(symbol _,GateMatrix,Sequence),
(symbol |,GateMatrix,GateMatrix),
(symbol |,GateMatrix,Matrix),
(symbol ||,GateMatrix,GateMatrix),
(symbol ||,GateMatrix,Matrix),
(isConstant,InputGate),
(symbol *,Matrix,Gate),
(symbol *,Matrix,GateMatrix),
(symbol +,Matrix,GateMatrix),
(symbol -,Matrix,GateMatrix),
(symbol |,Matrix,GateMatrix),
(symbol ||,Matrix,GateMatrix),
(net,DetGate),
(net,DivideGate),
(net,InputGate),
(net,ProductGate),
(net,SumGate),
(numColumns,GateMatrix),
(numRows,GateMatrix),
(symbol *,QQ,Gate),
(symbol +,QQ,Gate),
(symbol -,QQ,Gate),
(symbol *,RingElement,GateMatrix),
(symbol *,RR,Gate),
(symbol +,RR,Gate),
(symbol -,RR,Gate),
(submatrix,GateMatrix,List,List),
(substitute,DetGate,Option),
(substitute,DivideGate,Option),
(substitute,Gate,GateMatrix,GateMatrix),
(substitute,Gate,List),
(substitute,GateMatrix,GateMatrix,GateMatrix),
(substitute,GateMatrix,List),
(substitute,InputGate,Option),
(substitute,ProductGate,Option),
(substitute,SumGate,Option),
(support,DetGate),
(support,DivideGate),
(support,GateMatrix),
(support,InputGate),
(support,ProductGate),
(support,SumGate),
(transpose,GateMatrix),
(value,DetGate,ValueHashTable),
(value,DivideGate,ValueHashTable),
(value,GateMatrix,ValueHashTable),
(value,InputGate,ValueHashTable),
(value,ProductGate,ValueHashTable),
(value,SumGate,ValueHashTable),
(symbol *,ZZ,Gate),
(symbol +,ZZ,Gate),
(symbol -,ZZ,Gate)
    }

end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "SLPexpressions"
installPackage "SLPexpressions"
installPackage ("SLPexpressions",RerunExamples=>true, RemakeAllDocumentation=>true)
installPackage ("SLPexpressions",RerunExamples=>false, RemakeAllDocumentation=>true)

-- (old way) installPackage("SLPexpressions", SeparateExec=>true, AbsoluteLinks=>false)

-- install docs with no absolute links
uninstallPackage "Style"
installPackage("Style", AbsoluteLinks=>false)
installPackage("SLPexpressions", AbsoluteLinks=>false)

installPackage ("SLPexpressions", MakeDocumentation=>false)
check "SLPexpressions"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SLPexpressions "
-- End:
