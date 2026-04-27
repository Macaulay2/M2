-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

-- TODO: compress (with CacheTable)

newPackage(
     "SLPexpressions",
     Version => "1.21",
     Date => "Nov 2022",
     Headline => "straight line programs and algebraic circuits",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", 
	      Email => "leykin@math.gatech.edu",
	      HomePage => "https://people.math.gatech.edu/~aleykin3"
	      },
	  {Name => "Timothy Duff", 
                Email => "tduff3@gatech.edu",
                HomePage => "http://people.math.gatech.edu/~tduff3"
                },
	  {Name => "Justin Chen",
                Email => "jchen646@math.gatech.edu",
                HomePage => "https://people.math.gatech.edu/~jchen646"
                },
	  {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/~mike"
                }
	  },
     Configuration =>  {},	
     Keywords => {"Numerical Algebraic Geometry"},
     PackageExports => {"NAGtypes"},
     PackageImports => {},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     --DebuggingMode => true
     DebuggingMode => false
     )

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
    "Gate", 
    "GateMatrix", "gateMatrix",
    "InputGate", "declareVariable", "undeclareVariable", 
    "SumGate", "ProductGate", "DetGate", "DivideGate",
    "inputGate", "sumGate", "productGate", "detGate", 
    "constants",
    "countGates",  
    "printAsSLP", "cCode",
    "getVarGates", "gatePolynomial",
    "ValueHashTable","valueHashTable",
    "SLProgram", "InterpretedSLProgram", "makeInterpretedSLProgram",
    "setTryJustInTimeCompilation", "makeSLProgram",
    "CompiledSLProgram", "makeCompiledSLProgram"
    }
setTryJustInTimeCompilation = method()
setTryJustInTimeCompilation Boolean := v -> if v then (
    w := (run "gcc --version" == 0);
    if w then (
	print "-- SLPexpressions: Found `gcc`. Just-in-time compilation will be attempted by `makeSLProgram`.";
    	print "-- This is an experimental feature that works only for evaluation over real and complex numbers.";
	print "-- (To disable, `setTryJustInTimeCompilation false`)" 
	) else (
	print "-- SLPexpressions: Couldn't find `gcc` --- `makeSLProgram` will output `InterpretedSLProgram`.";
	);
    	TryJustInTimeCompilation = w
    ) else TryJustInTimeCompilation = false

TryJustInTimeCompilation = false
--setTryJustInTimeCompilation true 

exportMutable {
    }
debug Core 


concatenateNets = method()
concatenateNets List := L -> (
    result := net "";
    for a in L do result = result | net a;
    result
    )

Gate = new Type of HashTable
GateMatrix = new Type of List
InputGate = new Type of Gate -- "abstract" unit of input  

isConstant InputGate := a -> (instance(a.Name,Number) or instance(a.Name, RingElement))

gateCatalog = new MutableHashTable -- records constant gates (to avoid duplication)
gateCatalogCount = new MutableHashTable -- count for each gate
add2GC = method()
add2GC InputGate := g -> if isConstant g then (
    if gateCatalog#?g then (
    	gateCatalogCount#g = gateCatalogCount#g + 1;
    	gateCatalog#g -- value = stored identical gate 
    	) else (
    	gateCatalogCount#g = 1;
    	gateCatalog#g = g
    	)
    ) else g	 	  
add2GC Gate := g -> g
getGateCatalogCount = () -> gateCatalogCount  

inputGate = method()
inputGate Thing := a -> add2GC new InputGate from {
    Name => a
    }
toExternalString InputGate := a -> "inputGate(" | toExternalString a.Name | ")"  
 
net InputGate := g -> net g.Name

oneGate = inputGate 1
minusOneGate = inputGate(-1)
zeroGate = inputGate 0

declareVariable = method()
declareVariable Symbol :=  
declareVariable IndexedVariable := g -> (g <- inputGate g) 
declareVariable InputGate := g -> g
declareVariable Thing := g -> error "defined only for a Symbol or an IndexedVariable" 
-- syntactic sugar for declareVariable \ {symbols}
vars IndexedVariable := x -> declareVariable x
vars Symbol := x -> declareVariable x
vars InputGate := x -> x
InputGate .. InputGate := (A, B) -> value \ (A.Name .. B.Name)

undeclareVariable = method()
undeclareVariable InputGate := g -> 
if member(class g.Name, {Symbol, IndexedVariable}) 
then g.Name <- g.Name else error "expected the InputGate's Name to be a Symbol or an IndexedVariable" 

SumGate = new Type of Gate
net SumGate := g -> concatenateNets( {"("} | between(" + ", g.Inputs) | {")"} )
Gate + Gate := (a,b) -> add2GC (
    if a===zeroGate then b else 
    if b===zeroGate then a else 
    new SumGate from {
      	Inputs => {a,b}
      	} 
    )
sumGate = method()
sumGate List := L -> add2GC(
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new SumGate from {Inputs=>L}
    )
 
ProductGate = new Type of Gate
net ProductGate := g -> concatenateNets( {"("} | between(" * ", g.Inputs) | {")"} )
Gate * Gate := (a,b) -> add2GC (
    if a===zeroGate or b===zeroGate then zeroGate else 
    if a===oneGate then b else 
    if b===oneGate then a else 
    new ProductGate from {
      	Inputs => {a,b}
      	}	   
    )

productGate = method()
productGate List := L -> add2GC(
    if not all(L, a->instance(a,Gate)) 
    then error "expected a list of gates";
    new ProductGate from {Inputs=>L}
    )
Gate ^ ZZ := (g,n) -> if n == 0 then oneGate else 
    if n > 0 then productGate toList(n:g) else -- inefficient!!!
    -*(n < 0)*-   oneGate / productGate toList(n:g) 

- Gate := g -> minusOneGate*g
Gate - Gate := (a,b) -> a+(-b)

Number + Gate := (a,b) -> inputGate a + b
Gate + Number  := (a,b) -> a + inputGate b
Number * Gate := (a,b) -> inputGate a * b
Gate * Number  := (a,b) -> a * inputGate b
Number - Gate := (a,b) -> inputGate a - b
Gate - Number  := (a,b) -> a - inputGate b

RingElement + Gate := (a,b) -> inputGate a + b
Gate + RingElement  := (a,b) -> a + inputGate b
RingElement * Gate := (a,b) -> inputGate a * b
Gate * RingElement  := (a,b) -> a * inputGate b
RingElement - Gate := (a,b) -> inputGate a - b
Gate - RingElement  := (a,b) -> a - inputGate b

DetGate = new Type of Gate
net DetGate := g -> concatenateNets {"det", MatrixExpression applyTable(g.Inputs,net)}
detGate = method()
detGate GateMatrix := M -> detGate entries M 
detGate List := L -*doubly nested list*- -> add2GC(
    n := #L;
    if not all(L, a->instance(a,List) and #a==n and all(a,b->instance(b,Gate)))
    then error "expected a square matrix (a doubly nested list) of gates";
    new DetGate from {Inputs=>L}
    )

DivideGate = new Type of Gate
net DivideGate := g -> net Divide(first g.Inputs,last g.Inputs) 
divideGate = method()
divideGate List := L -> divideGate(L#0,L#1)
divideGate (Gate, Gate) := (a,b) -> add2GC(
    if b===zeroGate then error "division by zero"  else 
    if a===zeroGate then zeroGate else 
    new DivideGate from {
      	Inputs => {a,b}
      	}
    ) 
Gate / Gate := (a,b) -> divideGate(a,b)
    
ValueHashTable = new Type of MutableHashTable
valueHashTable = method()
valueHashTable (List,List) := (a,b) -> new ValueHashTable from apply(a,b,identity)

value (Gate,ValueHashTable) := (g,h) -> error "undefined for abstract type Gate"
value (InputGate,ValueHashTable) := (g,h) -> if h#?g then h#g else h#g = (if isConstant g then g.Name else error "value for inputGate is not set")
value (SumGate,ValueHashTable) :=  (g,h) -> if h#?g then h#g else h#g = (sum apply(g.Inputs, a->value(a,h)))
value (ProductGate,ValueHashTable) := (g,h) -> if h#?g then h#g else h#g = (product apply(g.Inputs, a->value(a,h)))
value (DetGate,ValueHashTable) := (g,h) -> if h#?g then h#g else h#g = (det matrix applyTable(g.Inputs, a->value(a,h)))
value (DivideGate,ValueHashTable) := (g,h) -> if h#?g then h#g else h#g = (value(first g.Inputs,h)/value(last g.Inputs,h))

support InputGate := 
support SumGate := 
support ProductGate := 
support DivideGate := 
support DetGate := g -> findSupport(g, new MutableHashTable) 
support List := L -> (
    t := new MutableHashTable;
    L/(g->findSupport(g,t))//flatten//unique 
    ) 
support GateMatrix := M -> support flatten entries M

findSupport = method(TypicalValue=>List)
findSupport (InputGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = if isConstant g then {} else {g}
findSupport (ProductGate,MutableHashTable) := 
findSupport (DivideGate,MutableHashTable) := 
findSupport (SumGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = g.Inputs/(i->findSupport(i,t))//flatten//unique
findSupport (DetGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = (flatten g.Inputs)/(i->findSupport(i,t))//flatten//unique

constants = method(TypicalValue=>List)
constants InputGate := 
constants SumGate := 
constants ProductGate := 
constants DivideGate := 
constants DetGate := g -> findConstants(g, new MutableHashTable) 
constants List := L -> (
    t := new MutableHashTable;
    L/(g->findConstants(g,t))//flatten//unique 
    ) 
constants GateMatrix := M -> constants flatten entries M

findConstants = method()
findConstants (InputGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = if isConstant g then {g} else {}
findConstants (ProductGate,MutableHashTable) := 
findConstants (DivideGate,MutableHashTable) := 
findConstants (SumGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = g.Inputs/(i->findConstants(i,t))//flatten//unique
findConstants (DetGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = (flatten g.Inputs)/(i->findConstants(i,t))//flatten//unique

depth InputGate := 
depth SumGate := 
depth ProductGate := 
depth DivideGate := 
depth DetGate := g -> findDepth(g, new MutableHashTable) 
depth List := L -> (
    t := new MutableHashTable;
    L/(g->findDepth(g,t))//max
    ) 
depth GateMatrix := M -> depth flatten entries M

findDepth = method()
findDepth (InputGate,MutableHashTable) := (g,t) -> 0 
findDepth (ProductGate,MutableHashTable) := 
findDepth (DivideGate,MutableHashTable) := 
findDepth (SumGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = 1 + g.Inputs/(i->findDepth(i,t))//max
findDepth (DetGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = 1 + (flatten g.Inputs)/(i->findDepth(i,t))//max

countGates = method()
countGates GateMatrix := M -> (
    t := new MutableHashTable from {cache=>new CacheTable};
    scan(flatten entries M, g->findTally(g,t));
    new HashTable from t
    ) 
countGates Gate := g -> (
    t := new MutableHashTable from {cache=>new CacheTable};
    findTally(g,t);
    new HashTable from t
    ) 

findTally = method()
findTally (InputGate,MutableHashTable) := (g,t) -> if t.cache#?g then t.cache#g = t.cache#g+1 else (
    t.cache#g = 1;
    if not t#?(class g) then t#(class g) = 1 else t#(class g)= t#(class g) + 1;
    ) 
findTally (ProductGate,MutableHashTable) := 
findTally (DivideGate,MutableHashTable) := 
findTally (SumGate,MutableHashTable) := (g,t) -> if t.cache#?g then t.cache#g = t.cache#g+1 else (
    t.cache#g = 1;
    if not t#?(class g) then t#(class g) = 1 else t#(class g)= t#(class g) + 1;
    scan(g.Inputs,i->findTally(i,t))
    )
findTally (DetGate,MutableHashTable) := (g,t) -> if t.cache#?g then t.cache#g = t.cache#g+1 else (
    t.cache#g = 1;
    if not t#?(class g) then t#(class g) = 1 else t#(class g)= t#(class g) + 1;
    scan(flatten g.Inputs,i->findTally(i,t))
    )

TEST ///
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C

support(X*X)
support(detXCCX + X)
support gateMatrix{{detXCCX,X}}

assert(set support(C*X) === set support(X*C))

constants(X*(1*X+2))
constants(3*detXCCX + 2*X)
constants gateMatrix{{detXCCX,12*X}}

depth(X*X)
depth(detXCCX + X)
depth gateMatrix{{detXCCX,X}}
assert (depth {X+((detXCCX+X)*X)/C}==5)

countGates matrix{{detXCCX,12*X}}
///

diff (InputGate, Gate) := (x,g) -> (
    t := new MutableHashTable;
    diffMemoize(x,g,t)
    )
diff (InputGate, GateMatrix) := (x,M) -> gateMatrix applyTable(entries M, g->diff(x,g))
diff (GateMatrix, GateMatrix) := (xx,M) -> joinVertical apply(
    applyTable(entries xx, x->diff(x,M)), 
    row-> joinHorizontal row
    )

diffMemoize = method()
diffMemoize (InputGate, InputGate, MutableHashTable) := (x,y,t) -> if y === x then oneGate else zeroGate
diffMemoize (InputGate, SumGate, MutableHashTable) := (x,g,t) -> if t#?g then t#g else t#g = g.Inputs/(s->diffMemoize(x,s,t))//sumGate
diffMemoize (InputGate, ProductGate, MutableHashTable) := (x,g,t) -> if t#?g then t#g else t#g = sumGate apply(#g.Inputs, i->(
	dgi := diffMemoize(x,g.Inputs#i,t);
	productGate drop(g.Inputs,{i,i}) * dgi -- commutativity assumed
	))
diffMemoize (InputGate, DetGate, MutableHashTable) := (x,g,t) -> if t#?g then t#g else t#g = sumGate apply(#g.Inputs, i->(
	dgi := apply(g.Inputs#i, a->diffMemoize(x,a,t));
	detGate replace(i,dgi,g.Inputs)
	))
diffMemoize (InputGate, DivideGate, MutableHashTable) := (x,g,t) -> if t#?g then t#g else t#g = (
    a := first g.Inputs;
    b := last g.Inputs;	
    da := diffMemoize(x,a,t);
    db := diffMemoize(x,b,t);
    if db===zeroGate then da/b else (da*b-a*db)/(b*b)
    )

sub (GateMatrix,HashTable) := (M,s) -> (
    if not all(keys s, k->instance(k, InputGate)) then error "only an InputGate can be substituted";
    if not all(values s, v->instance(v, Gate)) then error "can substitute with a Gate only";
    t := new MutableHashTable;
    matrix makeSub(entries M, s, t)
    )
sub (GateMatrix, GateMatrix, GateMatrix) := (M,A,B) -> (
    if numcols A != numcols B or numrows A != numrows B 
    then error "matrices are of different shape";
    sub(M, apply(flatten entries A, flatten entries B, (a,b)->a=>b))
    )
sub (GateMatrix, List) := (M,L) -> sub(M,new HashTable from L)
sub (GateMatrix, Option) := (M,ab) -> sub(M,{ab})

makeSub = method(TypicalValue=>List)
makeSub (InputGate,HashTable,MutableHashTable) := (g,s,t) -> if s#?g then s#g else g
makeSub (ProductGate,HashTable,MutableHashTable) := (g,s,t) -> if t#?g then t#g else t#g = productGate apply(g.Inputs, i->makeSub(i,s,t))
makeSub (DivideGate,HashTable,MutableHashTable) := (g,s,t) -> if t#?g then t#g else t#g =  divideGate apply(g.Inputs, i->makeSub(i,s,t))
makeSub (SumGate,HashTable,MutableHashTable) := (g,s,t) -> if t#?g then t#g else t#g = sumGate apply(g.Inputs, i->makeSub(i,s,t))
makeSub (DetGate,HashTable,MutableHashTable) := (g,s,t) -> if t#?g then t#g else t#g = detGate applyTable(g.Inputs, i->makeSub(i,s,t))
makeSub (List,HashTable,MutableHashTable) := (L,s,t) -> apply(L,g->makeSub(g,s,t))

compress Gate := g -> (
    t := new MutableHashTable;
    compressMemoize(g,t)
    )
compress GateMatrix := M -> (
    t := new MutableHashTable;
    gateMatrix applyTable(M,g->compressMemoize(g,t))
    )

compressMemoize = method()
compressMemoize (Gate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = g
compressMemoize (SumGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = (
    L := g.Inputs/(h->compressMemoize(h,t));
    nums := positions(L, a -> instance(a,InputGate) and isConstant a);
    not'nums := toList(0..<#L) - set nums;
    s := L_nums/(a->a.Name)//sum;
    c := (if s != 0 then {inputGate s} else {}) | L_not'nums;
    if #c == 0 then zeroGate else
    if #c == 1 then first c else 
    sumGate c
    )
compressMemoize (ProductGate,MutableHashTable) := (g,t) -> if t#?g then t#g else t#g = (
    L := g.Inputs/(h->compressMemoize(h,t));
    nums := positions(L, a -> instance(a,InputGate) and isConstant a);
    not'nums := toList(0..<#L) - set nums;
    p := L_nums/(a->a.Name)//product;
    if p==0 then return zeroGate;
    c := (if p != 1 then {inputGate p} else {}) | L_not'nums; -- assumes commutativity
    if #c == 0 then oneGate else
    if #c == 1 then first c else 
    productGate c
    )

getVarGates = method()
getVarGates PolynomialRing := R -> if R#?"var gates" then R#"var gates" else R#"var gates" = apply(gens R, v->inputGate [v])

gatePolynomial = method()
gatePolynomial RingElement := p -> (
    -- one naive way of converting a sparse polynomial to a circuit  
    X := getVarGates ring p;
    sumGate apply(listForm p, mc->(
	    (m,c) := mc;
	    c*product(#m,i->X#i^(m#i))
	    ))
    )
gatePolynomial Matrix := F -> gateMatrix applyTable(entries F, gatePolynomial) 

TEST ///
needsPackage "SLPexpressions"
R = QQ[x,y]
f = random(3,R)
gf = gatePolynomial f
printAsSLP(getVarGates R,{gf})
slp = makeInterpretedSLProgram(getVarGates R,{gf})
assert(evaluate(slp,vars R)==f)
///	

--------------------------------------------------
-- (Raw)SLProgram routines
--------------------------------------------------
SLProgram = new Type of HashTable -- abstract type
errorSLProgramAbstract := () -> error "not implemented (SLProgram is an abstract Type)"
evaluate(SLProgram, MutableMatrix, MutableMatrix) := (slp,I,O) -> errorSLProgramAbstract() 
evaluate(SLProgram, Matrix) := (slp, inp) -> (
		I := mutableMatrix inp;
		O := mutableMatrix(ring I, 1, numberOfOutputs slp);
		evaluate(slp,I,O);
		matrix O
		)
numberOfInputs = method()
numberOfInputs SLProgram := slp -> errorSLProgramAbstract()
numberOfOutputs = method()
numberOfOutputs SLProgram := slp -> errorSLProgramAbstract()

InterpretedSLProgram = new Type of SLProgram
CompiledSLProgram = new Type of SLProgram
 
-------------------
makeSLProgram = method()
makeSLProgram (List,List) := (inL,outL) -> (
    if TryJustInTimeCompilation then  
    makeCompiledSLProgram else makeInterpretedSLProgram
    ) (inL,outL)
makeSLProgram (GateMatrix,GateMatrix) := (inM,outM) -> makeSLProgram(flatten entries inM, flatten entries outM)
-------------------
makeCompiledSLProgram = method(TypicalValue=>CompiledSLProgram)
makeCompiledSLProgram (List,List) := (inL,outL) -> (
    new CompiledSLProgram from {
	"input" => inL,
	"output" => outL,
	cache => new CacheTable 
	}
    )
makeCompiledSLProgram (GateMatrix,GateMatrix) := (inM,outM) -> makeCompiledSLProgram(flatten entries inM, flatten entries outM)
numberOfInputs CompiledSLProgram := slp -> #(slp#"input")
numberOfOutputs CompiledSLProgram := slp -> #(slp#"output")

-------------------
makeInterpretedSLProgram = method(TypicalValue=>InterpretedSLProgram)
makeInterpretedSLProgram (List,List) := (inL,outL) -> ( 
    s := rawSLProgram(1); -- 1 means nothing anymore
    t := new MutableHashTable;
    varPositions := appendToSLProgram(s,inL,t); 
    out := appendToSLProgram(s,outL,t);
    rawSLPsetOutputPositions(s,out);
    consts := constants outL;
    constantPositions := appendToSLProgram(s,consts,t);
    constantValues := matrix{consts/(c->c.Name)}; -- conceptually: constants should be anything that can be evaluated to any precision
    new InterpretedSLProgram from {
				RawSLProgram => s, 
				"number of inputs" => #inL,
				"number of outputs" => #outL,
				"variable positions" => varPositions,
				"constants" =>  constantValues,
				"constant positions" => constantPositions,
				cache => new CacheTable 
				}
    )
makeInterpretedSLProgram (GateMatrix,GateMatrix) := (inM,outM) -> makeInterpretedSLProgram(flatten entries inM, flatten entries outM)
numberOfInputs SLProgram := slp -> slp#"number of inputs"
numberOfOutputs SLProgram := slp -> slp#"number of outputs"

----------------
appendToSLProgram = method()
appendToSLProgram (RawSLProgram, InputGate, MutableHashTable) := (slp, g, t) -> 
    if t#?g then t#g else t#g = rawSLPInputGate(slp)
appendToSLProgram (RawSLProgram, List, MutableHashTable) := (slp, L, t) -> 
    apply(L,a->appendToSLProgram(slp,a,t))
appendToSLProgram (RawSLProgram, SumGate, MutableHashTable) := (slp, g, t) -> 
    if t#?g then t#g else 
       t#g = rawSLPSumGate(slp, appendToSLProgram(slp,g.Inputs,t))
appendToSLProgram (RawSLProgram, ProductGate, MutableHashTable) := (slp, g, t) -> 
    if t#?g then t#g else 
       t#g = rawSLPProductGate(slp, appendToSLProgram(slp,g.Inputs,t))
appendToSLProgram (RawSLProgram, DetGate, MutableHashTable) := (slp, g, t) -> 
    if t#?g then t#g else 
       t#g = rawSLPDetGate(slp, appendToSLProgram(slp,flatten g.Inputs,t))
appendToSLProgram (RawSLProgram, DivideGate, MutableHashTable) := (slp, g, t) -> 
    if t#?g then t#g else 
       t#g = rawSLPDivideGate(slp, appendToSLProgram(slp,g.Inputs,t))

TEST /// 
-*
restart
needsPackage "SLPexpressions"
*-
debug SLPexpressions
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
s = makeInterpretedSLProgram({C,X},{XXC,detXCCX,XoC,XpC+XoC}) 

debug Core
(consts,indets):=(s#"constant positions",s#"variable positions")
assert(#consts == 0)
(newConsts,newIndets):=(take(indets,1),drop(indets,1))
eQQ = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_QQ}}) -- set C=3_QQ
output = mutableMatrix(QQ,1,4)
rawSLEvaluatorEvaluate(eQQ, raw mutableMatrix{{7_QQ}}, raw output) 
output
eCC = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_CC}})
output = mutableMatrix(CC,1,4)
rawSLEvaluatorEvaluate(eCC, raw mutableMatrix{{7_CC}}, raw output) 
output
R = CC_1000
eCC = rawSLEvaluator(s#RawSLProgram,newConsts,newIndets,raw mutableMatrix{{3_R}})
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
    if not isTable L then error "a table (nested list) is expected";
    new GateMatrix from applyTable(L,x->if instance(x,Gate) then x else 
	if instance(x,List) then error "cowardly refusing to create an InputGate with a List for its name
	(commonly results from having too many braces {{{...}}})"
	else inputGate x
	) 
    )
gateMatrix Matrix := M -> if numcols M == 0 then gateMatrix toList (numrows M:{}) else gateMatrix entries M

matrix GateMatrix := o -> identity
matrix List := o -> L -> (
    fL := flatten toList L;
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

value(GateMatrix, ValueHashTable) := (M,H) -> matrix applyTable(toList M,g->value(g,H))

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

flatten GateMatrix := M -> error "flatten is not defined for GateMatrix"

-------------------------
-- printAsSLP functions
PrintTable = new Type of MutableHashTable
newPrintTable = assignmentSymbol -> (h := new PrintTable; h#"assignment symbol" = assignmentSymbol; h#"#consts"=h#"#vars"=h#"#gates"=h#"#lines"=0; h)
addLine = method()
addLine (PrintTable, Thing) := (h,t) -> ( h#(h#"#lines") = t; h#"#lines" = h#"#lines" + 1; )    
printName = method()
printName (Gate, PrintTable) := (g,h) -> error "not implemented"
printName (InputGate, PrintTable) := (g,h) -> if h#?g then h#g else (
    if isConstant g then (
	h#g = "C"|toString h#"#consts";
	addLine(h,h#g | h#"assignment symbol" | 
	    if instance(g.Name,CC) then "C("|(toString realPart g.Name)|","|(toString imaginaryPart g.Name)|")" 
	    else toString g.Name
	    );
    	h#"#consts" = h#"#consts" + 1;
	)
    else (
	h#g = "X"|toString h#"#vars";
    	h#"#vars" = h#"#vars" + 1;
	);
    h#g
    )
printName (SumGate, PrintTable) := (g,h) -> if h#?g then h#g else (
    s := if #g.Inputs==0 then {"0"} else between(" + ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | h#"assignment symbol" | concatenate s);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (ProductGate, PrintTable) := (g,h) -> if h#?g then h#g else (
    s := if #g.Inputs==0 then {"1"} else between(" * ", apply(g.Inputs, gg->printName(gg,h)));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | h#"assignment symbol" | concatenate s);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (DivideGate, PrintTable) := (g,h) -> if h#?g then h#g else (
    (x,y) := toSequence apply(g.Inputs, gg->printName(gg,h));  
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | h#"assignment symbol" | x | " / " | y);  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )
printName (DetGate, PrintTable) := (g,h) -> if h#?g then h#g else (
    h#g = "G"|toString h#"#gates";
    addLine(h, h#g | " = det " | toString applyTable(g.Inputs, gg->printName(gg,h)));  
    h#"#gates" = h#"#gates" + 1;
    h#g 
    )

printAsSLP = method()
printAsSLP (List, List) := (inputs, outputs) -> (
    h := newPrintTable " <== ";
    scan(inputs, g-> << printName(g,h) << h#"assignment symbol" << g.Name << endl);  
    scan(outputs, g->printName(g,h));
    scan(h#"#lines", i->print h#i);
    print "output:";
    scan(outputs, g-> << printName(g,h) << endl);     
    )
printAsSLP (GateMatrix,GateMatrix) := (I,O) -> printAsSLP (flatten entries I, flatten entries O)

TEST ///
needsPackage "SLPexpressions"
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
printAsSLP ({X,C},{XXC,detXCCX,XoC+1+XpC})
///

-----------------------------------------------
-- cCode functions (use PrintTable from above)

cCode = method()
cCode (GateMatrix,GateMatrix,File):= (M,I,f) -> cCode(flatten entries M, flatten entries I,f)
cCode (List,List,File) := (outputs,inputs,f) -> (
    h := newPrintTable " = ";
    scan(inputs, g->printName(g,h));
    scan(outputs, g->printName(g,h));
    f << "void evaluate(const C* x, C* y) {" << endl;
    scan(h#"#vars", i -> f << ("C X"|i|" = x["|i|"];") << endl);
    scan(h#"#lines", i -> f << "C " << h#i << ";" << endl);
    scan(#outputs, i-> f << ("y["|i|"] = "|printName(outputs#i,h)|";") << endl); 
    f << "}" << endl;
    )
cCode (GateMatrix,GateMatrix) := (M,I) -> cCode(flatten entries M, flatten entries I)
cCode (List,List) := (outputs,inputs) -> cCode(outputs,inputs,stdio)


TEST ///
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C+2
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
cCode (matrix{{XXC,detXCCX,0},{XoC,1,2}},matrix{{X,C}})
///

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

rawSLEvaluatorK = method()
rawSLEvaluatorK (InterpretedSLProgram, Ring) := (slp, K) -> if slp.cache#?K then 
slp.cache#K else slp.cache#K = rawSLEvaluator(
    slp#RawSLProgram, 
    slp#"constant positions", 
    slp#"variable positions",
    raw mutableMatrix promote(slp#"constants",K)
    )
  
evaluate(InterpretedSLProgram, MutableMatrix, MutableMatrix) := (slp,I,O) -> (
		--if numrows I =!= 1 or numrows O =!= 1 then error "expected matrices with 1 row";
		if numrows I * numcols I =!= slp#"number of inputs" then error "wrong number of inputs";
		if numrows O * numcols O =!= slp#"number of outputs" then error "wrong number of outputs";
		K := ring I; 
    if ring O =!= K then error "expected same Ring for input and output";
    rawSLEvaluatorEvaluate(rawSLEvaluatorK(slp,K), raw I, raw O);
    )

rawSLEvaluatorK (CompiledSLProgram, Ring) := (slp, K) -> if slp.cache#?K then 
slp.cache#K else (
    typeName := (
	if K === RR_53 then "double" else 
	if K === CC_53 then "std::complex<double>" else 
    	error ("just-in-time compilation is not implemented for "| toString K) 
    	);
    fname := temporaryFileName() | "-GateSystem";
    cppName := fname | ".cpp";
    --cppName := fname | ".c";
    libName := fname | ".so";
    f := openOut cppName;
    f << "#include <complex>" << endl; 
    f << "std::complex<double> ii(0,1);" << endl;
    f << "typedef " | typeName | " C;" << endl; -- << "extern" << endl; -- the type needs to be adjusted!!!
    cCode (slp#"output", slp#"input", f);
    f << close;
    compileCommand := "g++ -shared -Wall -fPIC -Wextra -O3 -o "| libName | " " | cppName;
    --compileCommand := "gcc -shared -Wall -fPIC -Wextra -o "| libName | " " | cppName;
    print compileCommand;
    if run compileCommand > 0 then error ("error compiling a straightline program:\n"|compileCommand);      
    print get cppName;
    print libName;
    symNames := get ("!nm "|libName); 
    (a,b) := first regex("[0-9a-zA-Z_]*evaluate[0-9a-zA-Z_]*", symNames);
    print ("mangled function name: "|substring(symNames,a,b)); 
    slp.cache#K = rawCompiledSLEvaluator(libName, #(slp#"input"), #(slp#"output"),
	 raw mutableMatrix(K,0,0) -- we need to pass only the field 
	 )
    )

evaluate(CompiledSLProgram, MutableMatrix, MutableMatrix) := (slp,I,O) -> (
		--if numrows I =!= 1 or numrows O =!= 1 then error "expected matrices with 1 row";
		if numrows I * numcols I =!= #(slp#"input") then error "wrong number of inputs";
		if numrows O * numcols O =!= #(slp#"output") then error "wrong number of outputs";
		K := ring I; 
    if ring O =!= K then error "expected same Ring for input and output";
    rawSLEvaluatorEvaluate(rawSLEvaluatorK(slp,K), raw I, raw O);
    )

TEST /// 
X = inputGate symbol X
C = inputGate symbol C
XpC = X+C
XXC = productGate{X,X,C}
detXCCX = detGate{{X,C},{C,X}}
XoC = X/C
slp = makeInterpretedSLProgram(matrix{{C,X}},matrix{{XXC,detXCCX,XoC,XpC+2}}) 
inp = mutableMatrix{{1.2,-1}}
out = mutableMatrix(ring inp,1,4)
evaluate(slp,inp,out)
assert(clean_0.001(out - mutableMatrix {{1.2, -.44, -.833333, 2.2}})==0)  
inp = mutableMatrix{{1.2,ii+2}}
out = mutableMatrix(ring inp,1,4)
evaluate(slp,inp,out)
assert(clean_0.001(out - mutableMatrix {{3.6+4.8*ii, 1.56+4*ii, 1.66667+.833333*ii, 5.2+ii}})==0)  
assert(evaluate(slp, matrix{{1/2,2/3}}) === matrix {{2/9, 7/36, 4/3, 19/6}})
///


TEST /// -- moved from NumericalAlgebraicGeometry/SLP.m2; is it necessary?
needsPackage "SLPexpressions"

--InputGate
X = inputGate symbol X
Y = inputGate symbol Y

--SumGate and ProductGate
C = sumGate {X+Y,Y,X}
D = productGate {X*Y,Y,C}
h = valueHashTable({X,Y},{1,ii})
assert (value(D,h) == product{value(X*Y,h),value(Y,h),value(C,h)})
support (X*X)
support (D+C)

-- one way to handle constants
E = inputGate 2
F = product{E*(X*X+E*Y)+1, inputGate 1}

G = (sub(sub(matrix{{F}},X=>X+Y),Y=>X*Y))_(0,0) 
-- sub and compress = evaluate over a ring
R = CC[x,y] 
H = (sub(sub(matrix{{G}},X=>E),Y=>inputGate(x+2*y)))_(0,0)
I = compress H 

-- DetGate
J = detGate {{X,C,F},{D,Y,E},{G,F,X}}

-- diff
diff(X,F)
diff(X,J)
h = valueHashTable({X,Y},{x,y})
assert(
    value(diff(X,J),h) 
    ==
    diff(x, det matrix applyTable(J.Inputs, i->value(i,h)))
    )

-- DivideGate
G/F
diff(X,X/Y)
diff(Y,X/Y)
h = valueHashTable({X,Y},{2,3})
GY = value(diff(Y,G),h)
FY = value(diff(Y,F),h)
assert ( value(compress diff(Y,G/F), h) == (GY*value(F,h) - value(G,h)*FY)/(value(F,h))^2 )
///


------------------------
-- expression, net, html
expression InputGate := g -> expression g.Name
expression SumGate := g -> Parenthesize sum(g.Inputs,expression)
expression ProductGate := g -> Parenthesize product(g.Inputs,expression)
expression DivideGate := g -> expression g.Inputs#0 / expression g.Inputs#1
html GateMatrix := html @@ expression
eGM:=
expression GateMatrix := g -> new MatrixExpression from applyTable(g,expression)
expression DetGate := g -> (expression det) eGM g.Inputs
printLargeGate = g -> VerticalList { class g, "depth"=>depth g, "#gates"=> countGates g }
net Gate := g -> (
    n := net expression g;
    if depth g < 6 and width n <= printWidth and height n <= printWidth then n
    else net printLargeGate g
    )
html Gate := g -> (
    e := expression g;
    html if depth g < 6 and width net e <= printWidth then e else printLargeGate g
    )

beginDocumentation()
-* run

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
    SeeAlso=>{"NumericalAlgebraicGeometry","NAGtypes"}
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
    
*-

load "./SLPexpressions/doc.m2"

undocumented {
printAsSLP,
(printAsSLP,GateMatrix,GateMatrix),
(printAsSLP,List,List),
cCode,
(cCode,GateMatrix,GateMatrix),
(cCode,List,List),
constants,
(constants,DetGate),
(constants,DivideGate),
(constants,GateMatrix),
(constants,InputGate),
(constants,List),
(constants,ProductGate),
(constants,SumGate),
-- (isConstant,InputGate),
(net,DetGate),
(net,DivideGate),
(net,InputGate),
(net,ProductGate),
(net,SumGate),
(support,DetGate),
(support,DivideGate),
(support,GateMatrix),
(support,InputGate),
(support,ProductGate),
(support,SumGate)
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
debug loadPackage("SLPexpressions", Reload => true)
viewHelp "SLPexpressions"

-- (old way) installPackage("SLPexpressions", SeparateExec=>true)

-- install docs with no absolute links
uninstallPackage "Style"
installPackage("Style")
installPackage("SLPexpressions")

installPackage ("SLPexpressions", MakeDocumentation=>false)
check "SLPexpressions"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=SLPexpressions "
-- End:
