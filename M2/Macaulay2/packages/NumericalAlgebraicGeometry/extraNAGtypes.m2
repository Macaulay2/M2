------------------------------------------------------
-- additional NumericalAG types  
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
-- (common types are in ../NAGtypes.m2 
------------------------------------------------------

export{ 
    "GateSystem", "gateSystem", 
    "GateHomotopy", "GateParameterHomotopy", "gateHomotopy", "segmentHomotopy", "parametricSegmentHomotopy"
    }

debug SLPexpressions

gateSystem = method()

GateSystem = new Type of System -- this essentially is a wrapper for SLProgram
net GateSystem := S -> (
    out := net "gate system: " | net numVariables S | " ---> " | net numFunctions S;
    if numParameters S =!= 0 then out = out || net "(#parameters = " | net numParameters S | ")";
    out
    )
-- main constructor
-- IN: (variables,output) or (parameters,variables,output)
gateSystem (GateMatrix,GateMatrix) := (I,O) -> gateSystem(gateMatrix{{}},I,O)
gateSystem (GateMatrix,GateMatrix,GateMatrix) := (P,I,O) -> (
    if not instance(P,GateMatrix) or numrows P != 1 then error "expected the matrix of parameters (1st argument) to be a row vector";
    if numrows I != 1 then error "expected the matrix of inputs (2nd argument) to be a row vector";
    if numcols O != 1 then error "expected the output matrix (3rd argument) with 1 column";
    new GateSystem from {Variables=>I, GateMatrix=>O, Parameters=>P,
	"SLP"=>makeSLProgram(P|I,O), cache => new CacheTable from {}}
    )


-- serialize
toExternalString GateSystem := F -> (
    params := flatten entries parameters F;
    inputs := flatten entries vars F;
    outputs := flatten entries F#GateMatrix;
    h := newPrintTable ":=";
    scan(outputs, g->printName(g,h)); -- fills in the print table 
    s := "";
    s = s | "("; -- begin
    scan(inputs, g-> s = s | printName(g,h) | " := " | toExternalString g | "; " );
    scan(h#"#lines", i->s = s|h#i|"; ");
    s = s | "gateSystem( " | 
    "gateMatrix{" | toString apply(params, g-> toExternalString g) | "}, " | 
    "gateMatrix{" | toString apply(inputs, g-> toExternalString g) | "}, " | 
    "transpose gateMatrix{" | toString apply(outputs, g-> printName(g,h)) | "} )";
    s | ")" -- end
    )

-- take a Matrix of elements in a polynomial ring that looks like K[x] or K[p][x] 
gateSystem Matrix := GateSystem => F -> (
    if numcols F != 1 then error "expected a matrix with 1 column";
    R := ring F;
    if not instance(R,PolynomialRing) then error "expected a matrix over a polynomial ring with the form K[variables] or K[parameters][variables]";
    (FR,toFR) := flattenRing R;
    vars'params := getVarGates FR;
    if numgens R + numgens coefficientRing R != numgens FR then error "expected a matrix over a polynomial ring with the form K[variables] or K[parameters][variables]";    
    gateSystem(gateMatrix{drop(vars'params,numgens R)}, gateMatrix{take(vars'params,numgens R)}, gatePolynomial toFR F) 
    )

vars GateSystem := F -> F.Variables
parameters GateSystem := F -> F.Parameters
gateMatrix GateSystem := F -> F#GateMatrix
numVariables GateSystem := F -> numcols vars F
numFunctions GateSystem := F -> numrows gateMatrix F
numParameters GateSystem := F -> numcols parameters F

evaluate (GateSystem,Matrix,Matrix) := (F,p,x) -> (
    if numrows x =!= 1 or numrows p != 1 then "expected a 1-row matrix of values";
    if numVariables F =!= numcols x then error "wrong number of variables values";
    if numParameters F =!= numcols p then error "wrong number of parameter values";
    evaluate(F#"SLP", matrix p | matrix x)
    )

TEST ///
-* GateSystem *-
declareVariable \ {x,y,t}
S = matrix{{x^2-1},{y^3-1}}
T = matrix{{x*y-1},{x^3+y^2-2}}
fS = gateSystem(matrix{{x,y}},S)
fT = gateSystem(matrix{{x,y}},T)
evaluate(fS,point{{0.1,0.2}}) 
valueT = evaluate(fT,point{{0.1,2*ii}}) 
fH = gateSystem(matrix{{t}}, matrix{{x,y}}, (1-t)*S + (1+ii)*t*T) 
valueH1 = evaluate(fH,point{{1}},point{{0.1,2*ii}})
assert(norm(valueH1 - (1+ii)*valueT)<0.00001)  
R = CC[X,Y]
F = gateSystem matrix{{X*Y-1},{X^2+Y^3-2}}
vF = evaluate(F,point{{0.1,2*ii}}) 
R = CC[A,B][X,Y]
G = gateSystem matrix{{X*Y-A},{X^2+Y^3-B}}
vG = evaluate(G,point{{1,2}},point{{0.1,2*ii}}) 
assert(norm(vF - vG)<0.00001)  
toExternalString fS
toExternalString fT
///

--todo: bring into harmony with (jacobian, PolySystem)
jacobian (List, GateSystem) := (inds, GS) -> (
    if not GS.cache#?Jacobian then (
    	F := gateMatrix GS;
    	I := (vars GS)_inds;
    	J := diff(I,F);
    	GS.cache.Jacobian = makeSLProgram(parameters GS | vars GS, J);
	);
    GS.cache.Jacobian
    )
jacobian GateSystem := GS -> jacobian(toList(0..numVariables GS-1), GS)

-- overrides "implementation" for System
evaluateJacobian (GateSystem, Matrix) := (GS, x0) -> (
    J := jacobian GS;
    assert(numcols matrix x0 == J#"number of inputs");
    out := evaluate(jacobian GS,  matrix x0);
    matrix(out, numFunctions GS, numVariables GS)
    )
evaluateJacobian (GateSystem, AbstractPoint) := (GS, x0) -> evaluateJacobian(GS, matrix x0)
evaluateJacobian (GateSystem, Matrix, Matrix) := (GS, p0, x0) -> evaluateJacobian(GS, p0 | x0)
evaluateJacobian (GateSystem, AbstractPoint, AbstractPoint) := (GS, p0, x0) -> evaluateJacobian(GS, matrix p0, matrix x0)


TEST /// 
X = gateMatrix{declareVariable \ {x, y}}
G = gateSystem(X, transpose gateMatrix{{x^2+y^2-6, 2*x^2-y}})
x0 = point({{1.0_CC,2.3_CC}});
assert(numVariables G == 2)
assert(numFunctions G == 2)
evaluate(G,x0)
evaluateJacobian(G,x0)

P = gateMatrix{declareVariable \ {a, b, c}}
H = gateSystem(P, X, transpose gateMatrix{{a*x^2+c*y^2-6, 2*x^2-2*y*b}})
p0 = point({{1.1_CC,0.51,1}});
assert(numParameters H == 3)
assert(numFunctions G == 2)
evaluate(H,p0,x0)
evaluateJacobian(H,p0,x0)
///

--TEST 
/// -- package Serialization
restart
needsPackage "Serialization"
MyList = new Type of List
a = new MyList from {1}
MyList | MyList := (c,d)-> c + d
b = a | a    
serialize b

needsPackage "NumericalAlgebraicGeometry"
declareVariable \ {x,y,t}
S = matrix{{x^2-1},{y^3-1}}
fS = gateSystem(matrix{{x,y}},S)
errorDepth = 2
serialize fS
code x
///

-- jacobian PolySystem := ??? -- where is this used?

----------------------------------
-- GateHomotopy (::Homotopy::MutableHashTable)
-- keys: 
--   "X", GateMatrix: a row of X variables
--   "T", Gate: the continuation parameter  
--   "H", "Ht","Hx"; GateMatrix: matrix, derivatives
--   "EH", "EHx", "EHt", "EHxt", "EHxH"; Evaluators: top-level evaluators
--   K, a ring: rawEvaluator for the ring
GateHomotopy = new Type of Homotopy    
GateParameterHomotopy = new Type of ParameterHomotopy

numVariables GateHomotopy := H->numcols H#"X"
numVariables ParameterHomotopy := H->numcols H.GateHomotopy#"X" - numcols H.Parameters
numVariables SpecializedParameterHomotopy := H -> numVariables H.ParameterHomotopy 
 

canHaveRawHomotopy = method()
canHaveRawHomotopy Thing := T -> false 
canHaveRawHomotopy GateHomotopy := H -> H.Software == M2engine
canHaveRawHomotopy SpecializedParameterHomotopy := H -> canHaveRawHomotopy H.ParameterHomotopy.GateHomotopy

debug Core
getRawHomotopy = method() 
getRawHomotopy(GateHomotopy,Ring) := (GH,K) -> if GH#?K then GH#K else GH#K = --(GH#"EHx",GH#"EHxt",GH#"EHxH") / (e->rawSLEvaluatorK(e,K)) // rawHomotopy
    rawHomotopy(rawSLEvaluatorK(GH#"EHx",K),rawSLEvaluatorK(GH#"EHxt",K),rawSLEvaluatorK(GH#"EHxH",K)) 
getRawHomotopy(SpecializedParameterHomotopy,Ring) := (H,K) -> if H#?K then H#K else H#K = (
    GH := H.ParameterHomotopy.GateHomotopy;
    paramsK := raw mutableMatrix promote(H.Parameters,K);
    evaluators := (GH#"EHx",GH#"EHxt",GH#"EHxH") / (e->rawSLEvaluatorSpecialize(rawSLEvaluatorK(e,K),paramsK)); 
    (if H#?"evaluators" then H#"evaluators" else new MutableHashTable)#K = evaluators;
    evaluators // rawHomotopy 
    )
gateHomotopy = method(Options=>{Parameters=>null,Software=>null,Strategy=>compress})
gateHomotopy (GateMatrix, GateMatrix, InputGate) := o->(H,X,T) -> (
    para := o.Parameters=!=null;
    soft := if o.Software=!=null then o.Software else DEFAULT.Software;
    GH := new GateHomotopy;
    GH#"X" = X;
    if para then GH#"X" = o.Parameters | GH#"X";
    GH#"T" = T;    
    GH#"H" = H;
    GH#"Hx" = diff(X,H);
    GH#"Ht" = diff(T,H);
    if o.Strategy === compress then (
        GH#"H" = compress H;
        GH#"Hx" = compress GH#"Hx";
        GH#"Ht" = compress GH#"Ht";
        );
    GH.Software = soft;
    if soft === M2 then (
        )
    else if soft === M2engine then (
        varMat := X | matrix{{T}};
        if para then varMat = o.Parameters | varMat;
        GH#"EH" = makeSLProgram(varMat, GH#"H");
        GH#"EHx" = makeSLProgram(varMat, GH#"Hx");
        GH#"EHt" = makeSLProgram(varMat, GH#"Ht");
        GH#"EHxt" = makeSLProgram(varMat, GH#"Hx"|GH#"Ht");
        GH#"EHxH" = makeSLProgram(varMat, GH#"Hx"|GH#"H");
        )
    else error "unknown Software option value";
    if para then (
        GPH := new GateParameterHomotopy;
        GPH.GateHomotopy = GH;		
        GPH.Parameters = o.Parameters;
        GPH
        ) 
    else GH
    ) 
    
evaluateH (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"H", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retH",K) then H#("retH",K) else H#("retH",K) = mutableMatrix(K, 1, numcols H#"H"*numrows H#"H");
    evaluate(H#"EH", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"H", numcols H#"H")    
    )
evaluateHt (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Ht", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retHt",K) then H#("retHt",K) else H#("retHt",K) = mutableMatrix(K, 1, numcols H#"Ht"*numrows H#"Ht");
    evaluate(H#"EHt", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"Ht", numcols H#"Ht")
    )
evaluateHx (GateHomotopy,Matrix,Number) := (H,x,t) -> if H.Software===M2 then value(H#"Hx", 
    valueHashTable(flatten entries H#"X" | {H#"T"}, flatten entries x | {t}) 
    ) else if H.Software===M2engine then (
    K := ring x;
    r := if H#?("retHx",K) then H#("retHx",K) else H#("retHx",K) = mutableMatrix(K, 1, numcols H#"Hx"*numrows H#"Hx");
    evaluate(H#"EHx", mutableMatrix(transpose x | matrix{{t}}), r);
    matrix(matrix r, numrows H#"Hx", numcols H#"Hx")
    )
evaluateH (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateH(H.GateHomotopy,parameters||x,t)
evaluateHt (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHt(H.GateHomotopy,parameters||x,t)
evaluateHx (GateParameterHomotopy,Matrix,Matrix,Number) := (H,parameters,x,t) -> evaluateHx(H.GateHomotopy,parameters||x,t)

specialize (GateParameterHomotopy,MutableMatrix) := (PH, M) -> specialize(PH, mutableMatrix M)
specialize (GateParameterHomotopy,MutableMatrix) := (PH, M) -> (                                                                                                         
    if numcols M != 1 then error "1-column matrix expected"; 
    if numcols PH.Parameters != numrows M then error "wrong number of parameters";  
    SPH := new SpecializedParameterHomotopy;
    SPH.ParameterHomotopy = PH;                                                                                                                               
    SPH.Parameters = M;                                                                                                                                       
    SPH                                                                                                                                                       
    ) 

-- !!! replaces makeGateMatrix
gateSystem PolySystem := GateSystem => F -> if F#?GateSystem then F#GateSystem else 
  F#GateSystem = gateSystem(F,parameters F)
gateSystem (PolySystem,List-*of parameters*-) := (F,P) -> ( 
    R := ring F; 
    if not isSubset(P, gens R) then "some parameters are not among generators of the ring";
    X := getVarGates R;
    variables := gateMatrix {X_(positions(gens R, x->not member(x,P)))};  
    parameters := gateMatrix {X_(positions(gens R, x->member(x,P)))};
    gateSystem(parameters, variables, gatePolynomial F.PolyMap)
    ) 
 
-- !!! a general problem: some methods need PolySystem to be changed to GateSystem
gateMatrix PolySystem := F -> gateMatrix gateSystem F

-- Homotopy that follows a segment in the parameter space 
parametricSegmentHomotopy = method()

-- in:
--     F, PolySystem (F.GateMatrix, F.Variables, F.Parameters are assumed to be set)
-- in-place: 
--     the output is stored in F.GateParameterHomotopy
-- out:
--     GateParameterHomotopy
parametricSegmentHomotopy PolySystem := F -> parametricSegmentHomotopy gateSystem F    

-- in: 
--     S, polynomials (GateMatrix)
--     V, variables (list of InputGates)
--     W, parameters
-- out: 
--     Homotopy that has A_w and B_w as parameters, 
--     	       	      where v in V|W  are coordinates of the source space 

-- parametricSegmentHomotopy(GateMatrix,List,List) := (S,V,W) -> (

parametricSegmentHomotopy GateSystem := F -> (
    V := flatten entries vars F;
    W := flatten entries parameters F;
    A := matrix{apply(W, w->inputGate symbol A_w)};
    B := matrix{apply(W, w->inputGate symbol B_w)};
    t := inputGate symbol t;
    H := sub(gateMatrix F,matrix{W},(1-t)*A+t*B);
    gateHomotopy(H,matrix{V},t,Parameters=>A|B)
    )

TEST /// 
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,a]
PS = polySystem {x^2+y^2-1, x+a*y, (a^2+1)*y^2-1}
F = gateSystem(squareUp(PS,2), drop(gens R,2)) 
PH := parametricSegmentHomotopy F
a0 = 0; a1 = 1;
H = specialize (PH, transpose matrix{{a0,a1}})
s'sols = { {{0,1}},{{0,-1}} }/point
time sols = trackHomotopy(H,s'sols)
assert areEqual(sols,{{ { -.707107, .707107}, SolutionStatus => Regular }, { {.707107, -.707107}, SolutionStatus => Regular }} / point)
///

TEST ///
needsPackage "NumericalAlgebraicGeometry"
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
debug SLPexpressions
debug NumericalAlgebraicGeometry
gS = gateMatrix S
p = point {{1.0+3*ii,2.3+ii}};
X = getVarGates ring S
vals = valueHashTable(X, coordinates p)
assert(evaluate(S,p) == value(gS,vals))
assert(evaluate(jacobian S, p)== value(diff(matrix{X},gS),vals))
///

segmentHomotopy = method(Options=>{gamma=>1})
segmentHomotopy (List, List) := o -> (S,T) -> segmentHomotopy(polySystem S, polySystem T, o)
segmentHomotopy (GateSystem, GateSystem) := o -> (S,T) -> (
    if T.Variables =!= S.Variables 
    then error "expected systems with the same inputs";  
    if numFunctions T =!= numFunctions S
    then error "expected systems with the same dimension of codomain";
    t := local t;
    tt := inputGate [t];
    gateHomotopy((1-tt)*gateMatrix S+ o.gamma*tt*gateMatrix T, 
	S.Variables, tt, Strategy=>compress)
    )
segmentHomotopy (PolySystem, PolySystem) := o -> (S,T) -> (
    R := ring T;
    if R =!= ring S then error "systems in the same ring expected";  
    if numFunctions T =!= numFunctions S
    then error "expected systems with the same dimension of codomain";
    t := local t;
    tt := inputGate [t];
    gateHomotopy((1-tt)*gateMatrix S + o.gamma*tt*gateMatrix T, 
	gateMatrix{getVarGates R}, tt, Strategy=>compress)
    )

TEST ///
n = 2; d = 2;
debug needsPackage "NumericalAlgebraicGeometry"
R=QQ[x_0..x_(n-1)]
eps = 1/10^2
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+ii)
t0 = 0.3
x0 = matrix{{0.1,0.2*ii}}
assert (
    norm(
    	(1-t0)*evaluate(polySystem S,x0)+(1+ii)*t0*evaluate(polySystem T,x0)    
    	-
    	evaluateH(H,transpose x0,t0)
    	) < 1e-9
    )
V = matrix{getVarGates R}
GH = segmentHomotopy(gateSystem(V,transpose gatePolynomial matrix {S}), gateSystem(V,transpose gatePolynomial matrix{T}), gamma=>1+pi*ii)
///

segmentHomotopyProjective = method(Options=>{gamma=>1})
segmentHomotopyProjective (List, List) := o -> (S,T) -> segmentHomotopyProjective(polySystem S, polySystem T, o)
segmentHomotopyProjective (PolySystem, PolySystem) := o -> (S,T) -> (
    -- check if S and T are homogeneous!!!
    R := ring T;
    if R =!= ring S then error "systems in the same ring expected";  
    c := symbol c;
    coeffs := matrix{ apply(numgens R, i->inputGate c_i) };
    chartHyperPlane := coeffs * transpose matrix {getVarGates R} + matrix{{-1}}; 
    t := local t;
    tt := inputGate [t];
    gateHomotopy(((1-tt)*gateMatrix S + o.gamma*tt*gateMatrix T)||chartHyperPlane, 
	gateMatrix{getVarGates R}, tt, Parameters=>coeffs, Strategy=>compress)
    )

TEST /// 
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z];
PH = segmentHomotopyProjective({x^3-z^3,y^2-z^2},{x*y*z, x^2+y^2+z^2},gamma=>1+ii)
Hz = specialize(PH,transpose matrix{{0,0,1}})
start = flatten table(3,2,(i,j)->point{{exp(ii*2*pi*i/3),exp(ii*2*pi*j/2),1}})
sols = trackHomotopy(Hz,start)
assert (#select(sols, s->status s == Regular) == 4)
///

-------------------------------------------------------
-- trackHomotopy tests
TEST /// 
needsPackage "NumericalAlgebraicGeometry"
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

TEST ///-- Homotopy
needsPackage "NumericalAlgebraicGeometry"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G
Rvars = valueHashTable({X,Y,T},{x,y,t})
gV = matrix{{X,Y}}
gH = transpose matrix {H}
gHx = diff(gV,gH)
gHt = diff(T,gH)
value(gH, Rvars)
value(gHt, Rvars)
value(gHx, Rvars)

debug NumericalAlgebraicGeometry
HS = gateHomotopy(gH,gV,T)
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
HS = gateHomotopy(gH,gV,T)
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2)
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-4)
s = first trackHomotopy(HS,{matrix{{1_CC},{1}}},Software=>M2engine)
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-4)
///

TEST /// -- ParameterHomotopy
needsPackage "NumericalAlgebraicGeometry"
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
PHS = gateHomotopy(gH,gV,T,Parameters=>gP)
HS = specialize(PHS,matrix{{1_CC}})
x0 = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{x0},Software=>M2)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
s = first trackHomotopy(HS,{x0},Software=>M2engine)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
///

TEST ///
debug needsPackage "NumericalAlgebraicGeometry"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G
Rvars = valueHashTable({X,Y,T},{x,y,t})
gV = matrix{{X,Y}}
gH = transpose matrix {H}

HS = gateHomotopy(gH,gV,T)
inp = matrix{{1_CC},{1}}
s = first trackHomotopy(HS,{inp},Software=>M2engine)
peek s

NAGtrace 2
inpCCC = sub(inp,CC_1000) 
s = first trackHomotopy(HS,{inpCCC},Software=>M2engine)
peek s
assert (norm evaluateH(HS, transpose matrix s, 1) < 1e-6)
///
