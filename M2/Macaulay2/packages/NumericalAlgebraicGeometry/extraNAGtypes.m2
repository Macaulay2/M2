------------------------------------------------------
-- additional NumericalAG types  
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
-- (common types are in ../NAGtypes.m2 
------------------------------------------------------

export{ 
    "GateHomotopy", "GateParameterHomotopy", "gateHomotopy", "parametricSegmentHomotopy"
    }

debug SLPexpressions

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

numVars = method()
numVars GateHomotopy := H->numcols H#"X"
numVars ParameterHomotopy := H->numcols H.GateHomotopy#"X" - numcols H.Parameters
numVars SpecializedParameterHomotopy := H -> numVars H.ParameterHomotopy 
 

canHaveRawHomotopy = method()
canHaveRawHomotopy Thing := T -> false 
canHaveRawHomotopy GateHomotopy := H -> H.Software == M2engine
canHaveRawHomotopy SpecializedParameterHomotopy := H -> canHaveRawHomotopy H.ParameterHomotopy.GateHomotopy

debug Core
getRawHomotopy = method() 
getRawHomotopy(GateHomotopy,Ring) := (GH,K) -> if GH#?K then GH#K else GH#K = --(GH#"EHx",GH#"EHxt",GH#"EHxH") / (e->rawSLEvaluatorK(e,K)) // rawHomotopy
    rawHomotopy(rawSLEvaluatorK(GH#"EHx",K),rawSLEvaluatorK(GH#"EHxt",K),rawSLEvaluatorK(GH#"EHxH",K)) 
getRawHomotopy(SpecializedParameterHomotopy,Ring) := (H,K) -> if H#?K then H#K else (
    GH := H.ParameterHomotopy.GateHomotopy;
    paramsK := raw mutableMatrix promote(H.Parameters,K);
    H#K = (GH#"EHx",GH#"EHxt",GH#"EHxH") / (e->rawSLEvaluatorSpecialize(rawSLEvaluatorK(e,K),paramsK)) // rawHomotopy 
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
	GH#"EH" = makeEvaluator(H,varMat);
	GH#"EHx" = makeEvaluator(GH#"Hx",varMat);
	GH#"EHt" = makeEvaluator(GH#"Ht",varMat);
	GH#"EHxt" = makeEvaluator(GH#"Hx"|GH#"Ht",varMat);
	GH#"EHxH" = makeEvaluator(GH#"Hx"|GH#"H",varMat);
	)
    else error "uknown Software option value";
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
	
makeGateMatrix = method(Options=>{Parameters=>{}})
makeGateMatrix PolySystem := o -> F -> (
    R := ring F; 
    if not isSubset(o.Parameters, gens R) then "some parameters are not among generators of the ring";
    X := getVarGates R;
    F.Variables = X_(positions(gens R, x->not member(x,o.Parameters)));  
    F.NumberOfVariables = #F.Variables; -- reconsile 
    F.Parameters =X_(positions(gens R, x->member(x,o.Parameters)));
    polys := flatten entries F.PolyMap;
    F.GateMatrix = gateMatrix apply(polys, p->{gatePolynomial p})   
    ) 
 
gateMatrix PolySystem := F -> if F.?GateMatrix then F.GateMatrix else makeGateMatrix F

-- Homotopy that follows a segment in the parameter space 
parametricSegmentHomotopy = method()

-- in:
--     F, PolySystem (F.GateMatrix, F.Variables, F.Parameters are assumed to be set)
-- in-place: 
--     the output is stored in F.GateParameterHomotopy
-- out:
--     GateParameterHomotopy
parametricSegmentHomotopy PolySystem := F -> if F.?GateParameterHomotopy then F.GateParameterHomotopy else     
    parametricSegmentHomotopy(F.GateMatrix,F.Variables,F.Parameters)    

-- in: 
--     S, polynomials (GateMatrix)
--     V, variables (list of InputGates)
--     W, parameters
-- out: 
--     Homotopy that has A_w and B_w as parameters, 
--     	       	      where v in V|W  are coordinates of the source space 
parametricSegmentHomotopy(GateMatrix,List,List) := (S,V,W) -> (
    A := matrix{apply(W, w->inputGate symbol A_w)};
    B := matrix{apply(W, w->inputGate symbol B_w)};
    t := inputGate symbol t;
    H := sub(S,matrix{W},(1-t)*A+t*B);
    gateHomotopy(H,matrix{V},t,Parameters=>A|B)
    )
TEST /// 
debug needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,a]
PS = polySystem {x^2+y^2-1, x+a*y, (a^2+1)*y^2-1}
PS.NumberOfVariables = 2 -- hack!!!
squarePS = squareUp PS
makeGateMatrix(squarePS,Parameters=>drop(gens R,2))  
PH := parametricSegmentHomotopy squarePS
a0 = 0; a1 = 1;
H = specialize (PH, transpose matrix{{a0,a1}})
s'sols = { {{0,1}},{{0,-1}} }/point
time sols = trackHomotopy(H,s'sols)
assert areEqual(sols,{{ { -.707107, .707107}, SolutionStatus => Regular }, { {.707107, -.707107}, SolutionStatus => Regular }} / point)
///

TEST ///
needsPackage "NAGtools"
X = inputGate x
F = matrix{{X^2}} 
PH = gateHomotopy4preimage(F,{X})
K = CC_53
H = specialize (PH, transpose matrix{{1_K,2}})
time sols = trackHomotopy(H,{point{{1_K}}})
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
segmentHomotopy (PolySystem, PolySystem) := o -> (S,T) -> (
    R := ring T;
    if R =!= ring S then error "systems in the same ring expected";  
    t := local t;
    tt := inputGate [t];
    gateHomotopy(o.gamma*(1-tt)*gateMatrix S + tt*gateMatrix T, 
	gateMatrix{getVarGates R}, tt, Strategy=>compress)
    )
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
restart
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
restart
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
restart
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
