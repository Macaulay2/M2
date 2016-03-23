restart
needsPackage "NumericalAlgebraicGeometry"
debug NumericalAlgebraicGeometry
(x,y,t) = (X,Y,T)/inputGate
H = gateHomotopy(
    (1-t)*matrix{{x^2-1},{y^3-1}} + t*matrix{{x^2+y^2-1}, {x^3+y^3-1}}
    ,
    matrix{{x,y}},
    t 
    );
s0 = {{{1,1}},{{ -1,1}}} / point;
s1 = trackHomotopy(H,s0) 
peek oo
s2 = trackHomotopy(H,s1,Precision=>100,CorrectorTolerance=>1p100e-9)
peek oo

--- Mike example -------------------------------------------
restart
needsPackage "NumericalAlgebraicGeometry"
debug NumericalAlgebraicGeometry
(x,y,t) = (X,Y,T)/inputGate
H = gateHomotopy(
    (1-t)*matrix{{x^2-1},{y^3-1}} + t*matrix{{x^2+y^2-1}, {x^3+y^3-1}}
    ,
    matrix{{x,y}},
    t 
    );
s0 = {{{1,1}},{{ -1,1}}} / point;

inp = mutableMatrix(CC_53, 3, 2)
 inp_(0,0) = 1.0
 inp_(1,0) = 1.0
 inp_(2,0) = 0.0
 inp_(0,1) = -1.0
 inp_(1,1) = 1.0
 inp_(2,1) = 0.0
out = mutableMatrix inp -- copy
for i from 0 to numColumns inp-1 do out_(numRows inp-1, i) = 1.0
statusOut = mutableMatrix(ZZ, 2, 2)

debug NumericalAlgebraicGeometry
trackHomotopyM2engine(H, inp, out, statusOut, 
    .05,    -- o.tStep
    .000001, -- o.tStepMin
    .000001, -- o.CorrectorTolerance
    3,       -- maxCorrSteps
    100000.0) -- InfinityThreshold

-- Mike example 2:
restart
debug needsPackage "NumericalAlgebraicGeometry"
R=CC[x,y]
T = {x^5+x*y^2-y^2-1, x^3+y^5-x*y-1}
(S,solsS) := totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>
    exp(ii*0.00000000001)
    )
peek H
prec = 53 
prec = 10000
pts = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out,outStatus) = mesTracker(H, pts, tStepMin=>1e-9)
netList (entries outStatus | entries out)
sols = extractM2engineOutput(H,out,outStatus)/point
coordinates sols#4 
# solutionsWithMultiplicity sols

sols2 = trackHomotopy(H,solsS)

R = QQ[symbol x, symbol y]
I = ideal matrix{{x^5+x*y^2-y^2-1}, {x^3+y^5-x*y-1}}
netList primaryDecomposition I
radical I == I -- this one is radical.

-- example multiplicity=d^n
restart
debug needsPackage "NumericalAlgebraicGeometry"

-- these settings make 53 terminate around t=1, while 100 goes comfortably to t=2
st = 1e-10 -- min step size
tol = 1e-15 -- CorrectorTolerance
n = 2; d = 2;

R=QQ[x_0..x_(n-1)]
eps = 1/1000
T = apply(n, i->if i==0 then x_i^d-eps^d else (x_i-i)^d-eps^(d-1)*x_i)
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii)
peek H


prec = 53 
pts = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out,outStatus) = mesTracker(H, pts, tStepMin=>st, CorrectorTolerance=>tol, LastT=>2.0)
netList (entries outStatus | entries out)
sols = extractM2engineOutput(H,out,outStatus)/point
sols/(s->evaluateH(H, transpose matrix s , 2)) 

prec = 100
pts' = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out',outStatus') = mesTracker(H, pts', tStepMin=>st, CorrectorTolerance=>tol, LastT=>2.0)
netList (entries outStatus' | entries out')
sols' = extractM2engineOutput(H,out',outStatus')/point
sols'/(s->evaluateH(H, transpose matrix s , 2)) 

matrix out' - sub(matrix out,CC_prec)

-- (5.4) from "adaptive" by bertini
restart
debug needsPackage "NumericalAlgebraicGeometry"
QQ[x,y,z] 
T = {poly "14x2+6xy+5x-72y2-18y-850z+2/1000000000", 
    poly "1/2xy2+1/100xy+13/100y2+4/100y-40000",
    poly "3/100xz+4/100z-850"}
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii)
peek H

st = 1e-6 -- min step size
tol = 1e-13 -- CorrectorTolerance

prec = 53 
pts = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out,outStatus) = mesTracker(H, pts, tStepMin=>st,  CorrectorTolerance=> tol, LastT=>1)
sols = extractM2engineOutput(H,out,outStatus)/point

prec = 1000
pts' = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out',outStatus') = mesTracker(H, pts', tStepMin=>st, CorrectorTolerance=> tol, LastT=>1)
sols' = extractM2engineOutput(H,out',outStatus')/point

matrix out' - sub(matrix out,CC_prec)
netList (entries outStatus | entries out)
netList (entries outStatus' | entries out')

-- sqrt(2) example --------------------------------------------------------------------
restart
debug needsPackage "NumericalAlgebraicGeometry"
R=QQ[x]
T = {x^2-2}
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+pi*ii)
st = 1e-5 -- min step size

prec = 53 
pts = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out,outStatus) = mesTracker(H, pts, tStepMin=>st, LastT=>1)
netList (entries outStatus | entries out)
sols = extractM2engineOutput(H,out,outStatus)/point
sols/(s->evaluateH(H, transpose matrix s , 1)) 

prec = 1000
pts' = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out',outStatus') = mesTracker(H, pts', tStepMin=>st, CorrectorTolerance => 2.^(-64), LastT=>1)
netList (entries outStatus' | entries out')
sols' = extractM2engineOutput(H,out',outStatus')/point
sols'/(s->evaluateH(H, transpose matrix s , 1)) 
realPart first coordinates first sols' - sqrt sub(2, RR_prec)
