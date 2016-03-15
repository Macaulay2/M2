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

-- example multiplicity=d^2
restart
debug needsPackage "NumericalAlgebraicGeometry"
R=QQ[x,y]
eps = 1/100000
d = 3
T = {x^d-eps, (y-1)^d-eps*x}
(S,solsS) = totalDegreeStartSystem T
H = segmentHomotopy(S,T,gamma=>1+ii)
peek H
prec = 53 
prec = 10000
pts = mutableMatrix {apply(solsS, s->promote(transpose matrix s || matrix{{0}},CC_prec))}
(out,outStatus) = mesTracker(H, pts, tStepMin=>1e-5, LastT=>2.0)
netList (entries outStatus | entries out)
sols = extractM2engineOutput(H,out,outStatus)/point
