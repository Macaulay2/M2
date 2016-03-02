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
(x,y,t) = (X,Y,T)/inputGate
H = gateHomotopy(
    (1-t)*matrix{{x^5-1},{y^5-1}} + t*matrix{{x^5+x*y^2-y^2-1}, {x^3+y^5-x*y-1}}
    ,
    matrix{{x,y}},
    t 
    );
roots = for i from 1 to 5 list numeric(100, exp(i*2*ii*pi/5))
pts = mutableMatrix transpose matrix(CC_100, (toList(set roots ** set roots))/toList/(a -> append(a, 0.0)))

mesTracker(H, pts) -- this example seems to be missing 2 solutions?  And 2 solutions don't make it very far
  -- (1,0), (1,01) are missing.  This might be a good place to test adaptive?
transpose first oo
VerticalList last ooo

R = QQ[symbol x, symbol y]
I = ideal matrix{{x^5+x*y^2-y^2-1}, {x^3+y^5-x*y-1}}
netList primaryDecomposition I
radical I == I -- this one is radical.



