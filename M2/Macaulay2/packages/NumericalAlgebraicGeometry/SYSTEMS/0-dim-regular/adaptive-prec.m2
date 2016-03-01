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
