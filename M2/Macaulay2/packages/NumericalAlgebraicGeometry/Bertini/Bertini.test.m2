-- this file is created to test Bertini interface separately from NAG package
needsPackage "NumericalAlgebraicGeometry"
setDefault (Software=>BERTINI) 

R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
solsT = {(1,0),(0,1),(-1,0),(0,-1)};

/// -- larger example (commented out)
loadPackage "NumericalAlgebraicGeometry"
load "../benchmarks.m2"
T = (katsuraBench 11)_*; -- #sols=1024, M2:4, H:7, B:15, P:37                                                 
(S,solsS) = totalDegreeStartSystem T; 
///

-- solveSystem
sols = solveSystem(T)
assert(areEqual(sortSolutions solsT, sortSolutions sols))

-- track
sols = track(S,T,solsS)
assert(areEqual(sortSolutions solsT, sortSolutions sols))

-- refine 
R = CC[x,y];
T = {x^2+y^2-1, x*y};
sols = { {1.0001,0.00001}, { -0.00001,1.0000002} };
rsols = refine(T, sols, Software=>BERTINI, Bits=>1000)
assert areEqual(rsols, {{1,0},{0,1}})

-- numericalVariety
V = numericalIrreducibleDecomposition ideal T
assert(dim V == 0 and degree V == 4)

-- parameterHomotopy
R=CC[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
finalParameters0={{1,0,0}}
finalParameters1={{0,1+2*ii,0}}
sols = parameterHomotopy(
    {f1,f2},
    {u1,u2,u3},-- parameters
    {finalParameters0,finalParameters1}
    )
assert areEqual(sortSolutions first sols, sortSolutions {{11, 1}, {12, 1}, {13, 1}})

-- isOn
R = CC[x,y,z];
F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y};
NV = numericalIrreducibleDecomposition ideal F
p = point{{0,0,0}} --z-axis
assert isOn(p, NV)

end

restart
load "NumericalAlgebraicGeometry/Bertini/Bertini.test.m2"
