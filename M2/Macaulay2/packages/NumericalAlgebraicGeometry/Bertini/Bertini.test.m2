-- this file is created to test Bertini interface
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
sols = { {1.00000001,0.00000001}, { -0.0000001,1.0000002} };
rsols = refine(T, sols, Bits=>1000)
assert areEqual(sortSolutions rsols, {{0,1},{1,0}})

-- numericalVariety
V = numericalIrreducibleDecomposition ideal T
assert(dim V == 0 and degree V == 4)
V = numericalIrreducibleDecomposition ideal (x^6-y^2)
assert all(components V, W->W.IsIrreducible===true)

-- parameterHomotopy
R=CC[u1,u2,u3,x,y]
f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
f2=(x-11)*(x-12)*(x-13)
finalParameters0={1,0,0}
finalParameters1={0,1+2*ii,0}
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
needsPackage "NumericalAlgebraicGeometry"
errorDepth = 2
load "NumericalAlgebraicGeometry/Bertini/Bertini.test.m2"
