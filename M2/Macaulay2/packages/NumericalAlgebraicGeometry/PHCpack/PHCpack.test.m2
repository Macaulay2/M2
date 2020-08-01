-- this file is created to test PHCpack interface 

setDefault(Software=>PHCPACK)

R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {{1,-1},{1,1},{-1,1},{-1,-1}};
solsT = {{1,0},{0,1},{-1,0},{0,-1}};

-- solveSystem
sols = solveSystem(T)
assert(areEqual(sortSolutions solsT, sortSolutions sols))

-- track
sols = track(S,T,solsS/(s->point{s}))
assert(areEqual(sortSolutions solsT, sortSolutions sols))

-- refine 
R = CC[x,y];
T = {x^2+y^2-1, x*y};
sols = { {1.00000001,0.00000001}, { -0.0000001,1.0000002} };
rsols = refine(T, sols/(s->point{s}), Bits=>1000)
assert areEqual(sortSolutions rsols, {{0,1},{1,0}})
end

restart
errorDepth = 2
needsPackage "NumericalAlgebraicGeometry"
load "NumericalAlgebraicGeometry/PHCpack/PHCpack.test.m2"
 