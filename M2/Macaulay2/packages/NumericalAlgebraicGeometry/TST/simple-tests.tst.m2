///
restart
needsPackage "NumericalAlgebraicGeometry"
///

TEST ///
R=ZZ[x,y]
sols = solveSystem {x^2-1,y}
assert (#sols == 2 and all(sols,s->status s === Regular))

errorDepth = 2
sols = solveSystem {x*(x-1),y}
///
-- MISC. TESTS
--------------

--assert(multistepPredictor(2_QQ,{0,0,0}) === {-3/8, 37/24, -59/24, 55/24}) -- Wikipedia: Adams-Bashforth
--assert(multistepPredictor(2_QQ,{-1}) === {-1/8, 5/8}) -- computed by hand
--assert(flatten entries (coefficients first multistepPredictorLooseEnd(2_QQ,{0,0,0}))#1=={1/120, 1/16, 11/72, 1/8})

TEST ///-- random and good initial pairs
setRandomSeed 0
T = randomSd {2,3};
(S,solsS) = goodInitialPair T
M = track(S,T,solsS,Normalize=>true)
-- RM = refine(T,M,Software=>M2) -- projective refine is nom implemented!!!
RM = M
debug NumericalAlgebraicGeometry
assert areEqual(norm2 matrix first M, 1_CC, Tolerance=>0.001)
///
