///
restart
needsPackage "NumericalAlgebraicGeometry"
///

TEST ///
R=ZZ[x,y]
sols = solveSystem {x^2-1,y}
assert (#sols == 2 and all(sols,s->status s === Regular))

F = {x*(x-1),y}
sols = solveSystem F
s = first select(sols,s->status s === Origin)
assert(norm matrix s < 0.0001) 
///

TEST /// -- failed before because of CC to QQ conversion
R = QQ[x,y,z];
I = minors(2,matrix{gens R, gens R / (v->v^2)});
J = I + ideal(random(1,R)+1_R); 
solveSystem J_*
///

-- MISC. TESTS
--------------

--assert(multistepPredictor(2_QQ,{0,0,0}) === {-3/8, 37/24, -59/24, 55/24}) -- Wikipedia: Adams-Bashforth
--assert(multistepPredictor(2_QQ,{-1}) === {-1/8, 5/8}) -- computed by hand
--assert(flatten entries (coefficients first multistepPredictorLooseEnd(2_QQ,{0,0,0}))#1=={1/120, 1/16, 11/72, 1/8})

TEST ///-- random and good initial pairs
debug needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 0
T = randomSd {2,3};
(S,solsS) = goodInitialPair T
M = track(S,T,solsS,Normalize=>true)
-- RM = refine(T,M,Software=>M2) -- projective refine is nom implemented!!!
RM = M
assert areEqual(norm2 matrix first M, 1_CC, Tolerance=>0.001)
///
