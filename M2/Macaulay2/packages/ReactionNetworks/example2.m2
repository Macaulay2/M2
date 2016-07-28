restart
load "code/solveViaMonodromy.m2"
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"

FF = QQ

CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
R = createRing(CRN, FF)
coefficientRing R

CE = flatten entries random(FF^1, FF^2) - conservationEquations(CRN,FF)
I = ideal CE
SSE = steadyStateEquations CRN
J = ideal SSE
F = I + J

K = apply(gens coefficientRing R, k -> k => random FF)
-- F' = sub(F, K)

T = transpose gens F
rM = sub(random(FF^5, FF^7),R)

-- is this the correct way to create a polynomial system?
G = polySystem(rM * T)
peek G

-- Problems:
-- conservationEquations and steadyStateEquations does not work for InexactFieldFamily
-- does solveViaMonodromy work for QQ?
-- 

