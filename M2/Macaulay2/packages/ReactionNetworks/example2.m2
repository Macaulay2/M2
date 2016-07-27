restart
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"

FF = QQ

CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
R = createRing(CRN, FF)
coefficientRing R

kstart = apply(gens coefficientRing R, k -> k => random FF)
CE = flatten entries random(FF^1, FF^2) - conservationEquations CRN
I = ideal CE
SSE = steadyStateEquations CRN
J = ideal SSE
F = I + J

T = transpose gens F
rM = sub(random(FF^5, FF^7),R)

G = polySystem(rM * T)




