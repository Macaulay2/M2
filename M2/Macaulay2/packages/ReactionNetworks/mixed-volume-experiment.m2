restart
needsPackage "ReactionNetworks"
needsPackage "PHCpack"
CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, D --> B + E"
stoichiometricSubspace CRN
F = steadyStateEquations CRN
R = ring first F
describe R
R' = CC[gens R]
toR' = map(R',R, gens R')
monoms = F / monomials / toR'
F' = apply(monoms, m->(m*random(CC^(numcols m),CC^1))_(0,0))
mixedVolume F'
