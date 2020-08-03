restart
-- Symbolic primary decomposition via Noetherian Operators
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
needsPackage "K3Carpets"
needsPackage "MinimalPrimes"
needsPackage "NoetherianOperators"

installMinprimes()

setRandomSeed 1

J = carpet(3,3, Characteristic => 0)
#J_*
codim J
R =ring J

I = ideal((gens J) * random(R^10,R^5));
codim I

-- These will not finish
--elapsedTime minimalPrimes I
--elapsedTime primaryDecomposition I

-- Numerical version
S = CC monoid R
I' = sub(I,S)
elapsedTime nid = bertiniPosDimSolve(I', BertiniInputConfiguration => {RandomSeed => 1})




ws = (components nid)#0
pts = bertiniSample(100, ws, BertiniInputConfiguration => {RandomSeed => 1});
elapsedTime numericalNoetherianOperators(I', pts, DependentSet => {1, 2, 4, 5, 6} / (i -> S_i), InterpolationTolerance => 1e-12, NoetherianDegreeLimit => 2)

ws = (components nid)#1
pts = bertiniSample(20, ws, BertiniInputConfiguration => {RandomSeed => 1});
elapsedTime numericalNoetherianOperators(I', pts, DependentSet => {0, 1, 2, 3, 4} / (i -> S_i), InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 3)


-- Noetherian operators of J
noetherianOperators(J, DependentSet => {1, 2, 4, 5, 6} / (i -> R_i))
hybridNoetherianOperators(J, radical J, DependentSet => {1, 2, 4, 5, 6} / (i -> R_i))