restart
-- Symbolic primary decomposition via Noetherian Operators
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
needsPackage "K3Carpets"
needsPackage "MinimalPrimes"
needsPackage "NoetherianOperators"

installMinprimes()

setRandomSeed 1

I = carpet(3,3, Characteristic => 0)
#I_*
codim I
R =ring I

-- Choose a subset of the variables
J = (0,3,5,6,9) / (i -> I_i) // ideal
codim J

-- Symbolically compute Noetherian operators for components whose radicals are minimal primes
elapsedTime primes = minimalPrimes J
sort noetherianOperators(J, primes#0)
sort noetherianOperators(J, primes#1)
sort noetherianOperators(J, primes#2)
sort noetherianOperators(J, primes#3)
sort noetherianOperators(J, primes#4)
sort noetherianOperators(J, primes#5)
sort noetherianOperators(J, primes#6)


-- Numerical version
S = CC monoid R
J' = sub(J,S)
--computes a numerical irreductible decomposition
elapsedTime nid = bertiniPosDimSolve(J', BertiniInputConfiguration => {RandomSeed => 1})

-- sample points on the first component
ws = (components nid)#0 -- corresponds to primes#2
pts = bertiniSample(50, ws, BertiniInputConfiguration => {RandomSeed => 1});
-- Compute Noetherian operators. The dependent set can be found using a heuristic on the bottom of this file
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_1", S_"x_2", S_"x_3", S_"y_2", S_"y_3"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)

ws = (components nid)#1 -- corresponds to primes#4
pts = bertiniSample(350, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_0", S_"x_1", S_"x_2", S_"y_1", S_"y_2"}, InterpolationTolerance => 1e-5, NoetherianDegreeLimit => 3)

ws = (components nid)#2 -- corresponds to primes#3
pts = bertiniSample(100, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_0", S_"x_1", S_"y_0", S_"y_1", S_"y_2"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)

ws = (components nid)#3 -- corresponds to primes#1
pts = bertiniSample(350, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_1", S_"x_2", S_"y_1", S_"y_2", S_"y_3"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 3)

ws = (components nid)#4 -- corresponds to primes#5
pts = bertiniSample(120, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_0", S_"x_1", S_"x_2", S_"x_3", S_"y_1"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)

ws = (components nid)#5 -- corresponds to primes#0
pts = bertiniSample(120, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_0", S_"y_0", S_"y_1", S_"y_2", S_"y_3"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)

ws = (components nid)#6 -- corresponds to primes#6
pts = bertiniSample(120, ws, BertiniInputConfiguration => {RandomSeed => 1});
numericalNoetherianOperators(J', pts, DependentSet => {S_"x_0", S_"x_1", S_"x_2", S_"y_0", S_"y_1"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)


-- Finding dependent sets
needsPackage "NumericalImplicitization"
-- pick a point on any component
ws = (components nid)#1 -- corresponds to primes#4
pts = bertiniSample(1, ws, BertiniInputConfiguration => {RandomSeed => 1});

-- since codimention is 5, we will have 5 dependent variables
-- at first, every set of 5 variables is a potential set of dependent variables
candidates = subsets(gens S, 5)
candidates#(position(candidates, l -> numericalImageDim(matrix{l}, sub(primes#4,S), pts#0) == 0))