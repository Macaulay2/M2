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
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#0)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#1)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#2)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#3)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#4)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#5)
sort noetherianOperators(J, Strategy => "MacaulayMatrix", primes#6)


-- Numerical version
S = CC monoid R
J' = sub(J,S)
--computes a numerical irreductible decomposition
elapsedTime nid = bertiniPosDimSolve(J', BertiniInputConfiguration => {RandomSeed => 1})


debugLevel = 1
R = ring I
numericalNoetherianOperators(J,(components nid)#0, DependentSet => {R_"x_1", R_"x_2", R_"x_3", R_"y_2", R_"y_3"})
-- slow --
numericalNoetherianOperators(J,(components nid)#1, DependentSet => {R_"x_0", R_"x_1", R_"x_2", R_"y_1", R_"y_2"}, InterpolationTolerance => 1e-5, NoetherianDegreeLimit => 3)
numericalNoetherianOperators(J,(components nid)#3, DependentSet => {R_"x_1", R_"x_2", R_"y_1", R_"y_2", R_"y_3"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 3)
----------
numericalNoetherianOperators(J,(components nid)#2, DependentSet => {R_"x_0", R_"x_1", R_"y_0", R_"y_1", R_"y_2"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)
numericalNoetherianOperators(J,(components nid)#4, DependentSet => {R_"x_0", R_"x_1", R_"x_2", R_"x_3", R_"y_1"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)
numericalNoetherianOperators(J,(components nid)#5, DependentSet => {R_"x_0", R_"y_0", R_"y_1", R_"y_2", R_"y_3"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)
numericalNoetherianOperators(J,(components nid)#6, DependentSet => {R_"x_0", R_"x_1", R_"x_2", R_"y_0", R_"y_1"}, InterpolationTolerance => 1e-6, NoetherianDegreeLimit => 2)

-- sample points on the first component
-- (components nid)#0 -- corresponds to primes#2
-- (components nid)#1 -- corresponds to primes#4
-- (components nid)#2 -- corresponds to primes#3
-- (components nid)#3 -- corresponds to primes#1
-- (components nid)#4 -- corresponds to primes#5
-- (components nid)#5 -- corresponds to primes#0
-- (components nid)#6 -- corresponds to primes#6


-- Finding dependent sets
needsPackage "NumericalImplicitization"
-- pick a point on any component
ws = (components nid)#1 -- corresponds to primes#4
pts = bertiniSample(1, ws, BertiniInputConfiguration => {RandomSeed => 1});

-- since codimention is 5, we will have 5 dependent variables
-- at first, every set of 5 variables is a potential set of dependent variables
candidates = subsets(gens S, 5)
candidates#(position(candidates, l -> numericalImageDim(matrix{l}, sub(primes#4,S), pts#0) == 0))