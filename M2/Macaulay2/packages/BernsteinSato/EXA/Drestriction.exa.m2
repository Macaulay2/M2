restart
needsPackage "Dmodules"

------------------------- EXAMPLES for Drestriction --------------------------------

-- Example 1: Restriction of a rational function
W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}]
I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2) -- annihilator of 1/(t^2-y)
Drestriction(0, I, {1,0}) -- restriction to y = 0 produces annihilator of 1/t^2
Drestriction(0, I, {0,1}) -- restriction to t = 0 produces annihilator of 1/y


--Example 2: Derived restriction of a GKZ hypergeometric system
I = gkz(matrix{{1,1,1},{0,1,3}}, {2,3})
W = ring I

-- Restriction ideal
DrestrictionIdeal(I, {1,0,0})

-- 0th derived restriction module
Drestriction(0, I, {1,3,5}) -- restriction to origin
Drestriction(0, I, {1,3,0}) -- restriction to {x_1 = x_2 = 0}
Drestriction(0, I, {1,0,0}) -- restriction to {x_1 = 0}

-- All derived restriction modules
Drestriction(I, {1,3,5})
Drestriction(I, {1,3,0})
Drestriction(I, {1,0,0})

-- Explicit generating cycles of derived restriction modules
DrestrictionClasses(I, {1,3,5})
DrestrictionClasses(I, {1,3,0})
DrestrictionClasses(I, {1,0,0})

-- Derived restriction complex
DrestrictionComplex(I, {1,3,5})
DrestrictionComplex(I, {1,3,0})
DrestrictionComplex(I, {1,0,0})

-- Supplementary info
DrestrictionAll(I, {1,3,5})
DrestrictionAll(I, {1,3,0})
DrestrictionAll(I, {1,0,0})


------------------------- EXAMPLES for Dintegration --------------------------------

-- Example 1: Integration of a rational function
W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}]
I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2) -- annihilator of 1/(t^2-y)
Dintegration(0, I, {1,0}) -- Integrating out y
Dintegration(0, I, {0,1}) -- Integrating out t

--Example 2: Derived integration of a GKZ hypergeometric system
I = gkz(matrix{{1,1,1},{0,1,3}}, {-3,-2})
W = ring I

-- 0th derived integration module
Dintegration(0, I, {1,3,5}) -- integration to origin
Dintegration(0, I, {1,3,0}) -- integration to {x_1 = x_2 = 0}
Dintegration(0, I, {1,0,0}) -- integration to {x_1 = 0}

-- All derived integration modules
Dintegration(I, {1,3,5})
Dintegration(I, {1,3,0})
Dintegration(I, {1,0,0})

-- Explicit generating cycles of derived integration modules
DintegrationClasses(I, {1,3,5})
DintegrationClasses(I, {1,3,0})
DintegrationClasses(I, {1,0,0})

-- Derived integration complex
DintegrationComplex(I, {1,3,5})
DintegrationComplex(I, {1,3,0})
DintegrationComplex(I, {1,0,0})

-- supplementary info
DintegrationAll(I, {1,3,5})
DintegrationAll(I, {1,3,0})
DintegrationAll(I, {1,0,0})
