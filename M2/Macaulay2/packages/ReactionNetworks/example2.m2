restart
load "code/solveViaMonodromy.m2"
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"

FF = RR

CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
R = createRing(CRN, FF)
coefficientRing R
describe R
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

setUpPolysparse = G -> (
    
    )


c0 = point{ 
    flatten apply(polys,f->(
	    r := # exponents f;
	    t := apply(r-1, i->random CC);
	    t | { -sum t }
	    )) 
    }
pre0 = point{toList(n:1_CC)}
-- does solveViaMonodromy work for QQ?
-- 

