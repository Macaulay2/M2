needsPackage "NAGtools"
--setDefault(Software=>M2)

-- double cover
X = inputGate x
F = matrix{{X^2}} 
PH = gateHomotopy4preimage(F,{X})
K = CC_53
p = point{{1_K}}
preimageViaMonodromy(PH,p,{point p})

-- twisted cubic
F = transpose matrix{{X,X^2,X^3}}
S1 = inputGate s1
S2 = inputGate s2
K = CC_53
setRandomSeed 0
(v1,v2) = (random(K^3,K^1),random(K^3,K^1));
G = F - (S1*v1+S2*v2) -- this is a covering map (onto, finite-to-one generically)
PH = gateHomotopy4preimage(G,{X,S1,S2})
p = point{{1,1,1}}

print preimageViaMonodromy(PH,p,{point {{1,0,0}}})

nextP = () -> point {{ random CC, random CC, random CC }}
print preimageViaMonodromy(PH,p,{point {{1,0,0}}},RandomPointFunction=>nextP)

stop = (n,L)->n>30
print preimageViaMonodromy(PH,p,{point {{1,0,0}}},StoppingCriterion=>stop)

-- the following have overdetermined systems: 
-- does not work with Software=>M2engine yet
setDefault(Software=>M2)

-- implicit twisted cubic in projective space
XYZH = toList (
    (X,Y,Z,H) = inputGate \ (x,y,z,h)
    )
M = matrix{{X,Y,Z},{H,X,Y}}
S1 = matrix{apply(subsets({0,1,2},2), s->det M_s)}
A = toList (inputGate \(a_1..a_4))
S2 = matrix{A}*transpose matrix {XYZH} 
S = transpose(S1|S2|matrix{{H-1}})
F = transpose matrix{A}
PH = gateHomotopy4preimage(F,S,XYZH|A,A) -- one way of doing this
x0 = random CC
p0 = point{{
	x0^2, 0, -1, 0
	}}
pre0 = point{{x0,x0^2,x0^3,1}|coordinates p0}
stop = (n,L)->n>5
-- setDefault(Software=>M2) -- !!! need to square-up for the engine
print preimageViaMonodromy(PH,p0,{pre0},StoppingCriterion=>stop)

pre0 = point{{x0,x0^2,x0^3,1}}
debug NumericalAlgebraicGeometry
PS = parametricSegmentHomotopy(S,XYZH,A) -- a more efficient way
print preimageViaMonodromy(PS,p0,{pre0},StoppingCriterion=>stop)

end

restart
load "NumericalAlgebraicGeometry/SYSTEMS/monodromy/degree-computation.m2"
