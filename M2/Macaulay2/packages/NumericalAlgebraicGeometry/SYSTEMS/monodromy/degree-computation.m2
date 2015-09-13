needsPackage "NAGtools"
setDefault(Software=>M2)

-- double cover
X = inputGate x
F = matrix{{X^2}} 
PH = gateHomotopy4preimage(F,{X})
K = CC_53
setDefault(Software=>M2)
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

end
restart
load "degree-computation.m2"
peek PH.GateHomotopySystem
