A = QQ[a]
f = a^2+1
k = A/f
toField k
R = k[v_2]
p = (v_2-a)*(v_2^5-a)
q = (v_2-a)*(v_2^7-a)
debug Core
rawGCD(raw p,raw q,raw f)


F = ambient first flattenRing(R,CoefficientRing=>QQ)
p = lift(p,F)
q = lift(q,F)

