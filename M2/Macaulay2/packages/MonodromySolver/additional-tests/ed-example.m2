needsPackage "MonodromySolver"
m = 4
n = 2
declareVariable \ {t_1,t_2,u_0,u_1,u_2,u_3}
paramMatrix = gateMatrix{{u_0,u_1,u_2,u_3}}
varMatrix = gateMatrix{{t_1,t_2}}

phi = transpose gateMatrix{{t_1^3, t_1^2*t_2, t_1*t_2^2, t_2^3}}
phiEval = gateSystem(varMatrix, phi)
assert(m==numrows phi)
--distance = sum for i from 0 to 2 list (u_i-phi_(0,i))^2
loss = sum for i from 0 to 3 list (u_i - phi_(i,0))^2
dLoss = diff(varMatrix, gateMatrix{{loss}})
G = gateSystem(paramMatrix,varMatrix,transpose dLoss)
(u0, x0) = createSeedPair G
norm evaluate(G,u0,x0)
(P, p0, x0) = (G, u0, x0)
(p1, x1s) = solveFamily(G, Equivalencer=>(x-> point evaluate(phiEval, x)))
assert(length points x1s == 7)

end
restart
needs "ed-example.m2"
