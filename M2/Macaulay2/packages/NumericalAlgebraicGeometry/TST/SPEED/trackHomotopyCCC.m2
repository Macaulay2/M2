debug needsPackage "NumericalAlgebraicGeometry"
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
K = CC
R = K[x,y,t] 
F = {X*X-1, Y*Y-1}
G = {X*X+Y*Y-1, -X*X+Y}
H = (1 - T) * F + T * G
Rvars = valueHashTable({X,Y,T},{x,y,t})
gV = matrix{{X,Y}}
gH = transpose matrix {H}

HS = gateHomotopy(gH,gV,T)
prec = 53
inp = matrix(CC_prec,{{1,1,-1,-1},{1,-1,1,-1}})
s = trackHomotopy(HS,apply(numColumns inp,i->inp_{i}),Software=>M2engine)
peek s
assert (norm evaluateH(HS, transpose matrix first s, 1) < 1e-6)

elapsedTime for i to 1000 do trackHomotopy(HS,apply(numColumns inp,i->inp_{i}),Software=>M2engine)

NAGtrace 3
inp1 = matrix(CC_prec,{{1,1,-1,-1},{1,-1,1,-1}})
inp = matrix {toList(1000:inp1)}
elapsedTime  s = trackHomotopy(HS,apply(numColumns inp,i->inp_{i}),Software=>M2engine);


end
restart
load "NumericalAlgebraicGeometry/TST/SPEED/trackHomotopyCCC.m2"
