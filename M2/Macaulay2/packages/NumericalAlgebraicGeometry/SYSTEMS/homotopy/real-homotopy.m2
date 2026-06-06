restart
errorDepth = 0
needsPackage "NumericalAlgebraicGeometry"
setTryJustInTimeCompilation false
setTryJustInTimeCompilation true
-*
examples gateHomotopy
examples trackHomotopy
*-
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-0.5}
H = (1 - T) * F + T * G
HS = gateHomotopy(transpose matrix {H},matrix{{X,Y}},T)
x0 := point {{1.0,1.0}}
p := first trackHomotopy(HS,{x0})
peek p
coordinates p 
peek p.cache 
