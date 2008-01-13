R = QQ[x,y];
F = R^{1,2,3}
G = R^{10,20,30}
f = flip(F,G)
isHomogeneous f
target f
source f
target f === G**F
source f === F**G
u = x * F_0
v = y * G_1
u ** v
v ** u
f * (u ** v)
f * (u ** v) === v ** u
