-- Osaka, July 2015. 
K = QQ
n = 2
R = K[z_1..z_n]
f = 1+4*z_1+4*z_2+z_1^2+4*z_1*z_2+z_2^2
G = apply(n, i-> R_i * diff(R_i,f) / f)
diffRat = (x,h)->(
    f = numerator h;
    g = denominator h;
    (diff(x,f)*g - diff(x,g)*f) / g^2
    )
J = apply(gens R, v->apply(G,g->diffRat(v,g))) 
S = det matrix J
applyTable(J, a->(degree numerator a, degree denominator a))
factor numerator S
factor denominator S

restart
load "SLP-expressions.m2"
load "NAGtools.m2"
n = 2
for i from 1 to n do z_i = inputGate symbol z_i 
f = 1+4*z_1+4*z_2+z_1^2+4*z_1*z_2+z_2^2
zs = apply(n, i->z_(i+1))
F = transpose matrix{apply(zs, z->z*diff(z,f)/f)}
PHS = gateHomotopy4preimage(F,zs)
x0 = transpose matrix{{1_CC,1}}
y0 = value(F,hashTable(apply(zs,flatten entries x0,(i,j)->(i=>j))|{cache=>new CacheTable}))
y1 = transpose matrix{{0_CC,1.9}}
HS = specialize(PHS,y0||y1)
time s = first trackHomotopy(HS, {x0}, Software=>M2)
peek s
x1 = transpose matrix s
value(F,hashTable(apply(zs,flatten entries x1,(i,j)->(i=>j))|{cache=>new CacheTable}))
