-- example by Nobuki Takayama, July 2015
needsPackage "SLPexpressions"
load "NAGtools.m2"
debug NumericalAlgebraicGeometry
PH = (q,n)->if n==0 then 1 else product apply(n,i->q+i)   
makeF = abcz -> (
    (a,b,c,z) := abcz;
    m := #z; 
    Z := symbol Z; 
    R := QQ[Z_1..Z_m]; 
    exps := flatten entries basis(0,m,R) / exponents / first; 
    sum(exps, k->(
	    PH(a,sum k) * product apply(m,i->PH(b#i,k#i)) /
    	    PH(c,sum k) * product apply(m,i->PH(1,k#i)) 
	    ) * product apply(m,i->(z#i)^(k#i))
    	)
    )
end

restart -- Osaka, July 2015. 
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

restart -- No2: m=2  
load "nobuki.m2"
m = 2
for i from 1 to m do z_i = inputGate symbol z_i 
f = 1+4*z_1+4*z_2+z_1^2+4*z_1*z_2+z_2^2
zs = apply(m, i->z_(i+1))
F = transpose matrix{apply(zs, z->z*diff(z,f)/f)}
PHS = gateHomotopy4preimage(F,zs,zs)
x0 = transpose matrix{{1_CC,1}}
y0 = value(F,valueHashTable(zs,flatten entries x0))
y1 = transpose matrix{{0_CC,1.9}}
HS = specialize(PHS,y0||y1)
time s = first trackHomotopy(HS, {x0}, Software=>M2)
peek s
x1 = transpose matrix s
assert areEqual(value(F,valueHashTable(zs,flatten entries x1)),y1)

restart -- No1: arbitrary m  
load "nobuki.m2"
K = CC;
m = 8 -- crashes
m = 7; recursionLimit = 100000 -- works
for i from 1 to m do z_i = inputGate symbol z_i
zs = apply(m,i->z_(i+1)) 
f = makeF (-m, toList(m:-m), 1, zs);
F = transpose matrix{apply(zs, z->z*diff(z,f)/f)};
PHS = gateHomotopy4preimage(F,zs,zs)
x0 = transpose matrix{toList(m:1_K)}
y0 = value(F,hashTable(apply(zs,flatten entries x0,(i,j)->(i=>j))|{cache=>new CacheTable}))
while true do (
    x1known = transpose matrix{apply(m, i->toRR random 10)};
    y1 = value(F,hashTable(apply(zs,flatten entries x1known,(i,j)->(i=>j))|{cache=>new CacheTable}));
    HS = specialize(PHS,y0||y1);
    time s = first trackHomotopy(HS, {x0}, Software=>M2);
    print (transpose x1known=>status s=>s.NumberOfSteps);
    x1 = transpose matrix s;
    assert areEqual(x1, x1known);
    )


-- (-200,[-300,-400,-400,400,400,400],10)

restart -- No1: arbitrary m  
load "nobuki.m2"
K = CC;
m = 6; recursionLimit = 100000 -- works
for i from 1 to m do z_i = inputGate symbol z_i
zs = apply(m,i->z_(i+1)) 
f = makeF (-200, {-300,-400,-400,400,400,400}, 10, zs);
F = transpose matrix{apply(zs, z->z*diff(z,f)/f)};
PHS = gateHomotopy4preimage(F,zs)
x0 = transpose matrix{toList(m:1_K)}
y0 = value(F,hashTable(apply(zs,flatten entries x0,(i,j)->(i=>j))|{cache=>new CacheTable}))
while true do (
    x1known = transpose matrix{apply(m, i->-*random RR*- 10+0.1*random RR)}; -- unstable for a random value in the box 
    y1 = value(F,hashTable(apply(zs,flatten entries x1known,(i,j)->(i=>j))|{cache=>new CacheTable}));
    HS = specialize(PHS,y0||y1);
    time s = first trackHomotopy(HS, {x0}, Software=>M2);
    print (transpose x1known=>status s=>s.NumberOfSteps);
    x1 = transpose matrix s;
    assert areEqual(x1, x1known);
    )

-- (-200,[-300,-400,-400,400,400,400],10)
restart
load "curl.m2"
QQ[s,x,y,dx,dy,WeylAlgebra=>{x=>dx,y=>dy}]
genericBfct({x*dx+y*dy-3,x*dx-y*dy},{x,y},{dx,dy},{1,1})

fdHessian2(-2,{-3,-2},10,{1/2,1/3})



