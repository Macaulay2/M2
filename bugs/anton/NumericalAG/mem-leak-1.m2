-- SLOW MEMORY LEAK -- FIXED

needsPackage "SLPexpressions"
debug NumericalAlgebraicGeometry
load "NAGtools.m2"
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

m = 2
for i from 1 to m do z_i = inputGate symbol z_i 
f = 1+4*z_1+4*z_2+z_1^2+4*z_1*z_2+z_2^2
zs = apply(m, i->z_(i+1))
F = transpose matrix{apply(zs, z->z*diff(z,f)/f)}
PHS = gateHomotopy4preimage(F,zs)
x0 = transpose matrix{{1_CC,1}}
y0 = value(F,valueHashTable(zs,flatten entries x0))
y1 = transpose matrix{{0_CC,1.9}}
y0y1 = y0||y1;
HS = specialize(PHS,y0y1);

K = CC_53
evaluateH(HS, x0, random RR);
H = HS.ParameterHomotopySystem.GateHomotopySystem; eH = H#(H#"H",K)
debug Core
real = 0.0
for i to 1000000000 do (
    -- m0 = transpose y0y1 | transpose x0 | matrix{{0.0_CC}}; -- no leak
    -- m0 = transpose y0y1 | transpose x0 | matrix{{0}}; -- no leak
    m0 = transpose y0y1 | transpose x0 | matrix{{0.0}}; -- leaked
    matrix{{0.0}}; -- leaked
    -- matrix{{0.0_CC}}; -- no leak
    -- 0.0_RR; -- no leak
    matrix{{real}}; -- leaked
    -- matrix{{0.0p100}}; -- no leak
    -- matrix{{0.0p53+ii}}; -- no leak
    -- rawSLEvaluatorEvaluate(eH, raw m0);
    --evaluateHx(HS, x0, random RR);
    --evaluateHt(HS, x0, random RR);
--s = first trackHomotopy(HS, {x0}, Software=>M2);
if i%1000==0 then (collectGarbage(); print i); 
-- x1 = transpose matrix s;
-- assert areEqual(value(F,valueHashTable(zs,flatten entries x1)),y1)
)

end
restart
load "mem-leak-1.m2"

