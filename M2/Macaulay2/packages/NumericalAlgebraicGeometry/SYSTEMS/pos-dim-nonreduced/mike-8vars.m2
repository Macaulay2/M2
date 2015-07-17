kk = QQ
R = kk[a,b,c,d,u,v,w,x];

I = ideal"
a + b + c + d,
u + v + w + x,
3ab + 3ac + 3bc + 3ad + 3bd + 3cd + 2,
bu + cu + du + av + cv + dv + aw + bw + dw + ax + bx + cx,
bcu + bdu + cdu + acv + adv + cdv + abw + adw + bdw + abx + acx + bcx,
abc + abd + acd + bcd,
bcdu + acdv + abdw + abcx"

end

restart
load "mike-8vars.m2"
needsPackage "NumericalAlgebraicGeometry"

minPrimes = decompose I
comps = select(primaryDecomposition I, Q->any(minPrimes,Q'->radical Q==Q'))
comps / dim 
comps / (c -> degree c / degree radical c)
comps / degree

wallTime B := numericalIrreducibleDecomposition(I,Software=>BERTINI)
assert(sum(B#1/degree) == 18)
assert(sum(B#2/degree) == 6)

setRandomSeed 0 -- fails
setRandomSeed 1 -- works
wallTime M := numericalIrreducibleDecomposition I
assert(sum(M#1/degree) == 18)
assert(sum(M#2/degree) == 6)
