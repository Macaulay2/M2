R = ZZ/101[a,b,c,d,Degrees=>{{0,4},{1,3},{3,1},{4,0}},Heft=>{1,1}]
I = monomialCurveIdeal(R,{1,3,4})
B = flatten entries basis(degree(a*b*c*d),R)
time nB = hilbertFunction(degree(a*b*c*d),R)
assert(nB == #B)
