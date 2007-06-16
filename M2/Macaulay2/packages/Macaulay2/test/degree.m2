S = ZZ[t,Inverses=>true, MonomialOrder=>RevLex]
h = 1-3*t^3+3*t^6-t^9
q = h//(1-t)
r = h %(1-t)
assert( h == q*(1-t)+r )
assert( r == 0 )

S = ZZ[t,Degrees=>{{}}, Inverses=>true, MonomialOrder=>RevLex]
h = 1-3*t^3+3*t^6-t^9
q = h//(1-t)
r = h %(1-t)
assert( h == q*(1-t)+r )
assert( r == 0 )

R = QQ[x,y,z]
M = coker matrix {{x^3,y^3,z^3}}
assert( degree M == 27 )

R = ZZ[x]
M = subquotient( matrix {{x^3}}, matrix {{x^7}} )
assert ( degrees M == {{3}} )
