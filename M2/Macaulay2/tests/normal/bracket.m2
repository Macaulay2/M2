----- This file contains tests for bracket power of ideals.
R = QQ[a..d]
I = minors(2,matrix {{a,b,c},{b,c,d}})
J = monomialIdeal {a^2*b*c*d, b^2*c*d, b*c*d^4}

-- these assertions were generated via 'generateAssertions'
--- computational checks
assert( (I^[3]) === ideal map((R)^1,(R)^{{-6},{-6},{-6}},{{-b^6+a^3*c^3, -b^3*c^3+a^3*d^3, -c^6+b^3*d^3}}) );
assert( (I^[1,2,3,4]) === ideal map((R)^1,(R)^{{-4},{-5},{-6}},{{-b^4+a*c^3, -b^2*c^3+a*d^4, -c^6+b^2*d^4}}) );
assert( (I^[0]) === ideal map((R)^1,(R)^{{-2},{-2},{-2}},0) );

--- existence of error checks
assert( (try I^[-1] else oops) === oops );
assert( (try I^[1,2,-1,3] else oops) === oops );
assert( (try I^[2,3,1] else oops) === oops );

-- same checks for monomial ideals
assert( (J^[3]) === monomialIdeal map((R)^1,(R)^{{-15},{-12},{-18}},{{a^6*b^3*c^3*d^3, b^6*c^3*d^3, b^3*c^3*d^12}}) );
assert( (J^[1,2,3,4]) === monomialIdeal map((R)^1,(R)^{{-11},{-11},{-21}},{{a^2*b^2*c^3*d^4, b^4*c^3*d^4, b^2*c^3*d^16}}) );
assert( (J^[0]) === monomialIdeal map((R)^1,(R)^1,{{1}}) );

assert( (try J^[-1] else oops) === oops );
assert( (try J^[1,2,-5,2] else oops) === oops );
assert( (try J^[2,3,1] else oops) === oops );


--- some checks for towers of rings
A = QQ[x,y]
B = A[z,w]
K = ideal gens B

assert( (K^[2,3]) === ideal map((B)^1,(B)^{{-2,0},{-3,0}},{{z^2, w^3}}) );
assert( (K^[2]) === ideal map((B)^1,(B)^{{-2,0},{-2,0}},{{z^2, w^2}}) );
assert( (try K^[2,3,4,5] else oops) === oops );