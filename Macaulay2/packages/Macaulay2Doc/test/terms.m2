-- test 'terms'
QQ[a,b][x,y]
assert( (terms (1+a+b)^3) === {a^3, 3*a^2*b, 3*a*b^2, b^3, 3*a^2, 6*a*b, 3*b^2, 3*a, 3*b, 1+a-a} )
assert( (terms (1+a+x)^3) === {x^3, (3*a+3)*x^2, (3*a^2+6*a+3)*x, a^3+3*a^2+3*a+1+x-x} )
terms(ZZ,x)						    -- crashes!
-- ../../../../Macaulay2/e/polyring.cpp:1417: ring_elem PolyRing::get_logical_coeff(const Ring*, const Nterm*&) const: Assertion `KR' failed.
