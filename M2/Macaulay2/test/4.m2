-- taken from:
-- Bernd Sturmfels, FOUR COUNTEREXAMPLES IN COMBINATORIAL ALGEBRAIC GEOMETRY.

-- 1
R = QQ[a,b,c,d,e,f]
M = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
assert( regularity module M == 3 )
assert( regularity module M^2 == 7 )

S = ZZ/2[a,b,c,d,e,f]
M = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
assert( regularity module M == 4 )
assert( regularity module M^2 == 7 )

use R
M = ideal(d*e*f,c*e*f,c*d*f,c*d*e,b*e*f,b*c*d,a*c*f,a*d*e)
assert( regularity module M == 3 )
assert( regularity module M^2 == 7 )

-- 2
S = QQ[x,y,z]
Ideals = { ideal(x, y, z^8), ideal(y*z, x, y^2, z^7),
     ideal(y*z^2, x, y^2, z^6), 
     ideal(y*z^3, x, y^2, z^5),
     ideal(y*z^2, y^2*z, x, y^3, z^5), 
     ideal(y^2*z, y*z^3, x, y^3, z^4),
     ideal(x*y,x*z,y*z^2,x^2,y^2,z^5), 
     ideal(x*y,x*z,y*z^3,x^2,y^2,z^4),
     J = ideal(x*y,x*z^2,y*z^2,x^2,y^2,z^4), 
     ideal(x*y,y*z,x*z,x^2,y^2,z^6),
     ideal(x*y,x*z,y*z^2,y^2*z,x^2,y^3,z^4),
     I8 = ideal(x^2,x*y,x*z^2,y*z^2,y^2*z,z^3,y^3) 
     }

degtan = I -> (
     R := ring I;
     degree Hom(I, R^1/I))

hilbtan = I -> (
     Tangentspace = Hom(module I, S^1/I);
     << {numgens I, degree I, degtan I, I} << endl; 
     )

assert( degree J == 8 )
assert( degtan J == 36 )
assert( degree I8 == 8 )
assert( degtan I8 == 32 )

K = ideal (x^3, x^2*y, x*y*z, x^2*z, x*z^2, x*y^3, y^3*z, y^2*z^2, y*z^3, z^4, y^5)
assert( degree K == 16 )
assert( degtan K == 88 )

--

S = ZZ/101[a,b,c,d,e,f]
I = ideal( a*(c+d) - b*f,
     b*(b+f) - c*e,
     c*(a+c+f) - d*(c+d),
     d*f - e*(b+f),
     e*e - a*(a+c+f) )
assert( codim I == 4 )
assert( pdim coker gens I == 4 )
assert( # decompose I == 1 )

S = QQ[a,b,c,d,e,f]
I = ideal( a*(c+d) - b*f,
     b*(b+f) - c*e,
     c*(a+c+f) - d*(c+d),
     d*f - e*(b+f),
     e*e - a*(a+c+f) )
assert( codim I == 4 )
assert( pdim coker gens I == 4 )
J = trim( minors(4,jacobian I) + I )
assert( codim J == 6 )
assert( 
     hilbertPolynomial coker gens I == 
     -17 * projectiveHilbertPolynomial 0 + 11 * projectiveHilbertPolynomial 1
     )
R = S/I
C = res (coker vars R, LengthLimit => 5)
betti C
assert( regularity C == 1 )
assert( rank C_5 == 281 )
assert( tally degrees C_3 === tally splice {51 : {3}, 1 : {4}} )
assert( tally degrees C_5 === tally splice {216 : {5}, 65 : {6}} )

-- 

R = QQ[v,w,x,y,z]
S = QQ[a,b,c,d,e,f,g,h,i]
p = map(R,S, {z, v*z, w*z, x*z, v*w*x*y*z, v*w*x^2*y^2*z,
	  v*w^2*x^2*y^3*z, v*w^2*x^3*y^4*z, v*w^2*x^3*y^5*z})
IX = kernel p
assert( degree IX == 18 )
assert( codim IX == 4 )
assert( pdim coker gens IX == 4 )
inIX = ideal leadTerm gens gb IX
assert( codim inIX == 4 )
assert( pdim coker gens inIX == 5 )
S' = QQ[f,g,h,i,a,b,c,d,e]
IX' = (map(S',S)) IX
inIX' = ideal leadTerm gens gb IX'
assert( codim inIX' == 4 )
assert( pdim coker gens inIX' == 4 )
