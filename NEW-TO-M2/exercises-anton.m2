---------------------------------------------------------------------------
-- Shape Lemma 
---------------------------------------------------------------------------
Rlex = QQ[x,y,z,???]  -- what monomial order is necessary?
I = ideal {x^2+y^2+z^2-4, x^2+2*y^2-5, x*y*z-y}
??? -- compute the reduced GB, is its shape the "expected" shape?
J = sub(I,???) -- make a random change of coordinates 
??? -- compute the reduced GB, is its shape the "expected" shape?

---------------------------------------------------------------------------
-- Multiplication operators in quotient algebra 
---------------------------------------------------------------------------
I = ideal {x^2+y^2+z^2-4, x^2+2*y^2-5, x*y*z-y}
R = QQ[x,y,z]   
Mx = ??? -- start by making m_f for f=x (may require several lines)
eigenvalues Mx -- find x-coordinates of the points in V(I)

---------------------------------------------------------------------------
-- Is this a morphism?
---------------------------------------------------------------------------
R = QQ[x_0..x_2]
I = ideal {x_0^2+x_1^2,x_1^2+x_2^2}
???

---------------------------------------------------------------------------
-- Secant variety
---------------------------------------------------------------------------
R = QQ[x_0..x_2]
exps = flatten entries basis(2,R) / exponents / flatten
S = QQ[apply(exps,i->y_i)]
vars S
veronese = map(R,S,apply(exps,e->R_e))     
I = ??? veronese -- which command gets you the ideal you want?
ER = S/I**S/I**QQ[s,t] -- elimination ring
gens ER -- all y-variables seem to be duplicated... but only the NAMES coincide
n = numgens S
Y1 = (vars ER)_{0..n-1}
Y2 = (vars ER)_{n..2*n-1}
p = s*Y1+t*Y2 -- imagine a point on the secant variety
??? map(ER,S,p)

---------------------------------------------------------------------------
-- Area (squared) of a cyclic quadrilateral
---------------------------------------------------------------------------
-- Consider a quadrilateral with sides a,b,c,d inscribed in a circle.
-- Make extra assumptions: let the circle be centered at (0,0) and of radius r, 
-- let the vertices be A=(r,0), B=(x_1,y_1), C=(x_2,y_2), D=(x_3,y_3).
restart
distance2 = (A,B) -> (A#0-B#0)^2 + (A#1-B#1)^2 
signedArea = (A,B) -> 1/2 * det matrix {{A#0,A#1},{B#0,B#1}}
R = QQ[x_1..x_3,y_1..y_3,r,a,b,c,d,S];
xy = take(gens R, 6) -- you may want to eliminate these
??? -- get an elimination ideal (in a way similar to the example in the tutorial)
??? -- hint: you may have several components, each giving you a formula 

---------------------------------------------------------------------------
-- Homotopy continuation finds eigenvalues
---------------------------------------------------------------------------
needsPackage "NumericalAlgebraicGeometry"
KK = CC -- sometimes it helps to experiment with KK=QQ to gain intuition
R = KK[la,x_1,x_2]
A = random(F^2,F^2)
X = genericMatrix (R,x_1,2,1)
I = ideal(A*X - la*X)
polySystem I
???