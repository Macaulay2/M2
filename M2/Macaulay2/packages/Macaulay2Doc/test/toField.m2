k = toField(QQ[a]/ideal(a^2+15))
R = k[x,y,z];
f = (-3+5*a)*x^6+(135+15*a)*x^4*y*z-(45-15*a)*x^2*y^2*z^2-18*x*y^5-18*x*z^5+(15+5*a)*x^3*y^3;
I = ideal f;
assert( numgens R.FlatMonoid == 3 )
assert isHomogeneous I					    -- fails in 1.2, fixed in 1.3
assert( dim I == 2 )					    -- fails in 1.2
assert( dim Proj (R/I) == 1 )				    -- fails in 1.2
assert( hilbertPolynomial I == - 15 * hilbertPolynomial (QQ[x]) + 6 * hilbertPolynomial (QQ[x,y]) ) -- fails in 1.2


A = ZZ[a]/(a^2+3);
L = toField A
L[x,y,z]
b = try gb ideal (a*x^2-y^2-z^2, y^3, z^3) else getNonUnit L
assert( a == b )
gb ideal (a*x^2-y^2-z^2, y^3, z^3)			    -- this should give an error!
