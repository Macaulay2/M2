-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"


i1 : R = ZZ/15
stdio:1:7:(2): ZZ/n not implemented yet for composite n

--------------------------------------------------------------

i6 : r = QQ[a,b,c,d,x,y, MonomialOrder => Eliminate 4]

o6 = r

o6 : PolynomialRing

i7 : f = map(frac(r),r,{y^2/(x^3+x*y), x^5/(x^2*y+y^2), y^3/(x^5+x^3*y),--
		  x*y/(x^2+y),x,y});

o7 : RingMap frac(r) <--- r

i8 : p = kernel f
stdio:10:5:(2): not implemented yet


r = QQ[a,b,c,d,x,y, MonomialOrder => Eliminate 4]
f = map(frac(r),r,{y^2/(x^3+x*y), x^5/(x^2*y+y^2), y^3/(x^5+x^3*y),
		  x*y/(x^2+y),x,y});

p = kernel f

---------------------------------------------------------------------------------------

R = ZZ/101[x,y]
K = frac R
S = K[u,v]
I = ideal(u^2, u*v, v^2)
gb I
A = ZZ/101[a,b,c]
f = map(frac S, A, {u^3/v^4, u^2/v^2, (u+v)/v^4})
kernel f

-- the map above gives error message

------------------------------------------------------------------------------
-- RevLex seems to be the same as GRevLex

i1 : R = ZZ[a..d, MonomialOrder=>{RevLex}]

o1 = R

o1 : PolynomialRing

i2 : gens gb ideal(a^2+b*d^2,b*c + a*d)

o2 = | ad3-a2c bc+ad bd2+a2 |

             1       3
o2 : Matrix R  <--- R

i3 : R = ZZ[a..d, MonomialOrder=>{GRevLex}]

o3 = R

o3 : PolynomialRing

i4 : gens gb ideal(a^2+b*d^2,b*c + a*d)

o4 = | bc+ad bd2+a2 ad3-a2c |

             1       3
o4 : Matrix R  <--- R

-------------------------------------------------------------------

i39 : R = ZZ[a..d, MonomialOrder=>{LexSmall}];
-- same problem for LexTiny and GrevLexTiny, GrevLexSmall

i40 : a^(2^16-1)

       65535
o40 = a

o40 : R

i41 : a^(2^16)

o41 = 1

o41 : R


Apparently you want the upper limit on LexSmall is 2^15-1 rather
than 2^16-1, and for LexTiny to be 2^7-1 rather than the current
2^8 - 1.

----------------------------------------------------------------

implement leadTerm(ZZ, ring element)

---------------------------------------------------------------

i66 : R = ZZ/101[x,y];

i67 : K = frac R;

i68 : S = K[u,v];

i69 : I = ideal(y^2*u^3 + x*v^3, u^2*v, u^4);

o69 : Ideal of S

i70 : gens gb I

o70 = | u2v u3+x/y2v3 v4 uv3 |

              1       4
o70 : Matrix S  <--- S

the element u3+x/y2v3 may not be written in the clearest way

----------------------------------------------------------------

GroupRevLex does not seem to be installed
Then uncomment the lines in monomorderings.m2 in the manual
for GroupRevLex.

------------------------------------------------------