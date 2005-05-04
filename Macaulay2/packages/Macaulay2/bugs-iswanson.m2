-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"


i1 : R = ZZ/15
stdio:1:7:(2): ZZ/n not implemented yet for composite n

--------------------------------------------------------------

i2 : R = ZZ[x,y]

o2 = R

o2 : PolynomialRing

i3 : i = ideal(x^2)

            2
o3 = ideal(x )

o3 : Ideal of R

i4 : S = R/i

o4 = S

o4 : QuotientRing

i5 : frac S

o5 = frac(S)

o5 : FractionField

i6 : 1_S/x

     1
o6 = -
     x

o6 : frac(S)

i7 : x/x^2

Process M2 exited abnormally with code 139

-------------------------------------------------------------

When S as above was defined as S = ZZ[x,y]/x^2,
then x/x^2 returned 0, and didn't crash

------------------------------------------------------------


i6 : r = QQ[a,b,c,d,x,y, MonomialOrder => Eliminate 4]

o6 = r

o6 : PolynomialRing

i7 : f = map(frac(r),r,{y^2/(x^3+x*y), x^5/(x^2*y+y^2), y^3/(x^5+x^3*y),--
		  x*y/(x^2+y),x,y});

o7 : RingMap frac(r) <--- r

i8 : p = kernel f
stdio:10:5:(2): not implemented yet


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

-------------------------------------------------------------------------------------