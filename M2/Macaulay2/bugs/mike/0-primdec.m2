R = QQ [x_1, x_2, x_3, x_4, x_5, x_6, a_1, a_2, a_3, a_4, a_5, a_6]
I = ideal(a_3*a_5-a_2*a_6,x_1*a_4+x_2*a_5+x_3*a_6,x_3*a_3-x_4*a_4-x_5*a_5,x_2*a_3+x_5*a_6,x_1*a_3+x_4*a_6,x_2*a_2+x_5*a_5,x_1*a_2+x_4*a_5,x_4*a_1+x_5*a_2+x_6*a_3,x_1*a_1-x_5*a_5-x_6*a_6,x_2*x_4-x_1*x_5,-x_3*x_4+x_1*x_6,-x_4*a_2,x_4*a_1+x_5*a_2+x_6*a_3,x_4*a_3,-x_2*a_2-x_3*a_3+x_4*a_4,-x_3*a_2-x_6*a_5,x_3*a_2+x_6*a_5,-x_2*a_2-x_5*a_5,-x_1*a_2-x_4*a_5,x_4*a_5,x_1*a_5,-x_3*a_3-x_6*a_6,x_1*a_1-x_5*a_5-x_6*a_6,x_3*a_3+x_6*a_6,x_2*a_3+x_5*a_6,-x_4*a_6,x_1*a_3+x_4*a_6,-x_1*a_4-x_2*a_5-x_3*a_6,-x_1*a_6,-x_2*x_4*a_1+x_3*x_5*a_3-x_2*x_6*a_3-x_4*x_5*a_4,x_3*x_5*a_5-x_2*x_6*a_5,-x_3*x_5*a_6+x_2*x_6*a_6,-x_1*x_2*a_1-x_1*x_5*a_4-x_3*x_5*a_6+x_2*x_6*a_6,x_3^2*x_5*a_1-x_2*x_3*x_6*a_1+x_3*x_5*x_6*a_4-x_2*x_6^2*a_4,x_2*x_3*x_5*a_1-x_2^2*x_6*a_1+x_3*x_5^2*a_4-x_2*x_5*x_6*a_4)
time decompose I
time primaryDecomposition I

primary decomposition doesn't work over towers

A = ZZ/101[a..d]/(a*d-b*c)
B = A[x,y,z]
I = ideal"(ax+by)(cx+dz)"
time decompose I
time primaryDecomposition I

loadPackage "ConwayPolynomials"
A = GF(9,Variable=>a)[b..d]/(a*d^2-b*c)
B = A[x,y,z]
I = ideal"(ax+by)(cx+dz)"
time decompose I
time primaryDecomposition I

A = ZZ[a..d]/(2*a*d-2*b*c)
B = A[x,y,z]
I = ideal"(ax+by)(cx+dz)"
time decompose I
time primaryDecomposition I


flattenRing I
-- neither does decompose:

i2 : A = QQ[a..d]/(a^2+b^2)

o2 = A

o2 : QuotientRing

i3 : B = A[x,y,z]/((a*x+b*y+c*z)
     

i4 : B = A[x,y,z]/((a*x+b*y+c*z))

o4 = B

o4 : QuotientRing

i5 : I = ideal"ax2,(b+c)(x+y)"

o5 = ideal (- b*x*y - c*x*z, (b + c)x + (b + c)y)

o5 : Ideal of B

i6 : decompose I
stdio:7:1:(1):[0]: error: 'Eliminate' strategy for 'saturate' expected argument 2 to be a principal ideal

