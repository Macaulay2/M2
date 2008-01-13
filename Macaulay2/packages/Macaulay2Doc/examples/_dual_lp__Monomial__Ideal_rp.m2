QQ[a,b,c,d];
I = monomialIdeal(a*b, b*c, c*d)
dual I
intersect(monomialIdeal(a,b), 
               monomialIdeal(b,c),
               monomialIdeal(c,d))
dual dual I
QQ[x,y,z];
I = monomialIdeal(x^3, x*y, y*z^2)
dual(I, {4,4,4})
intersect( monomialIdeal(x^2),
               monomialIdeal(x^4, y^4),
               monomialIdeal(y^4, z^3))
QQ[x,y,z];
J = monomialIdeal( x^3*y^2, x*y^4, x*z, y^2*z)
dual dual J
dual( dual(J, {3,4,1}), {3,4,1})
