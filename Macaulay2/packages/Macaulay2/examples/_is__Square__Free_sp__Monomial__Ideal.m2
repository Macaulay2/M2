QQ[x,y,z];
J = monomialIdeal(x^3*y^5*z, y^5*z^4, y^3*z^5, 
	       x*y*z^5, x^2*z^5, x^4*z^3, x^4*y^2*z^2, 
	       x^4*y^4*z)
isSquareFree J
isSquareFree radical J
