R = ZZ/101[a..e];
I = monomialCurveIdeal(R,{2,3,5,7})
J = ideal presentation singularLocus(R/I)
codim J
radical J
