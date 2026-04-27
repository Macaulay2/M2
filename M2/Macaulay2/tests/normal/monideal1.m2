--
R=ZZ/101[x]
assert(monomialIdeal vars R != 0)
assert(monomialIdeal map(R^1,R^1,0) == 0)

--
R = QQ[x,y,z];
I = monomialIdeal(x^2,y^3,x*y^2*z,y*z^4);
J = polarize I;
assert(betti res I == betti res J)

I = monomialIdeal(x^2*y^2,y^2*z^2,x*y*z^4);
J = polarize(I, VariableBaseName => "whyNotAWord");
assert(betti res I == betti res J)
