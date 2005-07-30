QQ[w,x,y,z];
I = minors(2, matrix{{w,x,y},{x,y,z}})
isMonomialIdeal I
J = ideal leadTerm I
isMonomialIdeal J
K = monomialIdeal I
isMonomialIdeal K
