R = ZZ/31991[a,b,c,d];
I = monomialCurveIdeal(R,{1,3,5})
projplane = Proj(R)
II = sheaf module I
can = sheafExt^1(II,OO_projplane^1(-4))
codim can
