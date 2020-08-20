-- it doesn't work yet, sigh

S = QQ[p,q,r,s,t];
R = QQ[x,y,z,p,q,r,s,t];
POLY = -2*y*p*t^2+t^2*x^2+y^2*t^2+p^2*t^2-2*s*y*t*z+2*s*p*t*z+2*q*t*x-2*s*p*t*q-2*z*t*x+
       2*s*y*t*q-2*z*q+z^2*s^2+q^2*s^2+y^2+2*p*s*x-2*y*p-2*y*s*x+p^2+z^2+q^2-2*z*q*s^2+
       s^2*x^2-(1+s^2+t^2)*r;
V = {{0,1,0},{0,0,1},{0,-3/5,-4/5},{-1,-1,0},{1,0,-1}}
I = {};
i = 0;
while i < 5 do (
        I=append(I,substitute(POLY, {x => V_i_0, y => V_i_1, z => V_i_2 }));
        i=i+1)
J = substitute(ideal I,S);
dim J
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test sottile.out"
-- End:
