R = ZZ/32003[x,y,s,t,u,v]
f = x^2+x^3+y^2
jf = jacobian matrix {{f}}
tt = matrix{{s,t,u,v,0,0}}
f1 = tt*jf
jf1 = jacobian f1
f2 = tt*jf1
j2 = ideal (f | f1 | f2)
S = ZZ/32003[x,y,s,t]
A = R/j2
use A
phi = map(A,S,{x,y,s,t})
ker phi
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ker2.out"
-- End:
