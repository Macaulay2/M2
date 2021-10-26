-- tracking down a bug in test/4b.m2
S = QQ[x,y,z]
f = matrix{{x*y,x*z^2,y*z^2,x^2,y^2,z^4}}
s = syz f 
p = poincare cokernel f
g = matrix{{x*y,x*z^2,y*z^2,x^2,y^2,z^4}}
t = syz gb(g, Syzygies=>true, Hilbert => p)
assert(rank source s == rank source t)
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test 4-b.out"
-- End:
