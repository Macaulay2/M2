R = ZZ/101[x,y,z]
f = x^2
I = matrix{{x^2,y^2}}
P=matrix({{2,3,4}})
substitute(f,P)
substitute(I,P)
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst.out"
-- End:
