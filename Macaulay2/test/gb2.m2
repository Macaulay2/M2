gbTrace = 3
R = ZZ/101[a,b,c,d]
f = matrix{{a*b-c,b^2-d}}
I = ideal f
S = R/I
gb f
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test gb2.out"
-- End:
