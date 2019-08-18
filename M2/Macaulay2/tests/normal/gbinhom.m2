R = ZZ/101[a..d]
try matrix{{a,b},{c,d}} % gb ideal(b+1,d) -- crashes, but should just give error
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test gbinhom.out"
-- End:
