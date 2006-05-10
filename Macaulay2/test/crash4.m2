R = ZZ/101[a..d]
S = R/(a^3,b^3)
assert(Hom(S^1,S^1) == S^1)
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test crash4.out"
-- End:
