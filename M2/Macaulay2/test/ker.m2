R=ZZ/101[a,b,c]
S=ZZ/101[t]
assert( ker map(S,R,{t^4,t^5,t^6}) == ideal matrix {{b^2-a*c, a^3-c^2}} )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test ker.out"
-- End:
