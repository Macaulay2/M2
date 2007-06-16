debug Core

R=ZZ/101[a,b,c]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6})
assert not isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )

R=ZZ/101[a,b,c,Degrees=>{4,5,6}]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6})
assert isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
assert checkHilbertHint generators graphIdeal f

R=ZZ/101[a,b,c,DegreeRank=>3]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6},DegreeMap => d -> {4*d#0 + 5*d#1 + 6*d#2})
assert isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
assert checkHilbertHint generators graphIdeal f

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test ker.out"
-- End:
