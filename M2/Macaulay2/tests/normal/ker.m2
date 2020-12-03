R = QQ[x]
S = QQ[y]
f = map(S,R,{0})
assert( ker f === ideal vars R)
h = map(S^1,R^1,f,{{1}})
ker h
assert( ker h === image vars R)

debug Core

R=ZZ/101[a,b,c]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6})
assert not isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
h = map(S^1,R^1,f,{{1}})
assert( ker h == image matrix{{b^2-a*c, a^3-c^2}})

R=ZZ/101[a,b,c,Degrees=>{4,5,6}]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6})
assert isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
assert checkHilbertHint generators graphIdeal f
h = map(S^1,R^1,f,{{1}})
assert( ker h == image matrix{{b^2-a*c, a^3-c^2}})

R=ZZ/101[a,b,c,DegreeRank=>3]
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6},DegreeMap => d -> {4*d#0 + 5*d#1 + 6*d#2})
assert isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
assert checkHilbertHint generators graphIdeal f
h = map(S^1,R^1,f,{{1}})
-- assert( ker h == image matrix{{b^2-a*c, a^3-c^2}})

R=ZZ/101[c][b][a]
assert( degreeLength R == 3 )
S=ZZ/101[t]
f = map(S,R,{t^4,t^5,t^6},DegreeMap => d -> {4*d#0 + 5*d#1 + 6*d#2})
assert isHomogeneous f
assert( ker f == ideal matrix {{b^2-a*c, a^3-c^2}} )
-- assert checkHilbertHint generators graphIdeal f

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ker.out"
-- End:
