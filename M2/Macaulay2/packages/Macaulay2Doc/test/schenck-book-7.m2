end
print generateAssertions ///
needsPackage "NormalToricVarieties"
H2 = hirzebruchSurface(2)            
fan H2
rays H2
isFano H2
D=toricDivisor({3,0,0,-5},H2)
isCartier D
isAmple D
M = weilToClass H2
W=matrix{{3,0,0,-5}}
DinClass = M*transpose W
L = sheaf(H2, (ring H2)^{{3,-5}})
apply(3, i->rank HH^i(H2,L))
chi=(X,F)->(sum((dim X)+1,i->((-1)^i)*(rank HH^i(X,F))))
chi(H2, L)
needsPackage "BoijSoderberg"
needsPackage "BGG"
OM = cotangentSheaf H2
cohomologyTable(1,OM,{-2,-2},{2,2})
///
