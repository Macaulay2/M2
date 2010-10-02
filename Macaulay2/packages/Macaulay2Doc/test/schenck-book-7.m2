needsPackage "NormalToricVarieties"
H2 = hirzebruchSurface(2)            
assert( net (fan H2) === "{ambient dimension => 2         }
 number of generating cones => 4
 number of rays => 4
 top dimension of the cones => 2"^0 ) -- toExternalString fails
assert( (rays H2) === {{1,0},{0,1},{-1,2},{0,-1}} )
assert( (isFano H2) === false )
assert( net (D=toricDivisor({3,0,0,-5},H2)) === "3*D  - 5*D
   0      3"^0 ) -- toExternalString fails
assert( (isCartier D) === true )
assert( (isAmple D) === false )
assert( (try M = weilToClass H2 else oops) === oops )
assert( (W=matrix{{3,0,0,-5}}) === map(ZZ^1,ZZ^4,{{3, 0, 0, -5}}) )
assert( (try DinClass = M*transpose W else oops) === oops )
C = ring H2                                                 -- added
assert( (L = sheaf(H2, (ring H2)^{{3,-5}})) === new CoherentSheaf from {symbol module => C^{{3,-5}}, symbol variety => H2} )
assert( (apply(3, i->rank HH^i(H2,L))) === {0,2,6} )
chi'=(X,F)->(sum((dim X)+1,i->((-1)^i)*(rank HH^i(X,F))))
assert( (chi'(H2, L)) === 4 )
assert( (try needsPackage "BoijSoderberg" else oops) === oops )
needsPackage "BGG"
assert( (OM = cotangentSheaf H2) === new CoherentSheaf from {symbol module => cokernel map(C^{{-2,0},{1,-2},{1,-2}},C^{{0,-2}},{{2*x_1*x_3}, {x_0}, {-x_2}}), symbol variety => H2} )
assert( (try cohomologyTable(1,OM,{-2,-2},{2,2}) else oops) === oops )

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
C = ring H2						    -- added
L = sheaf(H2, (ring H2)^{{3,-5}})
apply(3, i->rank HH^i(H2,L))
chi'=(X,F)->(sum((dim X)+1,i->((-1)^i)*(rank HH^i(X,F))))
chi'(H2, L)
needsPackage "BoijSoderberg"
needsPackage "BGG"
OM = cotangentSheaf H2
cohomologyTable(1,OM,{-2,-2},{2,2})
///
