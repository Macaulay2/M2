needsPackage "NormalToricVarieties"
H2 = hirzebruchSurface(2)            
fan H2
assert( (rays H2) === {{1,0},{0,1},{-1,2},{0,-1}} );
assert( (isFano H2) === false );
assert( net (D=toricDivisor({3,0,0,-5},H2)) === "3*D  - 5*D
   0      3"^0 ); -- toExternalString fails
assert( (isCartier D) === true );
assert( (isAmple D) === false );
assert( (M = fromWDivToCl H2) === map((ZZ)^2,(ZZ)^4,{{1, -2, 1, 0}, {0, 1, 0, 1}}) );
assert( (W=matrix{{3,0,0,-5}}) === map((ZZ)^1,(ZZ)^4,{{3, 0, 0, -5}}) );
assert( (DinClass = M*transpose W) === map((ZZ)^2,(ZZ)^1,{{3}, {-5}}) );
C = ring H2     -- added
assert( (L = sheaf(H2, (ring H2)^{{3,-5}})) === new CoherentSheaf from {symbol variety => H2, symbol module => (C)^{{3,-5}}} );
assert( (apply(3, i->rank HH^i(H2,L))) === {0,2,6} );
chi'=(X,F)->(sum((dim X)+1,i->((-1)^i)*(rank HH^i(X,F))))
assert( (chi'(H2, L)) === 4 );
needsPackage "BoijSoederberg"
needsPackage "BGG"
assert( (OM = cotangentSheaf H2) === new CoherentSheaf from {symbol variety => H2, symbol module => cokernel map((C)^{{-2,0},{1,-2},{1,-2}},(C)^{{0,-2}},{{2*x_1*x_3}, {x_0}, {-x_2}})} );
cohomologyTable(ZZ,CoherentSheaf,List,List):=(k,F,lo,hi)->(
    degRange := toList(lo#0..hi#0);
    new CohomologyTally from select(flatten apply(degRange,
	        j -> apply(toList(lo#1..hi#1), 
	            i -> {(j,i-j), rank HH^k(variety F, F(i,j))})), 
	    p -> p#1 != 0)
    );
assert( (cohomologyTable(1,OM,{-2,-2},{2,2})) === new CohomologyTally from {(2,-4) => 3, (0,-2) => 3, (1,-3) => 4, (2,-3) => 2, (1,-2) => 2, (0,-1) => 2, (2,-2) => 1, (1,-1) => 1, (-1,1) => 1, (-2,2) => 1, (0,0) => 2, (-1,2) => 2, (-2,3) => 2, (0,1) => 2, (-1,3) => 4, (-2,4) => 3, (0,2) => 3} );

end--

end
-- The generateAssertions output needs to be modified to place
-- the function cohomologyTable in directly.
print generateAssertions ///
needsPackage "NormalToricVarieties"
H2 = hirzebruchSurface(2)            
fan H2
rays H2
isFano H2
D=toricDivisor({3,0,0,-5},H2)
isCartier D
isAmple D
M = fromWDivToCl H2
W=matrix{{3,0,0,-5}}
DinClass = M*transpose W
C = ring H2						    -- added
L = sheaf(H2, (ring H2)^{{3,-5}})
apply(3, i->rank HH^i(H2,L))
chi'=(X,F)->(sum((dim X)+1,i->((-1)^i)*(rank HH^i(X,F))))
chi'(H2, L)
needsPackage "BoijSoederberg"
needsPackage "BGG"
OM = cotangentSheaf H2
cohomologyTable(ZZ,CoherentSheaf,List,List):=(k,F,lo,hi)->(
    degRange := toList(lo#0..hi#0);
    new CohomologyTally from select(flatten apply(degRange,
	        j -> apply(toList(lo#1..hi#1), 
	            i -> {(j,i-j), rank HH^k(variety F, F(i,j))})), 
	    p -> p#1 != 0)
    );
cohomologyTable(1,OM,{-2,-2},{2,2})
///
