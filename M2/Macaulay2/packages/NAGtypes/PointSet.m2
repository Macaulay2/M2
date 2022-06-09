PointSet = new Type of HashTable
pointSet = method()
pointSet Thing := L -> (
    if not instance(L,List) and not instance(L,Set) then error "a list/set is expected"; 
    LL := toList L;
    if not all(LL, x->instance(x,AbstractPoint)) then error "a list/set of Points is expected"; 
    S := solutionsWithMultiplicity LL;
    new PointSet from apply(#S, i->((S#i).cache.Multiplicity=1; i=>S#i))	 
    ) 
net PointSet := P -> net values P

areEqual (PointSet,PointSet) := o-> (a,b) -> areEqual(values a, values b, o)
PointSet == PointSet := (A,B) -> areEqual(A,B)

unionPointSet = method(Options=>{Tolerance=>1e-6})
unionPointSet (PointSet,PointSet) := o -> (A,B) -> (
    S := solutionsWithMultiplicity(values A | values B, Tolerance=>o.Tolerance); 
    new PointSet from apply(#S, i->((S#i).cache.Multiplicity=1; i=>S#i))	 
    )
PointSet + PointSet := (A,B) -> unionPointSet(A,B)  
PointSet - PointSet := (A,B) -> differencePointSet(A,B)
differencePointSet = method(Options=>{Tolerance=>1e-6})
differencePointSet (PointSet,PointSet) := o -> (A,B) -> (
    D := new MutableHashTable;
    i := 0; 
    j := 0;
    c := 0;
    while i<#A and j<#B do (
	a := isGEQ(A#i,B#j,Tolerance=>o.Tolerance);
	if not a then (D#c = A#i; i=i+1; c=c+1)
	else (
	    if areEqual(A#i,B#j,Tolerance=>o.Tolerance) then i=i+1;
	    j=j+1;
	    ) 
	);
    if j==#B then for i' from i to #A-1 do (D#c = A#i'; c=c+1);
    new PointSet from D
    )

TEST /// 
    needsPackage "NAGtypes"
    A = set {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}}} /point // pointSet
    B = {{{1,3.1}},{{0,3}}}/point//pointSet
    assert(A + B == {{{1,3}},{{2,5}},{{0,3}},{{1+ii,3}},{{1,3.1}}} /point // pointSet)	
    assert(A - B == {{{1, 3}}, {{1+ii, 3}}, {{2, 5}}}/point//pointSet)
///
