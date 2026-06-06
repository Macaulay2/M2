needs "./GelfandTsetlin/gelfandTsetlinTypeA.m2"
needs "./GelfandTsetlin/gelfandTsetlinInvariant.m2"


dynkinToPartition = method(
    TypicalValue=>List
);

dynkinToPartition(String,List) := (type,v) -> (
    if type!="A" then error "Only implemented for type A";
    n:=0;
    if type=="A" then (
        n=#v;
        return append(apply(n, i -> sum apply(n-i, j -> v_(n-1-j))),0)
    );
);




---------------------------------
-- 1. Define the GTPattern type
---------------------------------

GTPattern = new Type of HashTable 

-- Lighten the return output
net GTPattern := G -> net(G#"type",G#"entries")



gtIndices = (type,n) -> (
    if not member(type,{"A","D"}) then error "Only implemented for types A and D";
    if type=="A" then return flatten apply(n, i -> apply(n-i,j -> (n-i,j+1)));
    if type=="D" then return flatten flatten apply(n, i -> if i==0 then {apply(n-i,j -> (n-i,j+1,1))} else {apply(n-i,j -> (n-i,j+1,0)),apply(n-i,j -> (n-i,j+1,1))})
);



---------------------------------
-- 2. Basic properties of a GTPattern
---------------------------------

isValidEntryList = (type,L) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return isValidEntryListA(L);
);



gtContent = (type,n,H) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return gtContentA(n,H);
);



gtWeight = (type,n,H) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return gtWeightA(n,H);
);



gtPatternFromEntries = method(
    TypicalValue=>List
);

gtPatternFromEntries(String,List) := (type,L) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return gtpA(L);
)



--------------------------------------
-- 3. List the GTPatterns of shape lambda
--------------------------------------

gtPolytope = method(
    TypicalValue=>List
);

gtPolytope(String,List) := (type,lambda) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return gtPolytopeA(lambda);    
);


-- This returns a list of the entries
gtPatterns = method(
    TypicalValue=>List
);

gtPatterns(String,List) := memoize((type,lambda) -> (
    if type!="A" then error "Only implemented for type A";
    if type=="A" then return gtPatternsA(lambda);    
));



GTrepresentationMatrices = method(
    TypicalValue=>List
);

GTrepresentationMatrices(LieAlgebraModule) := (V) -> (
    g:=V#"LieAlgebra";
    if g#"RootSystemType" != "A" then error "Only implemented for type A";
    if not isIrreducible(V) then error "Not implemented for reducible modules yet";
    GTrepresentationMatricesA(V)
);

