ParabolicGroup = new Type of HashTable;
globalAssignment ParabolicGroup;

HomogeneousVariety = new Type of MutableHashTable;
globalAssignment HomogeneousVariety;

FiltrationBundle = new Type of MutableHashTable;
globalAssignment FiltrationBundle;

HomogeneousVectorBundle = new Type of FiltrationBundle;
globalAssignment HomogeneousVectorBundle;

EmbeddedVariety = new Type of MutableHashTable;
globalAssignment EmbeddedVariety;





newParabolic = method(TypicalValue => ParabolicGroup);
newParabolic (RootSystem,Set) := (R,S) -> (
    for i in toList S do if not (instance(i,ZZ) and i>0 and i<=rank(R)) then error "The indices designating the simple roots in the parabolic need to be integers at least 1 and at most the rank of the root system.";
    P := new ParabolicGroup from {
        symbol cache => new CacheTable,
        "dynkinType" => dynkinType(dynkinDiagram(R)),
        "parabolic" => parabolic(R,S)
        }
    );

homogeneousVariety = method(TypicalValue => HomogeneousVariety);
homogeneousVariety (RootSystem,ParabolicGroup) := (R,P) -> (
    X := new HomogeneousVariety from {
        symbol cache => new CacheTable,
        "dynkinType" => P#"dynkinType",
        "rootSystem" => R,
        "parabolicSubgroup" => P,
        "positiveRoots" => positiveRoots(R) - positiveRoots(R,P#"parabolic"),
        "picardRank" => rank(R) - #(P#"parabolic"),
        "dimVariety" => null,        
        "dimAmbientSpaces" => null,
        "firstChernClass" => null,
        "cotangent" => null,
        "cohomology" => null,
        "expression" => null
        }
    );

homogeneousVariety (RootSystem,Set) := (R,S) -> (
    P := newParabolic(R,parabolic(R,S));
    X := new HomogeneousVariety from {
        symbol cache => new CacheTable,
        "dynkinType" => P#"dynkinType",
        "rootSystem" => R,
        "parabolicSubgroup" => P,
        "positiveRoots" => positiveRoots(R) - positiveRoots(R,P#"parabolic"),
        "picardRank" => rank(R) - #(P#"parabolic"),
        "dimVariety" => null,        
        "dimAmbientSpaces" => null,
        "firstChernClass" => null,
        "cotangent" => null,
        "cohomology" => null,
        "expression" => null
        }
    );

Gr = method(TypicalValue => HomogeneousVariety);
Gr List := p -> (
    if #p =!= 2 then error "expected a list of lenght two";
    k := p#0;
    n := p#1;
    R := rootSystemA(n-1);
    P := newParabolic(R,set toList (1..(n-1)) - set{k});
    homogeneousVariety (R,P)
    )

Fl = method(TypicalValue => HomogeneousVariety);
Fl List := p -> (
    n := last p;
    p = drop(p,-1);
    R := rootSystemA(n-1);
    P := newParabolic(R,set toList (1..(n-1)) - set p);
    homogeneousVariety (R,P)
    )

OGr = method(TypicalValue => HomogeneousVariety);
OGr List := p -> (
    if #p =!= 2 then error "expected a list of lenght two";
    k := p#0;
    m := p#1;
    if (even m) then (
        n := lift(m/2,ZZ);
        R := rootSystemD(n);
        if k =!= n-1 then P := newParabolic(R,set toList (1..(n)) - set{k});
        if k == n-1 then  P = newParabolic(R,set toList (1..n) - set{n-1,n});
        );
    if (odd m) then (
        n = lift((m-1)/2,ZZ);
        R = rootSystemB(n);
        P = newParabolic(R,set toList (1..n) - set{k});
        );
    return homogeneousVariety(R,P);
    )

SGr = method(TypicalValue => HomogeneousVariety);
SGr List := p -> (
    if #p =!= 2 then error "expected a list of lenght two";
    k := p#0;
    m := p#1;
    if (odd m) then error "expected an even dimensional vector space";
    n := lift(m/2,ZZ);
    R := rootSystemC(n);
    P := newParabolic(R,set toList (1..(n)) - set{k});
    homogeneousVariety (R,P)
    )


homogeneousVectorBundle = method(TypicalValue => HomogeneousVectorBundle);
homogeneousVectorBundle (List,List,HomogeneousVariety) := (S,m,X) -> (
    if #S =!= #m then error "the number of weights must be equal to the number of multiplicities";
    P := X#"parabolicSubgroup";
    R := X#"rootSystem";
    RP := rootSystem(R,P#"parabolic");
    L := pack(2,mingle(S,m));
    L = new HashTable from tally flatten for c in L list toList(c#1:c#0);
    S = keys L;
    m = values L;
    T := toParabolicWeights(S,P);
    if not all(T, l -> isDominant(l,RP)) then error "expected dominant weights for the parabolic subgroup";
    E := new HomogeneousVectorBundle from {
	symbol cache => new CacheTable,
	"underlyingVariety" => X,
	"globalRootSystem" => X#"rootSystem",
	"rootSystem" => RP,
	"parabolic" => P,
	"weights" => S,
	"multiplicities" => m,
	"parabolicWeights" => T,
	"irreducible" => #S == 1 and m#0 == 1,
	"totally reducible" => true,
	"rank" => null,
	"totalChernClass" => null,
	"eulerCharacteristic" => null,
	"cohomology" => null
	}
    );


-*
A FiltrationBundle can be produced in two ways:
1. from a list S of homogeneous vector bundle S_0,...,S_n which are the factors which
represent a vector bundle F resolved as F = E_0 > E_1 > E_2 where S_i = E_i/E_i+1
2. from a list S of weights. Each term S_0,...,S_n ("factor") will
give an irreducible homogeneous vector bundle F_i and it will represent
a vector bundle F resolved as  F = E_0 > E_1 > E_2 where F_i = E_i/E_i+1
*-

filtrationBundle = method(TypicalValue => FiltrationBundle);
filtrationBundle (List,HomogeneousVariety) := (S,X) -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";  
    RP := rootSystem(R,P#"parabolic");
    if class S#0 === HomogeneousVectorBundle then (
	L := {};
	for e in S do (
	    L = L | apply(e#"weights", l -> weight(R, entries l));
	    );
	T := toParabolicWeights(L,P);
	E := new FiltrationBundle from {
	    symbol cache => new CacheTable,
	    "underlyingVariety" => X,
	    "globalRootSystem" => R,
	    "rootSystem" => RP,
	    "factors" => S,
	    "weights" => L,
	    "parabolicWeights" => T,
	    "irreducible" => false,
	    "totally reducible" => false,
	    "rank" => null,
	    "totalChernClass" => null,
	    "eulerCharacteristic" => null,
	    "cohomology" => null,
	    "cohomologyBounds" => null
	    }
	)
    else (
	T = toParabolicWeights(S,P);
	F := new MutableList from {};
	i := 0;
	if not all(T, l -> isDominant(l,RP)) then error "expected dominant weights for the parabolic subgroup";
	for i from 0 to #S-1 do (
	    F#i = homogeneousVectorBundle({weight(X#"rootSystem", entries S#i)},{1},X);
	    );
	E = new FiltrationBundle from {
	    symbol cache => new CacheTable,
	    "underlyingVariety" => X,
	    "globalRootSystem" => X#"rootSystem",
	    "rootSystem" => RP,
	    "factors" => new List from F,
	    "weights" => S,
	    "parabolicWeights" => T,
	    "irreducible" => false,
	    "totally reducible" => false,
	    "rank" => null,
	    "totalChernClass" => null,
	    "eulerCharacteristic" => null,
	    "cohomology" => null,
	    "cohomologyBounds" => null
	    }
	)
    );


bundles HomogeneousVariety := X -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    l := sort toList set toList(1..rank(R)) -  P#"parabolic"; -- lista dei nodi colorati
    if ((dynkinType R)#0)#0 == "A" then (
	v := new MutableList from rank R:0;
	if l#0 == 1 then v#(l#0-1) = -1;
	if l#0 =!= 1 then v#(l#0-2) = 1, v#(l#0-1) = -1;
	bundleR := {homogeneousVectorBundle({weight(R,toList v)},{1},X)};
	for i from 1 to #l-1 do (
	    v = new MutableList from rank R:0;
	    if l#i == l#(i-1) then v#(l#i-1) = -1;
	    if l#i =!= l#(i-1) then v#(l#i-2) = 1, v#(l#i-1) = -1;
	    bundleR = bundleR | {homogeneousVectorBundle({weight(R,toList v)},{1},X)};
	    );
	bundleR = bundleR | {homogeneousVectorBundle({weight(R,toList((rank R - 1):0)|{1})},{1},X)};
	)
    else (
	error "bundles not implemented yet";
	);
    toSequence bundleR
    )

info HomogeneousVariety := X -> (
    R := X#"rootSystem";
    P := X#"parabolicSubgroup";
    D := " a homogeneous variety of dimension " | dim X | "\n";
    D = D | " first Chern class is " | toString entries chern(1,X) | "\n";
    D = D | " with Picard rank " | rank(R) - #(P#"parabolic") | "\n\n";
    D = D | " with respect to the diagram " | toString (X#"dynkinType")#0 | " with marked nodes " | toString sort toList (set toList( 1..rank X#"rootSystem") - (X#"parabolicSubgroup")#"parabolic");
    return D;
    )


info FiltrationBundle := F -> (
    X := F#"underlyingVariety";
    if F#"totally reducible" then (
	if F#"irreducible" then (
	    D := " an irreducible vector bundle of rank " | rank F | "\n";
	    D = D | " first Chern class is " | toString entries chern(1,F) | "\n";
	    D = D | " with weight " | toString entries (F#"weights")#0 | "\n\n";
	    )
	else (
	    D = " a completely reducible vector bundle of rank " | rank F | "\n";
	    D = D | " first Chern class is " | toString entries chern(1,F) | "\n";
	    D = D | " it is given by the sum of \n\n";
	    for i from 0 to #(F#"weights")-1 do (
		E := homogeneousVectorBundle({(F#"weights")#i},{1},X);
		B := " an irreducible vector bundle of rank " | rank E | "\n";
		B = B | " first Chern class is " | toString entries chern(1,E) | "\n";
		B = B | " with weight " | toString entries (E#"weights")#0 | "\n\n";
		D = D | (F#"multiplicities")#i | " times " | B;
		);
	    );
	)
    else (
	D = " a filtration bundle of rank " | rank F | "\n";
	D = D | " first Chern class is " | toString entries chern(1,F) | "\n";
	D = D | " its filtration is: ";
	for i from 0 to #(F#"factors")-1 do (
	    D = D | "E_" | i | " > ";
	    );
	D = D | "E_" | #(F#"factors") | " = 0\n"; 
	D = D | " with factors E_i/E_i+1 given by: \n\n";
	i := 0;
	for E in F#"factors" do (
	    B := "E_" | i | "/E_" | i+1 | ": a completely reducible vector bundle of rank " | rank E | "\n";
	    B = B | " first Chern class is " | toString entries chern(1,E) | "\n";
	    B = B | " it is given by the sum of \n\n";
	    for j from 0 to #(E#"weights")-1 do (
		E' := homogeneousVectorBundle({(E#"weights")#j},{1},X);
		C := " an irreducible vector bundle of rank " | rank E' | "\n";
		C = C | "  first Chern class is " | toString entries chern(1,E') | "\n";
		C = C | "  with weight " | toString entries (E'#"weights")#0 | "\n\n";
		B = B | " " | (E#"multiplicities")#j | " times " | C;
		);
	    D = D | B;
	    i = i + 1;
	    );
	);
    D = D | " with respect to the diagram " | toString (X#"dynkinType")#0 | " with marked nodes " | toString sort toList (set toList( 1..rank X#"rootSystem") - (X#"parabolicSubgroup")#"parabolic");
    return D;
    )


	


embeddedVariety = method(TypicalValue => EmbeddedVariety);
embeddedVariety HomogeneousVectorBundle := F -> (
    X := F#"underlyingVariety";
    Y := new EmbeddedVariety from {
	symbol cache => new CacheTable,
	"picardRank" => null,
	"dimension" => dim X - rank F,        
	"ambientSpace" => X,
	"firstChernClass" => null,
	"cohomology" => null,
	"normalBundle" => F,
	"expression" => null
	}
    )


info EmbeddedVariety := Y -> (
    X := Y#"ambientSpace";
    F := Y#"normalBundle";
    if not isGloballyGenerated(F,X) then error "expected a globally generated vector bundle";
    D := " a variety of dimension " | dim Y | "\n";
    D = D | " first Chern class is " | toString entries chern(1,Y) | "\n";
    C := lines info X;
    C = drop (C, -1);
    B := "";
    for c in C do (
	B = B | toString c | "\n";
	);
    D = D | "\n sitting inside" | B;
    D = D | " cut by" | info F | " \n";
    return D;
    )


summands = method();
summands HomogeneousVectorBundle := F -> (
    X := F#"underlyingVariety";
    if not F#"totally reducible" then error "summand only for a completely reducible vector bundle";
    if F#"irreducible" then return {F};
    output := for i from 0 to #(F#"weights")-1 list (
	apply((F#"multiplicities")#i, j -> homogeneousVectorBundle({(F#"weights")#i},{1},X))
	);
    return flatten output;
    )


-- aux method for the otpion Verbose in hodgeNumbers
timedIf = method();
timedIf (Boolean, Function) := (v, f) -> if v then elapsedTime f() else f();
