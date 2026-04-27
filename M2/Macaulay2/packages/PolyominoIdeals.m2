newPackage(
	"PolyominoIdeals",
	Version => "2.0",
	Date => "November, 2025",
	
	Authors => {
		{
			Name => "Carmelo Cisto", 
			Email => "ccisto@unime.it"
		},
		{
			Name => "Francesco Navarra", 
			Email => "francesco.navarra@sabanciuniv.edu",
			HomePage => "https://sites.google.com/sabanciuniv.edu/francesco-navarra/"
		},
		{
			Name => "Rizwan Jahangir", 
			Email => {"rizwan@sabanciuniv.edu", "rizwan.jahangir@nbs.nust.edu.pk"},
			HomePage => "https://rizwan.phd"
			}
	},
	Headline => "Collections of cells and binomial ideals",
	Keywords => {"Combinatorial Commutative Algebra"},
	PackageImports => { "Graphs", "gfanInterface" }
)


export {
-- Data type & constructor:
		"CollectionOfCells",
		"cellCollection",		
-- Functions:
		"polyoVertices",
		"polyoMatrix",
		"polyoMatrixReduced",
		"innerInterval",
		"polyoIdeal",
		"polyoToric",
		"polyoLattice",
		"adjacent2MinorIdeal",
		"isNonAttackingRooks",
		"allNonAttackingRookConfigurations",
		"rookPolynomial", 
 		"rookNumber",
    		"equivalenceClassesSwitchingRook",
    		"switchingRookPolynomial",
		"standardRookNumber",
		"standardNonAttackingRookConfigurations",
		"standardRookPolynomial",
    		"isPalindromic",
		"randomCollectionWithFixedRank",
		"randomCollectionOfCells",
		"randomPolyominoWithFixedRank", 
		"randomPolyomino",
		"cellGraph", 
		"collectionIsConnected", 
		"connectedComponentsCells", 
		"isRowConvex", 
		"isColumnConvex", 
		"isConvex", 
		"collectionIsSimple", 
		"rankCollection",		
--options
		"Field",
		"TermOrder",
		"RingChoice"
}

--------------------------------------------------------------------------------------------------
-- Declaration of some variables
--------------------------------------------------------------------------------------------------
y:=vars(24);
x:=vars(23);
u:=vars(20);
t:=vars(19);
v:=vars(21);
h:=vars(7);

--------------------------------------------------------------------------------------------------
-- Define the type
--------------------------------------------------------------------------------------------------
CollectionOfCells = new Type of List
CollectionOfCells.synonym = "collection of cells"

--------------------------------------------------------------------------------------------------
-- cellCollection
-- Constructs a collection of cells represented by their lower-left corners.
--------------------------------------------------------------------------------------------------
cellCollection = method(Options => {})
cellCollection List := opts -> L -> (
    if #L == 0 then error "a CollectionOfCells cannot be empty.";
    if unique L != L then error "there are two cells that are equal."; 
    if not all(L, c -> (
        instance(c, List) and #c == 2 and all(c, x -> instance(x, ZZ))
    )) then error "each cell must be a list of two integers.";
    new CollectionOfCells from L
)

--------------------------------------------------------------------------------------------------
-- Display and conversion methods
--------------------------------------------------------------------------------------------------
-- net CollectionOfCells := cc -> net toList cc
-- toString CollectionOfCells := cc -> toString toList cc

--------------------------------------------------------------------------------------------------
-- polyoVertices
-- Computes the set of vertices of a collection of cells.
--------------------------------------------------------------------------------------------------
polyoVertices = method()
polyoVertices CollectionOfCells := Q -> (
    V := toList set flatten (
        for i from 0 to #Q-1 list (
            c := Q#i;
            toList(c, c+{1,0}, c+{0,1}, c+{1,1})
        )
    );
    apply(V, toSequence)
)

--------------------------------------------------------------------------------------------------
-- polyoRingDefault
--------------------------------------------------------------------------------------------------
-- The function polyoRing defines the ring attached to a collection of cells, where the monomial
-- order is given by the order defined in the option Term order, induced by the following order of
-- the variables: x_a > x_b with a=(i,j) and b=(k,l), if i > k, or i = k and j > l.  
--------------------------------------------------------------------------------------------------
polyoRingDefault = method (Options=>{Field => QQ, TermOrder=>Lex})
polyoRingDefault CollectionOfCells := opts -> Q -> (
    V := reverse(sort(polyoVertices(Q)));
    Gen := for v in V list x_v; 
    R := (opts.Field)[Gen, MonomialOrder => opts.TermOrder];
    R
);

--------------------------------------------------------------------------------------------------
-- polyoMatrix
-- Constructs a matrix representing a collection of cells, having [a,b] as minimal bounding box
--------------------------------------------------------------------------------------------------	
polyoMatrix = method(TypicalValue => Matrix)
polyoMatrix CollectionOfCells := Q -> (
    V := polyoVertices(Q);
    R := polyoRing(Q);
    xVertices := apply(Q, c -> c#0);
    a := min xVertices;
    b := max xVertices + 1;
    yVertices := apply(Q, c -> c#1);
    c := min yVertices;
    d := max yVertices + 1;
    H := for j in reverse(c .. d) list (
        for i from a to b list (
            if member((i,j), V) then x_(i,j)_R else 0
        )
    );

    matrix H
);


--------------------------------------------------------------------------------------------------
-- polyoMatrixReduced
-- Returns a reduced form of polyoMatrix by switching rows and columns as explained in the paper:
-- H. Ohsugi and T. Hibi, "Koszul bipartite graphs ", Adv. Appl. Math. 22,  25-28, 1999.
--------------------------------------------------------------------------------------------------
antiLexLessEq = (A,B) -> (
    D := reverse(A - B);
    if all(D, i -> i == 0) then true
    else (
        p := first(select(D, i -> i != 0));
        p < 0
    )
);

supportVector = M -> apply(M, i -> if i != 0 then 1 else 0);

polyoMatrixReduced = method(Options => {})
polyoMatrixReduced CollectionOfCells := opts -> Q -> (
    M := polyoMatrix(Q);
    rowEntries := entries M;
    nRows := numgens target M;
    mutableM := mutableMatrix M;

    rowSupports := apply(rowEntries, supportVector);

    for i from 0 to nRows-1 do (
        for j from i to nRows-1 do (
            if not antiLexLessEq(rowSupports#i, rowSupports#j) then (
                mutableM = rowSwap(mutableM, i, j);
                rowSupports = switch(i, j, rowSupports);
            );
        );
    );

    colEntries := entries transpose matrix(mutableM);
    nCols := numgens source M;
    mutableFinal := mutableMatrix matrix(mutableM);

    colSupports := apply(colEntries, supportVector);

    for a from 0 to nCols-1 do (
        for b from a to nCols-1 do (
            if not antiLexLessEq(colSupports#a, colSupports#b) then (
                mutableFinal = columnSwap(mutableFinal, a, b);
                colSupports = switch(a, b, colSupports);
            );
        );
    );
    matrix(mutableFinal)
);


--------------------------------------------------------------------------------------------------
-- polyoRingConvex
-- Constructs the polynomial ring for a convex collection of cells, in which the monomial order is
-- defined as in the paper:
-- H. Ohsugi and T. Hibi, "Koszul bipartite graphs ", Adv. Appl. Math. 22,  25-28, 1999. 
-- We know that the generators of the binomial ideal associated with a weakly connected and  
-- convex collections of cells forms the reduced Groebner basis with respect to this order, and 
-- so the initial ideal is squarefree and generated in degree two. 
--------------------------------------------------------------------------------------------------
polyoRingConvex = method(Options => {Field => QQ})
polyoRingConvex CollectionOfCells := opts -> Q -> (
    reducedMat := polyoMatrixReduced(Q);
    entriesMat := entries reducedMat;
    nRows := numgens target reducedMat;
    nCols := numgens source reducedMat;

    gens := flatten apply(entriesMat, row -> select(row, x -> x != 0));

    S := (opts.Field)[gens, MonomialOrder => RevLex, Global => false];
    S
);

--------------------------------------------------------------------------------------------------
-- polyoRing
-- Returns the polynomial ring for a collection of cells based on RingChoice.
--------------------------------------------------------------------------------------------------
polyoRing = method(Options => {Field => QQ, TermOrder => Lex, RingChoice => 1})
polyoRing CollectionOfCells := opts -> Q -> (
    if opts.RingChoice == 1 then
        polyoRingDefault(Q, Field => opts.Field, TermOrder => opts.TermOrder)
    else
        polyoRingConvex(Q, Field => opts.Field)
);

--------------------------------------------------------------------------------------------------
-- isPairOfInteger
-- Checks if a list is a valid pair of integers.
--------------------------------------------------------------------------------------------------
isPairOfInteger = method()
isPairOfInteger List := L -> (
    if #L != 2 then (
        error "each endpoint must be a list of two integers."
    );
    if not all(L, x -> class x === ZZ) then (
        error "each coordinate must be an integer."
    );
    true
)

--------------------------------------------------------------------------------------------------
-- innerInterval
-- Checks if [a,b] forms an inner interval in a collection of cells.
--------------------------------------------------------------------------------------------------
innerInterval = method(Options => {})
innerInterval(List, List, CollectionOfCells) := opts -> (a, b, Q) -> (
    isPairOfInteger(a);
    isPairOfInteger(b);	
    if a#0 == b#0 or a#1 == b#1 then error "expected a#0 < b#0 and a#1 < b#1";
    all(toList(a#1..b#1-1), y -> all(toList(a#0..b#0-1), x -> member({x, y}, Q)))
)

--------------------------------------------------------------------------------------------------
-- polyoIdeal
-- Returns the ideal generated by inner 2-minors of a collection of cells.
-- The option RingChoice with value 1 and by default returns the ideal in the ambient ring given 
-- by polyoRingDefault. With a value different by 1 it returns the ideal in the ambient ring 
-- given by polyoRingConvex
--------------------------------------------------------------------------------------------------
polyoIdeal = method (Options => {Field => QQ, TermOrder => Lex, RingChoice => 1})

polyoIdeal CollectionOfCells := opts -> Q -> (
    R := polyoRing(Q, 
        Field => opts.Field, 
        TermOrder => opts.TermOrder, 
        RingChoice => opts.RingChoice
    );

    innerBinomials := flatten for i from 0 to #Q - 1 list (
        A := Q#i;
        flatten for j from 0 to #Q - 1 list (
            B := Q#j + {1,1};
            if (A#0 < B#0 and A#1 < B#1 and innerInterval(A, B, Q)) then (
                a := A#0;
                b := A#1;
                c := B#0;
                d := B#1;
                x_(a,b)_R * x_(c,d)_R - x_(a,d)_R * x_(c,b)_R
            ) else (
                continue
            )
        )
    );
   ideal toList set innerBinomials
);


--------------------------------------------------------------------------------------------------
-- polyoToric
-- Constructs the toric ideal associated with a polyomino and its holes encoded by the list H 
-- of the lower corners among the most-left corners of each hole
--------------------------------------------------------------------------------------------------
leq2 = (A, B) -> (A#0 <= B#0 and A#1 <= B#1);

polyoToric = method(TypicalValue => Ideal)
polyoToric(CollectionOfCells, List) := (Q, H) -> (
   if H =!= {} then (
    if not all(H, h -> class h === List) then
        error "each element of H must be a list.";
    scan(H, h -> isPairOfInteger(h));
   );
    V := reverse sort apply(polyoVertices Q, toList);
    xCoords := sort unique apply(V, p -> p#0);
    yCoords := sort unique apply(V, p -> p#1);

    verticalIntervals := flatten for x in xCoords list (
        localIntervals := {};
        y := min yCoords;
        while y < max yCoords do (
            segment := {};
            while member({x, y}, Q) or member({x - 1, y}, Q) do (
                segment = join(segment, {{x, y}, {x, y + 1}});
                y = y + 1;
            );
            if segment != {} then localIntervals = append(localIntervals, unique segment);
            y = y + 1;
        );
        localIntervals
    );

    horizontalIntervals := flatten for y in yCoords list (
        localIntervals := {};
        x := min xCoords;
        while x < max xCoords do (
            segment := {};
            while member({x, y}, Q) or member({x, y - 1}, Q) do (
                segment = join(segment, {{x, y}, {x + 1, y}});
                x = x + 1;
            );
            if segment != {} then localIntervals = append(localIntervals, unique segment);
            x = x + 1;
        );
        localIntervals
    );
-- Define the variables of the toric ring
    Svars := (
        (for i to #horizontalIntervals - 1 list u_(i)) 
        | (for i to #verticalIntervals - 1 list v_(i)) 
        | (for i to #H - 1 list h_(i))
    );
    S := QQ[Svars, MonomialOrder => Lex];
    images := for vertex in V list (
        m := 1;
        for i to #horizontalIntervals - 1 do (
            if member(vertex, horizontalIntervals#i) then m = m * u_(i)_S;
        );
        for i to #verticalIntervals - 1 do (
            if member(vertex, verticalIntervals#i) then m = m * v_(i)_S;
        );
        for j to #H - 1 do (
            if leq2(vertex, H#j) then m = m * h_(j)_S;
        );
        m
    );
    T := polyoRing(Q);
    f := map(S, T, images);
    kernel f
);



--------------------------------------------------------------------------------------------------
-- polyoLattice
-- Produces generators of the lattice ideal associated with a collection of cells.
--------------------------------------------------------------------------------------------------
polyoLattice = method (Options=>{})
polyoLattice CollectionOfCells := opts -> Q -> (
    V := apply(polyoVertices(Q), toList);
    n := #V;
    L := new MutableHashTable;
    for i from 0 to #Q-1 do (
        a := Q#i;
        b := a + {1,1};
        c := a + {0,1};
        d := a + {1,0};
        p1 := position(V, v -> v == a);
        p2 := position(V, v -> v == b);
        p3 := position(V, v -> v == c);
        p4 := position(V, v -> v == d);
        row := for j from 0 to n-1 list (
    		if j == p1 or j == p2 then 1
    		else if j == p3 or j == p4 then -1
    		else 0
		);
        L#i = row;
    );
    gensList := for i from 0 to #Q-1 list L#i; 
    gensLattice := gfanLatticeIdeal gensList;
    J := ideal gensLattice;
    R := ring J;
    X := for v in V list x_(v#0, v#1);
    S := QQ[X, MonomialOrder => Lex];
    phi := map(S, R, gens S);
    images := apply(gensLattice, g -> phi g);
    ideal images
);

--------------------------------------------------------------------------------------------------
-- adjacent2MinorIdeal
-- Returns the ideal generated by adjacent 2-minors of a collection of cells.
-- Specifically, for each cell with lower left corner (i,j), the function constructs the binomial
-- x_(i,j) * x_(i+1,j+1) - x_(i,j+1) * x_(i+1,j), and collects all such binomials to form the ideal.
--------------------------------------------------------------------------------------------------
adjacent2MinorIdeal = method (Options=>{Field => QQ, TermOrder=>Lex})
adjacent2MinorIdeal CollectionOfCells := opts -> Q ->(
R:=polyoRing(Q,Field=>opts.Field, TermOrder=>opts.TermOrder, RingChoice=> 1);
GensForIdeal := for cell in Q list (
        lowerLeftCorner  := toSequence(cell);
       	upperRightCorner := toSequence(cell + {1,1});
       	upperLeftCorner  := toSequence(cell + {0,1});
       	lowerRightCorner := toSequence(cell + {1,0});
	x_lowerLeftCorner_R*x_upperRightCorner_R - x_upperLeftCorner_R*x_lowerRightCorner_R
  	);
I:= ideal GensForIdeal;
I
);

--------------------------------------------------------------------------------------------------
-- isNonAttackingRooks
-- Checks if two rooks on a collection of cells are non-attacking.
--------------------------------------------------------------------------------------------------
isNonAttackingRooks = method(Options =>{})
isNonAttackingRooks(List, List, CollectionOfCells) := opts -> (A, B, Q) -> (
    isPairOfInteger(A);
    isPairOfInteger(B);
    if member(A, Q) and member(B, Q) then (
        a := min {A#0, B#0};
        b := min {A#1, B#1};
        c := max {A#0, B#0} + 1;
        d := max {A#1, B#1} + 1;

        if A#0 == B#0 or A#1 == B#1 then (
            not innerInterval({a, b}, {c, d}, Q)
        )
        else true
    )
    else (
        "One of the two rooks is not placed on Q"
    )
);


--------------------------------------------------------------------------------------------------
-- allNonAttackingRookConfigurations
-- Computes all non-attacking rook configurations on a collection of cells.
--------------------------------------------------------------------------------------------------
-- The output is organized as a list of lists, where each sublist contains
-- all configurations of a given size:
--   - The first sublist contains all configurations with 1 rook,
--   - The second sublist contains all configurations with 2 rooks,
--   - And so on, up to the maximum number of non-attacking rooks that can 
--     be placed on Q (that is, the rook number of Q).
-- This ordering by cardinality is convenient for computing the coefficients 
-- of the switching rook polynomial and for determining the rook number of Q.
---------------------------------------------------------------------------------------------------
allNonAttackingRookConfigurations = method(Options =>{})
allNonAttackingRookConfigurations CollectionOfCells := opts -> L -> (

    Q := for i from 0 to #L-1 list L#i; 
    Q = sort Q;

    conf := apply(Q, i -> {i}); 
    AllConf:={conf};
    while true do (
        out :={};
        for N in conf do (
            for c in Q do (
                if all(N, n -> isNonAttackingRooks(c, n, L)) then (
                    FF := sort join(N, {c});
                    out = join(out, {FF});
                );
            );
        );
        out = toList set out; 
        if #out == 0 then return AllConf;
        AllConf = append(AllConf, out);
        conf = out;
    );
);

--------------------------------------------------------------------------------------------------
-- rookNumber
-- Returns the maximum number of non-attacking rooks that can be placed on a collection of cells.
--------------------------------------------------------------------------------------------------
rookNumber = method(Options => {})
rookNumber CollectionOfCells := opts -> Q -> (
    #(allNonAttackingRookConfigurations(Q))
);

--------------------------------------------------------------------------------------------------
-- rookPolynomial
-- Computes the rook polynomial of a collection of cells.
--------------------------------------------------------------------------------------------------
rookPolynomial = method(Options => {})
rookPolynomial CollectionOfCells := opts -> Q -> (
    onlyRookConf := allNonAttackingRookConfigurations(Q); 
    New := ZZ[t];
    f := 1_New;
    for i from 0 to #onlyRookConf-1 do (
        f = f + (#(onlyRookConf#i))*t_New^(i+1);
    );
   f
);

--------------------------------------------------------------------------------------------------
-- SwitchOperation
-- Performs a switch operation on two rooks if they are in switching position.
--------------------------------------------------------------------------------------------------
SwitchOperation = (A, B, Q) -> (
    a := min {A#0, B#0};
    b := min {A#1, B#1};
    c := max {A#0, B#0} + 1;
    d := max {A#1, B#1} + 1;
    if innerInterval({a, b}, {c, d}, Q) then (
        if A == {a, b} or A == {c-1, d-1} then {{a, d-1}, {c-1, b}}
        else if A == {a, d-1} or A == {c-1, b} then {{a, b}, {c-1, d-1}}
    )
    else {}
);

--------------------------------------------------------------------------------------------------
-- areSwitchEquivalent
-- Tests if two rook configurations are switch-equivalent, that is, if one can be obtained from 
-- the other by a single switch operation.
--------------------------------------------------------------------------------------------------
areSwitchEquivalent = (R1, R2, Q) -> (
    diff1 := R1 - set R2;
    diff2 := R2 - set R1;
    (#diff1 == 2 and #diff2 == 2) and (
        set(diff1) === set(SwitchOperation(diff2#0, diff2#1, Q)) or
        set(diff2) === set(SwitchOperation(diff1#0, diff1#1, Q))
    )
);

--------------------------------------------------------------------------------------------------
-- equivalenceClassesSwitchingRook
-- Returns equivalence classes of rook configurations under switching equivalence.
--------------------------------------------------------------------------------------------------
equivalenceClassesSwitchingRook = method(Options => {})
equivalenceClassesSwitchingRook CollectionOfCells := opts -> Q -> (
rookConfigurations := allNonAttackingRookConfigurations(Q); 
numConfigurations := #rookConfigurations;
equivalenceClasses := for k from 0 to numConfigurations - 1 list (
    currentRooks := rookConfigurations#k;
    rookVertices := new MutableHashTable;
    for i from 0 to #currentRooks - 1 do rookVertices#i = currentRooks#i;
    edges := flatten for i from 0 to #currentRooks - 2 list (
        for j from i + 1 to #currentRooks - 1 list (
            if areSwitchEquivalent(rookVertices#i, rookVertices#j, Q) then {i, j} else continue
    	)
    );	
    rookGraph := graph(toList(0 .. #currentRooks - 1), edges);
    componentsAsCells := apply(connectedComponents rookGraph, comp -> apply(comp, idx -> currentRooks#idx));
    componentsAsCells
);
    equivalenceClasses
);

--------------------------------------------------------------------------------------------------
-- switchingRookPolynomial
-- Computes the switching rook polynomial of a collection of cells.
--------------------------------------------------------------------------------------------------
switchingRookPolynomial = method(Options => {})
switchingRookPolynomial CollectionOfCells := opts -> Q -> (
    coeffs := equivalenceClassesSwitchingRook(Q);
    NewRing := ZZ[t];
    f := 1_NewRing;
    for i from 0 to #coeffs-1 do (
        f = f + (#coeffs#i) * t_NewRing^(i+1);
    );
    f
);

--------------------------------------------------------------------------------------------------
-- standardNonAttackingRooks
-- Tests if two rooks are in standard non-attacking position.
--------------------------------------------------------------------------------------------------
standardNonAttackingRooks = (A,B) -> ( not(A#0==B#0 or A#1==B#1) );

--------------------------------------------------------------------------------------------------
-- standardNonAttackingRookConfigurations
-- Computes all standard non-attacking rook configurations.
--------------------------------------------------------------------------------------------------
standardNonAttackingRookConfigurations = method(Options => {})
standardNonAttackingRookConfigurations CollectionOfCells := opts -> L -> (
    
    Q := for i from 0 to #L-1 list L#i; 
    Q = sort Q;
    conf := apply(Q, i -> {i}); 
    AllConf:={conf};
    while true do (
        out :={};
        for N in conf do (
            for c in Q do (
                if all(N, n -> standardNonAttackingRooks(c, n)) then (
                    FF := sort join(N, {c});
                    out = join(out, {FF});
                );
            );
        );
        out = toList set out; 
        if #out == 0 then return AllConf;
        AllConf = append(AllConf, out);
        conf = out;
    );
);

--------------------------------------------------------------------------------------------------
-- standardRookNumber
-- Returns the maximum number of standard non-attacking rooks.
--------------------------------------------------------------------------------------------------
standardRookNumber = method(Options => {})
standardRookNumber CollectionOfCells := opts -> Q -> (
	#standardNonAttackingRookConfigurations(Q)
);

--------------------------------------------------------------------------------------------------
-- standardRookPolynomial
-- Computes the standard rook polynomial of a collection of cells.
--------------------------------------------------------------------------------------------------
standardRookPolynomial = method(Options => {})
standardRookPolynomial CollectionOfCells := opts -> Q -> (
    standRookConf:= standardNonAttackingRookConfigurations(Q);
    NewTwo := ZZ[t];
    f := 1_NewTwo;
    for i from 0 to #standRookConf-1 do (
        f = f + (#(standRookConf#i)) * t_NewTwo^(i+1);
    );
   f
);

---------------------------------------------------------------------------------------------------
-- isPalindromic f
---------------------------------------------------------------------------------------------------
-- Determine whether a polynomial f in QQ[x] with non-null coefficients is palindromic. 
-- If f = a_0 + a_1 x + ... + a_d x^d then f is palindromic if a_i = a_{d-i} for all i = 0, ..., d.
-- The function returns true when palindromic and false otherwise.
---------------------------------------------------------------------------------------------------
isPalindromic = method(Options => {})
isPalindromic RingElement := opts -> f -> (
    coeffs := flatten entries ((coefficients f)#1);
    deg := #coeffs - 1;
    if deg != (degree f)#0 then error "polynomial has zero coefficients";
    for i from 0 to floor(deg/2) do (
        if coeffs#i =!= coeffs#(deg - i) then return false;
    );
    true
)

--------------------------------------------------------------------------------------------------
-- aroundCells
-- Returns the eight surrounding cells of a given cell.
--------------------------------------------------------------------------------------------------
aroundCells = (C) -> (
    i := C#0;
    j := C#1;
    allAroundCells := {
        {i-1, j},    -- West
        {i+1, j},   -- East
        {i,   j+1},   -- North
        {i,   j-1},     -- South
        {i-1, j+1},  -- North-West
        {i+1, j+1},   -- North-East
        {i-1, j-1},    -- South-West
        {i+1, j-1}     -- South-East
    };
  allAroundCells
);

--------------------------------------------------------------------------------------------------
-- admissibleCells
-- Returns cells adjacent to a given cell that are not in the collection.
--------------------------------------------------------------------------------------------------
admissibleCells = (Q,C) -> (
	toList(set(aroundCells(C)) - set(Q))
);

--------------------------------------------------------------------------------------------------
-- randomCollectionWithFixedRank
-- Generates a random collection of cells with fixed rank (that is, number of cells).
--------------------------------------------------------------------------------------------------
randomCollectionWithFixedRank = method(Options => {})
randomCollectionWithFixedRank ZZ := opts -> n -> (
Q := { {1,1} };  
while (#Q < n) do (
    allCandidates := flatten apply(Q, c -> admissibleCells(Q, c));
    available := toList(set allCandidates - set Q);
    newC := available#(random(#available-1));
    Q = append(Q, newC);
    );
 cellCollection Q
);

--------------------------------------------------------------------------------------------------
-- randomCollectionOfCells
-- Generates a random collection of cells of random rank up to a bound.
--------------------------------------------------------------------------------------------------
randomCollectionOfCells = method(Options => {})
randomCollectionOfCells ZZ := opts -> m -> (
    n := random(1,m);
    Q := { {1,1} };  
    while (#Q < n) do (
    allCandidates := flatten apply(Q, c -> admissibleCells(Q, c));
    available := toList(set allCandidates - set Q);
    newC := available#(random(#available-1));
    Q = append(Q, newC);
    );
 cellCollection Q
);

--------------------------------------------------------------------------------------------------
-- aroundCells2
-- Returns the four orthogonally adjacent cells of a given cell.
--------------------------------------------------------------------------------------------------
aroundCells2 = (C) -> (
    i := C#0;
    j := C#1;
    allAroundCells := {
        {i-1, j}, -- West
        {i+1, j},  -- East
        {i,   j+1}, -- North
        {i,   j-1}    -- South
    };
    allAroundCells
);

--------------------------------------------------------------------------------------------------
-- admissibleCells2
-- Returns cells adjacent orthogonally to a given cell that are not in the collection.
--------------------------------------------------------------------------------------------------
admissibleCells2 = (Q,C) -> (
	 toList(set(aroundCells2(C)) - set(Q))
);

--------------------------------------------------------------------------------------------------
-- randomPolyominoWithFixedRank
-- Generates a random polyomino with fixed number of cells.
--------------------------------------------------------------------------------------------------
randomPolyominoWithFixedRank = method(Options => {})
randomPolyominoWithFixedRank ZZ := opts -> n -> (
    Q := { {1,1} };  
    while (#Q < n) do (
    allCandidates := flatten apply(Q, c -> admissibleCells(Q, c));
    available := toList(set allCandidates - set Q);
    newC:= available#(random(#available-1));
    Q = append(Q, newC);
    );
 cellCollection Q
);

--------------------------------------------------------------------------------------------------
-- randomPolyomino
-- Generates a random polyomino with random number of cells up to a bound.
--------------------------------------------------------------------------------------------------
randomPolyomino = method(Options => {})
randomPolyomino ZZ := opts -> m -> (
    n := random(1,m);
    Q := { {1,1} };  
    while (#Q < n) do (
    allCandidates := flatten apply(Q, c -> admissibleCells(Q, c));
    available := toList(set allCandidates - set Q);
    newC := available#(random(#available-1));
    Q = append(Q, newC);
    );
 cellCollection Q
);

--------------------------------------------------------------------------------------------------
-- isAdjacent
-- Checks if two cells are adjacent.
--------------------------------------------------------------------------------------------------
isAdjacent = (A,B) -> (
    i := A#0; 
    j := A#1;
    p := B#0; 
    q := B#1;
    abs(i - p) + abs(j - q) == 1
);

--------------------------------------------------------------------------------------------------
-- cellGraph
-- Constructs the graph of a collection of cells with edges between adjacent cells.
--------------------------------------------------------------------------------------------------
cellGraph = method(Options => {})
cellGraph CollectionOfCells := opts -> Q -> (
    vertCell := toList(0 .. #Q-1);
    edgeCell := flatten for i from 0 to #Q - 2 list (
    for j from i + 1 to #Q - 1 list (
        if isAdjacent(Q#i, Q#j) then {i, j} else continue
        )
    );
    G := graph(vertCell, edgeCell);
    G
);

--------------------------------------------------------------------------------------------------
-- collectionIsConnected
-- Tests if a collection of cells is connected.
--------------------------------------------------------------------------------------------------
collectionIsConnected = method(Options => {})
collectionIsConnected CollectionOfCells := opts -> Q -> (
    G := cellGraph(Q);
    comps := connectedComponents G;
    (#comps == 1,#comps)
);

--------------------------------------------------------------------------------------------------
-- connectedComponentsCells
-- Returns the connected components of a collection of cells.
--------------------------------------------------------------------------------------------------
connectedComponentsCells = method(Options => {})
connectedComponentsCells CollectionOfCells := opts -> Q -> (
    G := cellGraph(Q);
    comps := connectedComponents G;
    cellComponents := for comp in comps list (
        for i in comp list Q#i
    );
    cellComponents
);

--------------------------------------------------------------------------------------------------
-- isRowConvex
-- Tests if a collection of cells is row-convex.
--------------------------------------------------------------------------------------------------
isRowConvex = method(Options => {})
isRowConvex CollectionOfCells := opts -> Q -> (
    n := #Q;
    for i from 0 to n-2 do (
        for j from i+1 to n-1 do (
            A := Q#i;
            B := Q#j;
            if (A#1 == B#1) then (
		  	a := min {A#0, B#0};
    			b := min {A#1, B#1};
    			c := max {A#0, B#0} + 1;
   			d := max {A#1, B#1} + 1;
                    	if not innerInterval({a,b},{c,d},Q) then (
                    	return false;
                );
            );
        );
    );
    true
);

--------------------------------------------------------------------------------------------------
-- isColumnConvex
-- Tests if a collection of cells is column-convex.
--------------------------------------------------------------------------------------------------
isColumnConvex = method(Options => {})
isColumnConvex CollectionOfCells := opts -> Q -> (
    n := #Q;
    for i from 0 to n-2 do (
        for j from i+1 to n-1 do (
            A := Q#i;
            B := Q#j;
            if (A#0 == B#0) then (
		a := min {A#0, B#0};
    		b := min {A#1, B#1};
    		c := max {A#0, B#0} + 1;
    		d := max {A#1, B#1} + 1;
                if not innerInterval({a,b},{c,d},Q)  then (
                    return false;
                );
            );
        );
    );
    true
);

--------------------------------------------------------------------------------------------------
-- isConvex
-- Tests if a collection of cells is convex (both row- and column-convex).
--------------------------------------------------------------------------------------------------
isConvex = method(Options => {})
isConvex CollectionOfCells := opts -> Q -> (
    isRowConvex(Q) and isColumnConvex(Q)
);

--------------------------------------------------------------------------------------------------
-- collectionIsSimple
-- Tests if the collection of cells is simple (no holes in the complement).
--------------------------------------------------------------------------------------------------
collectionIsSimple = method(Options => {})
collectionIsSimple CollectionOfCells := opts -> Q -> (
    if #Q <= 1 then return true; 
    minRow := min apply(Q, A -> A#1) - 1;
    minCol := min apply(Q, A -> A#0) - 1;
    maxRow := max apply(Q, A -> A#1) + 2;
    maxCol := max apply(Q, A -> A#0) + 2;
    Rcells := flatten for i from minCol to maxCol-1 list (
    			for j from minRow to maxRow-1 list {i, j}
	              );
    ExtPol := cellCollection(toList(set(Rcells) - set(Q)));
    comps := connectedComponentsCells(ExtPol);
    #comps == 1
);

--------------------------------------------------------------------------------------------------
-- rankCollection
-- Returns the number of cells in the collection.
--------------------------------------------------------------------------------------------------
rankCollection = method(Options => {})
rankCollection CollectionOfCells := opts -> Q -> (
	#Q
);


---------------------------------------------------------------------------------------------------------
					-- End of source code --
---------------------------------------------------------------------------------------------------------


---------------------------------------------------------------------------------------------------------
-----------------------------------------    DOCUMENTATION    -------------------------------------------
---------------------------------------------------------------------------------------------------------


beginDocumentation()

document {
        Key => PolyominoIdeals,
        Headline => "a package to work with collections of cells, polyominoes and associated binomial ideals ",
        EM "PolyominoIdeals", " is a package for making several computations with collections of cells, polyominoes and binomial ideals attached to them.\n ",
        BR{},BR{},
	"Collections of cells, and in particular polyominoes, are plane figures formed by joining unit squares edge to edge. The systematic study of polyominoes was initiated by Solomon W. Golomb in 1953 and further developed in his seminal monograph [G1994]. Beyond their combinatorial interest in problems such as tiling the plane, polyominoes also connect to several other fields, including theoretical computer science, statistical physics, and discrete geometry.\n",
BR{},BR{},
"More recently, since 2012, polyominoes and, more generally, collections of cells have been investigated from an algebraic–combinatorial perspective. In [AAQ2012], Ayesha Asloob Qureshi established a link between combinatorial commutative algebra and collections of cells by associating to each collection the binomial ideal generated by its inner $2$-minors.\n",
BR{},BR{},
        " Consider the natural partial order on $\\NN^2$ and let $a,b \\in \\N^2$ with $a\\leq b$. The set $[a, b] = \\{c \\in \\NN^2 : a \\leq c \\leq b\\}$ is called an interval of $\\NN^2$; moreover, if $b=a+(1,1)$, then $[a,b]$ is called a cell of $\\NN^2$. An interval $C=[a, b]$, where $a = (i, j)$ and $b = (k, l)$, is said to be a proper interval if $i < k$ and $j < l$. The elements $a, b$ are said the diagonal corners of $C$ and  $c = (k, j)$ and $d = (i, l)$ the anti-diagonal ones. If $C$ is a cell, then $V(C)=\\{a,a+(1,1),a+(0,1),a+(1,0)\\}$ is the set of the corners of $C$.\n",
       BR{},
        "To a collection of cells $\\mathcal{P}$ we associate the ideal $I_{\\mathcal{P}}$ as follows. Let $K$ be a field and set $S = K[x_a : a\\in V(\\mathcal{P})]$, where $V(\\mathcal{P})$ is the union of the vertex sets of the cells in $\\mathcal{P}$. A proper interval $[a,b]$ is called an inner interval of $\\mathcal{P}$ if every cell contained in $[a,b]$ belongs to $\\mathcal{P}$. If $c$ and $d$ are the anti-diagonal corners of an inner interval $[a,b]$, then the binomial $f = x_a x_b - x_c x_d$ is called an inner $2$-minor of $\\mathcal{P}$. If we restrict the generators to those corresponding only to the cells, we obtain the so-called adjacent $2$-minor ideals, introduced in [HH2012]. \n",
        BR{},
        "A collection $\\mathcal{P}$ is a polyomino if for every two cells $A,B\\in\\mathcal{P}$ there exists a sequence $A=C_1,\\dots,C_m=B$ of cells in $\\mathcal{P}$ such that $C_i$ and $C_{i+1}$ share a common edge for all $i$. In this case $I_{\\mathcal{P}}$ is called the polyomino ideal of $\\mathcal{P}$.\n",
        BR{},BR{},
        "The aim of this package is to provide several tools to help mathematicians in the study of collections of cells, polyominoes and the related binomial ideals. Every contribution is very welcome. \n",
	BR{},BR{},
	BOLD "Literature \n",
	UL {
	LI {"[G1994] ", EM "Polyominoes, Puzzles, Patterns, Problems, and Packagings", "(S.W. Golomb, 1994, Second edition, Princeton University Press).\n"},
LI {"[HH2012] ", EM "Ideals generated by adjacent 2-minors", "(J. Herzog, T. Hibi, 2012, J. Commut. Algebra).\n"},
	  LI {"[AAQ2012] ", EM "Ideals generated by 2-minors, collections of cells and stack polyominoes ", "(A. A. Qureshi, 2012,  J. Algebra).\n"}
	 },
	 
	Subnodes => {
     TO "CollectionOfCells",
     TO "cellCollection",
     TO "polyoVertices",
     TO "polyoMatrix",
     TO "polyoMatrixReduced",
     TO "innerInterval",
     TO "polyoIdeal",
     TO "polyoToric",
     TO "polyoLattice",
     TO "adjacent2MinorIdeal",
     TO "isNonAttackingRooks",
     TO "allNonAttackingRookConfigurations",
     TO "rookPolynomial",
     TO "rookNumber",
     TO "equivalenceClassesSwitchingRook",
     TO "switchingRookPolynomial",
     TO "standardRookNumber",
     TO "standardNonAttackingRookConfigurations",
     TO "standardRookPolynomial",
     TO "isPalindromic",
     TO "randomCollectionWithFixedRank",
     TO "randomCollectionOfCells",
     TO "randomPolyominoWithFixedRank",
     TO "randomPolyomino",
     TO "cellGraph",
     TO "collectionIsConnected",
     TO "connectedComponentsCells",
     TO "isRowConvex",
     TO "isColumnConvex",
     TO "isConvex",
     TO "collectionIsSimple",
     TO "rankCollection",
     TO "Field",
     TO "TermOrder",
     TO "RingChoice"
	}
 }


----------------------------------------------------------------------------------------------------
-------------------------------- Documentation for exported functions ------------------------------
----------------------------------------------------------------------------------------------------

document {
     Key => {CollectionOfCells},
     Headline => "Type representing a collection of cells",
          "Represents the type of a collection of cells, which is used as an input and output type 
           for several functions in this package."
}


document {
     Key => {cellCollection, (cellCollection, List)},
     Headline => "Create a collection of cells",
     Usage => "cellCollection L",
     Inputs => {
          "L" => {"a list of lists of two integers, each representing the lower-left corner of a cell"}
     },
     Outputs => {
          {"the collection of cells represented by L"}
     },
     
     "In the plane, a cell can be represented by the coordinates of its lower-left corner. 
      This function constructs a collection of cells from a given list of such coordinates.",

     BR{}, BR{},
     EXAMPLE {
          "L = {{1,1}, {2,1}, {2,2}}",
          "Q = cellCollection L"
     }
}

document {
     Key => {polyoVertices, (polyoVertices, CollectionOfCells)},
     Headline => "Vertices of a collection of cells",
     Usage => "polyoVertices Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the list of all vertices of the collection of cells encoded by Q"}
     },
     
     "Let $\\mathcal{P}$ be a collection of cells. 
      The set of vertices of $\\mathcal{P}$ is the union of the vertices of all its cells. 
      This function returns the list of all vertices of $\\mathcal{P}$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1}, {2,1}, {2,2}, {3,3}}",
          "V = polyoVertices Q"
     },
     SeeAlso =>{cellCollection}
}


document {
     Key => {innerInterval, (innerInterval, List, List, CollectionOfCells)},
     Headline => "Inner interval of a collection of cells",
     Usage => "innerInterval(a, b, Q)",
     Inputs => {
          "a" => {"a list of two integers representing a point in $\\mathbb{Z}^2$"},
          "b" => {"a list of two integers representing a point in $\\mathbb{Z}^2$"},
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if $[a,b]$ is an inner interval of Q, and false otherwise"}
     },

     "Let $\\mathcal{P}$ be a collection of cells. 
      An interval $[a,b] \\subseteq \\mathbb{Z}^2$ is called an inner interval of $\\mathcal{P}$ 
      if every cell contained in $[a,b]$ also belongs to $\\mathcal{P}$. 
      This function returns true if $[a,b]$ is an inner interval of $\\mathcal{P}$, and false otherwise.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1}, {1,2}, {2,1}, {2,3}};",
          "innerInterval({1,1}, {3,3}, Q)",
          "innerInterval({1,1}, {3,2}, Q)"
     },
     SeeAlso =>{cellCollection}
}

          	 
document {
     Key => {polyoIdeal,(polyoIdeal, CollectionOfCells)},
     Headline => "Ideal of inner 2-minors of a collection of cells",
     Usage => "polyoIdeal Q",
     Inputs => {
          "Q" => { "a collection of cells"} },
     Outputs => { {"the inner 2-minors ideal of the collection of cells encoded by Q"} },
     
    "Let $\\mathcal{P}$ be a collection of cells. This routine returns the ideal $I_{\\mathcal{P}}$ of the inner 2-minors of $\\mathcal{P}$.\n",
    	 BR{},
     "Moreover, if $\\mathcal{P}$ is a polyomino, then it returns the polyomino ideal of $\\mathcal{P}$.\n",
  	 BR{},BR{},
    	 EXAMPLE {
          "Q = cellCollection {{1,1}, {2,1}, {2,2}}",
          "I = polyoIdeal Q",
        },
   	 BR{},BR{},
   	 EXAMPLE {
   "Q= cellCollection {{1, 1}, {2, 1}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 2}};",
   "I = polyoIdeal Q",
    	},
        }
          
document {
	Key => {polyoMatrix, (polyoMatrix, CollectionOfCells)},
	Headline => "Matrix attached to a collection of cells",
	Usage => "polyoMatrix Q",
	Inputs => { "Q" => "a collection of cells" },
	Outputs => {"the matrix attached to the collection of cells encoded by Q."},
	"Let $\\mathcal{P}$ be a collection of cells and $[(a,b),(c,d)]$ be the smallest interval of $\\ZZ^2$ containing $\\mathcal{P}$. The matrix $M(\\mathcal{P})$ is a matrix having $d-b+1$ rows and $c-a+1$ columns with $M(\\mathcal{P})_{i,j}=x_{(i,j)}$ if $(i,j)$ is a vertex of $\\mathcal{P}$, otherwise it is zero.\n",
	BR{},
	 "This routine returns the matrix of the collection of cells encoded by Q. In the following example we assume for simplicity that the smallest interval containing $\\mathcal{P}$ is $[(1,1),(c,d)]$, with $c,d>1$.",
	BR{},BR{},
	EXAMPLE {
          "Q = cellCollection {{1,1}, {2,1}, {2,2}}",
          "M = polyoMatrix Q",
         },
        BR{},BR{},
        EXAMPLE {
          "Q = cellCollection {{1, 3}, {2, 2}, {3, 1}, {2, 4}, {3, 5}, {4, 4}, {5, 3}, {4, 2}, {6, 4}, {5, 1}, {7, 5}, {7, 1}, {4, 6}, {5, 7}, {6, 6}};",
          "M = polyoMatrix Q",
          },
        BR{},BR{},
        EXAMPLE {
          "Q = cellCollection {{1, 1}, {2, 2}, {3, 3}, {4, 3}};",
          "M = polyoMatrix Q",
	},
     SeeAlso =>{cellCollection}
}



document {
     Key => {polyoMatrixReduced, (polyoMatrixReduced, CollectionOfCells)},
     Headline => "Reduced form of the polyomino matrix",
     Usage => "polyoMatrixReduced Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the reduced form of the polyomino matrix obtained by switching rows and columns as described in [HH1999]"}
     },

     "It returns the reduced form of the polyomino matrix by appropriately switching rows and columns, 
      as explained in the reference below:",

     UL {
          LI {"[HH1999] ", EM "Koszul bipartite graphs,", 
              " H. Ohsugi and T. Hibi, ", 
              "Adv. Appl. Math. 22 (1999), 25–28."}
     },

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,2}};",
          "M = polyoMatrix Q",
          "N = polyoMatrixReduced Q"
     },
     SeeAlso =>{cellCollection, polyoMatrix}
}


document {
	Key => {polyoToric, (polyoToric, CollectionOfCells, List)},
	Headline => "Toric ideal of a polyomino",
	Usage => "polyoToric(Q,H)",
	Inputs => { "Q" => {"a collection of cells"},
		    "H" => {"a list of the lower left corners of the holes"}
	},
	Outputs => {"a toric ideal attached to Q."},
	
	"Let $\\mathcal{P}$ be a polyomino. A finite collection $\\mathcal{H}$ of cells not in $\\mathcal{P}$ is called a hole of $\\mathcal{P}$, if any two cells in $\\mathcal{H}$ are connected by a path of cells in $\\mathcal{H}$ and $\\mathcal{H}$ is maximal with respect to the set inclusion. Consider the following total order on $V(\\mathcal{P})$: $a=(i,j)>b=(k, l)$, if $i > k$, or $i = k$ and $j > l$. If $\\mathcal{H}$ is a hole of $\\mathcal{P}$, then we call lower left corner $e$ of $\\mathcal{H}$ the minimum, with respect to <, of the vertices of $\\mathcal{H}$. Let $\\mathcal{H}_1,\\dots, \\mathcal{H}_r$ be the holes of $\\mathcal{P}$ and $e_k = (i_k, j_k)$ be the lower left corner of $\\mathcal{H}_k$. For $k \\in K =[r]$, we define the following subset $F_k = \\{(i, j) \\in V (\\mathcal{P}) : i \\leq i_k, j \\leq j_k\\}$. Denote by $\\{V_i\\}_{i\\in I}$ the set of all the maximal vertical edge intervals of $\\mathcal{P}$, and by $\\{H_j \\}_{j\\in J}$ the set of all the maximal horizontal edge intervals of $\\mathcal{P}$. Let $\\{v_i\\}_{i\\in I}$, $\\{H_j\\}_{j\\in J}$ , and $\\{w_k\\}_{k\\in K}$ be three sets of variables. We consider the map $$\\alpha : V (\\mathcal{P}) \\rightarrow K[{h_i, v_j , w_k } : i \\in I, j \\in J, k \\in K]$$ $$a \\rightarrow \\prod_{a\\in H_i \\cap V_j} h_iv_j \\prod_{a\\in F_k} w_k$$ The toric ring $T_{\\mathcal{P}}$ associated to $\\mathcal{P}$ is defined as $T_{\\mathcal{P}} = K[\\alpha(a):a \\in V (\\mathcal{P})]$. The homomorphism $\\psi : S \\rightarrow T_{\\mathcal{P}}$ with $x_a \\rightarrow \\alpha(a)$ is surjective and the toric ideal $J_{\\mathcal{P}}$ is the kernel of $\\psi$.\n",
	BR{},
	"Note that the homomorphism $\\psi$ defined before is a natural generalization of the one given in [QSS2017] for simple polyominoes.\n",
	BR{},BR{},
	"Given the polyomino encoded by Q and the list H of the lower left corners of each hole of the polyomino, the function ", TT "polyoToric", " returns the toric ideal $J_{\\mathcal{P}}$ defined before.\n",


	BR{},BR{},
	BOLD "Literature \n",
	UL {
	LI {"[QSS2017] ", EM "Simple polyominoes are prime ", "(A.A. Qureshi, T. Shibuta, A. Shikama, 2017, J. Commut. Algebra 9(3), 413-422).\n"},
	LI {"[MRR2020] ", EM "Primality of multiply connected polyominoes ", "(C. Mascia, G. Rinaldo, F. Romeo, 2020, Illinois J. Math. 64(3), 291-304).\n"},
	LI {"[CNU2022] ", EM "Primality of weakly connected collections of cells and weakly closed path polyominoes ", "(C. Cisto, F. Navarra, R. Utano, 2022, Illinois J. Math. 66(4), 545-563).\n"},
	   },
	   
     	BR{},
     	BOLD "Examples \n",
     	BR{},BR{},
     
     	EXAMPLE {
 	"Q= cellCollection {{1, 1}, {2, 1}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 2}};",
        "J=polyoToric(Q,{{2,2}})",
        },  
      	BR{},BR{},
       
      	EXAMPLE {
       "Q= cellCollection {{3, 1}, {4, 1}, {5, 1}, {5, 2}, {5, 3}, {5, 4}, {5, 5}, {4, 5}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 4}, {1, 5}, {2, 5}, {3, 5}};",
        "I=polyoIdeal(Q);",
        "J=polyoToric(Q,{{2,4}});",
        "R=ring I",
        "J=substitute(J,R);",
        "J==I",
       } ,
        
      	BR{},BR{},
	"Morevover, if $\\mathcal{P}$ is a simple polyomino, that is, it has not any hole, then the function ", TT "polyoToric", " works by setting ", TT "H={}", " and it returns the polyomino ideal of $\\mathcal{P}$ in according to [QSS2017].\n",  
	BR{},BR{},
	EXAMPLE {
       "Q= cellCollection {{2, 1}, {3, 1}, {2, 2}, {1, 2}, {3, 2}, {2, 3}};",
       "I=polyoIdeal(Q);",
       "J=polyoToric(Q,{});",
       "R=ring I",
       "J=substitute(J,R);",
       "J==I",
        } ,
	 
	BR{},BR{},
	"In general, the function ", TT "polyoToric", " works also for weakly connected collection of cells. If $\\mathcal{P}$ is the non-simple collections of cells in Figure (A) of the Remark 3.4 in [CNU2022], we know that the inner 2-minor ideal of $\\mathcal{P}$ is not prime.\n",  
	BR{},BR{},
	EXAMPLE {
       "Q= cellCollection {{2, 1}, {3, 2}, {1, 2}, {2, 3}};",
       "I=polyoIdeal(Q);",
       "J=polyoToric(Q,{{2,2}});",
       "R=ring I",
       "J=substitute(J,R);",
       "J==I",
       "select(first entries gens J,f->first degree f>=3)",
        },
     SeeAlso =>{cellCollection}
	 
}


document {
     Key => {polyoLattice, (polyoLattice, CollectionOfCells)},
     Headline => "Lattice associated with a collection of cells",
     Usage => "polyoLattice Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the lattice ideal associated with Q"}
     },

     "Given a collection of cells $\\mathcal{P}$, this function returns the lattice ideal associated with $\\mathcal{P}$.",
    BR{}, BR{},
     "Let $\\mathcal{P}$ be a collection of cells. For each $a \\in V(\\mathcal{P})$, denote by $\\mathbf{v}_a$ the vector in $\\mathbb{Z}^{|V(\\mathcal{C})|}$ having a $1$ in the coordinate indexed by $a$ and a $0$ elsewhere.",
     "If $C = [a,b] \\in \\mathcal{P}$ is a cell with diagonal corners $a,b$ and anti-diagonal corners $c,d$, we set $$\\mathbf{v}_{[a,b]} = \\mathbf{v}_a + \\mathbf{v}_b - \\mathbf{v}_c - \\mathbf{v}_d \\in \\mathbb{Z}^{|V(\\mathcal{C})|}.$$\n",
     "We define $\\Lambda_{\\mathcal{P}}$ as the sublattice of $\\mathbb{Z}^{|V(\\mathcal{P})|}$ generated by the vectors $\\mathbf{v}_C$ for all $C \\in \\mathcal{P}$. Observe that the rank of $\\Lambda_{\\mathcal{P}}$ equals $|\\mathcal{P}|$.",
    BR{},
 "Let $n = |V(\\mathcal{P})|$. For $\\mathbf{v} \\in \\mathbb{N}^n$, denote by $x^{\\mathbf{v}}$ the monomial in $S$ having $\\mathbf{v}$ as its exponent vector. For $\\mathbf{e} \\in \\mathbb{Z}^n$, let $\\mathbf{e}^+$ be the vector obtained from $\\mathbf{e}$ by replacing all negative components with zero, and set $\\mathbf{e}^- = - (\\mathbf{e} - \\mathbf{e}^+)$ for its non-positive part.\n",
BR{},
     "The lattice ideal $L_{\\mathcal{P}}$ of $\\Lambda_{\\mathcal{P}}$ is then the binomial ideal in $S$ defined as",
     "$$L_{\\mathcal{P}} = (\\,x^{\\mathbf{e}^+} - x^{\\mathbf{e}^-} \\mid \\mathbf{e} \\in \\Lambda_{\\mathcal{P}}\\,).$$",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{3,2},{1,2},{1,3},{2,3}};",
          "L = polyoLattice Q"
     }
}

document {
     Key => {adjacent2MinorIdeal, (adjacent2MinorIdeal, CollectionOfCells)},
     Headline => "Ideal generated by adjacent 2-minors",
     Usage => "adjacent2MinorIdeal Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the ideal generated by the adjacent $2$-minors of Q"}
     },

     "This function constructs the ideal generated by all adjacent $2$-minors of Q, as defined in [HH2012].",
	BR{},BR{},
	UL {
	LI {"[HH2012] ", EM "Ideals generated by adjacent 2-minors", "(J. Herzog, T. Hibi, 2012, J. Commut. Algebra).\n"}
	 },

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "I = adjacent2MinorIdeal Q"
     },
     SeeAlso =>{cellCollection}
}


document {
     Key => {isNonAttackingRooks, (isNonAttackingRooks, List, List, CollectionOfCells)},
     Headline => "Check if a rook configuration is non-attacking",
     Usage => "isNonAttackingRooks(A,B,Q)",
     Inputs => {
    "A" => {"the lower left corner of the cell where the rook is placed"},
    "B" => {"the lower left corner of the cell where the rook is placed"},
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if no two rooks attack each other, otherwise false"}
     },

     "Given a collection of cells $\\mathcal{P}$, this function checks whether a configuration of two rooks placed on the cells of $\\mathcal{P}$ is non-attacking, that is, whether no two rooks share the same row or column or, if they are on the same row (resp. column), they are not connected by a horizontal (resp. vertical) path of cells of $\\mathcal{P}$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "isNonAttackingRooks({1,1}, {2,2}, Q)",
          "isNonAttackingRooks({1,1}, {1,2}, Q)"
     },
     SeeAlso =>{cellCollection}
}


document {
     Key => {allNonAttackingRookConfigurations, (allNonAttackingRookConfigurations, CollectionOfCells)},
     Headline => "All non-attacking rook configurations in Q",
     Usage => "allNonAttackingRookConfigurations(Q)",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the list of all configurations of non-attacking rooks on $\\mathcal{P}$"}
     },

     "For a collection of cells $\\mathcal{P}$, this function returns the list of all configurations of non-attacking rooks on $\\mathcal{P}$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{3,1},{3,2}};",
          "netList allNonAttackingRookConfigurations Q"
     },
     SeeAlso =>{cellCollection,isNonAttackingRooks}
}


document {
     Key => {rookNumber, (rookNumber, CollectionOfCells)},
     Headline => "Rook number of a collection of cells",
     Usage => "rookNumber(Q)",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the maximum number of rooks that can be placed on Q in non-attacking positions."}
     },

     "Given a collection of cells $\\mathcal{P}$, this routine computes the maximum number of rooks that can be placed on $\\mathcal{P}$ in non-attacking position.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "rookNumber Q"
     },
     SeeAlso =>{cellCollection,isNonAttackingRooks,allNonAttackingRookConfigurations}
}


document {
     Key => {rookPolynomial, (rookPolynomial, CollectionOfCells)},
     Headline => "Rook polynomial of a collection of cells",
     Usage => "rookPolynomial Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the rook polynomial of Q"}
     },

     "The rook polynomial of a collection of cells $\\mathcal{P}$ is defined as 
      $r_\\mathcal{P}(t) = \\sum_{k \\ge 0} r_k(\\mathcal{P}) t^k,$
      where $r_k(\\mathcal{P})$ denotes the number of non-attacking configurations of $k$ rooks on $\\mathcal{P}$. 
      This function computes and returns the polynomial $r_\\mathcal{P}(t)$.\n",
	BR{},BR{},
	"The importance of this rook polynomial is underlined by the fact that it appears to describe the Hilbert series of the coordinate ring of collections of cells that do not contain the square tetromino, also known as thin (see [RR2021]).\n",
BR{},
	UL {
	LI {"[RR2021] ", EM "Hilbert series of simple thin polyominoes", "(R. Rinaldo, F. Romeo, 2021, J. Algebr. Comb.).\n"}
	 },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "rookPolynomial Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{3,1},{3,2}};",
          "rookPolynomial Q"
     },
     SeeAlso =>{cellCollection,isNonAttackingRooks,allNonAttackingRookConfigurations, rookNumber}
}


document {
     Key => {equivalenceClassesSwitchingRook, (equivalenceClassesSwitchingRook, CollectionOfCells)},
     Headline => "Equivalence classes of rook configurations under switching",
     Usage => "equivalenceClassesSwitchingRook Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the list of equivalence classes of rook configurations under the switching equivalence"}
     },

     "This function computes the equivalence classes of non-attacking rook configurations 
      under the switching operation.\n",
	BR{}, BR{},
      "Let $\\mathcal{P}$ be a collection of cells. Two rooks are non-attacking if they do not share the same row or column or, if they are on the same row (resp. column), they are not connected by a horizontal (resp. vertical) path of cells of $\\mathcal{P}$ A $j$-rook configuration in $\\mathcal{P}$ is a set of $j$ non-attacking rooks placed within $\\mathcal{P}$, where $j \\ge 0$; by convention, the $0$-rook configuration is the empty set $\\emptyset$. We denote by $\\mathcal{R}_j(\\mathcal{P})$ the set of all $j$-rook configurations of $\\mathcal{P}$ for $j \\in \\{0, \\dots, r(\\mathcal{P})\\}$, adopting the convention that $\\mathcal{R}_0(\\mathcal{P}) = \\{\\emptyset\\}$ (so $|\\mathcal{R}_0(\\mathcal{P})| = 1$).\n",
 BR{},
"The collection $\\bigcup_{j=0}^{r(\\mathcal{P})} \\mathcal{R}_j(\\mathcal{P})$ forms a simplicial complex, called the chessboard complex of $\\mathcal{P}$.\n",
 BR{},BR{},
      "Two non-attacking rooks in $\\mathcal{P}$ are said to be in switching position if they occupy the diagonal (respectively, anti-diagonal) corner cells of an inner interval $I$ of $\\mathcal{P}$. 
      In that case, we say the rooks are in a diagonal (respectively, anti-diagonal) position.\n",
BR{},
      "Fix $j \\in \\{0, \\dots, r(\\mathcal{P})\\}$ and let $F \\in \\mathcal{R}_j(\\mathcal{P})$.\n",
BR{},
      "Suppose that two rooks $R_1, R_2 \\in F$ are in switching position in $I$, for some inner interval $I$. 
      Let $R_1', R_2'$ denote the rooks in the opposite diagonal (or anti-diagonal) cells of $I$. 
      Then the configuration $(F \\setminus \\{R_1, R_2\\}) \\cup \\{R_1', R_2'\\}$ also belongs to $\\mathcal{R}_j(\\mathcal{P})$. 
      This operation of replacing $R_1, R_2$ with $R_1', R_2'$ is called a switch of $R_1$ and $R_2$. \n",
	BR{},
      "It defines an equivalence relation $\\sim$ on $\\mathcal{R}_j(\\mathcal{P})$, where $F_1 \\sim F_2$ if and only if $F_2$ can be obtained from $F_1$ by a finite sequence of switches.",
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "netList equivalenceClassesSwitchingRook Q"
     },
	     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2},{1,3},{2,3},{3,1}};",
          "netList equivalenceClassesSwitchingRook Q"
     },
     SeeAlso =>{cellCollection,isNonAttackingRooks, rookNumber}
}



document {
     Key => {switchingRookPolynomial, (switchingRookPolynomial, CollectionOfCells)},
     Headline => "Switching rook polynomial of a collection of cells",
     Usage => "switchingRookPolynomial Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the switching rook polynomial of Q"}
     },

     "The switching rook polynomial encodes the number of switching equivalence classes 
      of rook configurations on a collection of cells $\\mathcal{P}$. 
      Each coefficient corresponds to the number of distinct equivalence classes 
      of the chessboard complex modulo the switching rook equivalence relation.\n",

	"Switching rook polynomial appears to describe the Hilbert series of the coordinate ring of collections of cells (see [JN2024], [QRR2022]). Note that when a collection of cells does not contain a square tetromino, then the switching rook polynomial coincides with the rook polynomial. \n",
BR{},
	UL {
	LI {"[JN2024] ", EM "Shellable simplicial complex and switching rook polynomial of frame polyominoes", "(R. Jahangir, F. Navarra, 2024, J. Pure Appl. Algebra).\n"},
	LI {"[QRR2022] ", EM "Hilbert series of parallelogram polyominoes", "(A.A. Qureshi, R. Rinaldo, F. Romeo, 2022, Res. Math. Sci.).\n"},
	 },

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "switchingRookPolynomial Q"
     },
     SeeAlso =>{cellCollection,isNonAttackingRooks,allNonAttackingRookConfigurations, rookNumber, equivalenceClassesSwitchingRook}
}

document {
     Key => {standardNonAttackingRookConfigurations, (standardNonAttackingRookConfigurations, CollectionOfCells)},
     Headline => "Standard non-attacking rook configurations",
     Usage => "standardNonAttackingRookConfigurations Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the list of all standard non-attacking rook configurations on Q"}
     },

     "This function generates all standard non-attacking rook configurations 
      on a collection of cells. Two rooks are in non-attacking position if they do not share the same row or column.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{3,1},{3,2}};",
          "netList standardNonAttackingRookConfigurations Q"
     },
     SeeAlso =>{cellCollection}
}


document {
     Key => {standardRookNumber, (standardRookNumber, CollectionOfCells)},
     Headline => "Standard rook number of a collection of cells",
     Usage => "standardRookNumber Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the standard rook number of Q"}
     },

     "The standard rook number is the maximum number of rooks that can be place on a collection of cells in (standard) non-attacking positions. Two rooks are in (standard) non-attacking position if they do not share the same row or column.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{3,1},{3,2}};",
          "standardRookNumber Q"
     },
     SeeAlso =>{cellCollection,standardNonAttackingRookConfigurations}
}


document {
     Key => {standardRookPolynomial, (standardRookPolynomial, CollectionOfCells)},
     Headline => "Standard rook polynomial of a collection of cells",
     Usage => "standardRookPolynomial Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the standard rook polynomial of Q"}
     },

     "This function computes the standard rook polynomial of a collection of cells $\\mathcal{P}$. 
      The polynomial encodes the number of standard non-attacking rook configurations of each size.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{3,1},{3,2}};",
          "standardRookPolynomial Q"
     },
     SeeAlso =>{cellCollection,standardNonAttackingRookConfigurations,standardRookNumber}
}


document {
     Key => {
          isPalindromic, 
          (isPalindromic, RingElement)
     },
     Headline => "Check if a polynomial is palindromic",
     Usage => "isPalindromic f",
     Inputs => {
          "f" => {"a polynomial in one variable with nonzero coefficients"}
     },
     Outputs => {
          {"true if $f$ is palindromic, otherwise false"}
     },

     "Let $f = a_0 + a_1 x + \\cdots + a_d x^d$. 
      The polynomial $f$ is palindromic if and only if $a_i = a_{d-i}$ for all $i = 0, \\ldots, d$. 
      This routine checks whether $f$ satisfies this condition.",

     BR{}, BR{},
     EXAMPLE {
          "R = QQ[x];",
          "isPalindromic(x^4 + 2*x^3 + 3*x^2 + 2*x + 1)"
     }
}



document {
     Key => {randomCollectionWithFixedRank, (randomCollectionWithFixedRank, ZZ)},
     Headline => "Random collection of cells with fixed rank",
     Usage => "randomCollectionWithFixedRank n",
     Inputs => {
          "n" => {"an integer specifying the number of cells"}
     },
     Outputs => {
          {"a randomly generated collection of $n$ cells"}
     },

     "Generates a random collection of $n$ cells.",

     BR{}, BR{},
     EXAMPLE {
          "Q = randomCollectionWithFixedRank 5"
     },
     SeeAlso =>{cellCollection,randomCollectionOfCells}
}


document {
     Key => {randomCollectionOfCells, (randomCollectionOfCells, ZZ)},
     Headline => "Random collection of cells up to a given size",
     Usage => "randomCollectionOfCells m",
     Inputs => {
          "m" => {"a positive integer bound on the number of cells"}
     },
     Outputs => {
          {"a randomly generated collection of cells of size at most m"}
     },

     "Produces a random collection of cells with a randomly chosen number of cells 
      between $1$ and the given bound $m$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = randomCollectionOfCells 8"
     },
     SeeAlso =>{cellCollection,randomCollectionWithFixedRank}
}


document {
     Key => {randomPolyominoWithFixedRank, (randomPolyominoWithFixedRank, ZZ)},
     Headline => "Random polyomino with fixed rank",
     Usage => "randomPolyominoWithFixedRank n",
     Inputs => {
          "n" => {"an integer specifying the number of cells"}
     },
     Outputs => {
          {"a random polyomino composed of $n$ connected cells"}
     },

     "Generates a random connected polyomino consisting of $n$ cells.",

     BR{}, BR{},
     EXAMPLE {
          "P = randomPolyominoWithFixedRank 6"
     },
     SeeAlso =>{cellCollection,randomPolyomino}
}

document {
     Key => {randomPolyomino, (randomPolyomino, ZZ)},
     Headline => "Random polyomino with random size up to a given bound",
     Usage => "randomPolyomino m",
     Inputs => {
          "m" => {"a positive integer bound on the number of cells"}
     },
     Outputs => {
          {"a randomly generated polyomino with at most $m$ cells"}
     },

     "Produces a random polyomino whose number of cells is randomly chosen 
      between $1$ and the given bound $m$.",

     BR{}, BR{},
     EXAMPLE {
          "P = randomPolyomino 10"
     },
     SeeAlso =>{cellCollection,randomPolyominoWithFixedRank}
}


document {
     Key => {cellGraph, (cellGraph, CollectionOfCells)},
     Headline => "Graph associated with a collection of cells",
     Usage => "cellGraph Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the graph whose vertices correspond to cells and edges to adjacency relations"}
     },

     "Constructs the adjacency graph $G$ of a collection of cells, where the vertices of $G$ are the cells and two vertices of $G$ are connected by an edge in $G$ if the corresponding two cells share a common side.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "G = cellGraph Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isColumnConvex,isConvex,collectionIsSimple}
}


document {
     Key => {collectionIsConnected, (collectionIsConnected, CollectionOfCells)},
     Headline => "Check if a collection of cells is connected",
     Usage => "collectionIsConnected Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"a sequence. The first component is a Boolean value: true if Q is connected, otherwise false. The second component gives the number of connected components of the collection of cells."}
     },

     "A collection of cells $\\mathcal{P}$ is connected if for every pair of cells $A$ and $B$ of $\\mathcal{P}$ there exists a path of cells $A=C_1,...,C_t=B$ of $\\mathcal{P}$, such that $C_i\\cap C_{i+1}$ is an edge, for all $i=1,..,t-1$.\n 
This function determines whether the collection of cells $\\mathcal{P}$ is connected and the number of the connected components.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,2}};",
          "collectionIsConnected Q"
     },
     SeeAlso =>{cellCollection,connectedComponentsCells,isRowConvex,isColumnConvex,isConvex,collectionIsSimple}
}


document {
     Key => {connectedComponentsCells, (connectedComponentsCells, CollectionOfCells)},
     Headline => "Connected components of a collection of cells",
     Usage => "connectedComponentsCells Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the list of connected components of Q"}
     },

     "Computes and returns the connected components of a collection of cells.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,3},{4,3},{4,4}};",
          "netList connectedComponentsCells Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected}
}


document {
     Key => {isRowConvex, (isRowConvex, CollectionOfCells)},
     Headline => "Row convexity of a collection of cells",
     Usage => "isRowConvex Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if Q is row-convex, otherwise false"}
     },

     "Checks whether a collection of cells $\\mathcal{P}$ is row-convex, 
      i.e., every pair of cells in the same row determines an inner interval fully contained in $\\mathcal{P}$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{1,2},{3,2}};",
          "isRowConvex Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{1,3},{2,3},{2,1}};",
          "isRowConvex Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isColumnConvex,isConvex,collectionIsSimple}
}

document {
     Key => {isColumnConvex, (isColumnConvex, CollectionOfCells)},
     Headline => "Column convexity of a collection of cells",
     Usage => "isColumnConvex Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if Q is column-convex, otherwise false"}
     },

     "Checks whether a collection of cells $\\mathcal{P}$ is column-convex, 
      i.e., every pair of cells in the same column determines an inner interval fully contained in $\\mathcal{P}$.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{1,2},{3,2}};",
          "isColumnConvex Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{1,3},{2,3},{2,1}};",
          "isColumnConvex Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isConvex,collectionIsSimple}
}

document {
     Key => {isConvex, (isConvex, CollectionOfCells)},
     Headline => "Convexity of a collection of cells",
     Usage => "isConvex Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if Q is convex (both row- and column-convex), otherwise false"}
     },

     "Checks whether a collection of cells is convex, 
      that is, both row- and column-convex.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};",
          "isConvex Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{1,2},{3,2}};",
          "isConvex Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{1,2},{1,3},{2,3},{2,1}};",
          "isConvex Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isColumnConvex,collectionIsSimple}
}

document {
     Key => {collectionIsSimple, (collectionIsSimple, CollectionOfCells)},
     Headline => "Simplicity of a collection of cells",
     Usage => "collectionIsSimple Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"true if Q is simple, otherwise false"}
     },

     "This function checks if a collection of cells is simple, that is, it has no holes.",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{1,2},{2,2}};",
          "collectionIsSimple Q"
     },
	     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{1,2},{3,2}};",
          "collectionIsSimple Q"
     },
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{3,1},{3,2},{1,2},{1,3},{2,3}};",
          "collectionIsSimple Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isColumnConvex,isConvex}
}


document {
     Key => {rankCollection, (rankCollection, CollectionOfCells)},
     Headline => "Rank of a collection of cells",
     Usage => "rankCollection Q",
     Inputs => {
          "Q" => {"a collection of cells"}
     },
     Outputs => {
          {"the number of cells in the collection"}
     },

     "Returns the number of cells (i.e., the rank).",

     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{2,2}};",
          "rankCollection Q"
     },
     SeeAlso =>{cellCollection,collectionIsConnected,connectedComponentsCells,isRowConvex,isColumnConvex,isConvex,collectionIsSimple}
}



-----------------------------------------------------------------------------------------------------
---------------------------------------- Options documentation --------------------------------------
-----------------------------------------------------------------------------------------------------


document {
     Key => {RingChoice,[polyoIdeal,RingChoice]},
     Headline => "optional argument for polyoIdeal",
     "Let $\\mathcal{P}$ be a collection of cells and $[(1,1),(m,n)]$ be the smallest interval of $\\NN^2$ containing $\\mathcal{P}$. Then we attach to $\\mathcal{P}$ the following polynomial ring $S_{\\mathcal{P}}=K[x_a:a\\in V(\\mathcal{P})$, where $K$ is a field.\n",
     BR{},
     "Whether it is 1 or by default it returns the ideal computed by ", TT "polyoIdeal ", "in the ambient ring given by ", TT "polyoRingDefault",". With a value different by 1 it returns the ideal in the ambient ring given by ", TT "polyoRingConvex",".",
     BR{},BR{},
     "Here we describe the using of the two options ", TT "polyoRingDefault"," and ", TT "polyoRingConvex",".\n",   
     BR{},BR{},
     BOLD "PolyoRingDefault \n",
     BR{},BR{},
     "This option gives the ideal $I_{\\mathcal{P}}$ in the polynomial ring $S_{\\mathcal{P}}$ where the monomial order is defined by ", TT "Term order ", "induced by the following order of the variables: $x_a > x_b$ with $a=(i,j)$ and $b=(k, l)$, if $i > k$, or $i = k$ and $j > l$.",
     	BR{},BR{},
     	ITALIC "Examples \n",
     	BR{},BR{},
     	EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{2,2}};",
          "I = polyoIdeal(Q);",
          "R=ring I;",
          "describe R"
          },
        BR{},  
     	EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{2,2}};",
          "I = polyoIdeal(Q,RingChoice=>1);",
          "R=ring I;",
          "describe R"
          },  
        BR{},  
     	EXAMPLE {
          "Q = cellCollection {{1,1},{2,1},{2,2}};",
          "I = polyoIdeal(Q,RingChoice=>1,TermOrder=> GRevLex);",
          "R=ring I;",
          "describe R"
          },
          
     BR{},BR{},
     BOLD "PolyoRingConvex \n",
     BR{},BR{},
     "A very interesting class of collections of cells which are studied from a combinatorial point of view is given by the weakly connected and convex ones.\n",
	BR{}, 
	"Let $\\mathcal{P}$ be a collection of cells. We say that $\\mathcal{P}$ is weakly connected if for any two cells $C$ and $D$ of $\\mathcal{P}$, there exist a sequence of cells of $\\mathcal{P}$ as $C = C_1,\\dots, C_m = D$ such that $C_i \\cap C_{i+1} \\neq \\emptyset$, for $i = 1,\\dots, m − 1$. Observe trivially that every polyomino is a weakly connected collection of cells. We say that a weakly connected collection $\\mathcal{P}$ of cells is row convex, if the horizontal cell interval $[A, B]$ is contained in $\\mathcal{P}$ for any two cells $A$ and $B$ of $\\mathcal{P}$ whose lower left corners are in horizontal position. Similarly one defines column convex. Hence $\\mathcal{P}$ is called convex if it is row and column convex.\n",
	BR{},
	"Assume that the smallest interval containing $\\mathcal{P}$ is $[(1,1),(m,n)]$. Consider the edge ring $R = K[s_it_j: (i, j) \\in V (\\mathcal{P})]$ associated to the bipartite graph $G$ with vertex set $\\{s_1,\\dots, s_m\\} \\cup\\{t_1,\\dots, t_n\\}$ to $\\mathcal{P}$ such that each vertex $(i, j) \\in V (\\mathcal{P})$ determines the edge $\\{s_i,t_j \\}$ in $G$. Let $S=K[x_a:a\\in V(\\mathcal{P})$ and $\\phi : S \\rightarrow R$ be the $K$-algebra homomorphism defined by $\\phi(x_{ij} ) = s_it_j$, for all $(i, j) \\in V (\\mathcal{P})$ and set $J_\\mathcal{P} = ker(\\phi)$. From Theorem 2.1 of [AAQ2012], we know that $I_{\\mathcal{P}}=J_{\\mathcal{P}}$, if $\\mathcal{P}$ is a weakly connected and convex collection of cells. In such a case, from [OH1999] we get that the generators of $I_{\\mathcal{P}}$ forms the reduced Groebner basis with respect to a suitable order <, and in particular the initial ideal $\\mathrm{in}_<(I_{\\mathcal{P}})$ is squarefree and generated in degree two. \n",
	BR{}, 
	"Following the proof in [OH1999], this routine implements an algorithm which gives the polynomial ring where the monomial order is <.",
	BR{},BR{},
	"If $\\mathcal{P}$ is a weakly connected and convex collection of cells (or in particular a convex polyomino), then the function", TT " polyoRingConvex ", "returns the polynomial ring attached to $\\mathcal{P}$ whose monomial order $<$ is such that $\\mathrm{in}_<(I_{\\mathcal{P}})$ is squarefree and generated in degree two.",
	BR{},BR{},
	ITALIC "Literature \n",
	UL {
	 LI {"[AAQ2012] ", EM "Ideals generated by 2-minors, collections of cells and stack polyominoes ", "(A. A. Qureshi, 2012,  J. Algebra).\n"},
	 LI {"[OH1999] ", EM "Koszul bipartite graphs ", "(H. Ohsugi and T. Hibi, 1999, Adv. Appl. Math. 22,  25-28)."}
	 },
	BR{},
     	ITALIC "Examples \n",
     	BR{},BR{},
	EXAMPLE {
          "Q = cellCollection {{1, 2}, {2, 2}, {1, 3}, {2, 3}, {2, 4}, {3, 2}, {3, 1}};",
          "I = polyoIdeal(Q,RingChoice=>2);",
          "R=ring I;",
          "describe R"
         },
         
        BR{},BR{},
        EXAMPLE {
 	"Q = cellCollection {{1, 3}, {2, 2}, {2, 3}, {2, 4}, {3, 4}, {3, 3}, {3, 2}, {3, 1}, {3, 5}, {4, 4}, {4, 3}, {5, 4}};",
         "I = polyoIdeal(Q,RingChoice=>2);",
         "In = monomialIdeal(leadTerm(I))"
         }, 
     SeeAlso =>{polyoIdeal}
     }
     
     
document {
     Key => {Field, [polyoIdeal,Field], [adjacent2MinorIdeal, Field]},
     Headline => "optional argument for polyoIdeal and adjacent2MinorIdeal",
     "Specifies the coefficient field of the ambient ring used to define the ideal. 
      If not provided, the default field is the one associated with the current polynomial ring.",
     BR{}, BR{},
     EXAMPLE {
          "F = GF(9, Variable => a)",
          "Q = cellCollection {{1,1}, {1,2}, {2,1}, {2,2}};",
          "I = adjacent2MinorIdeal(Q, Field => F, TermOrder => Lex)"
     },
 BR{}, BR{},
   	 EXAMPLE {
          "F = GF(81,Variable=>a)",
          "Q= cellCollection {{1,1},{2,1},{2,2}};",
          "I = polyoIdeal(Q,Field=> F,RingChoice=>1,TermOrder=> GRevLex)"
          },
     SeeAlso =>{polyoIdeal, adjacent2MinorIdeal}  
 }
          
document {
     Key => {TermOrder, [polyoIdeal,TermOrder], [adjacent2MinorIdeal, TermOrder]},
      Headline => "optional argument for polyoIdeal and adjacent2MinorIdeal",
     "Sets the term order for the polynomial ring where the ideal is defined. 
      Typical options include Lex, GRevLex, or other standard monomial orders.",
     BR{}, BR{},
     EXAMPLE {
          "Q = cellCollection {{1,1}, {2,1}, {2,2}};",
          "I = adjacent2MinorIdeal(Q, TermOrder => GRevLex)"
     },
     "For polyoIdeal, this option can be used just when ", TT "RingChoice => 1.",
     BR{},BR{},
     EXAMPLE {
          "Q= cellCollection {{1,1},{2,2},{3,3}};",
          "I = polyoIdeal(Q,RingChoice=>1,TermOrder=> GRevLex);",
          "R=ring I;",
          "describe R"
          },
     SeeAlso =>{polyoIdeal, adjacent2MinorIdeal, RingChoice}
}
 

   
--------------------------------------------------------------------------------------------------
----------------------------------    TEST   -----------------------------------------------------
--------------------------------------------------------------------------------------------------

TEST ///   
-- polyoVertices test
Q = cellCollection {{1,1},{1,2},{2,1}};
V = sort toList polyoVertices Q;
W = sort toList{(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2)};
assert(V == W);
///


TEST ///
-- innerInterval test
Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};
a = {1,1};
b = {2,2};
assert(innerInterval(a,b,Q));
///

          
TEST ///
--polyoIdeal test
Q= cellCollection {{1, 3}, {2, 2}, {2, 3}, {2, 4}, {3, 4}, {3, 3}, {3, 2}, {3, 1}, {3, 5}, {4, 4}, {4, 3}, {5, 4}};
I = polyoIdeal Q;
J = ideal(x_(4,2)*x_(3,1)-x_(4,1)*x_(3,2), x_(5,4)*x_(1,3)-x_(5,3)*x_(1,4), x_(4,5)*x_(2,3)-x_(4,3)*x_(2,5), x_(6,5)*x_(5,4)-x_(6,4)*x_(5,5), x_(3,4)*x_(1,3)-x_(3,3)*x_(1,4), x_(4,3)*x_(3,1)-x_(4,1)*x_(3,3), x_(4,5)*x_(2,4)-x_(4,4)*x_(2,5), x_(6,5)*x_(3,4)-x_(6,4)*x_(3,5), x_(3,3)*x_(2,2)-x_(3,2)*x_(2,3), x_(4,4)*x_(3,1)-x_(4,1)*x_(3,4), x_(4,3)*x_(3,2)-x_(4,2)*x_(3,3), x_(3,4)*x_(2,2)-x_(3,2)*x_(2,4), x_(4,5)*x_(3,1)-x_(4,1)*x_(3,5), x_(4,4)*x_(3,2)-x_(4,2)*x_(3,4), x_(5,4)*x_(2,3)-x_(5,3)*x_(2,4), x_(5,4)*x_(4,3)-x_(5,3)*x_(4,4), x_(3,4)*x_(2,3)-x_(3,3)*x_(2,4), x_(3,5)*x_(2,2)-x_(3,2)*x_(2,5), x_(4,6)*x_(3,1)-x_(4,1)*x_(3,6), x_(4,5)*x_(3,2)-x_(4,2)*x_(3,5), x_(4,4)*x_(3,3)-x_(4,3)*x_(3,4), x_(5,5)*x_(2,3)-x_(5,3)*x_(2,5), x_(5,5)*x_(4,3)-x_(5,3)*x_(4,5), x_(3,5)*x_(2,3)-x_(3,3)*x_(2,5), x_(4,6)*x_(3,2)-x_(4,2)*x_(3,6), x_(4,5)*x_(3,3)-x_(4,3)*x_(3,5), x_(5,5)*x_(2,4)-x_(5,4)*x_(2,5), x_(5,5)*x_(4,4)-x_(5,4)*x_(4,5), x_(3,5)*x_(2,4)-x_(3,4)*x_(2,5), x_(4,6)*x_(3,3)-x_(4,3)*x_(3,6), x_(4,5)*x_(3,4)-x_(4,4)*x_(3,5), x_(2,4)*x_(1,3)-x_(2,3)*x_(1,4), x_(4,4)*x_(1,3)-x_(4,3)*x_(1,4), x_(6,5)*x_(2,4)-x_(6,4)*x_(2,5), x_(4,6)*x_(3,4)-x_(4,4)*x_(3,6), x_(6,5)*x_(4,4)-x_(6,4)*x_(4,5), x_(4,3)*x_(2,2)-x_(4,2)*x_(2,3), x_(4,6)*x_(3,5)-x_(4,5)*x_(3,6), x_(5,4)*x_(3,3)-x_(5,3)*x_(3,4), x_(4,4)*x_(2,2)-x_(4,2)*x_(2,4), x_(5,5)*x_(3,3)-x_(5,3)*x_(3,5), x_(4,5)*x_(2,2)-x_(4,2)*x_(2,5), x_(4,4)*x_(2,3)-x_(4,3)*x_(2,4), x_(5,5)*x_(3,4)-x_(5,4)*x_(3,5));
assert(I==J);
///



TEST ///
--polyoMatrix test
Q= cellCollection {{3, 1}, {4, 1}, {5, 1}, {5, 2}, {5, 3}, {5, 4}, {5, 5}, {4, 5}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 4}, {1, 5}, {2, 5}, {3, 5}};
M = polyoMatrix Q;
m = matrix({{x_(1,6), x_(2,6), x_(3,6), x_(4,6), x_(5,6), x_(6,6)}, {x_(1,5), x_(2,5), x_(3,5), x_(4,5), x_(5,5), x_(6,5)}, {x_(1,4), x_(2,4), x_(3,4), x_(4,4), x_(5,4), x_(6,4)}, {x_(1,3), x_(2,3), x_(3,3), x_(4,3), x_(5,3), x_(6,3)}, {0, 0, x_(3,2), x_(4,2), x_(5,2), x_(6,2)}, {0,  0, x_(3,1), x_(4,1), x_(5,1), x_(6,1)}});
assert(m==M);
///

TEST ///
--polyoMatrixReduced test
Q= cellCollection {{1,1}, {2,2}};
M = polyoMatrixReduced Q;
m = matrix({{x_(1,1), 0, x_(2,1)}, {0, x_(3,3), x_(2,3)}, {x_(1,2), x_(3,2), x_(2,2)} });
assert(m==M);
///


TEST ///
--polyoToric test
Q= cellCollection { {1, 1}, {2, 1}, {3, 1}, {3, 2}, {3, 3}, {2, 3}, {1, 3}, {1, 2} };
J = polyoToric(Q,{{2,2}});
K = ideal(-x_(2,2)*x_(1,1)+x_(2,1)*x_(1,2), -x_(3,2)*x_(1,1)+x_(3,1)*x_(1,2), -x_(4,2)*x_(1,1)+x_(4,1)*x_(1,2), -x_(2,3)*x_(1,1)+x_(2,1)*x_(1,3), -x_(2,3)*x_(1,2)+x_(2,2)*x_(1,3), -x_(2,4)*x_(1,1)+x_(2,1)*x_(1,4), -x_(2,4)*x_(1,2)+x_(2,2)*x_(1,4), -x_(2,4)*x_(1,3)+x_(2,3)*x_(1,4), -x_(3,4)*x_(1,3)+x_(3,3)*x_(1,4), -x_(4,4)*x_(1,3)+x_(4,3)*x_(1,4), -x_(3,2)*x_(2,1)+x_(3,1)*x_(2,2), -x_(4,2)*x_(2,1)+x_(4,1)*x_(2,2), -x_(3,4)*x_(2,3)+x_(3,3)*x_(2,4), -x_(4,4)*x_(2,3)+x_(4,3)*x_(2,4), -x_(4,2)*x_(3,1)+x_(4,1)*x_(3,2), -x_(4,3)*x_(3,1)+x_(4,1)*x_(3,3), -x_(4,3)*x_(3,2)+x_(4,2)*x_(3,3), -x_(4,4)*x_(3,1)+x_(4,1)*x_(3,4), -x_(4,4)*x_(3,2)+x_(4,2)*x_(3,4), -x_(4,4)*x_(3,3)+x_(4,3)*x_(3,4));
assert(K == J);
///


TEST ///
--polyoToric with polyoIdeal test
Q = cellCollection {{1, 2}, {2, 2}, {1, 3}, {2, 3}, {2, 4}, {3, 2}, {3, 1}};
I = polyoIdeal Q;
J = polyoToric(Q,{});
R = ring I;
J = substitute(J,R);
assert(I == J);
///

TEST ///
-- adjacent2MinorIdeal test
Q = cellCollection {{1,1},{2,1},{1,2},{2,2}};
I = adjacent2MinorIdeal Q;
J = ideal(x_(1,1)*x_(2,2)-x_(1,2)*x_(2,1), x_(2,1)*x_(3,2)-x_(2,2)*x_(3,1), x_(1,2)*x_(2,3)-x_(1,3)*x_(2,2), x_(2,2)*x_(3,3)-x_(2,3)*x_(3,2));
assert(I == J);
///

TEST ///
-- isNonAttackingRooks test
Q = cellCollection {{1,1},{2,2},{1,2},{2,1}};
assert(isNonAttackingRooks({1,1},{2,2},Q));
assert(not isNonAttackingRooks({1,1},{1,2},Q));
///

TEST ///
-- allNonAttackingRookConfigurations test
Q = cellCollection {{1,1},{1,2},{2,1}};
conf = allNonAttackingRookConfigurations Q;
assert(conf == {{{{1, 1}}, {{1, 2}}, {{2, 1}}}, {{{1, 2}, {2, 1}}}});
///

TEST ///
-- rookPolynomial test
Q = cellCollection {{1,1},{1,2},{2,1},{3,1}, {3,2}};
f = rookPolynomial Q;
assert( flatten entries ((coefficients f)#1) ==  {1,5,5,1});
///

TEST ///
-- equivalenceClassesSwitchingRook test
Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};
E = equivalenceClassesSwitchingRook Q;
F = {{{{{1, 1}}}, {{{1, 2}}}, {{{2, 1}}}, {{{2, 2}}}}, {{{{1, 2}, {2, 1}}, {{1, 1}, {2, 2}}}}};
assert(E == F);
///

TEST ///
-- switchingRookPolynomial test
Q = cellCollection {{1,1},{1,2},{2,1},{2,2}};
f = switchingRookPolynomial Q;
assert( flatten entries ((coefficients f)#1) ==  {1,4,1});
///

TEST ///
-- standardRookPolynomial test
Q = cellCollection {{1,1},{1,2},{2,1},{3,1}, {3,2}};
f = standardRookPolynomial Q;
assert( flatten entries ((coefficients f)#1) ==  {4,5,1});
///

TEST /// 
-- isPalindromic test
R = QQ[z];
f = z^3 + 2*z^2 + 2*z + 1;
assert(isPalindromic f);
///

TEST ///
-- randomCollectionWithFixedRank test
n = 5;
Q = randomCollectionWithFixedRank n;
assert(#Q == n);
///

TEST ///
-- randomCollectionOfCells test
m = 8;
Q = randomCollectionOfCells m;
assert( #Q <= m);
///

TEST ///
-- randomPolyominoWithFixedRank test
n = 6;
Q = randomPolyominoWithFixedRank n;
assert(#Q == n);
///

TEST ///
-- randomPolyomino test
m = 10;
Q = randomPolyomino m;
assert(#Q <= m);
///


TEST ///
-- collectionIsConnected test
Q = cellCollection { {1,1}, {1,2}, {2,1}, {2,2} };
assert( collectionIsConnected Q == (true, 1) );
///

TEST ///
-- connectedComponentsCells test
Q = cellCollection { {1,1}, {1,2}, {5,5} };
C = connectedComponentsCells Q;
assert( C== {{{1, 1}, {1, 2}}, {{5, 5}}});
///

TEST ///
-- isRowConvex test
Q = cellCollection { {1,1}, {2,1}, {3,1}, {3,2}, {1,2} };
assert( not(isRowConvex Q) );
///

TEST ///
-- isColumnConvex test
Q = cellCollection { {1,1}, {1,2}, {1,3}, {1,4}, {2,1}, {2,4}};
assert( not(isColumnConvex Q) );
///

TEST ///
-- isConvex test
Q =cellCollection  { {1,1}, {1,2}, {2,1}, {2,2} }; 
assert( isConvex Q );
///

TEST ///
-- collectionIsSimple test
Q = cellCollection { {1,1}, {1,2}, {2,1}, {2,2} };
assert( collectionIsSimple Q );
///

TEST ///
-- rankCollection test
Q = cellCollection { {1,1}, {1,2}, {2,1}, {2,2} };
assert( rankCollection Q == 4 );
///


end