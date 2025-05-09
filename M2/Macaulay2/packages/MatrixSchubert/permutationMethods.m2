-----------------------------------------------
-----------------------------------------------
--**Useful functions for permutations**
-----------------------------------------------
-----------------------------------------------

-----------------------------------------------------------
--tests if a list is a permutation in 1-line notation
--INPUT: A list of integers w
--OUTPUT: TRUE if w is a permutation; else FALSE
-----------------------------------------------------------
isPerm = method()
isPerm List := Boolean => (w) -> (
    n := #w;
    (sort w) == toList(1..n)
)

--------------------------------
--auxiliary function for making a permutation matrix out of a perm in 1-line notation
--INPUT: a list w, which is a permutation in 1-line notation
--OUTPUT: a permutation matrix A corresponding to to w
--NOTE: this might be the transpose of what some would want
--TODO: add documentation
-----------------------------------
permToMatrix = method()
permToMatrix List := Matrix => (w) -> (
    if not(isPerm w) then error("The input must be a permutation.");
    n := #w;
    transpose (id_(ZZ^n))_(apply(w, i-> i-1))
)


-----------------------------------------------------------
--tests of a permutation is the identity permutation
--INPUT: a list of integers w
--OUTPUT: TRUE if w is the identity perm; else FALSE
-----------------------------------------------------------

isIdentity = method()
isIdentity List := Boolean => (w) -> (
    n := #w;
    w == toList(1..n)
)

-----------------------------------------------------------
--finds index of last descent in a permutation
--INPUT: a permutation in 1-line notation
--OUPTUT: last index where w_i > w_(i+1)
-----------------------------------------------------------


lastDescent = method()
lastDescent List := ZZ => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
    if isIdentity(w) then error ("Expecting a non-identity permutation.");
    n := #w;
   
    result := -1;
    scan (reverse (0..n-2), i -> if w_i > w_(i+1) then (result = i+1; break));
    result
)

-----------------------------------------------------------
--find index of first descent in a permutation
--INPUT: a permutation in 1-line notation
--OUTPUT: first index where w_i > w_(i+1)
-----------------------------------------------------------


firstDescent = method()
firstDescent List := ZZ => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
    if isIdentity(w) then error ("Expecting a non-identity permutation.");
    n := #w;
   
    result := -1;
    scan ((0..n-2), i-> if w_i > w_(i+1) then (result = i+1; break));
    result
)


descentSet = method()
descentSet List := List => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
--    if isIdentity(w) then error ("Expecting a non-identity permutation.");
    n := #w;

    result := new MutableList;
    apply ((0..n-2), i-> if w_i > w_(i+1) then (result#(#result) = i+1));
    toList result
)


-----------------------------------------------------------
--computes the length of a permutation
-----------------------------------------------------------

permLength = method()
permLength List:= ZZ => (p) -> (
    if not(isPerm p) then error("The input must be a permutation.");
    l := 0;
    scan(#p, i -> scan(i..#p-1, j -> (if p#i > p#j then l = l+1)));
    l
)
 
-----------------------------------------------------------
--swaps the (zero-indexed) entries in positions i and j
-----------------------------------------------------------

swap = (L,i,j) -> (
    apply(#L, k -> if k != i and k != j then L_k
	               else if k == i then L_j
				   else L_i)
)


-----------------------------------------------------------
--switches entries with values a and b
-----------------------------------------------------------

swapValues = (w,a,b) -> (
    winv := inverseOf(w);
    apos := winv#(a-1) - 1;
    bpos := winv#(b-1) - 1;
    swap(w, apos, bpos)
)

-----------------------------------------------------------
--computes inverse permutation
-----------------------------------------------------------

inverseOf = method()
inverseOf List := List => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
    winv := new MutableList;
    scan(#w, i-> winv#(w_i-1)=i+1);
    toList winv
    )

-----------------------------------------------------------
--creates longest word/longest permutation {n,n-1,...2,1}
-----------------------------------------------------------

longestPerm = method()
longestPerm ZZ := List => (n) -> (
    if n < 1 then error ("Expecting a positive integer.");
    toList (reverse (1..n))
    )

-----------------------------------------------------------
--INPUT: a permutation as a list of integers w
--OUTPUT: list of simple transpositions giving a reduced word for w
-----------------------------------------------------------

getOneReducedWord = method()
getOneReducedWord List := List => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
    if isIdentity(w) then  {}
    else (
        i := firstDescent(w);
        newPerm := swap(w, i-1, i);
        getOneReducedWord(newPerm)) | {i}
    )


------------------------------------
--INPUT: A transposition in cycle notation, and the n for which to regard perm 
--       as an element of S_n
--       OR a permutation matrix
--       IF the matrix is not a permutation matrix, then returns the empty permutation.
--OUTPUT: the transposition in one-line notation
--TODO: docs and tests
------------------------------------
toOneLineNotation = method()
toOneLineNotation (List, ZZ) := List => (perm, maxIdx) -> (
    if maxIdx == 1 then return {1};
    switch(perm_0-1, perm_1-1, toList(1..maxIdx))
)
toOneLineNotation (Matrix) := List => (P) -> (
     if (isASM P == false) then return {};
     nonzeroIndices := flatten for j from 0 to (numColumns P - 1) list positions(flatten entries P^{j}, i -> i != 0);
     w := nonzeroIndices / (i -> i+1);
     if isPerm w then return w;
     if not (isPerm w) then return {};
)


------------------------------------
--INPUT: An index (i,j)
--OUTPUT: the corresponding transposition according to antidiagonal term order
--TODO: docs and tests
------------------------------------
toAntiDiagTrans = method()
toAntiDiagTrans (Sequence, ZZ) := List => (idx, maxIdx) -> (
    transposition := {sum(toList idx)-1, sum(toList idx)};
    toOneLineNotation(transposition, maxIdx)
)
toAntiDiagTrans (List, ZZ) := List => (idx, maxIdx) -> (
    transposition := {sum(toList idx)-1, sum(toList idx)};
    toOneLineNotation(transposition, maxIdx)
)

------------------------------------
--INPUT: Two permutations in one-line notation
--OUTPUT: the composition of the two permutations w = vu
------------------------------------
composePerms = method()
composePerms (List, List) := List => (u,v) -> (
    if not (isPerm u) then error("The first argument is not a permutation.");
    if not (isPerm v) then error("The second argument is not a permutation.");
    if not (#v==#u) then error("Expected permutations of the same length.");
    u0 := apply(u, i->i-1);
    v0 := apply(v, i->i-1);
    apply(u0_v0, i-> i+1)
)

--------------------------------
--checks if a permutation is pattern-avoiding
--INPUT: a permutation (in 1-line notation), written as a list
--OUTPUT: whether the permutation avoid the pattern
--TODO: input validation/type checking
--------------------------------
isPatternAvoiding = method()
isPatternAvoiding (List,List) := Boolean => (perm, pattern) -> (
    --input validation
    if not (isPerm perm) then error(toString perm | " is not a permutation.");
    --assume permutation is pattern-avoiding, break if not true
    for idx in subsets(0..#perm-1, #pattern) do {
        vals := perm_(idx);
        sortedVals := sort(vals);
        relPositions := hashTable toList apply(0..#vals-1, i -> {sortedVals#i, i});
        p := toList apply(vals, i -> (relPositions#i) + 1); 
        if p == pattern then return false;
    };
    true
)

--------------------------------
--checks if a permutation is vexillary, i.e. 2143-avoiding
--INPUT: a permutation (1-line notation), written as a list
--OUTPUT: whether the permutation is vexillary
--TODO: input validation/type checking
--------------------------------
isVexillary = method()
isVexillary List := Boolean => (perm) -> (
    if not (isPerm perm) then error(toString perm | " is not a permutation.");
    isPatternAvoiding(perm, {2,1,4,3})
)

--------------------------------
--checks if a permutation avoids all of the given patterns, i.e. 2143-avoiding
--INPUT: a permutation (1-line notation), written as a list, and a list of lists (patterns)
--OUTPUT: whether the permutation avoids all of the patterns
--TODO: input validation/type checking
--------------------------------
avoidsAllPatterns = method()
avoidsAllPatterns (List, List) := Boolean => (perm, patterns) -> (
    all(patterns, pattern -> isPatternAvoiding(perm, pattern))
)

--------------------------------
--checks if a permutation is Cartwright-Sturmfels
--INPUT: a permutation (1-line notation)
--OUTPUT: whether the permutation avoids all of the patterns
--TODO: input validation/type checking
--------------------------------
isCartwrightSturmfels = method()
isCartwrightSturmfels List := Boolean => (perm) -> (
    patterns := {{1,2,5,4,3}, 
                 {1,3,2,5,4}, 
                 {1,3,5,2,4}, 
                 {1,3,5,4,2}, 
                 {2,1,5,4,3}, 
                 {1,2,5,3,6,4}, 
                 {1,2,5,6,3,4}, 
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {3,1,5,2,6,4},
                 {3,1,5,6,2,4},
                 {3,1,5,6,4,2}};
    all(patterns, pattern -> isPatternAvoiding(perm, pattern))
)

--------------------------------
--checks if a permutation is CDG
--INPUT: a permutation (1-line notation)
--OUTPUT: whether the permutation avoids all of the patterns
--TODO: input validation/type checking
--------------------------------
isCDG = method()
isCDG List := Boolean => (perm) -> (
    patterns := {{1,3,2,5,4},
                 {2,1,5,4,3},
                 {2,1,4,6,3,5},
                 {2,1,5,3,6,4},
                 {2,1,5,6,3,4},
                 {2,4,1,6,3,5},
                 {3,1,5,2,6,4},
                 {4,2,6,1,7,3,5}};
    all(patterns, pattern -> isPatternAvoiding(perm, pattern))
)

------------------------------------------
--INPUT: longestIncrSeq, takes a previous value, a previous size, and a permutation in one line notation
--OUTPUT: returns the length of the longest consecutive permutation plus the previous size
--        that starts at the beginning of the permutation and has the elements of the sequence larger than the previous value
------------------------------------------
longestIncrSeq = method()
longestIncrSeq (ZZ,ZZ,List) := List => memoize ((preVal,prevSZ,w) -> (
    if w == {} then return prevSZ;
    
    longestSZ := prevSZ;
    currSZ := prevSZ;
    currVal := preVal;
    for i from 0 to #w-1 do (
    	currVal = w_i;
	if (currVal < preVal) then currSZ = longestIncrSeq(preVal, prevSZ, w_{i+1..#w-1})
	else currSZ = longestIncrSeq(currVal, prevSZ+1, w_{i+1..#w-1});
	longestSZ = max(longestSZ, currSZ);
    );
    longestSZ
))

------------------------------------------
--INPUT: rajcode, takes a permutation in one line notation
--OUTPUT: returns the rajcode of the permutation
------------------------------------------
rajcode = method()
rajcode List := ZZ => (w) -> (
    if not (isPerm w) then error ("Expecting a permutation.");
    rajCodeVec := new MutableList;
    for k from 0 to #w-1 do (
	rajCodeVec#(#rajCodeVec) = (#w-k) - longestIncrSeq(w_k, 1, w_{k+1..#w-1});
    );
    toList rajCodeVec
)

------------------------------------------
--INPUT: rajIndex, takes a permutation in one line notation
--OUTPUT: returns the rajIndex of the permutation
------------------------------------------
rajIndex = method()
rajIndex List := ZZ => (w) -> ( 
    if not (isPerm w) then error ("Expecting a permutation.");
    return sum rajcode w;
)

----------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: single Grothendieck polynomials
----------------------------
grothendieckPolynomial = method(Options=>{Algorithm=>"DividedDifference"})
grothendieckPolynomial(List) := opts -> w -> (
    if not(isPerm w) then error("The input must be a permutation in one-line notation.");
-*    if opts.Algorithm == "Degree" then (
        I := schubertDeterminantalIdeal w;
        R := ring I;
        kk := coefficientRing R;
        possibleDegs := apply(#w, i-> toList insert(i,1,(#w-1):0));
        degs := splice apply(possibleDegs, i->#w:i);
        Q := kk[R_*, Degrees => degs];
        numerator hilbertSeries sub(I,Q)
    )*- --Ayah: this requires a change of coordinates, and anyway it's always less efficient
    if opts.Algorithm == "DividedDifference" then (
        n := #w;
        x := local x;
        Q := QQ[x_1..x_n];
        polyByDividedDifference(w, Q, PolyType => "Grothendieck")        	
    )
    else if opts.Algorithm == "PipeDream" then (
        n = #w;
	x = local x;
	Q = QQ[x_1..x_n];
	pds := pipeDreamsNonReduced(w);
	wlen := permLength(w);
	sum apply(pds, D-> (wt:=weight(D,Q); (-1)^((degree(wt))_0-wlen)*wt  ))
    )
    else error("Invalid option for Algorithm.")
)


schubertPolyHelper = method(Options=>{Double=>false})
schubertPolyHelper(List, PolynomialRing) := opts -> (w, Q) -> (
    isDouble := opts.Double;
    n := #w;
    if not (isPerm w) then error ("The input must be a permutation in one-line notation.");
    if (isIdentity w) then return 1;
    r := lastDescent(w) - 1;
    s := -1;
    scan(reverse(r+1..n-1), i -> if w_i < w_r then (s = i; break));
    v := swap(w,r,s);
    previnds := select(0..r-1, q -> permLength(swap(v,q,r)) == permLength(v)+1);
    us := apply(previnds, i -> swap(v, i, r));
    if not isDouble then
        sum(toList(apply(us, u -> schubertPolyHelper(u, Q, Double=>isDouble)))) + Q_r * schubertPolyHelper(v, Q, Double=>isDouble)
    else 
        sum(toList(apply(us, u -> schubertPolyHelper(u, Q, Double=>isDouble)))) + (Q_r - Q_(n-1+v_r)) * schubertPolyHelper(v, Q, Double=>isDouble)
)

schubertPolynomial = method(Options=>{Algorithm=>"DividedDifference"})
schubertPolynomial(List) := opts-> (w) -> (
    
    n := #w;
    x := local x;
    Q := QQ[x_1..x_n];
    if opts.Algorithm == "Transition" then 
        schubertPolyHelper(w, Q, Double=>false)
    else if opts.Algorithm == "DividedDifference" then
        polyByDividedDifference(w,Q)
    else error("Invalid option for Algorithm.")
    
)

doubleSchubertPolynomial = method()
doubleSchubertPolynomial(List) := (w) -> (
    n := #w;
    x := local x;
    y := local y;
    Q := QQ[x_1..x_n,y_1..y_n];
    schubertPolyHelper(w, Q, Double=>true)
)



dividedDifference = method(Options=>{Operator=>null})
dividedDifference (RingElement, ZZ) := opts-> (f,i) -> (
    Q:= ring f;
    sf := sub(f, {Q_(i-1)=>Q_i, Q_i=>Q_(i-1)});
    if opts.Operator === null then 
        (f-sf) // (Q_(i-1)-Q_i)
    else if opts.Operator == "Grothendieck" then
        dividedDifference((1-Q_i)*f,i)
    else error("Invalid option for operator.")
    )


polyByDividedDifference = method(Options=>{PolyType=>"Schubert"})
polyByDividedDifference (List, PolynomialRing) := opts -> (w, Q) -> (
    n := #w;
    w0 := longestPerm(n);
    schubw0 := product(n, i->(Q_i)^(n-1-i));
    curpoly := schubw0;
    if (w0 == w) then schubw0
    else (
        v := composePerms(inverseOf(w), w0);
        redword := getOneReducedWord(v);
	if opts.PolyType == "Schubert" then
            polys := apply(reverse redword, i-> (curpoly=dividedDifference(curpoly,i); curpoly))
	else if opts.PolyType == "Grothendieck" then
	    polys = apply(reverse redword, i-> (curpoly=dividedDifference(curpoly,i, Operator=>"Grothendieck"); curpoly))
	else error ("Invalid option for this polynomial type.");
	polys#(#polys-1)
    )
    )


------------------------------------------------
------------------------------------------------
--Pipe Dreams
--(Implementation based on code of Allen Knutson)
--Sample Usage:  pipeDreams({1,4,3,2})
------------------------------------------------
------------------------------------------------

PipeDream = new SelfInitializingType of BasicList
PipeDream.synonym = "pipe dream"

-- for testing
PipeDream == List := (D,L) -> toList D == L

-- for printing
-- for printing
net       PipeDream := D -> netList(toList D, Boxes => false)
describe         PipeDream := D -> Describe (expression PipeDream) (expression toList D)
-- TODO: texMath PipeDream := ...
toString         PipeDream := toString @@ net
toExternalString PipeDream := toString @@ describe


permAfterTopRow = (w, ro) -> (
    neww := w;
    scan(reverse toList (0..#w-1),
	 i -> (if (ro#i == "+") then neww = swapValues(neww, i+1, i+2)));
    neww
    )

possibleTopRows = w->(n := #w; lw := permLength(w);
    rows := {apply(w#0-1, i->"+")}; -- a list of lists of "/", "+"
    rows = apply(rows, ro -> ro|{"/"});
    scan(n-w#0-1, i->(rows = flatten apply(rows, 
		ro->{ro|{"+"}, ro|{"/"} })));

    if (n-w#0-1 >= 0) then rows = apply(rows, ro->ro|{"/"});

    select(rows, ro->(
	    permLength(permAfterTopRow(w,ro)) == lw - #select(ro,r->(r=="+"))))
    )

pipeDreams = method()
pipeDreams (List) := List => (w) -> PipeDream \ (
    if (w=={1}) 
        then {{{"/"}}}
	else flatten apply(possibleTopRows(w), ro->(
	    wp := permAfterTopRow(w,ro);
	    wp = apply(#wp-1, i->(wp_(i+1)-1));
	    apply(pipeDreams(wp), D->
		prepend(ro, apply(D, Dro -> append(Dro, "/"))))
	    ))
        )

permsAfterTopRowNonReduced = (w, ro) -> (
    newws := {w};
    n := #w;
    scan(reverse toList (w#0..n-1),
	 i -> (if (ro#i == "+") then 
	           newws = flatten apply(newws, nw-> (
		       nwinv := inverseOf(nw);
		       if (nwinv#i > nwinv#(i+1)) then {swapValues(nw, i+1, i+2),nw}
		       else {nw})))
	);
    scan(reverse toList (0.. w#0-2),
	 i -> newws = apply(newws, nw-> swapValues(nw,i+1,i+2) ));
    newws
    )

pipeDreamsNonReduced = method()
pipeDreamsNonReduced (List) := List => w -> PipeDream \ (
    if (w=={1})
        then {{{"/"}}}
	else flatten apply(possibleTopRows(w), ro->(
		wps := permsAfterTopRowNonReduced(w, ro);
		flatten apply(wps, wp ->(
			wpinv := inverseOf(wp);
			newp := wp;
			wp = apply(#wp-1, i->(wp_(i+1)-1));
			scan(#ro-1, j-> 
			    if (ro#j == "+" and wpinv#(j)<wpinv#(j+1)) 
			    then (newp=swapValues(newp, j+1,j+2); wpinv=swap(wpinv, j,j+1) ));
			
			if (newp == w) then			
			      apply(pipeDreamsNonReduced(wp), D-> (
			            prepend(ro, apply(D, Dro -> append(Dro, "/")))))
			       else {}
			) ))) 
       )

weight = (D, R) -> (
    product apply(#D, i->(
	R_i^ (sum(apply(D#i, t-> if t=="+" then 1 else 0)))
	))
    
    )

ASMToMonotoneTriangle = method()
ASMToMonotoneTriangle (Matrix) := List => (M) -> (
    if not(isPartialASM M) then error("The input must be a partial alternating sign matrix or a permutation.");
    A := partialASMToASM M;    
    -- adding a zero row to the top of the ASM and then calculating the cumulative sums of the rows yields the monotone triangle
    indicatorVectors := accumulate(plus, toList(numColumns A:0), {toList(numColumns A:0)} | entries A);
    selectedSubsets := apply(indicatorVectors, indicatorVector -> positions(indicatorVector, i -> i != 0));
    apply(selectedSubsets, I -> if #I == 0 then I else I / (i -> i+1)) -- change to 1-index notation
)

monotoneTriangleToASM = method()
monotoneTriangleToASM (List) := Matrix => (M) -> (
    -- calculating pairwise differences of indicator vectors given from the monotone triangle yields the rows of the ASM
    reindexedM := apply(M, I -> if #I == 0 then I else I / (i -> i-1)); -- change to 0-index notation
    indicatorVectors := apply(reindexedM, I -> if #I == 0 then toList(#reindexedM-1:0) else apply(#reindexedM-1, i -> if member(i,I) then 1 else 0));
    matrix apply(indicatorVectors_{1..#indicatorVectors-1}, indicatorVectors_{0..#indicatorVectors-2}, difference)
)
