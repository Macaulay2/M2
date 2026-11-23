-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

FiniteGroupAction = new Type of GroupAction

finiteAction = method()

finiteAction (List, PolynomialRing) := FiniteGroupAction => (G, R) -> (
    -- check at least one generator is provided
    if G === {} then (
        error "finiteAction: Expected at least one generator."
        );
    -- check coefficient ring is a field
    if not isField coefficientRing R then (
	error "finiteAction: Expected the second argument to be a polynomial ring over a field."
	);
    -- check all generators are square matrices
    if any (G, g -> not instance(g, Matrix) or numRows g =!= numColumns g) then (
	error "finiteAction: Expected the first argument to be a list of square matrices."
	);
    -- check all generators have the right size to act on ring
    if any(G, g -> (numRows g) =!= numgens R) then (
        error "finiteAction: Expected the number of rows of each matrix to equal the number of variables in the polynomial ring."
        );
    -- check all generators are matrices defined over the coefficient ring
    try (
	gensG := apply(G, g -> sub(g, coefficientRing R))
	)
    else (
	error "finiteAction: Expected a list of matrices over the coefficient field of the polynomial ring."
	);
    new FiniteGroupAction from {
	cache => new CacheTable,
	(symbol ring) => R, 
	(symbol generators) => gensG,
	(symbol numgens) => #(gensG),
	}
    )

finiteAction (Matrix, PolynomialRing) := FiniteGroupAction => (g, R) -> finiteAction({g}, R)



--net of FiniteGroupAction object
net FiniteGroupAction := G -> (net G.ring)|" <- "|
    horizontalJoin( {"<"} | mingle(apply(G.generators,net),toList(G.numgens-1:", ")) | {">"})

--tex of FiniteGroupAction object
texMath FiniteGroupAction := G -> (texMath G.ring) |"\\curvearrowleft" |
    "\\left\\langle" |
    (concatenate mingle(apply(G.generators,texMath),toList(G.numgens-1:", "))) |
    "\\right\\rangle"

    
generators FiniteGroupAction := opts -> G -> G.generators

numgens FiniteGroupAction := ZZ => G -> G.numgens


-------------------------------------------

isAbelian = method(Options => true)

isAbelian FiniteGroupAction := { } >> opts -> (cacheValue (symbol isAbelian)) (
    G -> runHooks((isAbelian, FiniteGroupAction), G) )


addHook((isAbelian, FiniteGroupAction), G -> (
	X := G.generators;
    	n := #X;
    	if n == 1 then true 
    	else all(n - 1, i -> all(n - 1 - i, j -> (X#j)*(X#(n - 1 - i)) == (X#(n - 1 - i))*(X#j) ) )
	))
  
  

generateGroup = method(Options => true)

generateGroup FiniteGroupAction := {} >> opts -> (cacheValue (symbol generateGroup)) (G -> runHooks((generateGroup, FiniteGroupAction), G) )

addHook((generateGroup, FiniteGroupAction), G -> (
    m := numgens G;
    n := dim G;
    K := coefficientRing ring G;
    X := gens G;
    
    S := new MutableHashTable from apply(m, i -> 
	i => new MutableHashTable from {id_(K^n) => X#i}
	);
    
    A := new MutableHashTable from {id_(K^n) => {{}}}|apply(m, i -> X#i => {{i}});
    
    toUpdate := X;
    
    local h; local a;
    while #toUpdate > 0 do(
	h = first toUpdate;
	a = first A#h;
	
	scan(m, i -> (
		g := h*(X#i);
		a' := a|{i};
		S#i#h = g;
		if A#?g then (
		    A#g = (A#g)|{a'}
		    )
		else (
		    A#g = {a'};
		    toUpdate = toUpdate|{g}
		    )
		)
	    );
	
	toUpdate = drop(toUpdate, 1);
	);
    A = hashTable pairs A;
    S = hashTable apply(keys S, i -> i => hashTable pairs S#i);
    (S, A)
    )) 


-------------------------------------------

schreierGraph = method(Options => true)

schreierGraph FiniteGroupAction := {} >> opts -> (cacheValue (symbol schreierGraph)) (G -> runHooks((schreierGraph, FiniteGroupAction), G) )

addHook((schreierGraph, FiniteGroupAction),  G -> (generateGroup G)_0 )    
   

-------------------------------------------

group = method(Options => true)

group FiniteGroupAction := { } >> opts -> (cacheValue (symbol group)) (G -> runHooks((group, FiniteGroupAction), G) )

addHook((group, FiniteGroupAction), G -> keys first schreierGraph G )


-------------------------------------------

words = method(Options => true)

words FiniteGroupAction := { } >> opts -> (cacheValue (symbol words)) (G -> runHooks((words, FiniteGroupAction), G) )

addHook((words, FiniteGroupAction), G -> applyValues((generateGroup G)_1, val -> first val) )


-------------------------------------------

relations FiniteGroupAction := { } >> opts -> (cacheValue (symbol relations)) (
    G -> runHooks((relations, FiniteGroupAction), G) )

addHook((relations, FiniteGroupAction), G -> (
    relators := values last generateGroup G;
    W := apply(relators, r -> first r);
    relators = flatten apply(#W, i -> apply(drop(relators#i, 1), a -> {W#i,a} ) );
    relators = apply(relators, r -> (
	    w1 := first r;
	    w2 := last r;
	    j := 0;
	    while (j < #w1 and w1#j == w2#j) do j = j + 1;
	    {drop(w1, j), drop(w2, j)}
	    )
	);
    unique relators 
    )) 


-------------------------------------------
-- new permutationMatrix code
-------------------------------------------

permutationMatrix = method()

-- permutation from one-line notation
permutationMatrix Array := Matrix => p -> (
    n := max p;
    if #p =!= n or set (1..n) =!= set p then (
        error "permutationMatrix: Expected an array of positive integers
        representing a permutation in one-line notation."
        );
    -- shuffle columns of identity matrix
    (id_(ZZ^n))_(apply(toList p,i->i-1))
    )

-- convert single cycle to one-line notation
cycleToOneLine = (n,c) -> (
    new Array from for i from 1 to n list (
        pos := position(c, x -> x == i);
        if pos =!= null then (
            c_( (pos+1) % #c)
            )
        else i
        )
    )

-- multiply permutations in one-line notation
multiplyOneLine = L -> fold(L, (P,Q) -> P_(toList apply(Q, i -> i-1)))

permutationMatrix (ZZ,List) := Matrix => (n,L) -> (
    if n <= 0 then error "permutationMatrix: Expected a positive integer.";
    if #L == 0 or any(L, c -> not instance(c,Array)) then (
        error "permutationMatrix: Expected a nonempty list of arrays
        representing the cycles of a permutation."
        );
    if any(L, c -> #(set c) =!= #c or not isSubset(c, toList(1..n))) then (
        error ("permutationMatrix: Expected cycles to be arrays of distinct
            integers between 1 and " | toString(n) | "."
            );
        );
    -- convert cycles to one-line notation
    C := apply(L, c -> cycleToOneLine(n,c));
    -- multiply cycles in one-line notation
    p := multiplyOneLine C;
    -- return matrix from one-line notation
    (id_(ZZ^n))_(apply(toList p,i->i-1))
    )



-- Permutation action is finite group action whose generators are
-- permutations of the variables

PermutationAction = new Type of FiniteGroupAction

-- constructor for PermutationAction
permutationAction = method(Options => {
        CoefficientRing => QQ,
        Variable => "x",
        }
    )

-- general case use list of generating permutations and polynomial ring
-- does not call permutationMatrix to avoid duplicating checks, but uses auxiliary functions
permutationAction (List, PolynomialRing) := PermutationAction => opts -> (P,R) -> (
    -- check we have at least one generator
    if P === {} then (
        error "permutationAction: Expected at least one permutation."
        );
    if not isField coefficientRing R then ( --check if field
        error "permutationAction: Expected a polynomial ring over a field."
        );
    K := coefficientRing R;
    n := numgens R;
    if n < 1 then (
        error "permutationAction: Expected at least one variable."
        );
    -- check permutations are well-defined and convert to one-line notation
    P = apply(P, p -> (
            if instance(p,Array) then (
                if #p =!= n or set (1..n) =!= set p then (
                    error "permutationAction: Expected an array of positive integers
                    representing a permutation of the variables in one-line notation."
                    );
                p
                )
            else if instance(p,List) then (
                if #p == 0 or any(p, c -> not instance(c,Array)) then (
                    error "permutationAction: Expected a nonempty list of arrays
                    representing a permutation of the variables in cycle notation."
                    );
                if any(p, c-> #(set c) =!= #c or not isSubset(c,toList(1..n))) then (
                    error (
                        "permutationAction: Expected cycles to be arrays of distinct
                        integers between 1 and " | toString(n) | "."
                        )
                    );
                -- convert cycles to one-line notation
                C := apply(p, c -> cycleToOneLine(n,c));
                -- multiply cycles in one-line notation
                multiplyOneLine C
                )
            else (
                error "permutationAction: Expected permutations as one-line notation
                arrays or as lists of cycles."
                )
            )
        );
    new PermutationAction from {
        cache => new CacheTable,
        (symbol ring) => R,
        -- Turn one-line notations arrays into matrices over field of definition
        (symbol generators) => apply(P,
            p -> sub((id_(ZZ^n))_(apply(toList p,i->i-1)), K)),
        (symbol numgens) => #P,
        -- In Marcus Cassell's code one-line notation was lists, not arrays
        -- we convert to lists to use MC's code to compute invariants
        (symbol permutations) => apply(P,toList)
        }
    )

-- constructor overload with no ring, just number of variables
-- gives F[x_1..x_n], where F is passed as an option
-- note: F defaults to QQ
permutationAction (ZZ, List) := PermutationAction => opts -> (n,P) -> (
    --check if optional coefficient ring is a field
    if not isField opts.CoefficientRing then (
        error "permutationAction: Expected a field as coefficient ring."
        );
    F := opts.CoefficientRing;
    x := getSymbol opts.Variable;
    R := F(monoid[x_1..x_n]);
    permutationAction(P, R)
    )

--net of PermutationAction object showing permutations not matrices
-- turns one-line notation from lists to arrays for printing
net PermutationAction := A -> (net A.ring)|" <- "|
horizontalJoin( {"<"} | mingle(apply(apply(A.permutations, p -> new Array from p),
            net), toList(A.numgens-1:", ")) | {">"})

--tex of net of PermutationAction object
-- turns one-line notation from lists to arrays for printing
texMath PermutationAction := A -> (texMath A.ring) |"\\curvearrowleft" |
"\\left\\langle" |
(concatenate mingle(apply(apply(A.permutations, p -> new Array from p),
            texMath), toList(A.numgens-1:","))) |
"\\right\\rangle"

-- find all special exponents, namely sorted ascending with first entry 0 and no jumps larger than 1
-- FG: caching uses a lot of memory and does not speed this up much
specialExponents = n -> (
    L := {{0}}; -- list of exponent vectors
    for i from 2 to n do (
        -- either add 1 or repeat last entry, flatten removes the nesting
        L = flatten apply(L, v ->  {v | {last v}, v | {last v + 1}}
            )
        );
    L
    )

-- find the orbit of a vector v
-- we do this by applying the generators to v and all new vectors until we get everything
orbitExponents = (A, v) -> (
    -- get permutations
    P := A.permutations;
    -- "found set": hash table gives quick membership checks
    seen := new MutableHashTable from {v => true};
    -- vectors found but we still need to apply generators to
    toUpdate := {v};
    local h;
    while #toUpdate > 0 do (
        h = first toUpdate;
        toUpdate = drop(toUpdate, 1); -- drop h from toUpdate
        scan(P, p -> (
                w := apply(p, i -> h#(i-1)); -- apply the action
                if not seen#?w then (
                    seen#w = true;
                    toUpdate = toUpdate | {w}
                    )
                ));
        );
    keys seen -- return the vectors in the hashtable
    )

-- FG: the next two methods are not exported
-- they are also not used by invariants computations
-- the were likely used for debugging and can still be
-- exported by doing 'debug InvariantRing'

specialMonomials = method()
-- returns all special monomials in the ring
specialMonomials PermutationAction := List => A -> (
    R := ring A;
    flatten apply(specialExponents numgens R, v -> 
        if sum v == 0 then {} -- this would give 1 	
        else apply(unique permutations v, i -> R_i)
        )
    )

orbitSum = method()
-- return the orbit sum of a monomial
orbitSum (RingElement, PermutationAction) := RingElement => (r, A) -> (
    R := ring A;
    if not instance(r, R) then error "orbitSum: Expected an element of the ring being acted on.";
    if #(terms r) =!= 1 then error "orbitSum: Expected a monomial.";
    -- the orbit sum construction ignores coefficients
    sum(orbitExponents(A, flatten exponents r), i-> R_i)
    ) 
