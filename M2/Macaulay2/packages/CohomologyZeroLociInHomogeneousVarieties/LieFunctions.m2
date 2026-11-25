
-------------------- conversion to LieAlgebraRepresentations --------------------

-- The representations computed in this file are those of the semisimple part
-- of a Levi subgroup. Its RootSystem (from WeylGroups) keeps the nodes of the
-- ambient Dynkin diagram, in increasing order, while LieAlgebraRepresentations
-- numbers the nodes of each simple type in its own standard way, and
-- dynkinType sorts the components and renames the small types (D3 -> A3,
-- C2 -> B2, ...). So the LieAlgebra is identified directly from the Cartan
-- matrix, together with a node permutation: node i of R is node perm#i of the
-- LieAlgebra. When several permutations exist they differ by a diagram
-- automorphism, and any of them gives the same results once converted back.

-- simple types of rank k, as named in LieAlgebraRepresentations
-- (C2 is found as B2 and D3 as A3)
simpleTypesOfRank = k -> select({("A",k),("B",k),("C",k),("D",k),("E",k),("F",k),("G",k)}, t ->
    (t#0 == "A" and k >= 1) or (t#0 == "B" and k >= 2) or (t#0 == "C" and k >= 3)
    or (t#0 == "D" and k >= 4) or (t#0 == "E" and k >= 6 and k <= 8)
    or (t#0 == "F" and k == 4) or (t#0 == "G" and k == 2))

-- a permutation p with A#i#j == B#(p#i)#(p#j) for all i, j, or null if there
-- is none. A and B are the Cartan matrices (lists of rows) of two connected
-- Dynkin diagrams: these are trees, so the nodes of A are matched one at a
-- time along its edges, and each node has only a few possible images.
cartanIsomorphism = (A,B) -> (
    n := #A;
    if #B != n then return null;
    -- order the nodes of A so that each one after the first is adjacent to an
    -- earlier one, its parent
    order := {0};
    parent := new MutableList from n:null;
    seen := new MutableList from n:false;
    seen#0 = true;
    k := 0;
    while k < #order do (
        u := order#k;
        for v from 0 to n-1 do if not seen#v and A#u#v != 0 then (
            seen#v = true;
            parent#v = u;
            order = append(order, v);
            );
        k = k + 1;
        );
    p := new MutableList from n:null;
    used := new MutableList from n:false;
    -- match order#i, ..., order#(n-1), given the images of the earlier nodes
    matchFrom := i -> (
        if i == n then return true;
        v := order#i;
        candidates := if i == 0 then toList(0..n-1)
            else select(toList(0..n-1), j -> B#(p#(parent#v))#j != 0);
        for j in candidates do (
            if not used#j and all(take(order, i), u -> A#v#u == B#j#(p#u) and A#u#v == B#(p#u)#j) then (
                p#v = j;
                used#j = true;
                if matchFrom(i+1) then return true;
                used#j = false;
                );
            );
        false
        );
    if matchFrom 0 then toList p else null
    )

-- LieAlgebra, node permutation and cache of irreducible modules of a root
-- system. Keyed by rootSystemKey (see FirstPart.m2) rather than stored on R,
-- since equal Levi root systems are rebuilt as new objects per bundle.
-- For rank 0 (complete flags) there is no LieAlgebra: callers handle that
-- case before getting here.
rootSystemLARCache = new MutableHashTable;

rootSystemLARData = method();
rootSystemLARData RootSystem := R -> (
    key := rootSystemKey R;
    if not rootSystemLARCache#?key then (
        C := entries WeylGroups$cartanMatrix R;
        n := #C;
        -- connected components of the Dynkin diagram, as increasing lists of nodes
        comps := {};
        seen := new MutableList from n:false;
        for i from 0 to n-1 do if not seen#i then (
            comp := {i};
            seen#i = true;
            k := 0;
            while k < #comp do (
                u := comp#k;
                for v from 0 to n-1 do if not seen#v and C#u#v != 0 then (
                    seen#v = true;
                    comp = append(comp, v);
                    );
                k = k + 1;
                );
            comps = append(comps, sort comp);
            );
        perm := new MutableList from n:null;
        algebras := {};
        offset := 0;
        for comp in comps do (
            A := apply(comp, i -> apply(comp, j -> C#i#j));
            found := null;
            for t in simpleTypesOfRank(#comp) do (
                p := cartanIsomorphism(A, entries LieAlgebraRepresentations$cartanMatrix t);
                if p =!= null then (found = (t, p); break);
                );
            if found === null then error "rootSystemLARData: unrecognized Dynkin diagram";
            scan(#comp, i -> perm#(comp#i) = offset + (found#1)#i);
            algebras = append(algebras, simpleLieAlgebra found#0);
            offset = offset + #comp;
            );
        rootSystemLARCache#key = new HashTable from {
            "lieAlgebra" => if n == 0 then null else fold(algebras, (g,h) -> g ++ h),
            "permutation" => toList perm,
            "modules" => new MutableHashTable
            };
        );
    rootSystemLARCache#key
    )

-- weight of R -> Dynkin labels for the LieAlgebra of R
toLARWeight = (l,R) -> (
    perm := (rootSystemLARData R)#"permutation";
    v := new MutableList from #perm:0;
    scan(#perm, i -> v#(perm#i) = (entries l)#i);
    toList v
    )

-- Dynkin labels for the LieAlgebra of R -> weight of R
fromLARWeight = (v,R) -> weight(R, apply((rootSystemLARData R)#"permutation", j -> v#j))

-- irreducible module of highest weight l, built once per weight, so that the
-- powers LieAlgebraRepresentations caches in it are reused by later calls
larModule = (l,R) -> (
    data := rootSystemLARData R;
    v := toLARWeight(l,R);
    data#"modules"#v ??= irreducibleLieAlgebraModule(data#"lieAlgebra", v)
    )

-- decomposition of a LieAlgebraModule, as a Tally Weight => multiplicity
fromLARModule = (M,R) -> new Tally from apply(pairs M#"DecompositionIntoIrreducibles",
    (v,m) -> (fromLARWeight(v,R), m))


--------------- --------- tensor product -------------------------------------

-- tensor product of two irreducible representations
tensorProduct = method();
tensorProduct (Weight,Weight,RootSystem) := (l1,l2,R) -> (
    new HashTable from fromLARModule(larModule(l1,R) ** larModule(l2,R), R)
    )


-- tensor product of two completely reducible representations
tensorProduct (HashTable,HashTable,RootSystem) := (L1,L2,R) -> (
    T := new MutableHashTable;
    for l1 in keys L1 do (
        for l2 in keys L2 do (
            Ttemp := tensorProduct(l1,l2,R);
            for l in keys Ttemp do (
                if T#?l then T#l = T#l + Ttemp#l * L1#l1 * L2#l2
                else T#l = Ttemp#l * L1#l1 * L2#l2;
                );
            );
        );
    return T;
    )



----------------------- symmetric power ---------------------------------------------------------------------------

-- nth symmetric power of an irreducible representation
symmetricPower (ZZ, Weight,RootSystem) := (n,l,R) -> (
    if n < 0 then error "the power must be a non-negative integer"
    else if n == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if n == 1 then return new HashTable from {l => 1}
    else new HashTable from fromLARModule(symmetricPower(n, larModule(l,R)), R)
    )

-- returns the list of symmetric powers of an irreducible representation from 0 to k;
-- LieAlgebraRepresentations keeps the powers already computed in the module,
-- which larModule builds only once per weight
multipleSymmetricPowerLie = method();
multipleSymmetricPowerLie(ZZ, Weight, RootSystem) := (k, l, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return new HashTable from {l => 1}
    else apply(k+1, i -> symmetricPower(i,l,R))
    )

-- given a hash table whose keys are the weights and whose values are the multiplicities, i.e. a completely reducible representation,
-- returns a hash table whose keys are the same weights and whose values are the symmetric powers of these weights
-- the multiplicities are therefore useless in the current version
-- the idea is to do kunneth directly on the vector bundles
multipleSymmetricPowerLie (ZZ, HashTable, RootSystem) := (k, L, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return L
    else new HashTable from apply(keys L, l -> (l, multipleSymmetricPowerLie(k,l,R)))
    )


---------------------------- exterior power ----------------------------------------------------------------------

-- nth exterior power of an irreducible representation
exteriorPower (ZZ, Weight, RootSystem) := o -> (n,l,R) -> (
    d := weylFormula(l,R);
    if n < 0 then error "the power must be a non-negative integer"
    else if n == 0 or n == d then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if n == 1 then return new HashTable from {l => 1}
    else if n > d then return new HashTable from {}
    else new HashTable from fromLARModule(exteriorPower(n, larModule(l,R)), R)
    )


-- returns the list of exterior powers of an irreducible representation from 0 to k
-- (the ones above the dimension are empty); as for symmetric powers, the
-- powers already computed are kept in the module
multipleExteriorPowerLie = method();
multipleExteriorPowerLie(ZZ, Weight, RootSystem) := (k, l, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return new HashTable from {l => 1}
    else apply(k+1, i -> exteriorPower(i,l,R))
    )

-- given a hash table whose keys are the weights and whose values are the multiplicities, i.e. a completely reducible representation
-- returns a hash table whose keys are the same weights and whose values are the exterior powers of these weights
-- the multiplicities are therefore useless in the current version
-- the idea is to do kunneth directly on the vector bundles
multipleExteriorPowerLie (ZZ, HashTable, RootSystem) := (k, L, R) -> (
    if k < 0 then error "the power must be a non-negative integer"
    else if k == 0 then return new HashTable from {weight(R,toList(rank(R):0)) => 1}
    else if k == 1 then return L
    else new HashTable from apply(keys L, l -> (l, multipleExteriorPowerLie(k,l,R)))
    )
