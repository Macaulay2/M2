-- Direct summands and their labels, in the order of the underlying basis.
label(Thing,Module) := (k,M) -> directSum(k => M)

labels = method()
labels Module := M -> if isDirectSum M then indices M else {null}

isLabeled = method()
isLabeled Module := M -> M.cache.?indices

areLabelsUnique = method()
areLabelsUnique Module := M -> #set labels M == #labels M

labelsAndComponents = method()
labelsAndComponents Module := M -> apply(labels M,components M,(k,N) -> (k,N))

summands = method()
summands Module := M -> (
    if not areLabelsUnique M then error "expected distinct summand labels";
    hashTable apply(labelsAndComponents M,(k,N) -> k => N))

-- Build a fresh direct sum so cached inclusions and projections of the
-- input and any aliases of it continue to use the original labels.
unlabel = method()
unlabel Module := M -> if isLabeled M then Module.directSum toSequence components M else M

relabel = method()
relabel(Module,Function) := (M,f) -> (
    pairs := apply(labelsAndComponents M,(k,N) -> f k => N);
    if #set apply(pairs,first) != #pairs then error "expected distinct new labels";
    directSum pairs)

labelHomologically = method()
labelHomologically Complex := C -> (
    terms := for i from min C to max C list label(i,C_i);
    if length C == 0 then complex(first terms, Base => min C)
    else complex(for i from 1 to length C list
            map(terms_(i-1),terms_i,C.dd_(i+min C)), Base => min C))

labeledDirectSum = method()
labeledDirectSum (Module,Module) := (M,N) -> labeledDirectSum {M,N}
labeledDirectSum Sequence := L -> labeledDirectSum toList L
labeledDirectSum List := L -> (
    if #L == 0 then error "expected a nonempty list of modules or complexes";
    if all(L,M -> instance(M,Module)) then (
        if all(L,M -> not isLabeled M) then return directSum flatten apply(L,components);
        -- Preserve the label on a one-component module when it becomes
        -- a summand of the larger module.
        pairs := flatten apply(L,M ->
            if isLabeled M and #(labels M) == 1 then {(first labels M) => M}
            else apply(labelsAndComponents M,(k,N) -> k => N));
        if #set apply(pairs,first) != #pairs then error "expected distinct summand labels";
        return directSum pairs);
    if not all(L,C -> instance(C,Complex)) then error "expected modules or complexes";
    C := directSum L;
    terms := for i from min C to max C list (
        active := select(L,X -> min X <= i and i <= max X);
        if #active == 0 then (ring C)^0 else labeledDirectSum apply(active,X -> X_i));
    if length C == 0 then complex(first terms, Base => min C)
    else complex(for i from 1 to length C list
        map(terms_(i-1),terms_i,C.dd_(i+min C)),Base => min C))

labeledTensor = method()
labeledTensor (Ring,List,ZZ,ZZ) := (R,L,lo,hi) -> (
    if #L == 0 or not all(L,C -> instance(C,Complex)) then error "expected a nonempty list of complexes";
    if lo > hi then error "expected lower bound <= upper bound";
    S := ring first L;
    if not all(L,C -> ring C === S) then error "expected complexes over the same ring";
    p := #L;
    lows := apply(L,min);
    highs := apply(L,max);
    bottom := sum lows;
    top := sum highs;
    -- Subtract each lower bound before enumerating weak compositions. This
    -- applies equally to negative shifts and to truncation outside the support.
    terms := hashTable for i from lo to hi list i => (
        words := if i < bottom or i > top then {} else
            select(apply(compositions(p,i-bottom),c -> c+lows),
                c -> all(p,j -> c_j <= highs_j));
        if #words == 0 then R^0 else directSum for w in words list
            w => tensor(R,tensor apply(p,j -> (L_j)_(w_j))));
    if lo == hi then return complex(terms#lo,Base => lo);
    maps := for i from lo+1 to hi list (
        T := terms#(i-1);
        U := terms#i;
        if T == 0 or U == 0 then map(T,U,0) else
        map(T,U,matrix table(indices T,indices U,(v,w) -> (
            delta := w-v;
            j := position(delta,a -> a == 1);
            if j === null or any(delta,a -> a < 0) then
                return map(component(T,v),component(U,w),0);
            factors := apply(p,k -> if k == j then (L_k).dd_(w_k)
                else id_((L_k)_(w_k)));
            map(component(T,v),component(U,w),(-1)^(sum take(w,j)) * tensor(R,tensor factors))))));
    result := complex(maps,Base => lo);
    result.cache.tensor = L;
    result)

labeledTensor (List,ZZ,ZZ) := (L,lo,hi) -> (
    if #L == 0 or not all(L,C -> instance(C,Complex)) then error "expected a nonempty list of complexes";
    labeledTensor(ring first L,L,lo,hi))
labeledTensor Complex := X -> labeledTensor({X},min X,max X)
labeledTensor List := L -> (
    if #L == 0 or not all(L,C -> instance(C,Complex)) then error "expected a nonempty list of complexes";
    labeledTensor(L,sum apply(L,min),sum apply(L,max)))
labeledTensor (Ring,Module) := (R,M) -> (
    if not isDirectSum M then return tensor(R,M);
    if not isLabeled M then return directSum apply(components M,N -> tensor(R,N));
    directSum apply(labelsAndComponents M,(k,N) -> k => tensor(R,N)))
labeledTensor (Ring,Complex) := (R,C) -> (
    terms := for i from min C to max C list labeledTensor(R,C_i);
    if length C == 0 then complex(first terms,Base => min C)
    else complex(for i from 1 to length C list
        map(terms_(i-1),terms_i,tensor(R,C.dd_(i+min C))),Base => min C))
