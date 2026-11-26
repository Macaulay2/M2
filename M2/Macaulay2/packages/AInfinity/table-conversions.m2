-- Table constructors are deliberately strict about missing operations. The
-- global computation records zero maps, whereas a complete sparse typed
-- structure treats omitted entries as zero.
tableProducts = h -> hashTable select(pairs h,(key,value) -> class key === List)

typedTableAlgebra = (h,opts) -> (
    if not h#?"SResolution" then
        error "expected the unshifted ambient resolution from aInfinity";
    C := h#"resolution";
    S := ring C;
    d := map(S^1,C_2,(h#"SResolution").dd_1);
    B := sAInfAlg(S,gradedModule C,tableProducts h,d,opts);
    B.cache#"AInfinityPresentedRing" = h#"ring";
    B
)

sAInfAlg HashTable := SAInfAlg => o -> h -> (
    checkAInfinityAlgebraForDual(h,null);
    typedTableAlgebra(h,o)
)

sAInfMod(HashTable,HashTable) := SAInfMod => o -> (p,h) -> (
    checkAInfinityModuleForDual(p,h,null);
    B := typedTableAlgebra(p,options sAInfAlg);
    if h#?"algebra" and h#"algebra" =!= p and sAInfAlg(h#"algebra") != B then
        error "the dual module requires its own opposite algebra";
    C := p#"resolution";
    D := h#"resolution";
    for j from min D to max D do if D_j != 0 then (
        for word in bWords(max C,max D+1-j) do (
            key := append(word,j);
            if D_(sum key-1) != 0 and all(word,i -> C_i != 0) and not h#?key then
                error("incomplete A-infinity module table, missing operation ",toString key);
        ));
    G := sAInfMod(B,gradedModule D,tableProducts h);
    if not checkStasheff G then error "the supplied module operations do not satisfy the module identities over this algebra";
    if o.TensorOrder === null or o.TensorOrder === infinity then G else sAInfMod(G,o)
)
