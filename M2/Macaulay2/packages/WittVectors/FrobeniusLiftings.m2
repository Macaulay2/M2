findFrobeniusLift = method(Options=>{Nontrivial => false, Homogeneous => false, Verbose => false, PerturbationTerm => null})

findFrobeniusLift(ZZ, RingElement) := opts -> (d, f) -> findFrobeniusLift(d, ideal f, opts)
findFrobeniusLift(ZZ, Ring) := opts -> (d, R) -> findFrobeniusLift(d, ideal R, opts)
findFrobeniusLift(ZZ, Ideal) := opts -> (d, I) -> (
    S := ring I;
    R := S/I;
    n := numgens S;
    J := findFrobeniusLiftConstraints(I, PerturbationTerm => opts.PerturbationTerm, Homogeneous => opts.Homogeneous);
    T := ring J;
    j := 0;

    if not opts.Nontrivial then L := toList((n):0)
        else L = for i from 0 to n-1 list if opts.Homogeneous == false then sum for i from 0 to d list random(i, S) else random(d, S);

    while (evalMap(L, I, T))(J) != 0 or (opts.Nontrivial and unique apply(L, i -> sub(i, R)) == {0}) do (
        if opts.Verbose then print j;
        j = j + 1 ;
        L = for i from 0 to n-1 list
            if opts.Homogeneous == false then sum for i from 0 to d list random(i, S)
            else random(d, S));
    apply(L, i -> sub(i, R))
)


findFrobeniusLiftConstraints = method(Options => {PerturbationTerm => null, Homogeneous => false})

findFrobeniusLiftConstraints(RingElement) := opts -> f -> findFrobeniusLiftConstraints(ideal f, opts)

findFrobeniusLiftConstraints(Ring) := opts -> R -> findFrobeniusLiftConstraints(ideal 0_R, opts)

findFrobeniusLiftConstraints(Ideal) := opts -> I -> (
    c := numgens I;
    fp := toList(c:0);
    if opts.PerturbationTerm =!= null  then(
        fp = opts.PerturbationTerm;
        if length fp != numgens I then error "expected perturbation to have same number of generators of ideal");
    S := ring I;
    fp = apply(fp, i -> sub(i, S));
    p := char S;
    R := S/I;
    d := numgens S;
    aa := symbol aa;
    T := prune( S[aa_0..aa_(d-1)] );
    TR := T/sub(I, T);
    if I == 0 then return ideal 0_TR;
    trim sum for r from 0 to numgens I-1 list (
        f := I_r;
        g := fp_r;
        Cf := apply(flatten entries last coefficients f, i -> sub(i, ZZ));
        Ef := flatten( exponents \ flatten entries first coefficients f);
        WCf := apply(Ef, i -> product for j from 0 to d-1 list (witt{T_(d+j), T_j})^(i_j));
        Cfp := apply(flatten entries last coefficients g, i -> sub(i, ZZ));
        Efp := flatten( exponents \ flatten entries first coefficients g);
        WCfp := apply(Efp, i -> product for j from 0 to d-1 list (witt{T_(d+j), T_j})^(i_j));
        if WCfp == {} then WCfp = {witt{0_T, 0_T}};
        if Cfp == {} then Cfp = {0};
        sub(ideal last (p * Cfp_0 * WCfp_0 + sum flatten (for i from 0 to length Cf - 1 list Cf_i * WCf_i)).tuple, TR)
    )
)



expandFrobeniusConstraints = method(Options=>{Homogeneous => false, PerturbationTerm => null})

expandFrobeniusConstraints(ZZ, RingElement) := opts -> (d, f) -> expandFrobeniusConstraints(d, ideal f, opts)

expandFrobeniusConstraints(ZZ, Ideal) := opts -> (d, J) -> (
    S := ring J;
    I := findFrobeniusLiftConstraints(J, PerturbationTerm => opts.PerturbationTerm);
    A := ring I;
    n := dim ring J;
    c := symbol c;
    monomials := if opts.Homogeneous then
        apply(select(latticePoints hypercube(n, 0, d), i -> sum entries i == {d}), j -> entries j)
    else
        apply(select(latticePoints hypercube(n, 0, d), i -> sum entries i <= {d}), j -> entries j);
    B := S[ flatten for i in monomials list for j from 1 to n list c_(flatten i, {j}) ];
    mapList := for j from 1 to n list sum apply(monomials, i -> S_(flatten i) * c_(flatten i, {j}) );
    expand := map(B/sub(J, B), A, mapList|gens S);
    expand I
)

createEquations = method(Options => {Homogeneous => false, PerturbationTerm => null})
createEquations(ZZ, Ring) := opts -> (d, R) -> createEquations(d, ideal R, opts)
createEquations(ZZ, RingElement) := opts -> (d, f) -> createEquations(d, ideal f, opts)
createEquations(ZZ, Ideal) := opts -> (d, I) -> (
    G := expandFrobeniusConstraints(d, I, opts);
    n := dim ring I;
    S := ring I;
    p := char S;
    T := ring G;
    cc := symbol cc;
    monomials := if opts.Homogeneous then
        apply(select( latticePoints hypercube(n, 0, d), i -> sum entries i == {d}), j -> entries j)
    else
        apply(select( latticePoints hypercube(n, 0, d), i -> sum entries i <= {d}), j -> entries j) ;
    Bcc := T[ flatten flatten for k1 in 0..numgens G - 1 list for k2 in 0..numgens I - 1 list for i in monomials list cc_(flatten i,{k1, k2}) ];
    genericElement :=  matrix for k1 in 0..numgens G - 1 list for k2 in 0..numgens I - 1 list sum apply( monomials, j -> S_(flatten j) * cc_(flatten j, {k1, k2}) );
    constraint := ideal( (genericElement) * transpose gens I - transpose sub(gens G, Bcc) );
    C := ((ZZ/p)[gens T|gens Bcc])[gens S];
    Cflatmap := inverse last flattenRing(C);
    Cflat := source Cflatmap;
    J := sub(ideal for j in constraint_* list (inverse Cflatmap)( ideal last coefficients Cflatmap sub(j, Cflat)), Cflat);
    elimList := (for i in gens Bcc list sub(i, Cflat)) | (for i in gens S list sub(i, Cflat));
    sub(eliminate(elimList, J), (ZZ/p)[gens T])
)


evalMap = method()
evalMap(List, Ideal, Ring):= (L, I, T) -> (
    S := ring I;
    if length L != numgens S then return "error: needed a list with as many entries as the variables of S";
    map(S/I, T, L|gens S)
)


