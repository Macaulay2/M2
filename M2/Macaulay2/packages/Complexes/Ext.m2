-- total ext over complete intersections

-- Code originally written by Dan Grayson
-- change 2009/1/12:
--  some modifications contributed 23 Mar 2007 by "Paul S. Aspinwall" <psa@cgtp.duke.edu>,
--  but we also get rid of the fudge factor entirely, depending instead on automatic
--  computation of the heft vector

-- TODO: Ext(R, S) should work as well
Ext(Ring, Ring)   :=
Ext(Ring, Ideal)  :=
Ext(Ring, Module) :=
Ext(Ideal, Ring)   :=
Ext(Ideal, Ideal)  :=
Ext(Ideal, Module) :=
Ext(Module, Ring)   :=
Ext(Module, Ideal)  := Module => opts -> (M, N) -> Ext(module M, module N, opts)
Ext(Module, Module) := Module => opts -> (M, N) -> (
    -- c.f. caching in Hom(Module,Module)
    cacheModule := youngest(M.cache.cache, N.cache.cache);
    cacheKey := (Ext,M,N);
    if cacheModule#?cacheKey then return cacheModule#cacheKey;
    B := ring M;
    if B =!= ring N
        then error "expected modules over the same ring";
    if not isCommutative B
        then error "'Ext' not implemented yet for noncommutative rings.";
    if not isHomogeneous B
        then error "'Ext' received modules over an inhomogeneous ring";
    if not isHomogeneous N or not isHomogeneous M
        then error "'Ext' received an inhomogeneous module";
    if N == 0 or M == 0 then return cacheModule#cacheKey = B^0;
    p := presentation B;
    A := ring p;
    I := ideal mingens ideal p;
    n := numgens A;
    c := numgens I;
    if c =!= codim B 
        then error "total Ext available only for complete intersections";
    f := I_*; -- apply(c, i -> I_i);
    pM := lift(presentation M,A);
    pN := lift(presentation N,A);
    M' := cokernel ( pM | p ** id_(target pM) );
    N' := cokernel ( pN | p ** id_(target pN) );
    assert isHomogeneous M';
    assert isHomogeneous N';
    C := freeResolution M';
    X := getSymbol "X";
    K := coefficientRing A;
    S := K(monoid [X_1 .. X_c, toSequence A.generatorSymbols,
        Degrees => {
            apply(0 .. c-1, i -> prepend(-2, - degree f_i)),
            apply(0 .. n-1, j -> prepend( 0,   degree A_j))
            }]);
    -- make a monoid whose monomials can be used as indices
    Rmon := monoid [X_1 .. X_c,Degrees=>{c:{2}}];
    -- make group ring, so 'basis' can enumerate the monomials
    R := K Rmon;
    -- make a hash table to store the blocks of the matrix
    blks := new MutableHashTable;
    blks#(exponents 1_Rmon) = C.dd;
    scan(0 .. c-1, i -> 
       blks#(exponents Rmon_i) = nullHomotopy(- f_i*id_C, FreeToExact => true));
    -- a helper function to list the factorizations of a monomial
    factorizations := (gamma) -> (
        -- Input: gamma is the list of exponents for a monomial
        -- Return a list of pairs of lists of exponents showing the
        -- possible factorizations of gamma.
        if gamma === {} then { ({}, {}) }
        else (
            i := gamma#-1;
            splice apply(factorizations drop(gamma,-1), 
                (alpha,beta) -> apply (0..i, 
                    j -> (append(alpha,j), append(beta,i-j))))));
    scan(4 .. length C + 1, d -> if even d then (
        scan( flatten \ exponents \ leadMonomial \ first entries basis(d,R), 
            gamma -> (
                s := - sum(factorizations gamma,
                    (alpha,beta) -> (
                        if blks#?alpha and blks#?beta
                        then blks#alpha * blks#beta
                        else 0));
                -- compute and save the nonzero nullhomotopies
                if s != 0 then blks#gamma = nullHomotopy(s, FreeToExact => true)))));
    -- make a free module whose basis elements have the right degrees
    (loC, hiC) := concentration C;
    Cstar := S^(apply(toList(loC..hiC),
                 i -> toSequence apply(degrees C_i, d -> prepend(i,d))));
    -- assemble the matrix from its blocks.
    -- We omit the sign (-1)^(n+1) which would ordinarily be used,
    -- which does not affect the homology.
    toS := map(S,A,apply(toList(c .. c+n-1), i -> S_i),
               DegreeMap => prepend_0);
    Delta := map(Cstar, Cstar, 
        transpose sum(keys blks, m -> S_m * toS sum blks#m),
        Degree => { -1, degreeLength A:0 });
    DeltaBar := Delta ** (toS ** N');
    if debugLevel > 10 then (
        assert isHomogeneous DeltaBar;
        assert(DeltaBar * DeltaBar == 0);
        stderr << describe ring DeltaBar <<endl;
        stderr << toExternalString DeltaBar << endl;
        );
    -- now compute the total Ext as a single homology module
    prune' := if opts.MinimalGenerators then prune else identity;
    cacheModule#cacheKey = prune' homology(DeltaBar,DeltaBar)
    )

