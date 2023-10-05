--needsPackage "RationalPoints2"

generalEndomorphism = method()
generalEndomorphism Module := M -> (
    R := ring M;
    EndM := Hom(M, M, degree 1_R);
    --EndM := End(M);
    B := smartBasis(0, EndM);
    --r := rank source B;
    --if r == 1 then return "warning: End(M)_0 is 1-dimensional";
    --rm=length select(for i from 0 to r-1 list( unique apply(flatten entries homomorphism B_{i},j->j% ideal gens R) == {0}) ,k->k==false);
    --if rm< 2 then return "warning: End(M)_0/maximal ideal is 1-dimensional";
    homomorphism(B * random(ZZ^(rank source B),ZZ^1)))
generalEndomorphism CoherentSheaf := M -> generalEndomorphism module M

charMatrix = A -> (
    R := ring A;
    m := ideal gens R;
    k := coefficientRing R;
    local t;
    T := k[t];
    AT := sub(cover A, T);
    n := rank source AT;
    AT-t*id_(T^n)
    )

protect Tries
findIdem = method(Options => { Tries => 50 })
findIdem Module      := opts ->  M     -> findIdem(M, fieldExponent coefficientRing ring M)
findIdem(Module, ZZ) := opts -> (M, e) -> (
    L := coefficientRing ring M;
    p := char L;
    tries:=0;
    r:=numgens M;
    er:=max(for i from 1 to r do if p^i>r then break i , e);
    for i to opts.Tries - 1 do (
        tries =tries +1;
        f := generalEndomorphism M;
        eigen := flatten rationalPoints ideal det charMatrix(f);
        opers := flatten for i in eigen list if p == 0 then (f-i*id_M) else for j from 0 to e list (((f-i*id_M)^(p^er))^(p^(j+1)-1));
        idem := position(opers, g -> isIdempotent( g**(quotient ideal gens ring g)) and g != id_M and g != 0);
        if idem =!= null then (
            print ("it took "|tries|" tries");
	    break opers_idem)))
findIdem CoherentSheaf := opts -> M -> findIdem module M

testSplitting = (L, M0)->(
    B := smartBasis(0, module sheafHom(L, M0));
    b := rank source B;
    C := smartBasis(0, module sheafHom(M0, L));
    c := rank source C;
    isSplitting := (i,j) -> reduceScalar(homomorphism C_{j} * homomorphism B_{i}) == id_(module L);
    l := if (P := position'(0..b-1, 0..c-1, isSplitting)) === null then return else first P;
    sheaf coker homomorphism B_{l}
    )

fieldExponent = L -> (
    p := char L;
    if p == 0 then return 1 else (
	a := L_0;
	e := 1;
	while a^(p^e) != a do (e = e + 1);
	e)
    )
