-- The explicitly selected point is a rational closed point of Spec R.
-- Legacy local resolutions read a point stored on their ambient ring. Compute
-- them over a private copy with private variable names, so neither the
-- caller's ring nor its generator bindings can change (including on errors).
burkeResolution(Module,ZZ,Ideal) := Complex => o -> (M,n,p) -> (
    if n < 0 then error "resolution length must be nonnegative";
    if not instance(o.Check,Boolean) then error "Check must be a Boolean";
    R := ring M;
    S := ring presentation R;
    P := rationalModelPoint(R,p);
    point := if R === S then P else promote(P,R);
    -- A module with zero stalk has the zero free resolution at this point,
    -- including when its homogeneous presentation has global support elsewhere.
    if not isSubset(ann M,point) then (
        zeroTerm := R^0;
        F := if n == 0 then complex zeroTerm else complex toList(n:map(zeroTerm,zeroTerm,0));
        F.cache#"AInfinityModelPoint" = point;
        return F);
    z := symbol z;
    T := newRing(S,Variables=>apply(numgens S,i -> z_i));
    move := map(T,S,gens T);
    U := if R === S then T else T/ideal(move presentation R);
    forward := map(U,R,gens U);
    back := map(R,U,gens R);
    setMaxIdeal(move P);
    if U =!= T then setMaxIdeal(forward point);
    N := forward ** M;
    -- A unit entry in the presentation at this point can remove a generator
    -- even when the global module or its defining equations are homogeneous.
    if U.residueMap(presentation N) != 0 then N = localPrune N;
    atOrigin := P == ideal vars S;
    gradedRing := atOrigin and isHomogeneous U;
    gradedModuleInput := gradedRing and isHomogeneous N;
    Gcx := first ambientModelResolution(N,T,gradedModuleInput,n,() -> restrictModelModule(T,N));
    if n == 0 then (
        V := back(U ** Gcx_0);
        F = complex(if V == 0 then V else directSum({0} => V));
        F.cache#"AInfinityModelPoint" = point;
        return F);
    Acx := first ambientModelResolution(U,T,gradedRing,infinity,() -> coker presentation U);
    B := algebraModelOnResolution(Acx,new OptionTable from {TensorOrder=>infinity,LengthLimit=>infinity,Check=>true});
    B.cache#"AInfinityModelPoint" = move P;
    G := moduleModelOnResolution(B,Gcx,new OptionTable from {TensorOrder=>infinity,LengthLimit=>n,Check=>true});
    C := complex apply(n,i -> burkeDifferential(U,B,G,i+1));
    -- Reapply labels in the original order. Ring maps preserve the internal
    -- shifts of free modules, but do not preserve their direct-sum metadata.
    terms := for i from min C to max C list (
        V := C_i;
        if V == 0 then R^0 else directSum apply(indices V,components V,(k,N) -> k => back N));
    F = if length C == 0 then complex(first terms,Base=>min C)
        else complex(for i from 1 to length C list
            map(terms_(i-1),terms_i,back(C.dd_(i+min C))),Base=>min C);
    F.cache#"AInfinityModelPoint" = point;
    if o.Check then checkBurkeExactness(F,point);
    F)
