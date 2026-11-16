
------------------------------------------------------------------------
----------------------------- Congruences ------------------------------
------------------------------------------------------------------------

CongruenceOfCurves = new Type of HashTable;

globalAssignment CongruenceOfCurves;

CongruenceOfCurves.synonym = "congruence of curves";

detectCongruence = method(TypicalValue => CongruenceOfCurves, Options => {Verbose => false});

detectCongruence (CubicFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruence (GushelMukaiFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruence (HodgeSpecialFourfold,ZZ) := o -> (X,e) -> congruenceOfCurves(X,e);

detectCongruenceInt = method(Options => {Verbose => false});
detectCongruenceInt (EmbeddedProjectiveVariety,HodgeSpecialFourfold) := o -> (p,X) -> (
    a := degreeHypersurface X;
    if not(isPoint p and ring ambient p === ring ambient X and isSubset(p,ambientFivefold X)) then error "expected a point in the ambient fivefold";
    phi := map X;
    imageOfAssociatedMap X; -- image of phi
    S := surface X;
    secants := new MutableList from {null,0,0,0,0,0,0};
    phip := phi p;
    E := coneOfLines(Var image phi,phip);
    if dim E == 0 then (
        if o.Verbose then <<"no ("<<a<<"e"<<-1<<")-secant curves have been found"<<endl;
        error "no congruences detected";
    );
    if dim E != 1 then error "expected cone of lines to be one dimensional";
    if o.Verbose then <<"number lines contained in the image of the "<<(if a == 2 then "quadratic" else (if a == 3 then "cubic" else ""))<<" map and passing through a general point: "<<degree E<<endl;
    if degree E == 1 then return detectCongruence(X,degree phi^* E);
    pr := rationalMap phip;
    v := (rationalMap {random(1,ring target pr),random(1,ring target pr)});
    pr' := toRationalMap((pr * v)|E);
    E' := Var kernel(map pr',SubringLimit=>1);
    if not(dim E' == 0 and degree E' == degree E) then error "something went wrong";
    local n; local C;
    P := apply(decompose E',q -> (assert(dim q == 0); C = phi^* pr'^* q; assert(dim C == 1 and dim(C*S) == 0); (degree q,C)));
    for nC in P do (
        (n,C) = nC;
        for e from 1 to 6 do
            if degree(C) == n*e and degree(C*S) == n*(a*e-1) and all(delete(e,toList(1..6)),e' -> not(degree(C) == n*e' and degree(C*S) == n*(a*e'-1)))
            then secants#e = secants#e + n;
    );
    secants = toList take(secants,-(#secants-1));
    if sum secants =!= degree E then <<"--warning: something went wrong: "<<sum secants<<" =!= "<<degree E<<endl;
    nC := apply({"lines","conics","cubics","quartics","quintics","sextics"},apply(toList(0..5),e -> a*(e+1)-1),(s,t) -> "number "|toString(t)|"-secant "|s);
    if o.Verbose then for e to #secants-1 do if secants_e != 0 then <<nC_e<<" = "<<secants_e<<endl;
    ee := null;
    ee = for e to #secants-1 do if secants_e == 1 then break (e+1);
    if ee === null then error "no congruences detected";
    return congruenceOfCurves(X,ee);
);
detectCongruence CubicFourfold := o -> X -> detectCongruenceInt(point ambient X,X,Verbose=>o.Verbose);
detectCongruence GushelMukaiFourfold := o -> X ->  detectCongruenceInt(pointOnLinearSectionOfG14 ambientFivefold X,X,Verbose=>o.Verbose);
detectCongruence HodgeSpecialFourfold := o -> X -> detectCongruenceInt(point ambientFivefold X,X,Verbose=>o.Verbose);

congruenceOfCurves = method();

congruenceOfCurves (HodgeSpecialFourfold,ZZ) := (X,e) -> (
    a := degreeHypersurface X;
    Y := ambientFivefold X;
    phi := map X;
    imageOfAssociatedMap X; -- image of phi
    S := surface X;
    f := method();
    f EmbeddedProjectiveVariety := p -> (
        if not isPoint p then error "expected a point";
        if not(ring ambient p === ring ambient Y and isSubset(p,Y)) then error "expected a point on the ambient fivefold containing the surface";
        phip := phi p;
        E := coneOfLines(Var image phi,phip);
        if dim E == 0 then error "no congruences detected";
        if dim E != 1 then error "expected cone of lines to be one dimensional";
        if degree E == 1 then (
            D := phi^* E;
            if not(dim D == 1 and degree D == e and dim(D*S) == 0 and degree(D*S) == a*e-1) then error "no congruences detected";
            if sectionalGenus D != 0 then D = top D;
            return makeSubvariety(D,Y);
        );
        pr := rationalMap phip;
        v := (rationalMap {random(1,ring target pr),random(1,ring target pr)});
        pr' := toRationalMap((pr * v)|E);
        E' := Var kernel(map pr',SubringLimit=>1);
        if not(dim E' == 0 and degree E' == degree E) then error "something went wrong";
        decE' := select(decompose E',q -> (dim q == 0 and degree q == 1));
        P := select(apply(decE',q -> phi^* pr'^* q),C -> (dim C == 1 and degree C == e and dim(C*S) == 0 and degree(C*S) == a*e-1));
        if #P != 1 then (if #P > 1
                         then error "got more than one curve of the congruence that passes through the point"
                         else error "got no curve of the congruence that passes through the point");
        C := first P;
        if sectionalGenus C != 0 then C = top C;
        makeSubvariety(C,Y)
    );
    try f (if instance(X,GushelMukaiFourfold) then pointOnLinearSectionOfG14 Y else point Y) else error "no congruences detected";
    new CongruenceOfCurves from {
        "function" => f,
        "fourfold" => X,
        "degree" => e,
        "string" => "of "|toString(a*e-1)|"-secant "|(if e == 1 then "lines" else if e == 2 then "conics" else if e == 3 then "cubic curves" else if e == 4 then "quartic curves" else if e == 5 then "quintic curves" else "curves of degree "|toString(e))
    }
);

toString CongruenceOfCurves := net CongruenceOfCurves := f -> if hasAttribute(f,ReverseDictionary) then toString getAttribute(f,ReverseDictionary) else "a congruence "|f#"string";
texMath CongruenceOfCurves := texMath @@ net;

CongruenceOfCurves#{WebApp,AfterPrint} = CongruenceOfCurves#{WebApp,AfterNoPrint} =
CongruenceOfCurves#{Standard,AfterPrint} = CongruenceOfCurves#{Standard,AfterNoPrint} = f -> (
    S := surface f#"fourfold";
    Y := ambientFivefold f#"fourfold";
    e := f#"degree";
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Congruence " << f#"string" << " to ";
    << if hasAttribute(S,ReverseDictionary) then toString getAttribute(S,ReverseDictionary) else "surface";
    <<" in ";
    << if hasAttribute(Y,ReverseDictionary) then toString getAttribute(Y,ReverseDictionary) else (if codim Y > 0 then "a fivefold in PP^"|toString(dim ambient Y) else "PP^5");
    if S.cache#?("fanoMap",Y) and ((S.cache#("fanoMap",Y))#"map").cache#"multiplicityFanoMap" == e
    then << "; parameter space: " << Var target S.cache#("fanoMap",Y);
    << endl;
);

CongruenceOfCurves EmbeddedProjectiveVariety := (f,p) -> f#"function" p;
CongruenceOfCurves Ideal := (f,Ip) -> idealOfSubvariety f projectiveVariety gens Ip;

member(EmbeddedProjectiveVariety,CongruenceOfCurves) := (C,f) -> (
    L := (map f#"fourfold") C;
    dim L == 1 and unique degrees ideal L === {{1}}
);

map CongruenceOfCurves := o -> f -> (
    X := f#"fourfold";
    e := f#"degree";
    mu := fanoMap(X,e);
    e' := (mu#"map").cache#"multiplicityFanoMap";
    if e' =!= e then error("the congruence seems not valid; try the command: detectCongruence(...,"|toString(e')|")");
    multirationalMap mu
);

check (ZZ,CongruenceOfCurves) := o -> (n,f) -> (
    X := f#"fourfold";
    try for i to n-1 do f(point ambientFivefold X) else error "the congruence of curves is not valid";
    f
);
check CongruenceOfCurves := o -> f -> check(5,f);
