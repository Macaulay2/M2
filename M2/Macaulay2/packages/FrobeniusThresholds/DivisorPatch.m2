--symbols

protect NoStrategy;
protect ReturnMap;
protect IdealStrategy;
protect Section;
protect KnownDomain;
protect IsGraded;
protect ModuleStrategy;

--the following code is take from Divisor.m2

reflexify = method( Options => { Strategy => NoStrategy, KnownDomain => true, ReturnMap => false } )

--the first variant simply reflexifies an ideal

reflexify Ideal := Ideal => o -> I1 -> 
(
    if o.Strategy == ModuleStrategy then 
    (
        --the user specified we use the ModuleStrategy
        S1 := ring I1;
        inc := inducedMap( S1^1, I1*(S1^1) );
        ddual := Hom( Hom( inc, S1^1 ), S1^1 );
	annihilator coker ddual
    )
    else ( --otherwise we use the default ideal strategy
        reflexifyIdeal( I1, KnownDomain => o.KnownDomain )
    )
)

--an internal function that reflexifies an ideal

reflexifyIdeal = method( Options => { KnownDomain => true } )

reflexifyIdeal Ideal := Ideal => o -> I1 -> 
(
    S1 := ring I1;
    assumeDomain := false;
    if o.KnownDomain  then assumeDomain = true else assumeDomain = isDomain S1;
    if assumeDomain then 
    (
        if I1 == ideal 0_S1 then I1
	else
        (
            x := 0_S1;
            i := 0;
            genList := I1_*;
            while i < #genList and x == 0_S1 do
            (
                x = genList#i;
                i = i+1
            );
            ideal x : (ideal x : I1)
	)
    )
    else 
    (
        inc := inducedMap( S1^1, I1*S1^1 );
        ddual := Hom( Hom( inc, S1^1 ), S1^1 );
	annihilator coker ddual
    )
)

--we also reflexify modules

reflexify Module := Module => o -> M1 -> 
(
    S1 := ring M1;
    if o.Strategy == IdealStrategy then 
    (
        --the user specified we use the ideal strategy, this only works if the module can be embedded as an ideal
        I1 := embedAsIdeal M1;
        I2 := reflexifyIdeal( I1, KnownDomain => o.KnownDomain );
        if o.ReturnMap then 
            inducedMap( I2*S1^1, I1*S1^1 )
        else I2*S1^1
    )
    else reflexifyModule( M1, ReturnMap => o.ReturnMap )
)

reflexifyModule = method( Options => { ReturnMap => false } )

reflexifyModule Module := Module => o -> M1 -> 
(
    S1 := ring M1;
    if o.ReturnMap then 
    (
        gensMatrix := gens M1;
        h := map( M1, source gensMatrix, id_(source gensMatrix) );
        ddh := Hom( Hom( h, S1^1 ), S1^1 );
        map( target ddh, M1, matrix ddh )
    )
    else Hom( Hom( M1, S1^1 ), S1^1 )
)

idealPower = method() 
-- it seems to be massively faster to reflexify ideals with few generators than ideals 
-- with many generators, at least some of the time...

idealPower ( ZZ, Ideal ) := Ideal => ( n, J ) -> 
    ideal apply( J_*, z -> z^n )

reflexivePower = method( Options => { Strategy => IdealStrategy } )

reflexivePower ( ZZ, Ideal ) := Ideal => o -> ( n1, I1 ) -> 
    reflexify( idealPower( n1, I1 ), Strategy => o.Strategy )

--this method embeds a rank 1 module as a divisorial ideal
--this method is based on and inspired by code originally written by Moty Katzman, earlier versions can be found in
-- http://katzman.staff.shef.ac.uk/FSplitting/ParameterTestIdeals.m2
--under canonicalIdeal

embedAsIdeal = method( Options => { IsGraded => false, ReturnMap => false, Section => null } )

embedAsIdeal Module := Ideal => o -> M1 -> embedAsIdeal( ring M1, M1, o )

embedAsIdeal Matrix := Ideal => o -> Mat1 -> 
    embedAsIdeal( ring Mat1, Mat1, IsGraded => o.IsGraded, ReturnMap => o.ReturnMap )

embedAsIdeal ( Ring, Module ) := Ideal => o -> ( R1, M2 ) ->
(
    if instance( o.Section, Matrix ) then 
    ( --if we are passing a section
        if target o.Section == M2 then 
            embedAsIdeal( R1, o.Section, IsGraded => o.IsGraded, ReturnMap => o.ReturnMap )
        else error "embedAsIdeal: the target of the section is not equal to the given module."
    )
    else internalModuleToIdeal( R1, M2, IsGraded => o.IsGraded, ReturnMap => o.ReturnMap )
)

embedAsIdeal ( Ring, Matrix ) := Ideal => o -> ( R1, Mat2 ) -> 
    internalModuleWithSectionToIdeal( R1, Mat2, IsGraded => o.IsGraded, ReturnMap => o.ReturnMap )

internalModuleToIdeal = method( Options => { IsGraded => false, ReturnMap => false } )

internalModuleToIdeal ( Ring, Module ) := Ideal => o -> ( R1, M2 ) ->
(--turns a module to an ideal of a ring
--	S1 := ambient R1;
    done := false;
    local answer;
    if M2 == 0 then 
    ( --don't work for the zero module
        answer = ideal 0_R1;
	if o.IsGraded then answer = { answer, degree 1_R1 };
        if o.ReturnMap then 
        (
            if #entries gens M2 == 0 then 
                answer = flatten { answer, map( R1^1, M2, sub( matrix{{}}, R1 ) ) }
            else 
                answer = flatten { answer, map( R1^1, M2, { apply( #(first entries gens M2), st -> 0_R1) } ) }
	);
        return answer
    );
--  M2 := prune M1;
--  myMatrix := substitute(relations M2, S1);
--  s1:=syz transpose substitute(myMatrix,R1);
--  s2:=entries transpose s1;
    s2 := entries transpose syz transpose presentation M2;
    h := null;
    --first try going down the list
    i := 0;
    local t;
    local d1;
    while i < #s2 and not done do 
    (
        t = s2#i;
        h = map( R1^1, M2**R1, { t } );
        if not isWellDefined h then 
            error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
        if isInjective h then 
        (
            done = true;
            answer = trim ideal t;
            if o.IsGraded then 
            (
                d1 = degree( t#0 ) - ( degrees M2 )#0;
                answer = { answer, d1 }
            );
            if o.ReturnMap then answer = flatten { answer, h }
	 );
	 i = i+1
    );
    -- if that doesn't work, then try a random combination/embedding
    i = 0;
    while not done and i < 10 do 
    (
        coeffRing := coefficientRing R1;
        d := sum( #s2, z -> random( coeffRing, Height => 100000 )*(s2#z) );
        h = map( R1^1, M2**R1, { d } );
	if not isWellDefined h then 
            error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
        if isInjective h then 
        (
            done = true;
            answer = trim ideal d;
            if o.IsGraded then 
            (
                d1 = degree(d#0) - ( degrees M2 )#0;
                answer = { answer, d1 }
	    );
            if o.ReturnMap then answer = flatten { answer, h }
	);
        i = i + 1
    );
    if not done then 
        error "internalModuleToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
    answer
)

--this variant takes a map from a free module of rank 1 and maps to another rank 1 module.  The function returns the second module as an ideal combined with the element

internalModuleWithSectionToIdeal = method( Options => { ReturnMap => false, IsGraded => false } )

internalModuleWithSectionToIdeal (Ring, Matrix ) := Ideal => o -> ( R1, f1 ) ->
(
    M1 := source f1;
    M2 := target f1;
    if not isFreeModule M1 or rank M1 != 1 then 
        error "internalModuleWithSectionToIdeal: Error, source is not a rank-1 free module";
    done := false;
    local answer;
    s2 := entries transpose syz transpose presentation M2;
    h := null;
    --first try going down the list
    i := 0;
    local t;
    local d1;
    while i < #s2 and not done do 
    (
        t = s2#i;
        h = map( R1^1, M2**R1, { t } );
        if not isWellDefined h  then 
            error "internalModuleWithSectionToIdeal: Something went wrong, the map is not well defined.";
        if isInjective h then 
        (
            done = true;
            answer = trim ideal t;
            if o.IsGraded then 
            (
                d1 = degree(t#0) - ( degrees M2 )#0;
                answer = { answer, d1 };
            );
            if o.ReturnMap then answer = flatten { answer, h }
	);
	i = i+1;
    );
    -- if that doesn't work, then try a random combination/embedding
    while not done and i < 10 do 
    (
        coeffRing := coefficientRing R1;
        d := sum( #s2, z -> random( coeffRing, Height => 100000 )*(s2#z) );
	h = map( R1^1, M2**R1, { d } );
	if not isWellDefined h then 
            error "internalModuleWithSectionToIdeal: Something went wrong, the map is not well defined.";
        if isInjective h then 
        (
            done = true;
            answer = trim ideal d;
            if o.IsGraded then 
            (
                d1 = degree(d#0) - ( degrees M2 )#0;
                answer = { answer, d1 }
	    );
            if o.ReturnMap then answer = flatten { answer, h }
	);
	i = i+1
    );
    if not done then 
        error "internalModuleWithSectionToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
    newMatrix := h*f1;
    flatten { first first entries newMatrix, answer }
)


isDomain = method()

isDomain Ring := Boolean =>  R1 -> isPrime ideal 0_R1

--gets a nonzero generator of an ideal.
getNonzeroGenerator := I2 -> 
(
    i := 0;
    found := false;
    genList := I2_*;
    localZero := 0_(ring I2);
    while i < #genList and not found do 
    (
        if genList#i != localZero then found = true;
        i = i + 1
    );
    if found then genList#(i-1)
    else null
)

isLocallyPrincipalIdeal = method()

--the following function should go elsewhere, it checks whether a given ideal is locally principal (really, invertible).  If it is locally principal, it returns the inverse ideal.
isLocallyPrincipalIdeal Ideal := I2 -> 
(
    localGen := getNonzeroGenerator I2;
    if localGen === null then return { false, 0_(ring I2) };
    inverseIdeal := ideal localGen : I2;
    idealProduct := inverseIdeal * I2;
    isLocPrinc := reflexify( idealProduct ) == idealProduct;
    if isLocPrinc then { true, inverseIdeal }
    else { false, 0_(ring I2) }
)
