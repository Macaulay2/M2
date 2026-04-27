newPackage(
    "NonPrincipalTestIdeals",
    Version => "1.0",
    Date => "September 28th, 2025",
    Authors => {
        {Name => "Trung Chau", Email => "chauchitrung1996@gmail.com", HomePage => "https://trungchaumath.github.io/"}, 
        {Name => "Karl Schwede", Email => "schwede@math.utah.edu", HomePage => "https://www.math.utah.edu/~schwede/"},
        {Name => "Hunter Simper", Email => "hunter.simper@utah.edu", HomePage => "https://www.huntersimper.com/"}},    
    Headline => "singularities of pairs with non-principal ideals",
    Keywords => {"Commutative algebra", "Singularities"},
    DebuggingMode => false,
    Reload=>false,     
    PackageExports => {"WeilDivisors", "TestIdeals", "FrobeniusThresholds", "ReesAlgebra", "Complexes"}
    )
export{
    "extendedReesAlgebra",
    "reesCanonicalModule",
    "reesModuleToIdeal",
    "gradedReesPiece",
    "testModuleMinusEpsilon",
    --"isFJumpingExponent",
    "isFJumpingExponentModule",
    "classicalReesAlgebra",
    "manualExt", --since Macaulay2's Ext doesn't give correct answers in rings with negatively graded variables, we make our own manually
    --"IsGraded",
    "AmbientCanonical",--option
    --"ExtendedReesAlgebra",--a flag to see if a ring was created via extendedReesAlgebra
    --"ClassicalReesAlgebra",--a flag to see if a ring was created via classicalReesAlgebra
    --"ForceExtendedRees", --option
    "isFRationalThreshold",
    --"ReturnMap",
    --"Map",
    "isInvertibleIdeal",
    "torsionOrder"
}

--the following function checks to see if an ideal is locally principal.
--it only is guaranteed to work in a normal domain.
--todo-rewrite this so it works in more general rings
isInvertibleIdeal = method(Options=>{});
isInvertibleIdeal(Ideal) := Boolean => opts -> (I1) -> (
    IDminus := dualize(I1); 
	myProduct := I1*IDminus;
	(myProduct == reflexify(myProduct))
);

--the following function tries to compute the torsion order of an ideal I1 up to a certain power n1.
torsionOrder = method(Options =>{});
torsionOrder(ZZ, Ideal) := (ZZ, Ideal) => opts -> (n1, I1) -> (
    i := 1;
    local curIdeal;
    while (i < n1) do (
        curIdeal = reflexivePower(i, I1);
        if isInvertibleIdeal(curIdeal) then return (i, curIdeal);        
        i = i+1;
    );
    (0, ideal(sub(0, ring I1)))
);


manualExt = method(Options=>{}); --the current implementation of Ext does not work if rings have negatively graded variables.  We do it manually instead.

manualExt(ZZ,Module,Module):= (Module) => opts -> (n1, M1, M2) -> (
    myRes := resolution(M1, LengthLimit =>n1+1); --, DegreeLimit => n1+1); --get weird errors
    myResHom := Hom(myRes, M2);
    HH^n1(myResHom)
);

--the degrees need to be fixed to work with extended Rees algebras
--KARL is looking at if this is correct right now.

--This takes a Rees algebra or extended Rees algebra, constructed in this package using either 
--classicalReesAlgebra
--or
--extendedReesAlgebra
--and computes the canonical Rees algebra.  
--If you specify the canonical module of the ambient ring, it will use that instead.
reesCanonicalModule = method(Options=>{AmbientCanonical => null})
reesCanonicalModule(Ring) := Module => o->(R1) -> (
	S1 := ambient R1;
	I1 := ideal R1;
	dR := dim R1;
	dS := dim S1;
	varList := first entries vars S1;
	degList := {};
    degSum := 0; --default value 0
    local ambcan;
    if o.AmbientCanonical === null then ( --we do things slightly weirdly for extended Rees algebras.
        if (R1#?"ExtendedReesAlgebra") and (R1#"ExtendedReesAlgebra") then (
            varList = select(varList, z -> ((degree z)#0 >= 0));--grab variables with positive initial degree
            degList = apply(varList, q -> (degree(q))); --grab their degrees next
            --print degList;
            degSum = -(sum degList)+{1,0};  --we do a shift for t^{-1} variable
        )
        else if (#varList > 0) then ( --then there are some variables
            if (#(degree(varList#0)) == 1) then (
                degList = apply(varList, q -> (degree(q))#0); )
            else (
                degList = apply(varList, q -> (degree(q))); );
            degSum = -(sum degList);
        );
        if (debugLevel > 1) then (
            print degList;
            print degSum;
        );
        ambcan = S1^{degSum}; 
    )
    else (
        ambcan = o.AmbientCanonical;
        --print (degrees ambcan);
    );    
    --print "doing classical Ext";
    --M1 := (Ext^(dS - dR)(S1^1/I1, ambcan))**R1
    M1 := (manualExt(dS - dR,S1^1/I1, ambcan))**R1
)

--this should get a variable name that is not incompatible with the variable names of the given ring.
getValidVarName = method();
getValidVarName(Ring) := (R1) -> (
    --this should be smarter, not sure the right way to do it.  This ought to work for now.
    s1 := toList("abcdefghijklmnopqrstuvwxyz");
    myStr := (s1#(random (#s1))) | (s1#(random (#s1)));
    myVal := value ("use R1; myStr");
    flagDone := false;

    while (flagDone == false) do (   
        if (IndexedVariableTable === class myVal) then (
            try ( myVal = sub(myVal_1, R1) ) then (
                myStr = (s1#(random (#s1))) | (s1#(random (#s1)));
                myVal = value ("use R1; myStr");
            )
            else(
                flagDone = true;
            );
        )
        else(
            flagDone = true;
        );
    );
    myStr
)

extendedReesAlgebra = method(Options => {});

extendedReesAlgebra(Ideal) := opts->(J1) -> (    
    if any (degrees ring J1, ll -> #ll > 1) then error "extendedReesAlgebra: currently only works for singly graded ambient rings";
    I1 := reesIdeal(J1, Variable=>getValidVarName(ring J1));
    local degList;
    if isHomogeneous J1 then ( 
        degList = apply( (degrees ring J1), j->{0,sum j} ) | (degrees ring I1) | {{-1,0}};
    )
    else(
        degList = apply( (degrees ring J1), j->{0,0} ) | apply(degrees ring I1, j->{1,0}) | {{-1,0}};
    );
--    print degList;
    ti := getSymbol "ti";
    T2 := (coefficientRing ring(J1))(monoid[ (gens ring J1)|(gens ring I1)|{ti}, Degrees=>degList]);
    ti = last gens T2;
    --T2 = ambient reesAlgebra J1; 
    --S2 := T2/(sub(I1, T2));    
    L1 := apply(gens ring I1, u -> sub(u, T2));
    reesList := first entries mingens J1;
    L0 := apply(reesList, h -> sub(h, T2));
    S2 := T2/((sub(ideal ring J1, T2) + sub(I1, T2) + ideal( apply(#(gens ring I1), j -> ti*(L1#j) - (L0#j)))));
    S2#"InverseVariable" = sub(ti, S2);
    S2#"BaseRing" = ring J1;
    S2#"Degree1" = apply(gens ring(I1), z -> sub(z, S2));
--    S2#"OriginalList" = apply(L0, z->sub(z, S2));
    S2#"BaseRingList" = reesList;
    S2#"ExtendedReesAlgebra" = true;    
    S2
)

classicalReesAlgebra = method(Options => {});

classicalReesAlgebra(Ideal) := opts -> (J1) -> (
    if any (degrees ring J1, ll -> #ll > 1) then error "classicalReesAlgebra: currently only works for singly graded ambient rings";
    --Rees2 := reesAlgebra J1;
--    degList := apply( (degrees ring J1), j->{0,sum j} ) | (degrees ring I1);
--    print degList;
--    T2 := (coefficientRing ring(J1))[ (gens ring J1)|(gens ring I1), Degrees=>degList];
    --ti = last gens T2;
    --T2 = ambient reesAlgebra J1; 
    --S2 := T2/(sub(I1, T2));    
    --reesList := first entries mingens J1;
    --S2 := (flattenRing Rees2)#0;--:= T2/((sub(ideal ring J1, T2) + sub(I1, T2) + ideal( apply(#(gens ring I1), j -> ti*(L1#j) - (L0#j)))));
    I1 := reesIdeal(J1, Variable=>getValidVarName(ring J1));
    local degList;
    if isHomogeneous J1 then ( 
        degList = apply( (degrees ring J1), j->{0,sum j} ) | (degrees ring I1) ;
    )
    else(
        degList = apply( (degrees ring J1), j->{0,0} ) | apply(degrees ring I1, j->{1,0});
    );
--    print degList;
    T2 := (coefficientRing ring(J1))(monoid[ (gens ring J1)|(gens ring I1), Degrees=>degList]);
    --T2 = ambient reesAlgebra J1; 
    --S2 := T2/(sub(I1, T2));    
    L1 := apply(gens ring I1, u -> sub(u, T2));
    reesList := first entries mingens J1;
    L0 := apply(reesList, h -> sub(h, T2));
    S2 := T2/((sub(ideal ring J1, T2) + sub(I1, T2)));

    S2#"BaseRing" = ring J1;
    S2#"Degree1" = apply(gens ring(I1), z -> sub(z, S2));
--    S2#"OriginalList" = apply(reesList, z->sub(z, S2));
    S2#"BaseRingList" = reesList;
    S2#"ClassicalReesAlgebra" = true;    
    S2
);


--this should be like basis(n, M)
gradedReesPiece = method(Options => {});

gradedReesPiece(ZZ, Ideal) := opts -> (n1, J1) -> (
    S1 := ring J1;
    if not ((S1#?"ExtendedReesAlgebra") or (S1#?"ClassicalReesAlgebra")) then error "gradedReesPiece:  Expected a ClassicalReesAlgebra or ExtendedReesAlgebra"; 
    R1 := S1#"BaseRing";
    genList := first entries gens J1;
    degList := apply(genList, zz->first (degree zz) );
    baseGens := S1#"BaseRingList";
    tempGens := ideal(0_R1);
    local badMap;
    local i;
    if (S1#?"ExtendedReesAlgebra") and (S1#"ExtendedReesAlgebra" == true) then (
        --if not isHomogeneous J1 then error "gradedReesPiece:  Expected a homogeneous ideal or a Reese pieces";
        --something is not working right, we should remove this error, and then debug
        badMap = map(R1, S1, (gens R1) | baseGens | {1}); --this is not well defined, but it should do the job.
        i = 0;
        while (i < #genList) do (
            if (degList#i == n1) then (
                tempGens = tempGens + ideal(badMap(genList#i));
            )
            else if (degList#i > n1) then (
                tempGens = tempGens + badMap(((S1#"InverseVariable")^((degList#i) - n1))*ideal((genList#i)));
            )
            else if (degList#i < n1) then (
                tempGens = tempGens + (ideal(badMap(genList#i)))*(ideal baseGens)^(n1 - degList#i);
            );
            i = i+1;
        );
        return tempGens;
    )
    else if (S1#?"ClassicalReesAlgebra") and (S1#"ClassicalReesAlgebra" == true) then (
        if not isHomogeneous J1 then error "gradedReesPiece:  Expected a homogeneous ideal or a Reese pieces";
        badMap = map(R1, S1,  (gens R1) | baseGens ); --this is not well defined, but it should do the job.
        i = 0;
        while (i < #genList) do (
            if debugLevel >= 1 then print ("gradedReesPiece: classical, looking at " | toString(genList#i));
            if (degList#i == n1) then (
                tempGens = tempGens + ideal(badMap(genList#i));
            )
            else if (degList#i < n1) then (
                tempGens = tempGens + (ideal(badMap(genList#i)))*(ideal baseGens)^(n1 - degList#i);
            );
            if debugLevel >= 1 then print ("gradedReesPiece: classical:" | toString(tempGens));
            i = i+1;
        );
        return tempGens;
    )
    else (
        error "gradedReesPiece: Expected a module over a ring constructed via classicalReesAlgebra or extendedReesAlgebra.";
    )
);


--turns a module of generic rank 1 into an ideal when working in a ring created by classicalReesAlgebra or extendedReesAlgebra.
reesModuleToIdeal = method(Options => {MTries=>10});

reesModuleToIdeal(Ring, Module) := (Ideal, ZZ, Matrix) => o ->(R1, M2) -> 
(--turns a module to an ideal of a ring
--	S1 := ambient R1;
	flag := false;
	answer:=0;
	if (M2 == 0) then ( --don't work for the zero module	    
	    answer = ideal(sub(0, R1));
	    --if (o.Homogeneous==true) then (		    
        answer = {answer, degree (sub(1,R1))};
        if (#entries gens M2 == 0) then (
            answer = flatten {answer, map(R1^1, M2, sub(matrix{{}}, R1))};
        )
        else (
            answer = flatten {answer, map(R1^1, M2, {apply(#(first entries gens M2), st -> sub(0, R1))})};
        );
		--);		
	    return toSequence answer;
	);
--	M2 := prune M1;
--	myMatrix := substitute(relations M2, S1);
--	s1:=syz transpose substitute(myMatrix,R1);
--	s2:=entries transpose s1;
	s2 := entries transpose syz transpose presentation M2;
	h := null;
	--first try going down the list
	i := 0;
	t := 0;
	d1 := 0;
    if (debugLevel > 0) then print "ReesModuleToIdeal : starting loop";
	while ((i < #s2) and (flag == false)) do (
		t = s2#i;
		h = map(R1^1, M2**R1, {t});
		if (isWellDefined(h) == false) then error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(t);
			--if (o.Homogeneous==true) then (
				--print {degree(t#0), (degrees M2)#0};
            d1 = degree(t#0) - (degrees M2)#0;
            if (debugLevel > 0) then print ("s2 : " | (toString(s2)));
            if (debugLevel > 0) then print ("t : "|(toString(s2#i)));
            if (debugLevel > 0) then print ("degrees M2 : "|(toString(degrees M2)));
            if (debugLevel > 0) then print ("d1 : " | toString(d1));
            answer = {answer, d1};
			--);
			--if (o.Map==true) then (
			answer = flatten {answer, h};
			--);
            --1/0;
		)
        else (print "warning");
		i = i+1;
	);
	-- if that doesn't work, then try a random combination/embedding
     i = 0;
	while ((flag == false) and (i < o.MTries) ) do (
		coeffRing := coefficientRing(R1);
        print coeffRing;
		d := sum(#s2, z -> random(coeffRing, Height=>100000)*(s2#z));
       -- print d;
		h = map(R1^1, M2**R1, {d});
		if (isWellDefined(h) == false) then error "internalModuleToIdeal: Something went wrong, the map is not well defined.";
		if (isInjective(h) == true) then (
			flag = true;
			answer = trim ideal(d);
			--if (o.Homogeneous==true) then (
            d1 = degree(d#0) - (degrees M2)#0;
            answer = {answer, d1};
			--);
			--if (o.Map==true) then (
			answer = flatten {answer, h};
			--);
		);
        i = i + 1;
	);
	if (flag == false) then error "internalModuleToIdeal: No way found to embed the module into the ring as an ideal, are you sure it can be embedded as an ideal?";
	toSequence answer
);

--testModule = method(Options => {ForceExtendedRees => false, AssumeDomain => false, FrobeniusRootStrategy => Substitution});
testModule(QQ, Ideal) := opts -> (n1, I1) -> (
    R1 := ring I1;
    p1 := char R1;
    local omegaS1;
    local omegaS1List;
    local tauOmegaSList;
    local tauOmegaS;
    local degShift;
    local S1;
    local answer;
    local baseCanonical;
    flag := true;
    if (floor n1 == n1) and (n1 > 0) then (
        if (debugLevel >= 1) then print "testModule (non principal): Using ordinary Rees algebra";
        if (dim I1 <= dim R1 - 2) then (
            S1 = classicalReesAlgebra(I1);  
            omegaS1 = reesCanonicalModule(S1);
            omegaS1List = reesModuleToIdeal(S1, omegaS1);--, Homogeneous=>true, Map => true);
            degShift = (omegaS1List#1)#0;        
            baseCanonical = reflexify gradedReesPiece(degShift+1, omegaS1List#0);
            tauOmegaSList = testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0, FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
            degShift = (omegaS1List#1)#0; 
            if (debugLevel >= 1) then print ("testModule (nonprincipal): degShift: " | toString(degShift));
            if (debugLevel >= 1) then print ("testModule (nonprincipal): module: " | toString(tauOmegaSList#0) | " in ambient " | toString(omegaS1List#0));
            answer = (gradedReesPiece(degShift + floor n1, tauOmegaSList#0));
            flag = false;--don't do the extended Rees approach
        );        
    );    
    if flag then ( --we do the extended Rees algebra thing
        if (debugLevel >= 1) then print "testModule (nonprincipal): Using extended Rees algebra";
        S1 = extendedReesAlgebra(I1);
        tvar := S1#"InverseVariable";
        omegaS1 = prune reesCanonicalModule(S1);    
        omegaS1List = reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
        degShift = (omegaS1List#1)#0; 
        baseCanonical = reflexify gradedReesPiece(degShift-1-dim R1, omegaS1List#0);
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
        tauOmegaS = tauOmegaSList#0;        
        if (debugLevel >= 1) then print ("testModule (nonprincipal): degShift " | toString(degShift));
        --print degShift;
         if (debugLevel >= 1) then print ("testModule (nonprincipal): module: " | toString(tauOmegaSList#0) | " in ambient " | toString(omegaS1List#0));
         --print tauOmegaS;
        answer = (gradedReesPiece(degShift, tauOmegaS));
    );
    (trim answer, baseCanonical)
);

testModule(ZZ, Ideal) := opts -> (n1, I1) -> (
    testModule(n1/1, I1)
);

--testIdeal = method(Options =>{ForceExtendedRees => false, MaxCartierIndex=>10 });
testIdeal(QQ, Ideal) := opts -> (n1, I1) -> (
    R1 := ring I1;
    p1 := char R1;
    local omegaS1;
    local omegaS1List;
    local tauOmegaSList;
    local tauOmegaS;
    local degShift;
    local S1;
    local answer;
    local baseCanonical;
    flag := true;
    if (floor n1 == n1) and (n1 > 0) then (
        if (debugLevel >= 1) then print "testIdeal (nonprincipal): Using ordinary Rees algebra";
        
        S1 = classicalReesAlgebra(I1);  
        omegaS1 = reesCanonicalModule(S1);
        omegaS1List = reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
        degShift = (omegaS1List#1)#0;
        if (dim I1 <= dim R1 - 2) then (
            baseCanonical = reflexify gradedReesPiece(degShift+1, omegaS1List#0);
            --baseCanonicalIdeal = reflexify(moduleToIdeal(baseCanonical));
            if (isInvertibleIdeal baseCanonical) then (
                tauOmegaSList = testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
                degShift = (omegaS1List#1)#0; 
                if (debugLevel >= 1) then print ("testIdeal (nonprincipal): degShift: " | toString(degShift));
                answer = (gradedReesPiece(degShift + floor n1, tauOmegaSList#0)) : baseCanonical;
                flag = false;--don't do the extended Rees approach
            );
        );        
    );    
    if flag then ( --we do the extended Rees algebra thing
        if (debugLevel >= 1) then print "testIdeal (nonprincipal): Using extended Rees algebra";
        S1 = extendedReesAlgebra(I1);
        tvar := S1#"InverseVariable";
        omegaS1 = prune reesCanonicalModule(S1);  
        --print omegaS1;      
        omegaS1List = reesModuleToIdeal(S1, omegaS1);-- , Homogeneous=>true, Map => true);
        degShift = (omegaS1List#1)#0; 
        baseCanonical = reflexify gradedReesPiece(-1 - dim R1 + degShift, omegaS1List#0); --is that small enough?
        if (isInvertibleIdeal baseCanonical) then (
            tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
            tauOmegaS = tauOmegaSList#0;
            --print tauOmegaS; 
            if (debugLevel >= 1) then print ("testIdeal (nonprincipal): degShift " | toString(degShift));
            --print degShift;
            answer = (gradedReesPiece(degShift, tauOmegaS)) : baseCanonical;
        )
        else( 
            torOrd := torsionOrder(opts.MaxCartierIndex, baseCanonical);
            if (torOrd#0 == 0) then error "testIdeal (nonprincipal) : base ring does not appear to be Q-Gorenstein, try increasing MaxCartierIndex";
            f := (first entries gens trim baseCanonical)#0;
            if (f == 0) then error "testIdeal (nonprincipal) : something went wrong";
            newPrinc := sub(first first entries gens ((ideal f^(torOrd#0)) : (torOrd#1)), S1);
            tauOmegaSList = testModule({1/(torOrd#0), n1}, {newPrinc, tvar}, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
            tauOmegaS = tauOmegaSList#0;
            --print tauOmegaS; 
            if (debugLevel >= 1) then print ("testIdeal (nonprincipal): degShift " | toString(degShift));
            --print degShift;
            answer = (gradedReesPiece(degShift, tauOmegaS)) : ideal(f);
        )
    );
    trim answer
);

testIdeal(ZZ, Ideal) := opts -> (n1, I1) -> (
    testIdeal(n1/1, I1, opts) 
);


testModuleMinusEpsilon= method(Options =>{});--this tries to compute tau(R, a^{t-epsilon})

testModuleMinusEpsilon(QQ, Ideal) := opts -> (n1, I1) -> (
    R1 := ring I1;
    pp := char R1;
    local computedHSLGInitial;
    local computedHSLG;
    local tauOmegaSList;
    local answer1;
    local answer2;    
    local tauOmegaS;
    S1 := extendedReesAlgebra(I1);
    --print "testing";
    tvar := S1#"InverseVariable";
    omegaS1 := reesCanonicalModule(S1);
    --print "test1";
    omegaS1List := reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
    degShift := (omegaS1List#1)#0;
    baseCanonical := reflexify gradedReesPiece(degShift-1 - dim R1, omegaS1List#0);
    --if (not isInvertibleIdeal baseCanonical) then error "testIdealMinusEpsilonNP: Expected a quasi-Gorenstein ambient ring";
    baseTauList := testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
    --print "test3";
    baseTau := baseTauList#0;
    genList := baseTauList#2;

    --now we have to run the sigma computation
    ( a1, b1, c1 ) := decomposeFraction( pp, n1, NoZeroC => true );
    if (instance(genList, RingElement)) then (
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => { genList } );
        computedHSLG = frobeniusRoot(b1, ceiling( ( pp^b1 - 1 )/( pp - 1 ) ), genList, sub(computedHSLGInitial, ambient S1));
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        return(answer2, baseCanonical);
    )
    else if instance(genList, BasicList) then ( -- Karl: I haven't tested this
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => genList );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, apply(#genList, zz -> ceiling( ( pp^b1 - 1 )/( pp - 1 ) )), genList, sub(computedHSLGInitial, ambient S1));
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        return(answer2, baseCanonical);
    );
    error "isFJumpingExponent (non-principal case): something went wrong with the generator list for the Fedder colon";
);

testModuleMinusEpsilon(ZZ, Ideal) := opts -> (n1, I1) -> (
    testModuleMinusEpsilon(n1/1, I1)
)

isFJumpingExponentModule = method(Options =>{ AtOrigin => false, FrobeniusRootStrategy => Substitution, Verbose=>false});

isFJumpingExponentModule(QQ, Ideal) := opts -> (n1, I1) -> (
    --if opts.AssumeDomain == false then error "isFJumpingExponentModule: not yet implemented for the non-domain case";    
    if opts.Verbose then print ("Running isFJumpingExponentModule with AtOrigin => " | toString(opts.AtOrigin));
    R1 := ring I1;
    pp := char R1;
    local computedHSLGInitial;
    local computedHSLG;
    local tauOmegaSList;
    local answer1;
    local answer2;    
    local tauOmegaS;
    local newAnn;
    local maxIdeal;
    S1 := extendedReesAlgebra(I1);
    --print "testing";
    tvar := S1#"InverseVariable";
    omegaS1 := reesCanonicalModule(S1);
    --print "test1";
    omegaS1List := reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
    degShift := (omegaS1List#1)#0;
--    if not (gradedReesPiece(degShift, omegaS1List#0) == ideal(sub(1, R1))) then error "isFJumpingExponent (non-principal case): not yet implemented for non(-obviously-)quasi-Gorenstein rings";--in the future, do some more work in this case to handle the Q-Gorenstein setting.   
    --print "test2";
    baseTauList := testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
    --print "test3";
    baseTau := baseTauList#0;
    genList := baseTauList#2;

    --now we have to run the sigma computation
    ( a1, b1, c1 ) := decomposeFraction( pp, n1, NoZeroC => true );
    if (instance(genList, RingElement)) then (
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => {genList}, CanonicalIdeal => omegaS1List#0, FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => { genList },FrobeniusRootStrategy=>opts.FrobeniusRootStrategy );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, ceiling( ( pp^b1 - 1 )/( pp - 1 ) ), genList, sub(computedHSLGInitial, ambient S1),FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
        --print "test5";
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        if (opts.Verbose) then print ("tau(a^t) is " | toString(answer1));
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);        
        if (opts.Verbose) then print ("tau(a^(t-epsilon)) is " | toString(answer2));
        if opts.AtOrigin then (        
            newAnn = ann(answer2*R1^1/(answer2*R1^1));
            maxIdeal = ideal gens R1;
            return isSubset(newAnn, maxIdeal);
        )
        else( 
            return not(answer1 == answer2);
        );
    )
    else if instance(genList, BasicList) then ( -- Karl: I haven't tested this
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => genList, CanonicalIdeal => omegaS1List#0,FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => genList, FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
        --print "test4";
        computedHSLG = frobeniusRoot(b1, apply(#genList, zz -> ceiling( ( pp^b1 - 1 )/( pp - 1 ) )), genList, sub(computedHSLGInitial, ambient S1), FrobeniusRootStrategy=>opts.FrobeniusRootStrategy);
        --print "test5";
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        if (opts.Verbose) then print ("tau(a^t) is " | toString(answer1));
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        if (opts.Verbose) then print ("tau(a^(t-epsilon)) is " | toString(answer2));
        if opts.AtOrigin then (
            newAnn = ann(answer2*R1^1/(answer2*R1^1));
            maxIdeal = ideal gens R1;
            return isSubset(newAnn, maxIdeal);
        )
        else( 
            return not(answer1 == answer2);
        );
    );
    error "isFJumpingExponent (non-principal case): something went wrong with the generator list for the Fedder colon";
);

isFJumpingExponentModule(ZZ, Ideal) := opts -> (n1, I1) -> (
    isFJumpingExponentModule(n1/1, I1, opts)
);

isFRationalThreshold = method(Options =>{AtOrigin => false, FrobeniusRootStrategy => Substitution, Verbose=>false});

isFRationalThreshold(QQ, Ideal) := opts -> (n1, I1) -> (
    --if opts.AssumeDomain == false then error "isFRationalThreshold: not yet implemented for the non-domain case";    
    if opts.Verbose then print ("isFRationalThreshold: starting " | toString(I1) | "^" | toString n1);
    R1 := ring I1;
    pp := char R1;
    local computedHSLGInitial;
    local computedHSLG;
    local tauOmegaSList;
    local answer1;
    local answer2;    
    local tauOmegaS;
    S1 := extendedReesAlgebra(I1);
    local maxIdeal;
    local newAnn;
    --print "testing";
    tvar := S1#"InverseVariable";
    omegaS1 := reesCanonicalModule(S1);
    --print "test1";
    omegaS1List := reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
    degShift := (omegaS1List#1)#0;
    baseOmega := reflexify gradedReesPiece(degShift - 1 - dim R1, omegaS1List#0);
    if opts.Verbose then print ("ambient canonical module " | toString(baseOmega));
--    if not (gradedReesPiece(degShift, omegaS1List#0) == ideal(sub(1, R1))) then error "isFJumpingExponent (non-principal case): not yet implemented for non(-obviously-)quasi-Gorenstein rings";--in the future, do some more work in this case to handle the Q-Gorenstein setting.   
    --print "test2";
    baseTauList := testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
    --print "test3";
    baseTau := baseTauList#0;
    genList := baseTauList#2;

    --now we have to run the sigma computation
    ( a1, b1, c1 ) := decomposeFraction( pp, n1, NoZeroC => true );
    if opts.Verbose then print "Considering the Cartier action on omega";
    if (instance(genList, RingElement)) then (
        if opts.Verbose then print ("isFRationalThreshold : Macaulay2 can see that the test module Cartier action is principal: " | toString(genList));
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => {genList}, CanonicalIdeal => omegaS1List#0);
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        if opts.Verbose then print ("test module at t :" | toString(answer1));
        --if opts.AtOrigin then answer1 = saturate(answer1);
        if (answer1 == baseOmega) then (return 1==0);
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => { genList } );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, ceiling( ( pp^b1 - 1 )/( pp - 1 ) ), genList, sub(computedHSLGInitial, ambient S1));
        --print "test5";        
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);   
        if opts.Verbose then print ("test module at t-epsilon :" | toString(answer2));
        --if opts.AtOrigin then answer2 = saturate(answer2); 
        if  not (answer2 == baseOmega) then (return false;);            
        if opts.AtOrigin then (
            newAnn = ann(answer2*R1^1/(answer2*R1^1));
            maxIdeal = ideal gens R1;
            return isSubset(newAnn, maxIdeal);
        )
        else (
            return not(answer1 == answer2);
        );
    )
    else if instance(genList, BasicList) then ( -- Karl: I haven't tested this
        if opts.Verbose then print ("isFRationalThreshold : Macaulay2 failed to see that the test module Cartier action is principal: " | toString(genList));
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => genList, CanonicalIdeal => omegaS1List#0);
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        --if opts.AtOrigin then answer1 = saturate(answer1);
        if (answer1 == baseOmega) then (return false;);
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => genList );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, apply(#genList, zz -> ceiling( ( pp^b1 - 1 )/( pp - 1 ) )), genList, sub(computedHSLGInitial, ambient S1));
        --print "test5";
        if (opts.Verbose) then print ("tau(a^t) is " | toString(answer1));
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        --if opts.AtOrigin then answer2 = saturate(answer2);
        if  not (answer2 == baseOmega) then (return false;);    
        if (opts.Verbose) then print ("tau(a^(t-epsilon)) is " | toString(answer2)); 
        if opts.AtOrigin then (
            newAnn = ann(answer2*R1^1/(answer2*R1^1));
            maxIdeal = ideal gens R1;
            return isSubset(newAnn, maxIdeal);
        )
        else (
            return not(answer1 == answer2);
        );
    );
    error "isFRationalThreshold (non-principal case): something went wrong with the generator list for the Fedder colon";
);

isFRationalThreshold(ZZ, Ideal) := opts -> (n1, I1) -> (
    isFRationalThreshold(n1/1, I1, opts)
);



--isFJumpingExponent is defined in FrobeniusThresholds

isFJumpingExponent(QQ, Ideal) := opts -> (n1, I1) -> (
    if opts.AssumeDomain == false then error "isFJumpingExponentModule: not yet implemented for the non-domain case";    
    if (class ring I1 === PolynomialRing) then (  return isFJumpingExponentModule(n1, I1, AtOrigin => opts.AtOrigin, FrobeniusRootStrategy => opts.FrobeniusRootStrategy, Verbose=>opts.Verbose); );
     
    R1 := ring I1;
    
    baseCanonical := canonicalIdeal(R1);
    if (isInvertibleIdeal baseCanonical) then (
        return isFJumpingExponentModule(n1, I1, AtOrigin => opts.AtOrigin, FrobeniusRootStrategy => opts.FrobeniusRootStrategy, Verbose=>opts.Verbose);
    )
    else(
        torOrd := torsionOrder(opts.MaxCartierIndex, baseCanonical);
        if (torOrd#0 == 0) then error "testIdeal (nonprincipal) : base ring does not appear to be Q-Gorenstein, try increasing MaxCartierIndex";
        error "testIdeal (nonprincipal) : not yet implemented in the Q-Gorenstein case";
    );
    error "isFJumpingExponent (non-principal case): something went wrong with the generator list for the Fedder colon";
);

isFJumpingExponent(ZZ, Ideal) := opts -> (n1, I1) -> (
    isFJumpingExponent(n1/1, I1, opts)
);

isFPT(QQ, Ideal) := opts -> (n1, I1) ->(    
    isFRationalThreshold(n1,I1, AtOrigin => opts.AtOrigin, FrobeniusRootStrategy => opts.FrobeniusRootStrategy, Verbose=>opts.Verbose)
);

isFPT(ZZ, Ideal) := opts -> (n1, I1) ->(    
    isFRationalThreshold(n1/1,I1, AtOrigin => opts.AtOrigin, FrobeniusRootStrategy => opts.FrobeniusRootStrategy, Verbose=>opts.Verbose)
);

--isFPT = method(Options)
-*
isFPT(QQ, Ideal) := opts -> (n1, I1) -> (
     R1 := ring I1;
    pp := char R1;
    local computedHSLGInitial;
    local computedHSLG;
    local tauOmegaSList;
    local answer1;
    local answer2;    
    local tauOmegaS;
    S1 := extendedReesAlgebra(I1);
    --print "testing";
    tvar := S1#"InverseVariable";
    omegaS1 := reesCanonicalModule(S1);
    --print "test1";
    omegaS1List := reesModuleToIdeal(S1, omegaS1); --, Homogeneous=>true, Map => true);
    degShift := (omegaS1List#1)#0;
    targetAnswer := gradedReesPiece(degShift, omegaS1List#0);
    if not (targetAnswer == ideal(sub(1, R1))) then error "isFPT (non-principal case): not yet implemented for non(-obviously-)quasi-Gorenstein rings";--in the future, do some more work in this case to handle the Q-Gorenstein setting.   
    --print "test2";
    baseTauList := testModule(S1, AssumeDomain=>true, CanonicalIdeal=>omegaS1List#0);
    --print "test3";
    baseTau := baseTauList#0;
    genList := baseTauList#2;

    --now we have to run the sigma computation
    ( a1, b1, c1 ) := decomposeFraction( pp, n1, NoZeroC => true );
    if (instance(genList, RingElement)) then (
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => {genList}, CanonicalIdeal => omegaS1List#0);
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        if (targetAnswer == answer1) then return false; --we didn't hit the FPT
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => { genList } );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, ceiling( ( pp^b1 - 1 )/( pp - 1 ) ), genList, sub(computedHSLGInitial, ambient S1));
        --print "test5";
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        if not (targetAnswer == answer2) then return false; --we went past the fpt
        return true;
    )
    else if instance(genList, BasicList) then ( -- Karl: I haven't tested this
        tauOmegaSList = testModule(n1, tvar, AssumeDomain=>true, GeneratorList => genList, CanonicalIdeal => omegaS1List#0);
        tauOmegaS = tauOmegaSList#0;        
        answer1 = gradedReesPiece(degShift, tauOmegaS);
        if (targetAnswer == answer1) then return false; --we didn't hit the FPT
        computedHSLGInitial = first FPureModule( { a1/( pp^c1 - 1 ) }, { tvar }, CanonicalIdeal => baseTau, GeneratorList => genList );
        --print "test4";
        computedHSLG = frobeniusRoot(b1, apply(#genList, zz -> ceiling( ( pp^b1 - 1 )/( pp - 1 ) )), genList, sub(computedHSLGInitial, ambient S1));
        --print "test5";        
        answer2 = gradedReesPiece(degShift, computedHSLG*S1);
        if not (targetAnswer == answer2) then return false; --we went past the fpt
        return not(answer1 == answer2);
    );
    error "isFPT (non-principal case): something went wrong with the generator list for the Fedder colon";
);
*-

beginDocumentation()

document {
    Key => "NonPrincipalTestIdeals",
    Headline => "a package for calculations of singularities of pairs in positive characteristic",
	EM "NonPrincipalTestIdeals", " is a package that can compute a test ideal ", TEX ///$\tau(R, I^t)$///, " of a pair ",TEX ///$(R, I^t)$///, " where ", TEX ///$R$///, " is a domain, ", TEX ///$I$///,  " is an ideal, and ", TEX ///$t > 0$///, " is a rational number.  Currently, it works in Q-Gorenstein rings, although some functions (such as checking for F-pure thresholds) are restricted to quasi-Gorenstein strongly F-regular domains.",
    BR{}, BR{},
    "This package reduces the problem to the principal case by the mathematics developed in the preprint ", BR{}, EM "Test Modules of Extended Rees Algebras ", "by Rahul Ajit and Hunter Simper, ", "arXiv:2509.01693.", BR{}, BR{},"After reducing to the principal case, some functions from the ", EM "TestIdeals", " package are used.  Note that this package requires Macaulay2 version 1.25 or later.", BR{}, BR{},
	BOLD "Core functions",
	UL {
		{TO "testIdeal", " computes the test ideal ", TEX ///$\tau(R, I^t)$///,},
		{TO "testModule", " computes the test module ", , TEX ///$\tau(\omega_R, I^t)$///,},
        {TO "testModuleMinusEpsilon", " computes the test module ", TEX ///$\tau(\omega_R, I^{t-\epsilon})$///, " for arbitrary small ", TEX ///$\epsilon > 0$///,},
        {TO "isFJumpingExponentModule", " checks if ", TEX ///$t$///, " is an F-jumping exponent for the test module ", TEX ///$\tau(\omega_R, I^t)$///,},
        {TO "isFPT", " checks if ", TEX ///$t$///, " is the F-pure threshold of the pair ", TEX ///$(R, I)$///,},
	},
     "There are some other functions exported which people may also find useful.", BR{}, BR{},
	BOLD "Other useful functions",
	UL {
		{TO "gradedReesPiece", " computes a graded piece of a homogeneous ideal in a Rees algebra or extended Rees algebra"},
	},
    BR{}, BR{},
    BOLD "Requirements: ", "All functions in this package require the ambient ring to be a reduced equidimensional ring.  This ring must also be presented as a polynomial ring over a field of characteristic ", TEX ///$p > 0$///, " quotiented by an ideal.  Some other functions including ", TT "testIdeal", ", ", TT "isFPT", ", ", TT "isFJumpingNumber", ", ", TT "torsionOrder", ", ", " and ", TT "isInvertibleIdeal", " require the ring to be a normal (or at least G1+S2) domain, and in some cases even more.",
    BR{}, BR{},    
    Contributors => {"Rahul Ajit and Matthew Bertucci also contributed to the development of this package."},
    BR{}, BR{},
    BOLD "History and support: ","This package was started in the 2023-2024 RTG seminar for the NSF RTG grant #1840190 at the University of Utah.  Schwede also received support from NSF grants #2101800 and #2501903 while working on this package."
}

doc ///
    Key
        gradedReesPiece
        (gradedReesPiece, ZZ, Ideal)        
    Headline
        gets a certain degree piece of an ideal in an (extended) Rees algebra
    Usage
        J = gradedReesPiece(n, I)
    Inputs
        n:ZZ
            a natural number
        I:Ideal
            an ideal in an (extended Rees algebra)
    Outputs
        J:Ideal
            an ideal in an the base ring
    Description
        Text
            Given an ideal $J$ in a Rees ring $T = \oplus T_i$ constructed either with @TO classicalReesAlgebra@ or @TO extendedReesAlgebra@, this will take the $n$th graded piece of $J$ and express it as an ideal in $T_0$.
        Example
            R = QQ[x,y];
            J = ideal(x,y);
            S = classicalReesAlgebra(J);
            trim gradedReesPiece(0, ideal(1_S))
            trim gradedReesPiece(1, ideal(1_S))
            trim gradedReesPiece(3, ideal(1_S))
        
            T = extendedReesAlgebra(J);
            trim gradedReesPiece(-1, ideal(1_T))
            trim gradedReesPiece(0, ideal(1_T))
            trim gradedReesPiece(2, ideal(1_T))
        Text
            Note the command @TO basis@ cannot be used in the extended Rees algebra case as it cannot handle rings with both positive and negative degrees.
    Caveat
        This method is peanut-butter-free.
    SeeAlso
        classicalReesAlgebra
        extendedReesAlgebra
        basis
///

doc ///
    Key
        reesCanonicalModule
        (reesCanonicalModule, Ring)       
        [reesCanonicalModule, AmbientCanonical] 
        AmbientCanonical
    Headline
        constructs the graded canonical module in a ring constructed via classicalReesAlgebra or extendedReesAlgebra
    Usage
        M = reesCanonicalModule(S)        
    Inputs
        S:Ring
            constructed with classicalReesAlgebra or extendedReesAlgebra        
    Outputs
        M:Module
            the graded canonical module over S
    Description
        Text
            Computes the graded canonical modules of a ring constructed with @TO classicalReesAlgebra@ or @TO extendedReesAlgebra@ or 
        Example
            R = QQ[x,y];
            J = ideal(x^2,x*y,y^2);            
            S = classicalReesAlgebra(J);
            T = extendedReesAlgebra(J);
            reesCanonicalModule(S)
            reesCanonicalModule(T)
        Text
            The option AmbientCanonical is used to specify the canonical module of {\tt ambient S}.  
    Caveat
        One should use this function and not other similar functions to create canonical modules in extended Rees algebras, especially in older versions of Macaulay2.  This is because core Macaulay2 Ext function sometimes gives the wrong answer in rings with variables of negative degrees.  See this github issue @HREF "https://github.com/Macaulay2/M2/issues/3180"@
    SeeAlso
        classicalReesAlgebra
        extendedReesAlgebra
        gradedReesPiece
///

doc ///
    Key
        reesModuleToIdeal
        (reesModuleToIdeal, Ring, Module)    
        [reesModuleToIdeal, MTries]    
    Headline
        embeds a homogeneous rank 1 module as an ideal in a Rees algebra
    Usage
        (I,d,phi) = reesModuleToIdeal(S, M)
    Inputs
        S:Ring
            constructed with classicalReesAlgebra or extendedReesAlgebra        
        M:Module
            a homogeneous S-module
    Outputs
        I:Ideal
            an ideal of S isomorphic to M as a module
        d:ZZ
            the degree shift between the module and ideal
        phi:Matrix
            the map M to S^1 whose image is I
    Description
        Text
            In the following example we embed the canonical module of an extended Rees algebra and then embed it as an ideal.  Note there is a degree shift.
        Example
            R = QQ[x,y];
            J = ideal(x^2,x*y^2,y^3);            
            S = extendedReesAlgebra(J);
            canMod = reesCanonicalModule(S)
            L = reesModuleToIdeal(S, canMod) --the first entry -1 of {-1,-2} refers to the Rees algebra degree
            L#2 -- the map from canMod to S 
        Text
            The {\tt MTries} option specifies how many attempts to make before giving up.
    SeeAlso
        classicalReesAlgebra
        extendedReesAlgebra
        gradedReesPiece
///

doc ///
    Key
        (testIdeal, QQ, Ideal)
        (testIdeal, ZZ, Ideal)
    Headline
        computes the test ideal of a pair
    Usage
        J = testIdeal(t, I)
    Inputs
        t:QQ
            a rational number
        I:Ideal
            an ideal
    Outputs
        J:Ideal
            an ideal, the test ideal
    Description
        Text
            This computes the test ideal $\tau(R, I^t)$ of an ideal $I$ in a normal quasi-Gorenstein domain $R$.  We begin with example in a regular ring.
        Example
            R = ZZ/5[x,y];
            I = ideal(x^2, y^3);
            testIdeal(5/6, I)
            testIdeal(5/6-1/25, I)
            testIdeal(2, I)
        Text
            We now include an example in a singular ring.
        Example
            R = ZZ/3[x,y,z]/ideal(x^2-y*z);
            I = ideal(x,y);
            testIdeal(1, I)
            I2 = ideal(x,y,z);
            testIdeal(3/2,I2)
    SeeAlso
        testIdeal
        (testModule, QQ, Ideal)
        testModule
///

doc ///
    Key
        (testModule, QQ, Ideal)
        (testModule, ZZ, Ideal)
    Headline
        computes the test ideal of a pair
    Usage
        J = testModule(t, I)
    Inputs
        t:QQ
            a rational number
        I:Ideal
            an ideal
    Outputs
        L:List
            a list with the test module and the canonical module
    Description
        Text
            This computes the test module $\tau(\omega_R, I^t)$ of an ideal $I$ in a domain $R$ of index not divisible by the characteristic $p > 0$.  
            It returns a list with two entries.  The first is the ideal of $R$ isomorphic viewed as an ideal of $R$ isomorphic to $\tau(\omega_R, J^t)$. The second is an ideal $R$ isomorphic to $\omega_R$, in which the first module sits as a subideal.
        Text
            We begin with example in a regular ring.
        Example
            R = ZZ/5[x,y];
            I = ideal(x^2, y^3);
            testModule(5/6, I)
            testModule(5/6-1/25, I)
        Text
            We now include an example in a non-Gorenstein ring
        Example
            T = ZZ/2[a,b,c,d];
            S = ZZ/2[x,y];
            f = map(S, T, {x^3, x^2*y, x*y^2, y^3});
            R = T/(ker f);
            m = ideal(a,b,c,d);
            testModule(1, m)
            testModule(1-1/16, m)
    SeeAlso
        testModule
        (testModule, QQ, Ideal)
        testModuleMinusEpsilon
        testIdeal
        (testIdeal, QQ, Ideal)        
///

doc ///
    Key 
        classicalReesAlgebra
        (classicalReesAlgebra, Ideal)
    Headline
        computes the flattened Rees algebra 
    Usage
        S = classicalReesAlgebra(J)
    Inputs
        J:Ideal
    Outputs
        S:Ring
    Description
        Text
            This function calls the function reesAlgebra from the package ReesAlgebras and formats it for our purposes. 
        Text
            The difference is this ring is flattened, and there are certain keys added for obtaining information about this Rees algebra, as demonstrated in the example below.
        Example
            R = QQ[x,y,z];
            J= ideal(x^2,y);
            S1 = reesAlgebra J;
            describe S1
            S2 = classicalReesAlgebra J;
            describe S2
            degrees S2
        Text
            BaseRing provides the ring where we blew up the ideal.  Degree1 is the generators of the degree 1 part of the Rees algebra.  BaseRingList is the list of generators of the ideal we blew up. 
    Caveat
        Currently, this only works for singly graded base rings.
    SeeAlso
        reesAlgebra
        extendedReesAlgebra
///

doc ///
    Key 
        extendedReesAlgebra
        (extendedReesAlgebra, Ideal)
    Headline
        computes the flattened extended Rees algebra
    Usage
        S = extendedReesAlgebra(J)
    Inputs
        J:Ideal
    Outputs
        S:Ring
    Description
        Text
            This function creates an extended Rees algebra.  Unlike the reesAlgebra command, this function creates a flattened ring with certain other keys added for the convenience of other functions in this package.        
        Example
            R = QQ[x,y,z];
            J= ideal(x^2,y);
            S2 = extendedReesAlgebra J;
            describe S2
            degrees S2        
    Caveat
        Currently, this only works for singly graded base rings.
    SeeAlso
        reesAlgebra
        classicalReesAlgebra
///

doc ///
    Key 
        testModuleMinusEpsilon
        (testModuleMinusEpsilon, ZZ, Ideal)
        (testModuleMinusEpsilon, QQ, Ideal)        
    Headline
        computes the (parameter) test module of a pair for values arbitrarily close to, but below, t
    Usage
        M = testModuleMinusEpsilon(t, J)
    Inputs
        t:QQ
            the exponent we are interested in
        J:Ideal
            the ideal were are interested in
    Outputs
        S:Ring
    Description
        Text
            This function computes $\tau(\omega_R, J^{t-\epsilon})$, the test module of the pair $(R, J^{t-\epsilon})$ for $1 \gg \epsilon > 0$.  It returns a list with two entries.  The first is the ideal of $R$ isomorphic viewed as an ideal of $R$ isomorphic to $\tau(\omega_R, J^t)$. The second is an ideal $R$ isomorphic to $\omega_R$, in which the first module sits as a subideal.
        Example
            R = ZZ/5[x,y,z];
            J = ideal(x^2,y^2,z^2);
            testModuleMinusEpsilon(3/2,J)
            apply(oo, JJ -> trim JJ)
            testModule(3/2, J)               
        Text
            In the above example, one can see that 3/2 is the F-pure threshold of the given ideal $J$, as expected.        
    Caveat
        There is not a guarantee different calls to the function will always produce the same ambient canonical module.
    SeeAlso
        testModule
///

doc ///
    Key
        isFJumpingExponentModule
        (isFJumpingExponentModule, QQ, Ideal)
        (isFJumpingExponentModule, ZZ, Ideal)  
        (isFJumpingExponent, QQ, Ideal)
        (isFJumpingExponent, ZZ, Ideal)        
        [isFJumpingExponentModule, AtOrigin]
        [isFJumpingExponentModule,FrobeniusRootStrategy]
        [isFJumpingExponentModule,Verbose]
    Headline
        decides if a rational number is a jumping exponent of a generalized parameter test module
    Usage
        b = isFJumpingExponentModule(t, J)
    Inputs
        t:QQ
            the exponent
        J:Ideal
            the ideal of the pair
    Outputs
        b:Boolean
    Description
        Text
            Given an ideal $J$ in a ring $R$ and a rational number $t$, this function determines if $\tau(\omega_R, J^t) \neq \tau(\omega_R, J^{t-\epsilon})$ for all $1 \gg \epsilon > 0$.  If so, it returns true, that is $t$ is a jumping exponent of $\tau(\omega_R, J^*)$, and otherwise it returns false.
        Example
            R = ZZ/3[x,y,z];
            J = ideal(x^3,y^4,z^5);
            isFJumpingExponent(1/3+1/4+1/5, J) -- should be true, this is the fpt = lct
            isFJumpingExponent(1/3+1/4, J) -- should be false
        Text
            The option {\tt AtOrigin} (default) checks whether the jump happens is done at the origin (as opposed to anywhere).
        Example
            R = ZZ/3[x,y];
            J = ideal(x-1,y);
            isFJumpingExponentModule(2, J, AtOrigin=>false)
            isFJumpingExponentModule(2, J, AtOrigin=>true)
        Text
            The formulation {\tt isFJumpingExponent(t, J)} also computes whether the test ideal jumps at $t$.  However, it only works if $R$ is a quasi-Gorenstein normal domain as in that case the jumping numbers of $\tau(R, J^t)$ agree with those of $\tau(\omega_R, J^t)$.
    SeeAlso
        isFJumpingExponent
        (isFPT, QQ, Ideal)
///

doc ///
    Key
        manualExt
        (manualExt, ZZ, Module, Module)
    Headline
        computes the Ext module of two modules as the core Ext function sometimes does not work properly in the negatively graded case
    Usage
        M = manualExt(t, N, P)
    Inputs
        t:ZZ
            the homological degree
        N:Module
            a module
        P:Module
            a module
    Outputs
        M: Module
    Description
        Text
           Given two modules $N$ and $P$ over a ring $R$ and an integer $t$, this function computes and returns ${Ext}_R^t(N,P)$.
        Example
            R = S = QQ[X,T,Degrees=>{1,-1}];
            N = S^1/ideal(X*T-1);
            P = S^1;
            manualExt(1, N, P) 
            Ext^1(N,P)          
    SeeAlso
        Ext        
///

doc ///
    Key
        isInvertibleIdeal
        (isInvertibleIdeal, Ideal)
    Headline
        checks whether an ideal is locally principal (invertible)
    Usage
        b = isInvertibleIdeal(I)
    Inputs
        I: Ideal
    Outputs
        b: Boolean
    Description
        Text
            Given an ideal $I$, this function returns true if $I$ is locally principal, and false otherwise.  It will work in a normal domain (or more generally a G1 + S2 domain reduced equidimensional ring).
        Example
            R = QQ[x,y]/(y^2-x*(x-1)*(x+1)); --an elliptic curve
            I = ideal(x,y); --corresponding to a point, should be invertible, even if not principal
            isInvertibleIdeal(I)--should be true
            S = QQ[u,v];
            J = ideal(u,v);
            isInvertibleIdeal(J) --should be false
        Text
            This frequently also works in non-domains.  
        Example
            R = QQ[w..z]/ideal(x*y,x*z,y*z,x^2-y^2,x^2-z^2);
            isInvertibleIdeal(ideal(w,x)) -- should be false
            isInvertibleIdeal(ideal(x,y,z)) -- should be true
    SeeAlso
///

doc ///
    Key
        torsionOrder
        (torsionOrder, ZZ, Ideal)
    Headline
        computes the local torsion order of an ideal in a normal domain
    Usage
        (m,J) = torsionOrder(n, I)
    Inputs
        n: ZZ
            the maximum order to check
        I: Ideal
            the ideal you want to check
    Outputs
        m: ZZ
            the local torsion order of the ideal in the class group, or 0 if not found
        J: Ideal
            the reflexive hull of $I^n$
    Description
        Text
            Given an ideal $I$ in a normal (or G1 + S2) domain, this computes the local torsion order of the class of $I$.  In other words, this computes the smallest integer $m$ such that $I^{m}$ is locally principal up to reflexification (that is, $(I^{m})^{**}$ is locally principal).  
        Text
            This function returns a pair $(m, (I^{m})^{**})$ if such an $m$ is found.  If no $m$ is found, this returns $(0, ideal 0)$.
        Example
            R = QQ[x,y,z]/ideal(x*y+z^7);
            I = ideal(x,z);
            torsionOrder(10, I) --the order should be 7
            torsionOrder(5, I) --we only check up to 5, so this should return 0        
        Text
            As we are computing the reflexive hull, this function does not distinguish between the ideal $I$ and the ideal made up of the height-one components of a primary decomposition of $I$.
        Example
            R = ZZ/5[a,b,c]/ideal(a*b-c^8)
            torsionOrder(10, (ideal(a,b,c))^2*(ideal(a,c)))
///

doc ///
    Key
        isFRationalThreshold
        (isFRationalThreshold, QQ, Ideal)
        (isFRationalThreshold, ZZ, Ideal)
        (isFPT, QQ, Ideal)
        (isFPT, ZZ, Ideal)
        [isFRationalThreshold, AtOrigin]
        [isFRationalThreshold, FrobeniusRootStrategy]
        [isFRationalThreshold, Verbose]
    Headline
        determines if a given number is the F-rational threshold, or, equivalently in the case of a quasi-Gorenstein ring, the F-pure threshold
    Usage
        b = isFRationalThreshold(t, I)
        b = isFPT(t,I)
    Inputs 
        t: QQ
            the number which might equal the threshold
        I: Ideal
            the ideal whose threshold you want to compute
    Outputs
        b: Boolean
            is it the threshold or not
    Description
        Text
            Given an ideal $I$ in a domain $R$, this determines whether a rational $t$ is the $F$-rational threshold, that is whether or not $\tau(\omega_R, I^t) \neq \omega_R$ while $\tau(\omega_R, I^{t-\epsilon}) = \omega_R$.  In the case of a quasi-Gorenstein strongly $F$-regular ring (ie, if $\omega_R$ is an invertible ideal, for instance if its principal), this is equivalent to the $F$-pure threshold of $(R, I)$.  Hence, you may also call {\tt isFPT(R, I)} in that case.
        Example
            R = ZZ/3[x,y,z];
            I = ideal(x^2,y^3,z^5); --fpt should be 1/2 + 1/3 + 1/5 = 31/30
            isFPT(31/30, I)
            isFRationalThreshold(31/30, I)
            isFRationalThreshold(1, I)
            isFPT(32/30, I)
        Text
            The option {\tt AtOrigin} says that the computation must only be checked at the origin.
        Example
            R = ZZ/3[u,v]
            I = ideal( (u-1)^2, v^3) --fpt should be 1/2+1/3 = 5/6, but it's not the threshold at the origin
            isFPT(5/6, I)
            isFRationalThreshold(5/6, I, AtOrigin=>true)
        Text
            The {\tt Verbose} option turns on debugging output if true.  The option {\tt FrobeniusRootStrategy} is passed on to the {\tt TestIdealsPackage}.
    SeeAlso
        isFPT
        isFJumpingExponent
        isFJumpingExponentModule

///

TEST /// --check #0, monomial ideals, dimension 2
    needsPackage "MultiplierIdeals";
    S = QQ[a,b];
    J = monomialIdeal(a^2,b^3);
    J1 = multiplierIdeal(J, 5/4);
    J2 = multiplierIdeal(J, 5/6);
    J3 = multiplierIdeal(J, 13/12);
    J4 = multiplierIdeal(J, 2);
    R = ZZ/5[x,y];
    I = ideal(x^2,y^3);
    I1 = testIdeal(5/4, I);
    I2 = testIdeal(5/6, I);
    I3 = testIdeal(13/12, I);
    I4 = testIdeal(2, I);
    phi = map(S, R, {a,b});
    assert(phi(I1)==J1);
    assert(phi(I2)==J2);
    assert(phi(I3)==J3);
    assert(phi(I4)==J4);
    assert(I1*I == testIdeal(9/4, I));--testing Skoda
///

TEST /// --check #1, monomial ideals, dimension 3
    needsPackage "MultiplierIdeals";
    S = QQ[a,b,c];
    J = monomialIdeal(a^2,b^3,c^4);
    J1 = multiplierIdeal(J, 5/4);
    J2 = multiplierIdeal(J, 13/12);
    J3 = multiplierIdeal(J, 21/10);
    J4 = multiplierIdeal(J, 2);
    R = ZZ/7[x,y,z];
    I = ideal(x^2,y^3,z^4);
    I1 = testIdeal(5/4, I);
    I2 = testIdeal(13/12, I);
    I3 = testIdeal(21/10, I);
    I4 = testIdeal(2, I);
    phi = map(S, R, {a,b,c});
    assert(phi(I1)==J1);
    assert(phi(I2)==J2);
    assert(phi(I3)==J3);
    assert(phi(I4)==J4);
///

TEST /// --check #2, monomial ideals, dimension 4
    needsPackage "MultiplierIdeals";
    S = QQ[a,b,c,d];
    J = monomialIdeal(a^3,b^2*c,c^3,d^3*c^2);
    J1 = multiplierIdeal(J, 2/3);
    J2 = multiplierIdeal(J, 5/4);
    J3 = multiplierIdeal(J, 11/8);
    J4 = multiplierIdeal(J, 2);
    R = ZZ/3[x,y,z,w];
    I = ideal(x^3,y^2*z,z^3,w^3*z^2);
    I1 = testIdeal(2/3, I);
    I2 = testIdeal(5/4, I);
    I3 = testIdeal(11/8, I); 
    I4 = testIdeal(2, I); 
    phi = map(S, R, {a,b,c,d});
    assert(phi(I1)==J1);
    assert(phi(I2)==J2);
    assert(phi(I3)==J3);
    assert(phi(I4)==J4);
///

TEST /// --check #3, non-monomial ideals, dimension 3
    --there is no reason these should agree in general, but they seem to in this case
    needsPackage "Dmodules";
    S = QQ[a,b,c];
    J = ideal(a^2+b^2,b^3,c^2+a^2);    
    J2 =  ideal(c,b,a); --produced via a slow call to multiplierIdeal(J, 3/2);    
    J4 = ideal(c^2,b*c,a*c,b^2,a*b,a^2); -- produced via a slow call to multiplierIdeal(J, 2);
    R = ZZ/5[x,y,z];
    I = ideal(x^2+y^2, y^3, z^2+x^2);    
    I2 =  testIdeal(3/2, I);
    I4 =  testIdeal(2, I);
    phi = map(S, R, {a,b,c});    
    assert(phi(I2) == J2);    
    assert(sub(phi(I4), S) == J4);
///

TEST /// --check #4, ambient singular ring, dimension 2, A1 singularity
    R = ZZ/2[x,y,z]/ideal(x^2-y*z);
    J = ideal(x,y,z);
    m = ideal(x,y,z);
    uI = ideal(sub(1,R));
    assert(testIdeal(10/11, J) == uI);
    assert(testIdeal(1/1, J) == m);
    assert(testIdeal(17/16, J) == m);    
    assert(testIdeal(2, J) == m^2);    
///

TEST /// --check #5, ambient singular ring, dimension 2, E6 singularity (see [TW, Example 2.5])
    R = ZZ/5[x,y,z]/ideal(x^2+y^3+z^4);
    J = ideal(x,y,z);
    m = ideal(x,y,z);      
    assert(testIdeal(1/3-1/30, J) == m);    
///

TEST /// --check #6, ambient singular ring, dimension 2, E7 singularity (see [TW, Example 2.5])
    R = ZZ/5[x,y,z]/ideal(x^2+y^3+y*z^3);
    J = ideal(x,y,z);
    m = ideal(x,y,z);
    uI = ideal(sub(1,R));    
    --assert(testIdeal(1/5, J) == uI);
    --assert(testIdeal(1/4, J) == m);    
    assert(isFPT(1/4, J))
///

TEST /// --check #7, dim 4, codim 2 ideal (non-m-primary)
    R = ZZ/2[x,y,z,w];
    J = (ideal(x,y))*(ideal(z,w))*(ideal(x,w));
    --J1 = testIdeal(3/2, J); -- this one is too slow
    J2 = testIdeal(2/1, J);
    J3 = testIdeal(11/8, J);
    needsPackage "MultiplierIdeals";
    S = QQ[a,b,c,d];
    I = (monomialIdeal(a,b))*(monomialIdeal(c,d))*(monomialIdeal(a,d));
    --I1 = ideal multiplierIdeal(I,  3/2);
    I2 = ideal multiplierIdeal(I,  2/1);
    I3 = ideal multiplierIdeal(I, 11/8);
    phi = map(S, R, {a,b,c,d});    
    assert(phi(J2)==I2);
    assert(phi(J3)==I3);
///

TEST /// --check #8, dim 4, mixed ideal (some height one components)
    R = ZZ/2[x,y,z,w];
    J = (ideal(x^2,y))*(ideal(y^2,z,w^2));
    J1 = testIdeal(3/2, J);
    J2 = testIdeal(2/1, J);
    needsPackage "MultiplierIdeals";
    S = QQ[a,b,c,d];
    I = (monomialIdeal(a^2,b))*(monomialIdeal(b^2,c,d^2));
    I1 = ideal multiplierIdeal(I, 3/2);
    I2 = ideal multiplierIdeal(I, 2/1);
    phi = map(S, R, {a,b,c,d});
    assert(phi(J1)==I1);
    assert(phi(J2)==I2);
///

TEST /// --check #9, interesting toric construction, 
    R = ZZ/3[x,y,z]/ideal(x^2-y*z);
    J = (ideal(x,z))*(ideal(x,y,z));
    I = (ideal(x,z));
    I1 = (ideal(z));
    assert(not isFPT(1/3, J));
    assert(isFPT(1/2, J));
    assert(sub(testIdeal(1/1, J), R) == sub((ideal(x,z))*(ideal(x,y,z)),R));
    assert(sub(testIdeal(1/1, I), R) == testIdeal(1/2, I1));
///

TEST /// --check #10, Q-Gorenstein, index 3
--loadPackage "NonPrincipalTestIdeals"
T = ZZ/2[a,b,c,d];
S = ZZ/2[x,y];
f = map(S, T, {x^3, x^2*y, x*y^2, y^3});
R = T/(ker f);
m = ideal(a,b,c,d);
assert (testIdeal(2/3, m) == m);
assert (testIdeal(21/32, m) == ideal(sub(1, R)));
///

TEST /// --check #11, Q-Gorenstein, index 2
T = ZZ/3[a,b,c,d,e,f];
S = ZZ/3[x,y,z];
f = map(S, T, {x^2, x*y,x*z,y^2,y*z,z^2});
R = T/(ker f);
m = ideal(a,b,c,d,e,f);
n = ideal(a,d,f); --an ideal with the same integral closure as m
assert(testIdeal(3/2, n) == m);
--assert(testIdeal(40/27, n) == ideal(sub(1,R)));
///

TEST /// --check #12, checking basics of the gradedReesPiece
--checking basics
R = QQ[x,y];
J = ideal(x,y);
S = classicalReesAlgebra(J);
T = extendedReesAlgebra(J);
--we check gradedReesPiece

IS = ideal(1_S);
base = gradedReesPiece(0, IS);
use R
assert(base*(ideal(x,y))^1 == gradedReesPiece(1, IS))
assert(base*(ideal(x,y))^3 == gradedReesPiece(3, IS))

IT = ideal(1_T);
base = gradedReesPiece(0, IT);
assert(base == gradedReesPiece(-1, IT))
assert(base == gradedReesPiece(-3, IT))
assert(base*(ideal(x,y))^1 == gradedReesPiece(1, IT))
assert(base*(ideal(x,y))^3 == gradedReesPiece(3, IT))


R = QQ[a,b,c];
J = (ideal(a,b,c))^2;
S = classicalReesAlgebra(J);
T = extendedReesAlgebra(J);
IS = ideal(1_S);
base = gradedReesPiece(0, IS);
use R
assert(base*(ideal(a,b,c))^2 == gradedReesPiece(1, IS))
assert(base*(ideal(a,b,c))^6 == gradedReesPiece(3, IS))


IT = ideal(1_T);
base = gradedReesPiece(0, IT);
assert(base == gradedReesPiece(-1, IT))
assert(base == gradedReesPiece(-3, IT))
assert(base*(ideal(a,b,c))^2 == gradedReesPiece(1, IT))
assert(base*(ideal(a,b,c))^6 == gradedReesPiece(3, IT))
///

TEST /// --check #13, checking the basics of constructing canonical modules on simple Rees algebras
R = QQ[x,y,z];
J = ideal(x,y,z);
S = classicalReesAlgebra(J);
omegaS = reesCanonicalModule(S);
(omegaSIdeal, d, phi ) =  reesModuleToIdeal(S, omegaS);
degShift = d#0; --this is how far we have to shift
assert(0 == trim gradedReesPiece(0+degShift, omegaSIdeal)); -- the a invariant of the Rees algebra should be -1, so this should be zero.
baseOmega = trim gradedReesPiece(1+degShift, omegaSIdeal);  --this should be a free module
assert(isInvertibleIdeal(baseOmega)); --this is a Gorenstein rational singularity
assert(baseOmega == trim gradedReesPiece(2+degShift, omegaSIdeal)); --discrepancy 2, so nothing should change
use R;
assert((ideal(x,y,z))*baseOmega == trim gradedReesPiece(3+degShift, omegaSIdeal)); --but we should get a jump here.
T = extendedReesAlgebra(J);
omegaT = reesCanonicalModule(T);
(omegaTIdeal, d, psi ) =  reesModuleToIdeal(T, omegaT);
degShift = d#0; --this is how far we have to shift
baseOmega = trim gradedReesPiece(-10+degShift, omegaTIdeal);  --this should be a free module
assert(isInvertibleIdeal(baseOmega));
assert(baseOmega == trim gradedReesPiece(1+degShift, omegaTIdeal));
assert(baseOmega == trim gradedReesPiece(2+degShift, omegaTIdeal));
use R;
assert((ideal(x,y,z))*baseOmega == trim gradedReesPiece(3+degShift, omegaTIdeal)); --but we should get a jump here.
///

TEST ///--check #14, jumping number computations
R = ZZ/3[x,y,z];
m = ideal(x,y,z);
assert(isFJumpingExponentModule(5/2, m) == false);
assert(isFJumpingExponent(3, m) == true);
--
S = ZZ/5[a,b];
J = (ideal(a,b))*(ideal(a^2,b))^2;--this is a monomial ideal, we know the log resolution, computing by hand means that the lct = fpt should be min(2/3,3/5) = 3/5.
L = testModule(3/5, J)
assert((ideal(a,b))*(L#1) == L#0)
K = testModuleMinusEpsilon(3/5, J)
assert(K#1 == K#0)
assert(isFJumpingExponentModule(3/5, J))
--singular ambient example
T = ZZ/2[x,y,z]/ideal(x^2 -y*z);
J = ideal(x^3,y^3,z^3);
assert(isFJumpingExponent(1/3, J));
///

TEST ///--check #15, checking isFJumpingExponentModule non-homogeneous ideals
    R = ZZ/5[x,y];
    m = ideal((x-1)^2,y^2);
    assert(isFJumpingExponentModule(3/2, m));
    assert(not isFJumpingExponent(4/5, m));
    R = ZZ/2[a,b];
    J = ideal((a-2)^2,(b-1)^3);
    assert(isFJumpingExponentModule(5/6,J)); 
    assert(isFJumpingExponent(5/6,J));
    assert(not isFJumpingExponent(3/4,J)); 
///

TEST /// --#16, checking isFRationalThreshold
R = ZZ/3[x,y];
J = ideal(x^2,y^3);
assert (isFRationalThreshold(5/6, J));
assert (isFPT(5/6, J));
assert (not isFRationalThreshold(2/3, J));
assert( not isFPT(8/9, J));
assert (not isFPT(1+5/6, J));
assert (not isFPT(2/3, J));
assert( not isFPT(8/9, J));
assert (not isFPT(1+5/6, J));
///

TEST ///--#17, checking a non-polynomial FPT
R = ZZ/3[x,y,z]/ideal(x*y-z^2); --threshold should be 1
J = ideal(x,y,z);
assert(isFPT(1/1,J))
///

TEST ///--#18, checking AtOrigin
    R = ZZ/3[x,y];
    J = ideal(x-1,y);
    assert(isFJumpingExponentModule(2, J, AtOrigin=>false));
    assert(not isFJumpingExponentModule(2, J, AtOrigin=>true));
///

TEST ///--#19, checking various outputs
    R = ZZ/2[x,y,z];
    I = ideal(x^2,y^5,z^7); --fpt should be 1/2 + 1/5 + 1/7 = 59/70
    assert(isFPT(59/70, I))
    assert(not isFRationalThreshold(1, I))
///

TEST /// --#20, more checking AtOrigin
R = ZZ/3[u,v]
I = ideal( (u-1)^2, v^3) --fpt should be 1/2+1/3 = 5/6, but it's not the threshold at the origin
assert(isFPT(5/6, I))
assert(not isFRationalThreshold(5/6, I, AtOrigin=>true))
///

TEST ///-- #21, checking torsion order and isInvertibleIdeal
R = QQ[x,y,z]/ideal(x*y+z^5);
I = ideal(x,z);
tList = torsionOrder(7, I);
assert(tList#0 == 5);
assert(tList#1 == ideal(x));
assert((torsionOrder(4,I))#0 == 0);
assert(not isInvertibleIdeal(I));
assert(isInvertibleIdeal(tList#1));
J = (ideal gens R)^2*I
tList = torsionOrder(7, J);
assert(tList#0 == 5);
assert(not isInvertibleIdeal(J));
///


end--
loadPackage "NonPrincipalTestIdeals"

