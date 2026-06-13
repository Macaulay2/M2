
------------------------------------------------------------------------
--------------------- Associated K3 and Castelnuovo surfaces -----------
------------------------------------------------------------------------

SurfaceAssociatedToRationalFourfold = new Type of EmbeddedProjectiveVariety;
WeightedSurfaceAssociatedToRationalFourfold = new Type of WeightedProjectiveVariety;

globalAssignment SurfaceAssociatedToRationalFourfold;
globalAssignment WeightedSurfaceAssociatedToRationalFourfold;

WeightedSurfaceAssociatedToRationalFourfold.synonym = SurfaceAssociatedToRationalFourfold.synonym = "surface associated to rational fourfold";

expression WeightedSurfaceAssociatedToRationalFourfold := expression SurfaceAssociatedToRationalFourfold := U -> (
    X := recoverFourfold U;
    (S,F) := if instance(X,CubicFourfold)
             then ("K3 surface","cubic fourfold")
             else if instance(X,GushelMukaiFourfold)
             then ("K3 surface","GM fourfold")
             else if instance(X,IntersectionOfThreeQuadricsInP7)
             then ("Castelnuovo surface","complete intersection of 3 quadrics in PP^7")
             else ("surface","fourfold");
    if dim U == -1 then S = "not-fully-calculated "|S;
    A := if hasAttribute(X,ReverseDictionary) then toString getAttribute(X,ReverseDictionary) else (F|" of discriminant "|toString(discriminant X));
    expression(S|" associated to "|A)
);

net WeightedSurfaceAssociatedToRationalFourfold := net SurfaceAssociatedToRationalFourfold := U -> (
    if hasAttribute(U,ReverseDictionary) then return toString getAttribute(U,ReverseDictionary);
    if dim U >= 0 then return ? U;
    "-* some calculations are missing *-"
);
texMath WeightedSurfaceAssociatedToRationalFourfold := texMath SurfaceAssociatedToRationalFourfold := texMath @@ net;

describe WeightedSurfaceAssociatedToRationalFourfold := describe SurfaceAssociatedToRationalFourfold := S -> (
    out := "Fourfold: ";
    if hasAttribute(recoverFourfold S,ReverseDictionary) then out = out|(toString getAttribute(recoverFourfold S,ReverseDictionary))|", ";
    (out|shortDescriptionFourfold(recoverFourfold S,false))||(describeMirrorFourfoldAndK3 recoverFourfold S)
);

makeSurfaceAssociated = (X,mu,U,C,f) -> (
    assert(instance(X,HodgeSpecialFourfold) and instance(mu,MultirationalMap) and instance(U,EmbeddedProjectiveVariety) and instance(C,List) and (instance(f,Nothing) or instance(f,MultirationalMap)));
    S := if f =!= null then image f else projectiveVariety((coefficientRing X)[],Saturate=>false);
    S = if instance(S,WeightedProjectiveVariety) then new WeightedSurfaceAssociatedToRationalFourfold from S else new SurfaceAssociatedToRationalFourfold from S;
    S.cache#"construction of SurfaceAssociatedToRationalFourfold" = (mu,U,C,f);
    S.cache#"Hodge-special fourfold" = X;
    if f =!= null then (
        if instance(X,DoublySpecialCubicFourfold) then mu.cache#("AssociatedUnderlyingK3",X) = S else X.cache#"AssociatedSurfaceCompleteData" = S;
    ) else (
        if instance(X,DoublySpecialCubicFourfold) then mu.cache#("AssociatedK3PartialData",X) = S else X.cache#"AssociatedSurfacePartialData" = S;
    );
    return S;
);

building = method();
building WeightedSurfaceAssociatedToRationalFourfold := building SurfaceAssociatedToRationalFourfold := S -> S.cache#"construction of SurfaceAssociatedToRationalFourfold";

recoverFourfold = method();
recoverFourfold WeightedSurfaceAssociatedToRationalFourfold := recoverFourfold SurfaceAssociatedToRationalFourfold := S -> S.cache#"Hodge-special fourfold";

printFinalLogSurface = strSurf -> (
    Tstart := currentTime();
    TstartCPU := cpuTime();
    U -> (
        Tend := currentTime() - Tstart;
        TendCPU := cpuTime() - TstartCPU;
        U.cache#"computationTime" = (Tend, TendCPU);
        X := recoverFourfold U;
        isStandard := if instance(X,CubicFourfold) or instance(X,GushelMukaiFourfold) then isStandardK3surface U else true;
        statusOut := if dim U == 2 and isStandard
                     then (" ✦ ", "successfully completed")
                     else if dim U == 2
                     then (" ⟐ ", "completed")
                     else (" ✧ ", "completed (partial data)");
        << first statusOut << strSurf << last statusOut << " in " << humanReadableSeconds(Tend) << " (cpu: " << humanReadableSeconds(floor TendCPU) << ")" << endl;
    )
);

associatedK3surface = method(Options => {Verbose => false, Strategy => null, Singular => null});
associatedCastelnuovoSurface = method(Options => {Verbose => false, Strategy => null, Singular => null});

associatedK3surface CubicFourfold := associatedK3surface GushelMukaiFourfold := associatedCastelnuovoSurface IntersectionOfThreeQuadricsInP7 := o -> X -> (
    if X.cache#?"AssociatedSurfaceCompleteData" then return X.cache#"AssociatedSurfaceCompleteData";
    if X.cache#?"AssociatedSurfacePartialData" then (
        UtildeP := X.cache#"AssociatedSurfacePartialData";
        return buildAssociatedSurfaceFromPartialData(UtildeP,Verbose=>o.Verbose);
    );
    strSurf := if instance(X,IntersectionOfThreeQuadricsInP7) then "Castelnuovo " else "K3 ";
    if o.Verbose then (
        printFinalLog := printFinalLogSurface("associated "|strSurf);
        << "-- starting associated " << strSurf << "computation" << endl;
        << "-- input: fourfold containing a " << surfaceDescription surface X << endl;
        printInfoOnPlannedSteps X;
    );
    if (not X.cache#?(surface X,"label")) and o.Verbose then << "-- recognizing the fourfold" << endl;
    recognize X;
    if o.Verbose then (
        if X.cache#(surface X,"label") === "NotRecognized" then << "-- fourfold not recognized" << endl else << "-- the fourfold has been successfully recognized" << endl;
    );
    fanoMap(X,Singular=>o.Singular,Verbose=>o.Verbose);
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>o.Strategy);
    U := ambientVariety L;
    mu := multirationalMap fanoMap X;
    f := contractionMap(U,X,Verbose=>o.Verbose,Strategy=>o.Strategy,"ForceNormalization"=>false);
    Utilde := makeSurfaceAssociated(X,mu,U,{L,C},f);
    if o.Verbose then printFinalLog Utilde;
    Utilde
);

associatedUnderlyingK3Raw = method(TypicalValue => SurfaceAssociatedToRationalFourfold, Options => {Verbose => true, Strategy => null, FanoMapType => null});
associatedUnderlyingK3Raw DoublySpecialCubicFourfold := o -> X -> (
    mu := getCachedFanoMapIfCompatible(X,o.FanoMapType);
    if mu =!= null and mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if o.Verbose then (
        printFinalLog := printFinalLogSurface "underlying K3 ";
        << "-- starting underlying K3 computation" << endl;
        << "-- input: cubic fourfold containing two surfaces:" << endl;
        << ("  -- " | surfaceDescription first surfaces X) << endl;
        << ("  -- " | surfaceDescription last surfaces X) << endl;
        << "-- settings: Verbose => " << o.Verbose << ", Strategy => " << (if instance(o.Strategy, String) then "\"" | o.Strategy | "\"" else toString(o.Strategy)) << ", FanoMapType => " << (if instance(o.FanoMapType, String) then "\"" | o.FanoMapType | "\"" else toString(o.FanoMapType)) << endl;
        << "-- available strategies: \"Inverse\", \"Approximate\"" << endl;
        printInfoOnPlannedSteps X;
        if not any(surfaces X, isPlaneInP5) then << "-- warning: no plane detected; trying K3 computation anyway" << endl;
    );
    if mu === null then (
        if o.FanoMapType === "Standard" then setFanoMapToBeStandard(X,Verbose=>o.Verbose);
        if o.FanoMapType === "P2xP2" then setFanoMapToBeMapFromP5toP2xP2(X,Verbose=>o.Verbose);
        mu = fanoMapDSCF(X,Verbose=>o.Verbose);
    );
    if mu.cache#?("AssociatedUnderlyingK3",X) then return mu.cache#("AssociatedUnderlyingK3",X);
    if (not isPlaneInP5 last surfaces X) and X.cache#?"fusedVersion" and (surface fuse X).cache#?("fanoMap",ambientFivefold fuse X) then (
        if ((fanoMap fuse X)#"map").cache#"multiplicityFanoMap" >= 2 then (
            if o.Verbose then (
                << "-------------------------------------------------------------------" << endl;
                << "-- info: special configuration detected.                           " << endl;
                << "--       redirecting computation to associatedK3surface(fuse X)... " << endl;
                << "-------------------------------------------------------------------" << endl;
            );
            UtildeFuse := associatedK3surface(fuse X,Verbose=>o.Verbose,Strategy=>"F4");
            if o.Verbose then printFinalLog UtildeFuse;
            return UtildeFuse;
        );
    );
    Str := setStrategyDSCFtoK3(X,o.Strategy);
    (L,C) := exceptionalCurves(X,Verbose=>o.Verbose,Strategy=>Str);
    U := ambientVariety L;
    f := contractionMap(U,X,Verbose=>o.Verbose,Strategy=>Str,"ForceNormalization"=>isNormalizationKnownToTerminateQuickly(X));
    Utilde := makeSurfaceAssociated(X,mu,U,{L,C},f);
    if o.Verbose then printFinalLog Utilde;
    Utilde
);

printInfoOnPlannedSteps = X -> (
    << "-- planned steps:" << endl;
    << "-- 1. compute Fano map μ" << (if dim ambient X == 5 then " : ℙ⁵ ⇢ W" else "") << endl;
    << "-- 2. extract surface U from the base locus of (μ|X)⁻¹ : W ⇢ X" << endl;
    << "-- 3. take a general "<< (if instance(X,CubicFourfold) then "cubic " else "fourfold ") << "X' containing the " << (if instance(X,DoublySpecialCubicFourfold) then "two surfaces " else "surface ") << "and extract U'" << endl;
    << "-- 4. analyze the intersection U ∩ U' for exceptional curves" << endl;
    << "-- 5. compute the contraction map f : U -> Ũ" << endl;
    if instance(X,DoublySpecialCubicFourfold) then (
        << "-- 6. prepare data for lattice polarization" << endl;
        << "-- info: re-run the function for lattice computation and use building() to access" << endl;
        << "-- construction data (μ, U, exceptional curves, f)" << endl;
    ) else (
        << "-- info: use building() to access construction data (μ, U, exceptional curves, f)" << endl;
    );
    << endl;
);

buildAssociatedSurfaceFromPartialData = method(Options => {Verbose => true});
buildAssociatedSurfaceFromPartialData SurfaceAssociatedToRationalFourfold := o -> Utilde -> (
    (mu,U,exC,f) := building Utilde;
    if f =!= null or Utilde.cache#?"attemptedBuildAssociatedSurfaceFromPartialData" then return Utilde;
    X := recoverFourfold Utilde;
    EulerExpVal := if instance(X,IntersectionOfThreeQuadricsInP7) then 4 else 2;
    if euler hilbertPolynomial U == EulerExpVal then return Utilde;
    Str := null;
    if U.cache#?"strategy for surface U" then (
        Str = U.cache#"strategy for surface U";
    ) else (
        << "-- debug: missing 'strategy for surface U' in cache" << endl;
    );
    f = contractionMap(U,X,Verbose=>o.Verbose,Strategy=>Str,"ForceNormalization"=>true);
    Utilde.cache#"attemptedBuildAssociatedSurfaceFromPartialData" = true;
    if f === null then return Utilde;
    (L,C) := toSequence exC;
    return makeSurfaceAssociated(X,mu,U,{L,C},f);
);
