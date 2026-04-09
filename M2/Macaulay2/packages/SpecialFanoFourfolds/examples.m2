
------------------------------------------------------------------------
-------------------------- Prebuilt Examples ---------------------------
------------------------------------------------------------------------

trisecantFlop = method(Options => {Verbose => false});
trisecantFlop ZZ := o -> i -> (
    try needsPackage "TrisecantFlops" else (
        git := findProgram("git", "git --help");
        dir := temporaryFileName() | "/";
        mkdir dir;
        <<"The package TrisecantFlops is not present."<<endl;
        e := "";
        while not(e == "y" or e == "yes" or e == "Y" or e == "Yes") do (
            e = read("Do you want to download the latest version of the package now? (y/n) ");
            if e == "n" or e == "no" or e == "N" or e == "No" then error "required package TrisecantFlops";
        );
        <<"-- downloading the package TrisecantFlops from https://github.com/giovannistagliano"<<endl;
        runProgram(git,"clone --depth 1 --no-checkout https://github.com/giovannistagliano/TrisecantFlops.git", RunDirectory => dir);
        runProgram(git, "checkout master", RunDirectory => dir | "/TrisecantFlops");
        if not fileExists(dir|"TrisecantFlops/TrisecantFlops.m2") then error "something went wrong in downloading the package TrisecantFlops";
        try needsPackage("TrisecantFlops",FileName => dir|"TrisecantFlops/TrisecantFlops.m2") else error "something went wrong in loading the package TrisecantFlops";
        <<"The package TrisecantFlops has been successfully loaded."<<endl;
        f := "";
        while not(f == "y" or f == "yes" or f == "Y" or f == "Yes" or f == "n" or f == "no" or f == "N" or f == "No")
        do f = read("Do you want to install the package for future use? (y/n) ");
        if f == "y" or f == "yes" or f == "Y" or f == "Yes" then (
            <<"-- installing the package TrisecantFlops"<<endl;
            installPackage("TrisecantFlops",Verbose => false,FileName => dir|"TrisecantFlops/TrisecantFlops.m2");
        );
    );
    if not member(value "TrisecantFlops",loadedPackages) then error "something went wrong";
    if (value "TrisecantFlops").Options.Version < "1.7" then (
        if o.Verbose then <<"-- removing old version of TrisecantFlops"<<endl;
        uninstallPackage "TrisecantFlops";
        error "Your version of the TrisecantFlops package was outdated and has been removed. Please restart Macaulay2 and re-execute the function 'trisecantFlop'";
    );
    value("trisecantFlop("|toString(i)|",Verbose=>"|toString(o.Verbose)|")")
);

prebuiltExamplesOfRationalFourfolds = memoize(() -> (
    try needsPackage "PrebuiltExamplesOfRationalFourfolds" else (
        curl := findProgram("curl", "curl -h");
        dir := temporaryFileName() | "/";
        mkdir dir;
        <<"The package PrebuiltExamplesOfRationalFourfolds is not present."<<endl;
        e := "";
        while not(e == "y" or e == "yes" or e == "Y" or e == "Yes") do (
            e = read("Do you want to download the latest version of the package now? (y/n) ");
            if e == "n" or e == "no" or e == "N" or e == "No" then error "required package PrebuiltExamplesOfRationalFourfolds";
        );
        <<"-- downloading the package PrebuiltExamplesOfRationalFourfolds from https://github.com/giovannistagliano"<<endl;
        runProgram(curl,"-s -o PrebuiltExamplesOfRationalFourfolds.m2 https://raw.githubusercontent.com/giovannistagliano/PrebuiltExamplesOfRationalFourfolds/main/PrebuiltExamplesOfRationalFourfolds.m2",RunDirectory=>dir);
        if not fileExists(dir|"PrebuiltExamplesOfRationalFourfolds.m2") then error "something went wrong in downloading the package PrebuiltExamplesOfRationalFourfolds";
        try needsPackage("PrebuiltExamplesOfRationalFourfolds",FileName => dir|"PrebuiltExamplesOfRationalFourfolds.m2") else error "something went wrong in loading the package PrebuiltExamplesOfRationalFourfolds";
        <<"The package PrebuiltExamplesOfRationalFourfolds has been successfully loaded."<<endl;
        f := "";
        while not(f == "y" or f == "yes" or f == "Y" or f == "Yes" or f == "n" or f == "no" or f == "N" or f == "No")
        do f = read("Do you want to install the package for future use? (y/n) ");
        if f == "y" or f == "yes" or f == "Y" or f == "Yes" then (
            <<"-- installing the package PrebuiltExamplesOfRationalFourfolds"<<endl;
            installPackage("PrebuiltExamplesOfRationalFourfolds",Verbose => false,FileName => dir|"PrebuiltExamplesOfRationalFourfolds.m2");
        );
    );
    if not member(value "PrebuiltExamplesOfRationalFourfolds",loadedPackages) then error "something went wrong";
    if (value "PrebuiltExamplesOfRationalFourfolds").Options.Version < "1.1" then (
        <<"-- removing old version of PrebuiltExamplesOfRationalFourfolds"<<endl;
        uninstallPackage "PrebuiltExamplesOfRationalFourfolds";
        error "Your version of the PrebuiltExamplesOfRationalFourfolds package was outdated and has been removed. Macaulay2 should be restarted as soon as possible.";
    );
    value ///importFrom(PrebuiltExamplesOfRationalFourfolds,{"prebuiltExampleOfRationalFourfold"});///;
    value ///prebuiltExampleOfRationalFourfold///
));

------------------------------------------------------------------------
---------------------- Prime Fano fourfolds ----------------------------
------------------------------------------------------------------------

fanoFourfold = method(TypicalValue => EmbeddedProjectiveVariety, Options => {CoefficientRing => ZZ/65521});
fanoFourfold (ZZ,ZZ) := o -> (d,g) -> (
    K := o.CoefficientRing;
    if not (instance(K,Ring) and isField K) then error "CoefficientRing option expects a field";
    local Y; local j; local S; local psi;
    local X;
    dg := {(2,0),(3,1),(4,1),(5,1),(4,3),(6,4),(8,5),(10,6),(12,7),(14,8),(16,9),(18,10)};
    if not member((d,g),dg) then error("expected a pair of integers in the set "|toString(dg));
    if d == 2 and g == 0 then X = random({2},0_(PP_K^5));
    if d == 3 and g == 1 then X = random({3},0_(PP_K^5));
    if d == 4 and g == 1 then X = random({{2},{2}},0_(PP_K^6));
    if d == 5 and g == 1 then (
        Y = GG(K,1,4);
        j = parametrize random({{1},{1}},0_Y);
        X = j^* Y;
    );
    if d == 4 and g == 3 then X = random({4},0_(PP_K^5));
    if d == 6 and g == 4 then X = random({{2},{3}},0_(PP_K^6));
    if d == 8 and g == 5 then X = random({3:{2}},0_(PP_K^7));
    if d == 10 and g == 6 then (
        Y = GG(K,1,4);
        j = parametrize random({1},0_Y);
        X = (j^* Y) * random({2},0_(source j));
    );
    if d == 12 and g == 7 then (
        S = surface({3,4},K);
        psi = rationalMap(S * random({1},0_S),2);
        j = parametrize random({1},0_(target psi));
        X = j^* image(2,psi);
    );
    if d == 14 and g == 8 then (
        Y = GG(K,1,5);
        j = parametrize random({4:{1}},0_Y);
        X = j^* Y;
    );
    if d == 16 and g == 9 then (
        S = surface({2},K,ambient=>6);
        psi = rationalMap(S,3,2);
        j = parametrize random({{1},{1}},0_(target psi));
        X = j^* image psi;
    );
    if d == 18 and g == 10 then (
        -- p. 4 of [Kapustka and Ranestad - Vector Bundles On Fano Varieties Of Genus Ten]
        w := gens ring PP_K^13;
        M := matrix {{0,-w_5,w_4,w_6,w_7,w_8,w_0},
                     {w_5,0,-w_3,w_12,w_13,w_9,w_1},
                     {-w_4,w_3,0,w_10,w_11,-w_6-w_13,w_2},
                     {-w_6,-w_12,-w_10,0,w_2,-w_1,w_3},
                     {-w_7,-w_13,-w_11,-w_2,0,w_0,w_4},
                     {-w_8,-w_9,w_6+w_13,w_1,-w_0,0,w_5},
                     {-w_0,-w_1,-w_2,-w_3,-w_4,-w_5,0}};
        Y = Var trim pfaffians(4,M);
        j = toRationalMap parametrize random({1},0_Y);
        X = j^* Y;
    );
    if not (dim X == 4 and degree X == d and sectionalGenus X == g) then error("something went wrong while computing random fourfold of degree "|toString(d)|" and sectional genus "|toString(g));
    return X;
);

parametrizeFanoFourfold = method(TypicalValue => MultirationalMap, Options => {Strategy => 1});
parametrizeFanoFourfold EmbeddedProjectiveVariety := o -> X -> (
    X#InverseMethod = inverse3;
    if dim X != 4 then error "expected a fourfold";
    if degree X == 5 and sectionalGenus X == 1 then (
        if o.Strategy === 2 then return parametrize X;
        if o.Strategy =!= 1 then error("the available strategies are:"|newline|"-- 1: projection from the plane spanned by a conic contained in the fourfold"|newline|"-- 2: projection from the unique sigma_(2,2) plane contained in the fourfold (Todd's result)");
        p := point X;
        C := line(X,p) + line(X,p);
        if degree C != 2 then error "something went wrong while finding conic on fourfold";
        return inverse3 multirationalMap rationalMap(sub(ideal C,ring X),1);
    );
    if o.Strategy =!= 1 then error "strategy not available";
    parametrize X
);
