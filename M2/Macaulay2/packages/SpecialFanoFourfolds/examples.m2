
------------------------------------------------------------------------
-------------------------- Prebuilt Examples ---------------------------
------------------------------------------------------------------------

trisecantFlop = method(Options => {Verbose => false});
trisecantFlop ZZ := o -> i -> (
    if o.Verbose then << "-- warning: trisecantFlop may be deprecated in a future version. Consider importing examples with store and accessing them via example instead." << endl;
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
------------------------ User-defined examples -------------------------
------------------------------------------------------------------------

examplesPath = applicationDirectory() | "SpecialFanoFourfoldsExamples";
examplesDir = () -> (
    if not isDirectory examplesPath then makeDirectory(examplesPath | "/");
    if not(fileExists examplesPath and isDirectory examplesPath) then error("failed to create user examples directory: " | examplesPath);
    examplesPath
);

fourfoldTypeId = method();
fourfoldTypeId ZZ := n -> (
    if n == 0 then return ("dscf_", DoublySpecialCubicFourfold);
    if n == 1 then return ("cf_", CubicFourfold);
    if n == 2 then return ("gm_", GushelMukaiFourfold);
    if n == 3 then return ("i3q_", IntersectionOfThreeQuadricsInP7);
    if n == 4 then return ("hsf_", HodgeSpecialFourfold);
    error("invalid fourfold type identifier " | (toString n) | "; expected an integer between 0 and 4");
);
fourfoldTypeId HodgeSpecialFourfold := X -> (
    if instance(X,DoublySpecialCubicFourfold) then return 0;
    if instance(X,CubicFourfold) then return 1;
    if instance(X,GushelMukaiFourfold) then return 2;
    if instance(X,IntersectionOfThreeQuadricsInP7) then return 3;
    4
);

availableExamplesWithPrefix = pfx -> sort apply(select(readDirectory examplesDir(), s -> #s > #pfx + 4 and substring(s,0,#pfx) == pfx and substring(s,#s-4,4) == ".dat"), s' -> substring(s',#pfx,#s'-4-#pfx));
availableExamples = () -> flatten for i to 4 list apply(availableExamplesWithPrefix first fourfoldTypeId i, a -> (i,a));

LocalExamplesArchive = new Type of Nothing;
globalAssignment LocalExamplesArchive;
LocalExamplesArchive.synonym = "local examples archive";
localExamplesArchive = () -> new LocalExamplesArchive from null;

LocalExamplesArchive#{WebApp,AfterPrint} = LocalExamplesArchive#{WebApp,AfterNoPrint} = LocalExamplesArchive#{Standard,AfterPrint} = LocalExamplesArchive#{Standard,AfterNoPrint} = A -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : " << "Local examples archive of Hodge-special fourfolds" << endl;
    << displayLocalExamplesArchive() << endl;
    << "-- More info: ? oo" << endl;
);

displayLocalExamplesArchive = () -> (
    A := availableExamples();
    A' := apply(pairs memoizeValues exampleMem, p -> reverse take(first p,2)); -- users should consistently use either example(...,Verbose=>true) or example(...,Verbose=>false)
    s := "-- Number of examples: " | (toString length A) | " (loaded: " | (toString length A') | ")";
    local B; local B';
    for i to 4 do (
        B = apply(select(A, a -> first a == i), last);
        B' = apply(select(A', a -> first a == i), last);
        if #B > 0 then (
            s = s | newline | "-- example(ϑ," | (toString i) | "), ϑ ∈ " | (compactListOfNames B);
            if #B' > 0 then s = s | " (loaded: ϑ ∈ " | (compactListOfNames B') | ")";
        );
    );
    s
);

? LocalExamplesArchive := A -> (
    << ///-- Usage: 𝔸 = localExamplesArchive()
-- Main commands:
-- example(ϑ,n), example ϑ           load an example from archive 𝔸
-- store X                           save or update a fourfold X in 𝔸
-- store(X,ϑ)                        save a fourfold X in 𝔸 with name ϑ
-- isMember(X,𝔸)                     test membership in 𝔸
-- importFrom(𝑓,𝔸), store 𝑓          import examples into 𝔸 from file 𝑓
-- importFrom(𝑓,𝔸,KeepFiles=>true)   do not overwrite existing examples
-- importFrom("@",𝔸), store "@"      import examples from GitHub into 𝔸
-- export 𝔸                          export all examples in 𝔸
-- show 𝔸                            creation times and file sizes
-- texTables(n,𝔸)                    generate summary tables from examples of type n
-- delete(ϑ,n,𝔸), delete(X,𝔸)        remove an example from 𝔸
-- delete(n,𝔸)                       remove all examples of type n from 𝔸
-- clean 𝔸                           remove all examples from 𝔸
-- forgetType(...,𝔸) [internal]      move examples in 𝔸 to the generic section
-- Fourfold types:
-- 0 = doubly special cubic fourfolds, 1 = cubic fourfolds,
-- 2 = Gushel-Mukai fourfolds, 3 = intersections of three quadrics in ℙ⁷,
-- 4 = generic Hodge-special fourfolds/// << endl;
);

displayDatesAndSizes = () -> (
    getSavedDate := f -> (
        s := first lines get f;
        t := "-- Fourfold object exported by toExternalString on ";
        if substring(s,0,#t) === t then return substring(s,#t);
        << "-- warning: saved date not found" << endl;
        ""
    );
    getFileSize := f -> " (" | (toString round(fileLength(f)/1024.0)) | " KB)";
    E := availableExamples();
    if #E == 0 then return "";
    local E';
    local F;
    z := "-- Creation times and file sizes:";
    for n to 4 do (
        E' = apply(select(E, e -> first e === n), last);
        if #E' == 0 then continue;
        for str in E' do (
            F = examplesDir() | "/" | (first fourfoldTypeId n) | str | ".dat";
            z = z | newline | "example(\"" | str | "\"," | (toString n) | ") -> " | (getSavedDate F) | (getFileSize F);
        );
    );
    z
);
show LocalExamplesArchive := A -> (
    << displayDatesAndSizes() << endl;
);

convertNumericString = n -> (
    if not instance(n,String) then error "expected a string";
    I := apply(toList(0..9),toString);
    if #n > 0 and all(toList n, i -> member(i,I)) then value n else n
);

compactListOfNames = L -> (
    if not instance(L,List) then error "expected a list";
    L = apply(L,convertNumericString);
    Z := sort select(L, i -> instance(i,ZZ));
    Y := sort select(L, i -> not instance(i,ZZ));
    strZ := toString Z;
    if #Z >= 3 and toList(min Z .. max Z) === Z then strZ = "{" | (toString min Z) | ",...," | (toString max Z) | "}";
    strY := toString Y;
    if #Z > 0 and #Y > 0 then return (strZ | " ∪ " | strY);
    if #Z > 0 and #Y == 0 then return strZ;
    if #Z == 0 and #Y > 0 then return strY;
    "{}"
);

clean LocalExamplesArchive := A -> (
    e := run("rm -rf '" | examplesPath | "'");
    if e =!= 0 or fileExists examplesPath then error "failed to clean local examples archive";
    memoizeClear exampleMem;
    << "-- local examples archive cleared" << endl;
    localExamplesArchive()
);

export LocalExamplesArchive := A -> (
    archiveName := "ExamplesSFF_" | first lines get "!date +%Y-%m-%d_%H-%M";
    archivePath := currentDirectory() | archiveName | ".tar.gz";
    welcome := examplesDir() | "/welcome.m2";
    message := "-- " | concatenate(40:"-") | newline | "-- Hodge-special fourfold examples archive." | newline | "-- Created: " | (first lines get "!date +%Y-%m-%d") | newline | "-- " | concatenate(40:"-");
    welcome << "<< ///" << endl << message << endl << "///" << close;
    e := run("tar -czf '" | archivePath | "' -C '" | applicationDirectory() | "' SpecialFanoFourfoldsExamples");
    if not(e === 0 and fileExists archivePath) then error "failed to export archive";
    << "-- examples archive successfully exported to \"" << archivePath << "\"" << endl;
    archiveName
);

importArchive = f -> (
    if not instance(f,String) then error "expected a string";
    if not fileExists f then (
        if fileExists(f | ".tar.gz") then f = f | ".tar.gz" else error("file not found: " | f);
    );
    if #f < 7 or substring(f,#f-7,7) =!= ".tar.gz" then error "expected a .tar.gz archive";
    clean localExamplesArchive();
    e := run("tar -xzf '" | f | "' -C '" | applicationDirectory() | "'");
    if not(e === 0 and fileExists examplesPath) then error "failed to import archive";
    << "-- examples archive successfully imported" << endl;
    if fileExists(examplesPath | "/welcome.m2") then load(examplesPath | "/welcome.m2");
    localExamplesArchive()
);

downloadArchive = f -> (
    if not instance(f,String) then error "expected a string";
    if f === "@" then f = "https://raw.githubusercontent.com/giovannistagliano/SpecialFanoFourfoldsExamples/refs/heads/main/ExamplesSFF.tar.gz";
    if not(#f >= 8 and substring(f,0,8) === "https://") then error "expected a url: https://...";
    curl := findProgram("curl", "curl -h");
    tmpArchive := temporaryFileName() | ".tar.gz";
    << "-- downloading examples archive..." << endl;
    runProgram(curl, "-fLs -o " | tmpArchive | " " | f, RaiseError=>true, Verbose=>false);
    if not fileExists tmpArchive then error("failed to download archive from " | f);
    importArchive tmpArchive
);

mergeArchive = f -> (
    if not instance(f,String) then error "expected a string";
    if not fileExists f then (
        if fileExists(f | ".tar.gz") then f = f | ".tar.gz" else error("file not found: " | f);
    );
    if #f < 7 or substring(f,#f-7,7) =!= ".tar.gz" then error "expected a .tar.gz archive";
    dir := temporaryFileName();
    makeDirectory dir;
    run("tar -xzf '" | f | "' -C '" | dir | "'");
    newFiles := readDirectory(dir | "/SpecialFanoFourfoldsExamples");
    newFiles = select(newFiles, s -> not member(s, {"welcome.m2", ".", ".."}));
    oldFiles := readDirectory examplesDir();
    if any(newFiles, s -> member(s, oldFiles)) then error "cannot merge archive: some examples already exist in the local archive; use KeepFiles=>false to replace it";
    welcome := examplesDir() | "/welcome.m2";
    if fileExists welcome then removeFile welcome;
    e := run("tar -xzkf '" | f | "' -C '" | applicationDirectory() | "'");
    if e =!= 0 then error "failed to merge archive";
    << "-- examples archive successfully merged" << endl;
    localExamplesArchive()
);

importFrom (String,LocalExamplesArchive,Option) := (f,A,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === KeepFiles) then error "KeepFiles is the only available option for importFrom(String,LocalExamplesArchive)";
    if f === "" then return clean localExamplesArchive();
    isURL := (#f >= 8 and substring(f,0,8) === "https://") or f === "@";
    if isURL and (last o) then error "not implemented yet: importing a remote archive with KeepFiles=>true; use default KeepFiles=>false to replace the local archive";
    if isURL then return downloadArchive f;
    if last o then return mergeArchive f;
    importArchive f
);
importFrom (String,LocalExamplesArchive) := (f,A) -> importFrom(f,A,KeepFiles=>false);
store String := f -> importFrom(f,localExamplesArchive());

isMember (HodgeSpecialFourfold,LocalExamplesArchive) := (X,A) -> (
    -- heuristic test; may fail after delete/store operations
    if not X.cache#?"exampleNameInArchive" then return false;
    str := X.cache#"exampleNameInArchive";
    pfx := first fourfoldTypeId fourfoldTypeId X;
    F := examplesDir() | "/" | pfx | str | ".dat";
    fileExists F
);

delete (HodgeSpecialFourfold,LocalExamplesArchive) := (X,A) -> (
    if not X.cache#?"exampleNameInArchive" then error "cannot delete: fourfold was not loaded from the local examples archive";
    str := X.cache#"exampleNameInArchive";
    n := fourfoldTypeId X;
    pfx := first fourfoldTypeId n;
    F := examplesDir() | "/" | pfx | str | ".dat";
    if not fileExists F then error("example \"" | str | "\" was loaded from file \"" | F | "\", but this file no longer exists");
    removeFile F;
    if fileExists F then error("failed to delete file \"" | F | "\"");
    X1 := remove(memoizeValues exampleMem,(str,n,true));
    X2 := remove(memoizeValues exampleMem,(str,n,false));
    if X =!= X1 and X =!= X2 then (<< "-- warning: example(\"" << str << "\"," << n << ") was not present in the cache of example" << endl);
    << "-- example(\"" << str << "\"," << n << ") deleted" << endl;
    localExamplesArchive()
);
delete (String,ZZ,LocalExamplesArchive) := (str,n,A) -> (
    S := apply(pairs memoizeValues exampleMem, p -> (take(first p,2),last p));
    m := position(S, p -> first p === (str,n));
    if m =!= null then (
        << "-- deleting loaded example(\"" << str << "\"," << n << ")" << endl;
        X := last S_m;
        return delete(X,A);
    );
    pfx := first fourfoldTypeId n;
    F := examplesDir() | "/" | pfx | str | ".dat";
    if not fileExists F then error "example not found in the local examples archive";
    removeFile F;
    if fileExists F then error("failed to delete file \"" | F | "\"");
    << "-- example(\"" << str << "\"," << n << ") deleted" << endl;
    localExamplesArchive()
);
delete (ZZ,LocalExamplesArchive) := (n,A) -> (
    << "-- deleting all example(*," << n << ")" << endl;
    for s in select(availableExamples(), a -> first a == n) do delete(last s,n,A);
    localExamplesArchive()
);

forgetType = method();
forgetType (String,ZZ,LocalExamplesArchive) := (str,n,A) -> (
    oldFile := examplesDir() | "/" | (first fourfoldTypeId n) | str | ".dat";
    if not fileExists oldFile then error "example not found in the local examples archive";
    newFile := examplesDir() | "/" | (first fourfoldTypeId 4) | str | ".dat";
    if n == 4 then return localExamplesArchive();
    if fileExists newFile then error "cannot forget type: target example already exists";
    copyFile(oldFile,newFile,Verbose=>true);
    if not fileExists newFile then error "failed to move example";
    delete(str,n,A);
    << "-- example(\"" << str << "\"," << n << ") moved to example(\"" << str << "\",4)" << endl;
    localExamplesArchive()
);

example = method(Options => {Verbose => true});
exampleMem = memoize((str,n,verb) -> (
    (pfx,cls) := fourfoldTypeId n;
    F := examplesDir() | "/" | pfx | str | ".dat";
    if not fileExists F then error("example(\"" | str | "\"," | (toString n) | ") not found." | newline | displayLocalExamplesArchive());
    if verb then << "-- loading example(\"" << str << "\"," << n << ")..." << endl;
    dataString := get F;
    if verb then << "-- evaluating example(\"" << str << "\"," << n << ")..." << endl;
    X := value dataString;
    if not instance(X,cls) then error "corrupted example data";
    X.cache#"exampleNameInArchive" = str;
    if verb then << "-- example(\"" << str << "\"," << n << ") ready." << endl;
    X
));
example (String,ZZ) := o -> (str,n) -> exampleMem(str,n,o.Verbose);
example (ZZ,ZZ) := o -> (i,n) -> example(toString i,n,Verbose=>o.Verbose);
example String := o -> str -> (
    S := select(availableExamples(), a -> last a == str);
    if #S == 0 then error("example \"" | str | "\" not found." | newline | displayLocalExamplesArchive());
    if #S > 1 then error("the name \"" | str | "\" is used by examples of different fourfold types." | newline | "Use example(ϑ,n) to specify the type." | newline | displayLocalExamplesArchive());
    example(str,first first S,Verbose=>o.Verbose)
);
example ZZ := o -> i -> example(toString i,Verbose=>o.Verbose);

store (HodgeSpecialFourfold,String,Option) := (X,str,opt) -> (
    o := toList opt;
    if not(#o == 2 and first o === RaiseError) then error "RaiseError is the only available option for store(HodgeSpecialFourfold,String)";
    pfx := first fourfoldTypeId fourfoldTypeId X;
    F := examplesDir() | "/" | pfx | str | ".dat";
    alreadyExists := fileExists F;
    if (last o) and alreadyExists then error("example \"" | str | "\" already exists; please choose another name or use RaiseError=>false to overwrite it");
    F << toExternalString X << close;
    if not fileExists F then error("failed to save example \"" | str | "\"");
    if alreadyExists then (
        << "-- example \"" << str << "\" successfully overwritten" << endl;
    ) else (
        << "-- example \"" << str << "\" successfully stored" << endl;
    );
    str
);
store (HodgeSpecialFourfold,ZZ,Option) := (X,i,opt) -> store(X,toString i,opt);
store (HodgeSpecialFourfold,String) := (X,str) -> store(X,str,RaiseError=>true);
store (HodgeSpecialFourfold,ZZ) := (X,i) -> store(X,i,RaiseError=>true);
store HodgeSpecialFourfold := X -> (
    if X.cache#?"exampleNameInArchive" then return store(X,X.cache#"exampleNameInArchive",RaiseError=>false);
    pfx := first fourfoldTypeId fourfoldTypeId X;
    i := 0;
    str := pfx | (toString vars i);
    F := examplesDir() | "/" | pfx | str | ".dat";
    while fileExists F do (
        i = i + 1;
        str = pfx | (toString vars i);
        F = examplesDir() | "/" | pfx | str | ".dat";
    );
    store(X,str,RaiseError=>true)
);

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
