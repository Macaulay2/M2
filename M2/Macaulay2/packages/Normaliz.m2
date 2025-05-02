-- -*- coding: utf-8 -*-

-*
Copyright 2009, 2010 Winfried Bruns and Gesa Kaempf.
Copyright 2011, 2012, 2015, 2016 Christof Soeger

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.
*-

newPackage(
           "Normaliz",
           Version=>"2.6",
           Date=>"February 4, 2023",
           Authors=>{{Name=> "Gesa Kaempf",
                    Email=>"gkaempf@uni-osnabrueck.de"},
                    {Name=> "Christof Soeger",
                    Email=>"csoeger@uni-osnabrueck.de"}},
           Headline=>"interface to Normaliz in Macaulay2",
           Keywords => {"Interfaces"},
           DebuggingMode => false,
	   Certification => {
		"journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
		"journal URI" => "https://msp.org/jsag/",
		"article title" => "A Macaulay2 interface for Normaliz",
		"acceptance date" => "2010-08-08",
		"published article URI" => "https://msp.org/jsag/2010/2-1/p04.xhtml",
		"published article DOI" => "10.2140/jsag.2010.2.15",
		"published code URI" => "https://msp.org/jsag/2010/2-1/jsag-v2-n1-x04-code.zip",
		"release at publication" => "d814bd64858da074bc73c626d27ff5494dfb7d4f",
		"version at publication" => "2.0",
		"volume number" => "2",
		"volume URI" => "https://msp.org/jsag/2010/2-1/"
		}
)

export {
    "rmNmzFiles",
    "writeNmzData", "readNmzData", "readMultipleNmzData",
    "normaliz", "setNmzOption", "showNmzOptions",
    "normalToricRing", "intclToricRing", "ehrhartRing", "intclMonIdeal",
    "torusInvariants", "finiteDiagInvariants", "diagInvariants", "RationalCone",
    "allComputations", "grading",
    "intersectionValRings", "intersectionValRingIdeals",
    "MonomialSubalgebra",  "createMonomialSubalgebra",
    "getNumInvs"
    }

exportMutable { "nmzVersion", "nmzDataPath", "nmzFilename", "nmzNumberThreads" }

----------------------------------------------------------------------
-- new classes
----------------------------------------------------------------------

RationalCone = new Type of HashTable
RationalCone.synonym = "rational cone"

MonomialSubalgebra = new Type of HashTable
MonomialSubalgebra.synonym = "monomial subalgebra"
--MonomialSubalgebra.GlobalAssignHook = globalAssignFunction
--MonomialSubalgebra.GlobalReleaseHook = globalReleaseFunction
MonomialSubalgebra#AfterPrint = S -> (class S, " of ", ring S)
MonomialSubalgebra#Print = S -> (coefficientRing ring S, "[", demark_", " apply(gens S, toString), "]")

createMonomialSubalgebra = method()
createMonomialSubalgebra List := L -> (
    if not uniform L then error "createMonomialSubalgebra: monomials must be elements of the same ring"
    else if #L == 0  then error "createMonomialSubalgebra: empty MonomialSubalgebra has no default coefficient ring";
    new MonomialSubalgebra from {
	symbol ring       => ring L#0,
	symbol generators => L,
	symbol cache      => new CacheTable
	})

createEmptyMonomialSubalgebra = method()
createEmptyMonomialSubalgebra Ring := K -> new MonomialSubalgebra from {
    symbol ring       => K,
    symbol generators => {},
    symbol cache      => new CacheTable
    }

vars MonomialSubalgebra := R -> matrix {R.generators}
numgens MonomialSubalgebra := R -> #R.generators
gens MonomialSubalgebra := opts -> R -> R.generators
ring MonomialSubalgebra := R -> R.ring

----------------------------------------------------------------------
-- initialising some values
----------------------------------------------------------------------

nmzDataPath="";
nmzFilename="";    -- Set by the user, if empty, then we use a temporary file
nmzNumberThreads=1;
nmzFile="";        -- Internal name of the data files
nmzVersion="";     -- normaliz
nmzMinExecVersion="2.11"; -- minimal normaliz version
nmzGen=true;      -- indicates whether ".gen" is generated

normalizProgram = findProgram("normaliz", "normaliz --help",
    Verbose => debugLevel > 0, MinimumVersion => (nmzMinExecVersion,
	"normaliz --version | head -1 | cut -d' ' -f 2 | tr -d '\n'"))

-- FIXME: not thread-safe
-- component 1 is name of option
-- 2 is default value
-- 3 is command line option to be passed to Normaliz
-- 4 indicates whether file "gen" is generated
-- value 2 of 4 indicates "no influence"
nmzOptions = new MutableList from {
    new MutableList from {"supp",       false, "-s",  false},
    new MutableList from {"triang",     false, "-tT", false},
    new MutableList from {"volume",     false, "-v", false},
    new MutableList from {"hvect",      false, "-p", false},
    new MutableList from {"height1",    false, "-1", false},
    new MutableList from {"normal",     false, "-n", true},
    new MutableList from {"normal_l",   false, "-N", true},
    new MutableList from {"hilb",       false, "-h", true},
    new MutableList from {"dual",       false, "-d", true},
    new MutableList from {"control",    false, "-c", 2},
    new MutableList from {"allf",       false, "-a", 2},
    new MutableList from {"errorcheck", false, "-e", 2},
    new MutableList from {"bigint",     false, "-B", 2},
    new MutableList from {"threads",    false, "-x", 2}
    }

----------------------------------------------------------------------
--  filenames and paths
----------------------------------------------------------------------

-- sets the file for the exchange of data
setNmzFile = () -> nmzFile = if nmzFilename != "" then nmzDataPath | nmzFilename else temporaryFileName()

-- checks if the user specified a new filename
checkNmzFile = errorMsg -> (
    -- if the user specified a filename, we use it.
    if nmzFilename != "" then nmzFile = nmzDataPath | nmzFilename
    -- otherwise, there should be a temporary one.
    else if nmzFile == "" then error(errorMsg | ": No filename specified."))

-- get the normaliz executable with full path
getNmzExec = () -> (
    if nmzVersion != "" then (
	if not member(nmzVersion, {"norm64", "normbig", "normaliz"})
	then error "nmzVersion must be one of the following: normaliz normbig norm64"
	else nmzVersion)
    else "normaliz")

-- removes the files created for and by normaliz
rmNmzFiles = () -> (
    suffixes := {"in","gen","out","sup","egn","esp","inv","tri","typ","ht1","ext","cst","tgn","lat","mod","dec"};
    checkNmzFile("rmNmzFiles");
    for s in suffixes do if fileExists(nmzFile | "." | s) then removeFile(nmzFile | "." | s);
    nmzFilename = "");

----------------------------------------------------------------------
--  parsing normaliz output (not exported)
----------------------------------------------------------------------

-- returns the next number in the string s, and the remaining string
getNumber = method()
getNumber String := s -> (
    l := regex("[0-9-]+", s);
    if instance(l, Nothing) then error "getNumber: no number found in the string.";
    if l#0#0 != 0 then error "getNumber: string must begin with a number";
    (substring(l#0, s), substring(l#0#0 + l#0#1, s)))

-- returns the next word (marked by whitespaces) in the string s, starting at position j and replaces "_" by " ", and the position of the first whitespace
-- if s contains no whitespace, the returned position is not in the string!
getKeyword = (s, j) -> (
    l := regex("[[:alnum:]_-]+", j, s);
    if instance(l, Nothing) then error "getKeyword: no word found in the string.";
    if l#0#0 != j then << "warning: getKeyword: no word at the beginning of the string.";
    replace("_", " ", substring(l#0, s)), l#0#0 + l#0#1)

-- eliminates whitespaces and -, and transforms the next letter to upper case if possible
elimWhitespaces = s -> (
    tmp := "";
    while match("[ -]", s) do (
	l := regex("[ -]", s);
	pos := l#0#0;
	tmp = tmp | substring(0, pos, s);
	if pos + 1 < #s then tmp = tmp | toUpper s#(pos+1);
	s = substring(pos+2, s));
    tmp | s)


-- changes column f with column s
changeColumns = (M, f, s) -> (
    columns:= new MutableList from entries transpose M;
    tmp:= columns#f;
    columns#f=columns#s;
    columns#s=tmp;
    transpose matrix toList columns)

----------------------------------------------------------------------
-- input and output to/from normaliz
----------------------------------------------------------------------

-- writes the given data in a normaliz input file
doWriteNmzData = method()
-- writes several matrices in a normaliz input file
doWriteNmzData List := matrices -> (
    checkNmzFile("doWriteNmzData");
    outf := nmzFile | ".in" << "";
    for p in matrices do (
	sgr := p#0;
	nmzMode := p#1;
	outf << numRows sgr << endl;
	outf << numColumns sgr << endl;
	--
	for i from 0 to numRows sgr - 1 do (
	    s := "";
	    for j from 0 to numColumns sgr - 1
	    do s = s | sgr_(i,j) | " ";
	    outf << s << endl;
	    );
	-- Until version 3.9.4, input type normal_toric_ideal was called lattice_ideal
	if normalizProgram#"version" < "3.10" and nmzMode == "normal_toric_ideal" then nmzMode = "lattice_ideal";
	outf << nmzMode << endl);
    outf << close)

-- writes the given data in a normaliz input file
writeNmzData = method()
writeNmzData(Matrix, String) := (sgr, nmzMode) -> doWriteNmzData {(sgr, nmzMode)}
writeNmzData List := matrices -> doWriteNmzData matrices

-- reads the Normaliz output file with the specified suffix
-- suffix should not be inv, in or out
readNmzData = method(TypicalValue => Matrix)
readNmzData String := nmzSuffix -> (
    if member(nmzSuffix, {"inv", "in", "out", "cst"})
    then error("readNmzData: ",
	"to read .inv use getNumInvs(), ",
	"to read .cst use readMultipleNmzData, ",
	"to read .out or .in there is no function provided");
    L := readMultipleNmzData if nmzSuffix == "sup" then "cst" else nmzSuffix; -- for backward compatibility
    L#0)

-- reads several matrices from one output file and returns them as list
-- at the moment (!) necessary only for the suffix "cst"
readMultipleNmzData = method()
readMultipleNmzData String := nmzSuffix -> (
    checkNmzFile("readMultipleNmzData");

    if not fileExists(nmzFile | "." | nmzSuffix)
    then error("readMultipleNmzData: No file ", nmzFile, ".", nmzSuffix, " found. Perhaps you need to activate another option.");

    if debugLevel > 0 then << "--reading " << nmzFile << "." << nmzSuffix << endl;

    s := lines get(nmzFile | "." | nmzSuffix);

    L := {};
    i := 0;
    j := 0;
    while i < #s do (
        while j < #s and match("[0-9-]+", s#j) do j = j + 1;
        nmzGen := if i == j then {{}} else apply(
	    take(s, {i, j - 1}), t -> value \ select("[0-9-]+", t));
        -- versions between 3.4.0 and 3.5.1 did not print row/column data
        -- we remove it if present
        if #nmzGen > 2
	and nmzGen#0#0 == #nmzGen - 2 -- number of rows
	and nmzGen#1#0 == #nmzGen#2   -- number of columns
        then nmzGen = drop(nmzGen, 2);
        L = append(L, matrix nmzGen);
        i = j = j + 1);
    L)

----------------------------------------------------------------------
-- retrieving normaliz numerical invariants
----------------------------------------------------------------------

getNumInvs = () -> (
    numInvs := {};
    key := "";
    inv := 0;

    checkNmzFile("getNumInvs");

    if not fileExists(nmzFile | ".inv")
    then error("getNumInvs: No file " | nmzFile | ".inv" | " found.");

    if debugLevel > 0 then << "--reading " << nmzFile << ".inv" << endl;
    s := lines get(nmzFile | ".inv");

    for i from 0 to #s - 1 do (  -- for each line in the file
	key = "";
	if match("^integer", s#i) then (
	    local j;
	    (key, j) = getKeyword(s#i, 8);
	    inv = value(getNumber substring(j + 3, s#i))#0;
	    )
	else(
	    if match("^boolean", s#i) then (
		(key, j) = getKeyword(s#i, 8);
		inv = s#i#(j + 3) == "t")
	    else (
		if match("^vector", s#i) then (
		    (len, str) := getNumber substring(7, s#i);
		    (key, j) = getKeyword(str, 1);
		    inv = {};
		    --   en:="";
		    --str=substring(j+10+#len,s#i);
		    t:= replace(".* = ","",s#i);
		    u:= select("[0-9-]+",t);
		    inv=toSequence(apply(u,value));
		    );
		);
	    );
	numInvs = append(numInvs,{key,inv}));
    hashTable numInvs)

----------------------------------------------------------------------
-- running normaliz (with options)
----------------------------------------------------------------------

setNmzOption = method()
setNmzOption(String, Boolean) := (s, onoff) -> (
    i := position(toList nmzOptions, x -> x#0 === s);
    if i === null then (
	print("setNmzOption: Invalid option ", s);
	false)
    else (
	nmzOptions#i#1 = onoff;
	if s == "threads" then nmzOptions#i#2 = "-x=" | nmzNumberThreads;
	true)
    )

collectNmzOptions = () -> (
    opts := " -f ";
    for i from 0 to #nmzOptions - 1 do if nmzOptions#i#1 then (
	opts = opts | nmzOptions#i#2 | " ";
	if nmzOptions#i#3 =!= 2 then nmzGen = nmzOptions#i#3);
    opts)

showNmzOptions = () -> ( << "The following options are set:" << endl; << collectNmzOptions() )

normaliz = method(Options => { allComputations => false, grading => {} })
normaliz(Matrix, String) := opts -> (sgr, nmzMode) -> runNormaliz({(sgr, nmzMode)}, opts)
normaliz List := opts -> L -> runNormaliz(L, opts)

-- sequence should contain pairs (sgr,nmzMode)
-- FIXME: why the duplicate?
runNormaliz = method(Options => options normaliz)
runNormaliz(Matrix, String) := opts -> (sgr, nmzMode) -> runNormaliz({(sgr, nmzMode)}, opts)
runNormaliz List := opts -> s -> (
    setNmzFile();

    if 0 < #opts.grading then s = append(s, (matrix {opts.grading}, "grading"));
    doWriteNmzData s;

    dir := select(".*/", nmzFile);
    runDir := if dir != {} then dir#0 else null;
    runProgram(normalizProgram, getNmzExec(), collectNmzOptions() | baseFilename nmzFile,
	RunDirectory => runDir, Verbose => debugLevel > 0);

    -- return nothing if .gen is not generated
    if not nmzGen then ( if nmzFilename == "" then rmNmzFiles(); return );

    if not opts.allComputations then (
	nmzData := readNmzData "gen";
	rc := new RationalCone from { "gen" => nmzData, "inv" => getNumInvs() };
	if nmzFilename == "" then rmNmzFiles();
	return rc);

    -- read all files written
    files := { "inv" => getNumInvs() };
    suffixes := { "gen","egn","esp","tri","typ","ht1","ext","tgn" };
    for s in suffixes do if fileExists(nmzFile | "." | s) then files = append(files, s => readNmzData s);

    L := readMultipleNmzData "cst";
    files = append(files, "sup" => L#0);
    files = append(files, "equ" => L#1);
    files = append(files, "cgr" => L#2);

    C := new RationalCone from files;

    if nmzFilename == "" then rmNmzFiles();
    C)

----------------------------------------------------------------------
-- intmats to/from monomials (not exported)
----------------------------------------------------------------------

mons2intmat = method()
mons2intmat Ideal := I -> matrix(flatten \ exponents \ I)

-- takes not column c
mons2intmat(Ideal, ZZ) := (I, c) -> (
    if c >= numgens ring I then (
	<< "mons2intmat: Warning! "|c|" exceeds the maximal index of a variable";
	return mons2intmat I);
    mat := flatten \ exponents \ I;
    mat = apply(mat, v -> drop(v, {c, c}));
    matrix mat)

-- expos: a matrix whose numColumns is <= numgens r
-- r: the ring where the ideal shall be
-- returns the ideal
intmat2mons = method()
intmat2mons(Matrix, Ring) := (expoVecs, r) -> (
    if numColumns expoVecs > numgens r
    then error "intmat2mons: not enough variables in the basering";
    l := {};
    for i from 0 to numRows expoVecs - 1 do l = append(l, r_((entries expoVecs)#i));
    l)

-- takes only the rows with entry d in column c, ignoring column c
intmat2mons(Matrix, Ring, ZZ, ZZ) := (expoVecs, r, d, c) -> (
    if numColumns expoVecs - 1 > numgens r
    then error "intmat2mons: not enough variables in the basering";
    v := gens r;  -- the variables of the basering
    l := {};
    rows := entries expoVecs;
    rows = select(rows,  row -> row#c == d); --only those rows with entry d in column c
    for i from 0 to #rows - 1 do (
	m := 1;
	for j from 0 to numColumns expoVecs - 1 do (
	    if not j == c then m = m * (v#j)^(rows#i#j));
	l = append(l, m));
    l)

----------------------------------------------------------------------
-- integral closure of rings and ideals
----------------------------------------------------------------------

runIntclToricRing = method(Options => options normaliz)
runIntclToricRing(Ideal, String) := opts -> (I, nmzMode) -> (
    expoVecs := mons2intmat I;
    result := runNormaliz(expoVecs, nmzMode, opts);
    if result =!= null then (
	S := createMonomialSubalgebra  intmat2mons(result#"gen", ring I);
	S.cache#"cone" = result;
	S))

intclToricRing = method(Options => options normaliz)
intclToricRing List := opts -> L -> (
    if not uniform L then error "intclToricRing: monomials must be elements of the same ring";
    if #L == 0       then error "intclToricRing: expected non-empty list";
    runIntclToricRing(ideal L, "integral_closure", opts))

intclToricRing MonomialSubalgebra := opts -> S -> intclToricRing(gens S, opts)

normalToricRing=method(Options => options normaliz)
normalToricRing List := opts -> L -> (
  if not uniform L then error "normalToricRing: monomials must be elements of the same ring";
  if #L == 0       then error "normalToricRing: expected non-empty list";
  runIntclToricRing(ideal L, "normalization", opts))

normalToricRing MonomialSubalgebra := opts -> S -> normalToricRing(gens S, opts)

-- input binomial ideal
normalToricRing(Ideal, Thing) := opts -> (I, t) -> (
    R := ring I;
    a := 0;
    b := 0;
    M := {};
    for g in flatten entries gens I do (
	if #(exponents g) != 2 then error "normalToricRing: ideal is not generated by binomials.";
	a = (exponents g)#0;
	b = (exponents g)#1;
	if  coefficient(R_a, g) == 1
	and coefficient(R_b, g) == -1
	then M = append(M, entries(vector a - vector b))
	else (
	    if coefficient(R_a, g) == -1 and coefficient(R_b, g) == 1
	    then M = append(M, entries(vector b - vector a))
	    else error "normalToricRing: ideal is not generated by binomials.");
	);
    nmzCone := normaliz(matrix M, "normal_toric_ideal", opts);
    nmzData := nmzCone#"gen";
    r := rank nmzData;
    n := numgens R;
    R = (coefficientRing R)(monoid[t_1..t_(n-r)]);
    S := createMonomialSubalgebra(for i in entries nmzData list R_i);
    S.cache#"cone" = nmzCone;
    S)

----------------------------------------------------------------------
-- Rees Algebra / Monomial Ideals
----------------------------------------------------------------------

runIntclMonIdeal = method(Options => options normaliz)
runIntclMonIdeal(Ideal, String) := opts -> (I, nmzMode) -> (
    -- new variable for Rees algebra
    alpha := "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    ovars := gens ring I;
    i := 0;

    while (unique append(ovars, value alpha#i) == ovars and i < 52) do i=i+1;

    if i == 52 then error "runIntclMonIdeal: no free letter found for auxiliary variable";
    S := (coefficientRing ring I) (monoid[append(ovars, value alpha#i)]);
    f := map(S, ring I, delete(last gens S, gens S));
    runIntclMonIdeal(f I, nmzMode, last gens S, opts))

runIntclMonIdeal(Ideal, String, RingElement) := opts -> (I, nmzMode, t) -> (
    if not member(t, gens ring I) then error "runIntclMonIdeal: second argument must be a variable of the ring of the ideal.";
    if member(t, support I)       then error "runIntclMonIdeal: extra ring variable can not be used in the ideal.";

    numIvs := {};
    c := index t;
    expoVecs := mons2intmat(I, c);
    result := runNormaliz(expoVecs, nmzMode, opts);

    if not nmzGen then return;

    nmzData := changeColumns(result#"gen", c, -1);

    S1 := createMonomialSubalgebra intmat2mons(nmzData, ring I, 1, c);
    S2 := createMonomialSubalgebra intmat2mons(nmzData, ring I);
    S1.cache#"cone" = S2.cache#"cone" = result;
    (S1, S2))

intclMonIdeal=method(Options => options normaliz)
intclMonIdeal Ideal := opts -> I -> (
    (intcl, alg) := runIntclMonIdeal(I, "rees_algebra", opts);
    (ideal gens intcl, alg))

-- if there is a free variable in the ring, it can be given as second argument
intclMonIdeal(Ideal, RingElement) := opts -> (I, t) -> (
    if not member(t, gens ring I) then error "intclMonIdeal: second argument must be a variable of the ring of the ideal.";
    (intcl, alg) := runIntclMonIdeal(I, "rees_algebra", t, opts);
    (ideal gens intcl, alg))

----------------------------------------------------------------------
-- Ehrhart ring
----------------------------------------------------------------------

ehrhartRing = method(Options => { allComputations => false }) -- for polytopes we use a fixed grading
ehrhartRing List := opts -> L -> (
    if not uniform L then error "ehrhartRing: monomials must be elements of the same ring";
    if #L == 0       then error "ehrhartRing: expected non-empty list";
    runIntclMonIdeal(ideal L, "polytope", opts))
--ehrhartRing MonomialSubalgebra := opts -> S -> ehrhartRing(gens S, opts)

-- if there is a free variable in the ring, it can be given as second argument
ehrhartRing(List, RingElement) := opts -> (L, t) -> (
    if not uniform L then error "ehrhartRing: monomials must be elements of the same ring";
    if #L == 0       then error "ehrhartRing: expected non-empty list";
    I := ideal L;
    if not member(t, gens ring I) then error "ehrhartRing: second argument must be a variable of the ring of the ideal.";
    runIntclMonIdeal(I, "polytope", t, opts))
--ehrhartRing(MonomialSubalgebra, RingElement) := opts -> (S, t) -> ehrhartRing(gens S, t, opts)

----------------------------------------------------------------------
-- torus invariants and valuation rings and ideals
----------------------------------------------------------------------

torusInvariants = method(Options => options normaliz)
torusInvariants(Matrix, Ring) := opts -> (T, R) -> (
    if numgens R != numColumns T then error "torusInvariants: wrong number of columns in matrix";

    M := runNormaliz(T, "equations", opts);
    if not nmzGen then return; -- M = null

    rt := createMonomialSubalgebra intmat2mons(M#"gen", R);
    rt.cache#"cone" = M;
    rt)

finiteDiagInvariants = method(Options => options normaliz);
finiteDiagInvariants(Matrix, Ring) := opts -> (M, R) -> (
    if numgens R != numColumns M - 1 then error "finiteDiagInvariants: wrong number of columns in matrix";

    C := normaliz(M, "congruences", opts);
    rt := C#"gen";
    if rt === null then return createEmptyMonomialSubalgebra R;

    S := createMonomialSubalgebra intmat2mons(rt, R);
    S.cache#"cone" = C;
    S)

diagInvariants = method(Options => options normaliz);
diagInvariants(Matrix, Matrix, Ring) := opts -> (T, F, R) -> (
    if numgens R != numColumns T or numgens R != numColumns F - 1
    then error "diagInvariants: wrong number of columns in matrix";

    C := normaliz({(T, "equations"), (F, "congruences")}, opts);
    rt := C#"gen";
    if rt === null then return createEmptyMonomialSubalgebra R;

    S := createMonomialSubalgebra intmat2mons(rt, R);
    S.cache#"cone" = C;
    S)

intersectionValRings = method(Options => options normaliz)
intersectionValRings(Matrix, Ring) := opts -> (V, R) -> (
    if numgens R != numColumns V then error "intersectionValRings: wrong number of columns in matrix";

    I := id_(ZZ^(numColumns V)); -- identity matrix
    V1 := I || V;

    M := runNormaliz(V1, "inequalities", opts);
    if not nmzGen then return; -- M = null

    vr := createMonomialSubalgebra intmat2mons(M#"gen", R);
    vr.cache#"cone" = M;
    vr)


intersectionValRingIdeals = method(Options => options normaliz)
intersectionValRingIdeals(Matrix, Ring) := opts -> (V, R) -> (
    nc := numColumns V;
    if numgens R != nc - 1 then error "intersectionValRingIdeals: wrong number of columns in matrix";

    I:=id_(ZZ^nc); -- identity matrix
    V1:=I||V;
    V1=mutableMatrix V1;

    for i from 0 to numRows V -1
    do(
       V1_(i+nc,nc-1)=-V1_(i+nc,nc-1);
    );
    V1=matrix V1;

    nmzCone:=runNormaliz(V1, "inequalities", opts);
    if(not nmzGen) then return; -- nmzCone=null
    M:=nmzCone#"gen";

    R1 := createMonomialSubalgebra intmat2mons(M, R, 0, numColumns M - 1);
    R1.cache#"cone" = nmzCone; -- ????? TODO
    R2 := intmat2mons(M, R, 1, numColumns M - 1);

    new HashTable from {"subalgebra" => R1, "module generators" => R2})

----------------------------------------------------------------------

beginDocumentation()

document {
     Key => Normaliz,
     Headline => "an interface to use Normaliz in Macaulay 2",
     "The package ", EM "Normaliz"," provides an interface for the use of ", TT "Normaliz 2.8"," within Macaulay 2.",

PARA{}, "The program ", TT "Normaliz 2.8", " (referred to as ", TT "Normaliz", " in the following) is mainly a tool for computing the Hilbert bases and enumerative invariants of rational cones. Several additional data can be computed.
It is included in the Macaulay 2 distribution. For more details on the program, see ", HREF "http://www.math.uos.de/normaliz/", ".
For the theory of affine semigroups and the notions of commutative algebra we refer to
W. Bruns and J. Gubeladze, ", EM "Polytopes, rings and K-theory.", " Springer 2009.",
PARA{},"For algorithms see: ",
UL{
{ "W. Bruns and R. Koch, ", EM "Computing the integral closure of an affine semigroup. ", "Univ. Iagiell. Acta Math. 39, (2001), 59-70"},
{ "W. Bruns and B. Ichim ", EM "Normaliz: Algorithms for affine monoids and rational cones,"," J. Algebra (2010)", ", available at ", HREF "http://dx.doi.org/10.1016/j.jalgebra.2010.01.031"},
{ "W. Bruns, B. Ichim and C.Soeger ", EM "The power of pyramid decomposition in Normaliz,"," arXiv:1206.1916v1", ", available at ", HREF "http://arxiv.org/abs/1206.1916v1"},
},

PARA{},"Using ", TT "Normaliz", " one may for example compute the following: ",
UL{
   {TO "The Hilbert basis and the support hyperplanes of a rational cone."},
   {TO "The lattice points, Ehrhart series and the support hyperplanes of an integral polytope."},
   {TOH "The generators of the integral closure of the Rees algebra of a monomial ideal." },
},

PARA{},"If the associated semigroup or corresponding semigroup algebra is graded, then one may also compute the Hilbert series and Hilbert (quasi)polynomial of the semigroup.",

PARA{}, "The package gives direct access to ", TT "Normaliz", ". The exchange of data between ", TT "Normaliz", " and Macaulay 2 is via files. These files are automatically created and erased behind the scenes. As long as one wants to use only the ring-theoretic functions there is no need for file management. The key function for the direct use of ", TT "Normaliz", " is ", TO normaliz, ", which calls the program ", TT "Normaliz", ". To handle the in- and output one can use the functions ", TO writeNmzData, " and ", TO readNmzData, ", to set the options for the program ", TO setNmzOption, ". The output files are explained in ", TO "output files written by Normaliz",".",

PARA{},"If you want to keep the results of the computations by ", TT "Normaliz"," (i.e. the files written by the program), the package offers several methods for this purpose, see ", TO "Keeping results of the computation by Normaliz", " for an example how to do this.",

PARA{},"The package introduces two new classes ", TO MonomialSubalgebra , " and " , TO RationalCone, ".",

PARA{}, "The package provides four top level functions that aim directly at algebraic objects:",
UL{
   TO normalToricRing,
   TO intclToricRing,
   TO intclMonIdeal,
   TO ehrhartRing,
},

PARA{}, "The package offers the following additional functions:",
UL{
   TO torusInvariants,
   TO finiteDiagInvariants,
   TO diagInvariants,
   TO intersectionValRings,
   TO intersectionValRingIdeals,
},
}

document {
     Key => "The Hilbert basis and the support hyperplanes of a rational cone.",

"We want to calculate the Hilbert basis of a rational cone.  The cone may be given by ",
UL{
   "a system of generators;",
   "a linear system of inequalities;",
   "a linear system of equations."
},
PARA{}, "First of all consider the cone generated by the 16 vectors",

PRE"
 1 0 0 0 0 0 0             1 0 1 0 1 0 1
 0 1 0 0 0 0 0             1 0 0 1 0 1 1
 0 0 1 0 0 0 0             1 0 0 0 1 1 1
 0 0 0 1 0 0 0             0 1 1 0 0 1 1
 0 0 0 0 1 0 0             0 1 0 1 1 0 1
 0 0 0 0 0 1 0             0 1 0 0 1 1 1
 1 1 1 0 0 0 1             0 0 1 1 1 0 1
 1 1 0 1 0 0 1             0 0 1 1 0 1 1
",
PARA{},"in dimension 7. We compute its integral closure  in the ambient lattice ", TEX "\\ZZ^7", ". The fastest way is applying the function ", TO intclToricRing, " to the ideal that is generated by the monomials whose exponent vectors are the generators of the cone (to compute it in the group of the monoid generated by these vectors use ", TO normalToricRing, ") .",
PARA{},"One can convert the vectors to monomials in the following way:",
EXAMPLE {
  "R=ZZ/37[x_1..x_7];",
  "l={{1, 0, 0, 0, 0, 0, 0},
   {0, 1, 0, 0, 0, 0, 0},
   {0, 0, 1, 0, 0, 0, 0},
   {0, 0, 0, 1, 0, 0, 0},
   {0, 0, 0, 0, 1, 0, 0},
   {0, 0, 0, 0, 0, 1, 0},
   {1, 1, 1, 0, 0, 0, 1},
   {1, 1, 0, 1, 0, 0, 1},
   {1, 0, 1, 0, 1, 0, 1},
   {1, 0, 0, 1, 0, 1, 1},
   {1, 0, 0, 0, 1, 1, 1},
   {0, 1, 1, 0, 0, 1, 1},
   {0, 1, 0, 1, 1, 0, 1},
   {0, 1, 0, 0, 1, 1, 1},
   {0, 0, 1, 1, 1, 0, 1},
   {0, 0, 1, 1, 0, 1, 1}};",
  "L=for i in l list R_i",
  "S=intclToricRing L"
},
PARA{},"The function ", TO intclToricRing, " returns a ", TO MonomialSubalgebra, ". The Hilbert basis of the cone consists of the exponent vectors of the generators of the subalgebra. The generators can be extracted with ", TO gens, ".",
EXAMPLE lines ///
hb = flatten \ exponents \  gens S
///,
PARA{}, "It is also possible to call ", TT "Normaliz", " directly to do this computation by using the function ", TO normaliz, ". It takes as input the matrix containing the generators as rows, and the type for the computation.",

EXAMPLE lines ///
M=matrix l;
d=(normaliz(M,"normalization"))#"gen"
set entries d===set hb
///,
PARA{}, "The result is an object of type ", TO RationalCone, " from which you obtain the Hilbert basis via the key \"gen\".",
PARA{}, "The support hyperplanes are stored in the ", TT "Normaliz", " file <filename>.sup. To inspect these, one has to assure that the computations by ", TT "Normaliz", " are kept. This can be done by specifying a filename, that is, assigning a non-empty string to the global variable ", TO "nmzFilename", ", see also ", TO "Keeping results of the computation by Normaliz", ". Then use ", TO readNmzData, " to read the .sup file.",


EXAMPLE lines ///
 nmzFilename="rproj2";
 intclToricRing L;
hypes=readNmzData("sup")
///,

PARA{}, "This means there are 24 support hyperplanes in ", TEX "\\ZZ^7",". So in contrast to the convention of M2 Normaliz writes vectors as rows. If you don't want to keep the results of the computations any longer, use ", TO rmNmzFiles, " to delete the files created by ", TT "Normaliz", ".",

EXAMPLE lines ///
 rmNmzFiles();
///,

HR{},
PARA{}, "Conversely, suppose the cone is given by the above hyperplanes. In that case, to compute the Hilbert basis, you should use ", TT "Normaliz", " in type inequalities. The result is an object of type ", TO RationalCone,". The Hilbert basis is accessible via the key \"gen\". Not surprisingly, it is the same Hilbert basis as above (but in another order).",
EXAMPLE lines ///
normaliz(hypes,"inequalities")
set entries oo#"gen"===set hb
///,

PARA{}, "Note that for the comparison ", TO set, " has to be used because the order of the generators may differ.",
HR{},
PARA{}, "To illustrate the third possibility we compute the Hilbert basis of the cone given by the equations ",
PRE"
1 1 1 -1 -1 -1  0  0  0
1 1 1  0  0  0 -1 -1 -1
0 1 1 -1  0  0 -1  0  0
1 0 1  0 -1  0  0 -1  0
1 1 0  0  0 -1  0  0 -1
0 1 1  0 -1  0  0  0 -1
1 1 0  0 -1  0 -1  0  0
",
PARA{}, "(this is the solution cone for a 3x3 magic square). To this end one has to choose type 5.",
EXAMPLE lines ///
eq=matrix {{1, 1, 1, -1, -1, -1,  0,  0,  0}, {1, 1, 1,  0,  0,  0, -1, -1, -1}, {0, 1, 1, -1,  0,  0, -1,  0,  0}, {1, 0, 1,  0, -1,  0,  0, -1,  0}, {1, 1, 0,  0,  0, -1,  0,  0, -1}, {0, 1, 1,  0, -1,  0,  0,  0, -1}, {1, 1, 0,  0, -1,  0, -1,  0,  0}};
normaliz(eq,"equations")
///,
"Again the rows of this matrix are the elements of the Hilbert basis.",
}

document {
     Key => "The lattice points, Ehrhart series and the support hyperplanes of an integral polytope.",

"The lattice points of the integral polytope with the 4 vertices",
PRE"
 (0,0,0),  (2,0,0),  (0,3,0),  (0,0,5)
",
"in ", TEX "\\RR^3", " are to be computed. This can be done using the function ", TO ehrhartRing, ".",

EXAMPLE lines ///
R=ZZ/37[x,y,z];
L={x^0,x^2,y^3,z^5};
(l,e)=ehrhartRing L;
 l
 e
 flatten \ exponents \ gens l
///,
PARA{},"The lattice points of the polytope are the exponent vectors of the generators of the first subalgebra, that can be obtained with ", TO exponents, " if necessary. Note that in this example there were no free auxiliary variable in the ring, so a new variable ", EM "a", " was added, whence at first the exponents have four components. The exponent vectors of the generators of the second subalgebra are the generators of the Ehrhart semigroup (the semigroup determined by the polytope).",
PARA{}, "It is also possible to call ", TT "Normaliz", " directly to do this computation by using the function ", TO normaliz, ". It takes a matrix as input, whose rows are the vertices of the polytope, and the type for the computation. It returns an object of type ", TO RationalCone, " that gives access to the lattice points via the key \" gen\".",
EXAMPLE lines ///
M=matrix {{0,0,0},{2,0,0},{0,3,0},{0,0,5}};
(normaliz(M,"polytope"))#"gen"
///,
}

document {
     Key =>"The generators of the integral closure of the Rees algebra of a monomial ideal.",
PARA{},"We use ", TO intclMonIdeal, " to compute the integral closure of a monomial ideal and of its Rees algebra.",
EXAMPLE lines ///
  R=ZZ/37[x_1..x_7];
  I=ideal(x_1..x_6, x_1*x_2*x_3*x_7, x_1*x_2*x_4*x_7, x_1*x_3*x_5*x_7, x_1*x_4*x_6*x_7, x_1*x_5*x_6*x_7, x_2*x_3*x_6*x_7, x_2*x_4*x_5*x_7, x_2*x_5*x_6*x_7,x_3*x_4*x_5*x_7,x_3*x_4*x_6*x_7);
  (intcl,rees)=intclMonIdeal I;
  intcl
  rees
///,
PARA{}, "The first entry is an ideal, the integral closure of the original ideal, the second one a monomial subalgebra. Each variable in the example appears in a generator of the ideal. Therefore an auxiliary variable ", EM "a", " is added to the ring. If there were a free variable in the ring, say ", TEX "x_8", ", then one can give this variable as a second argument to the function, which then is used as auxiliary variable.",
EXAMPLE lines ///
  R=ZZ/37[x_1..x_8];
  I=ideal(x_1..x_6, x_1*x_2*x_3*x_7, x_1*x_2*x_4*x_7, x_1*x_3*x_5*x_7, x_1*x_4*x_6*x_7, x_1*x_5*x_6*x_7, x_2*x_3*x_6*x_7, x_2*x_4*x_5*x_7, x_2*x_5*x_6*x_7,x_3*x_4*x_5*x_7,x_3*x_4*x_6*x_7);
  (intcl,rees)=intclMonIdeal(I,x_8);
  intcl
  rees
///,
}


document {
    Key => MonomialSubalgebra,
    Headline => "class of monomial subalgebras",
PARA{},
"A monomial subalgebra is a subalgebra of a polynomial ring generated by monomials. In other words, it is a monoid algebra. A new monomial subalgebra can be created using ", TO createMonomialSubalgebra, ".",
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
ring S
gens S
///,
SeeAlso => createMonomialSubalgebra,
}

document {
   Key => {createMonomialSubalgebra, (createMonomialSubalgebra,List)},
   Headline => "creates a monomial subalgebra",
   Usage => "createMonomialSubalgebra L",
   Inputs => {List => "list of monomials that are the generators of the subalgebra"},
   Outputs => { MonomialSubalgebra => " the subalgebra of the polynomial ring of the monomials that has the given monomials as generators"},
PARA{},
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
///,
SeeAlso =>MonomialSubalgebra
}



document {
   Key => (vars,MonomialSubalgebra),
   Headline => "row matrix of the generators of a monomial subalgebra",
   Usage => "vars S",
   Inputs => {MonomialSubalgebra => " "},
   Outputs => {Matrix => " with one row whose entries are the generators of the monomial subalgebra S"},
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
vars S
///,
TEST ///
R=ZZ/37[x,y,z];
assert (vars createMonomialSubalgebra {x^2*y, x*z, z^3}==matrix({{x^2*y, x*z, z^3}}))
///,
SeeAlso=> (gens, MonomialSubalgebra)
}

document {
   Key => (gens,MonomialSubalgebra),
   Headline => "generators of a monomial subalgebra",
   Usage => "gens S",
   Inputs => {MonomialSubalgebra => " "},
   Outputs => {List => " whose entries are the generators of the monomial subalgebra S"},
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
gens S
///,
TEST ///
R=ZZ/37[x,y,z];
assert (gens createMonomialSubalgebra {x^2*y, x*z, z^3}=={x^2*y, x*z, z^3})
///,
SeeAlso => (vars,MonomialSubalgebra)
}

document {
   Key => (numgens,MonomialSubalgebra),
   Headline => "number of generators of a monomial subalgebra",
   Usage => "numgens S",
   Inputs => {MonomialSubalgebra => " "},
   Outputs => {ZZ => " the number of generators of the monomial subalgebra S"},
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
numgens S
///,
TEST ///
R=ZZ/37[x,y,z];
assert (numgens createMonomialSubalgebra {x^2*y, x*z, z^3}==3)
///,
}

document {
   Key => (ring,MonomialSubalgebra),
   Headline => "surrounding ring of a monomial subalgebra",
   Usage => "ring S",
   Inputs => {MonomialSubalgebra => " "},
   Outputs => {Ring => "the surrounding ring of the monomial subalgebra S"},
EXAMPLE lines ///
R=ZZ/37[x,y,z];
S=createMonomialSubalgebra {x^2*y, x*z, z^3}
ring S
///,
TEST ///
R=ZZ/37[x,y,z];
assert (ring createMonomialSubalgebra {x^2*y, x*z, z^3}===R)
///,
}



document {
  Key => RationalCone,
  Headline => "class of rational cones",
PARA{}, "The method ", TO normaliz, " returns an object of type RationalCone. By default, that cone contains only the content of the output file .gen, under the key \"gen\" (i.e. the generators that have been computed) and the content of the output file .inv, which contains some additional data. Note that Normaliz writes the generators as rows, in contrast to the convention used in M2.",
EXAMPLE lines ///
setNmzOption("allf",true);
eq=matrix {{1, 1, 1, -1, -1, -1,  0,  0,  0}, {1, 1, 1,  0,  0,  0, -1, -1, -1}, {0, 1, 1, -1,  0,  0, -1,  0,  0}, {1, 0, 1,  0, -1,  0,  0, -1,  0}, {1, 1, 0,  0,  0, -1,  0,  0, -1}, {0, 1, 1,  0, -1,  0,  0,  0, -1}, {1, 1, 0,  0, -1,  0, -1,  0,  0}};
rc=normaliz(eq,"equations");
rc#"gen"
rc#"inv"
///,
PARA{}, "To obtain all the information written by ", TT "Normaliz", " set the option ", TO allComputations, " to true. Then the method returns an object of type RationalCone whose keys are the suffixes of all the output files written, with value the content of the corresponding output file (also to be read line by line).",
EXAMPLE lines ///
arc=normaliz(allComputations=>true,eq,"equations");
arc#"gen"
arc#"ext"
///,
PARA{}, "See ", TO "output files written by Normaliz", " for an explanation of the different output files.",
SeeAlso => {allComputations, readNmzData, "Keeping results of the computation by Normaliz","output files written by Normaliz"},
}

document {
   Key => "output files written by Normaliz",
PARA{},"Depending on the options enabled (see ", TO setNmzOption, "), ", TT "Normaliz", " writes additional output files. To obtain the content of these files within Macaulay2, use ", TO readNmzData, " or ", TO allComputations,". The following files may be written, provided certain conditions are satisfied and the information that should go into them has been computed. We denote the files simply by their types.
For the most types of inputs the ambient lattice is ", TEX "\\ZZ^n", " if the input of Normaliz is a matrix of n columns. In types polytope and rees_algebra the ambient lattice is ", TEX "\\ZZ^{n+1}", " since the input vectors are extended by 1 component. For congruences and inhomogeneous input it is ", TEX "\\ZZ^{n-1}", " and for inhomogeneous congruences ", TEX "\\ZZ^{n-2}", ".
For input of type normal_toric_ideal the lattice is ", TEX "\\ZZ^{r}", " where n-r is the rank of the input matrix. The essential lattice is gp(M) where M is the monoid computed by Normaliz internally, i.e. after a linear transformation such that the cone is full-dimensional and the integral closure has to be computed.
See the documentation of Normaliz at ", HREF "https://github.com/Normaliz/Normaliz/blob/master/doc/Normaliz.pdf", " for more details.",
UL{
   {TT "gen      ", "   The Hilbert basis"},
   {TT "ext      ", "   The extreme rays"},
   {TT "cst      ", "   The constraints defining the cone and the lattice in the same format as they would appear in the input. Using this file as input for ", TT "Normaliz"," will reproduce the Hilbert basis and all the other data computed."},
   {TT "egn, esp  ", "   These contain the Hilbert basis and the support hyperplanes respectively, however with respect to the essential lattice and a basis of it."},
   {TT "typ       ", "   This is the product of the matrices corresponding to ", TT "egn", " and ",  TT "esp", ". That is, the support hyperplanes of the cone are evaluated (as  linear forms) on the generators. "},
   {TT "tri       ", "   The file ", TT "tri", " contains a triangulation of the cone computed by ", TT "Normaliz", ". Each of the rows of the matrix specifies a simplicial cone D: the entries except the last are the indices (with respect to the order in ", TT "tgn",") of those generators that span D, and the last entry is the multiplicity of D in the essential lattice, i.e. the absolute value of the determinant of the matrix of the spanning vectors (as elements of the essential lattice)."},
   {TT "tgn        ", "The file ", TT "tgn", " contains a matrix of vectors (in the coordinates of the ambient lattice) spanning the simplicial cones in the triangulation."},
   {TT "ht1        ", "If the there was a grading available, the file ", TT "ht1", " contains the degree 1 elements of the cone."},
   {TT "inv         ", "   The file ", TT "inv", " contains all the information computed that is not contained in any of the other files, e.g. the h-vector, the Hilbert polynomial, whether the semigroup is generated in degree 1, the index, the multiplicity, and the cardinality of sets like the Hilbert basis, support hyperplanes and so one."},
},
}


document {
     Key => "Keeping results of the computation by Normaliz",
PARA{}, TT "Normaliz", " prints the Hilbert basis and some auxiliary data as, e.g., the support hyperplanes, into files. When ", TT "Normaliz", " is called within the package Normaliz, all the files created are deleted at the end of the function call by default. Sometimes it is desirable to keep these results for later use. To switch the file handling from \"delete\" to \"keep\", a filename has to be specified in the global variable ", TO "nmzFilename", ".",
EXAMPLE lines ///
nmzFilename="polytope";
setNmzOption("allf",true);
R=ZZ/37[x,y,z];
ehrhartRing {x^0,x^2,y^3,z^5};
///,
PARA{}, "Now all the files created by ", TT "Normaliz", " are saved as ", TT "polytope.suffix", ". The generators of the integral closure are in the file with suffix ", TT "gen", ", and the functions of the package return always the content of this file. The support hyperplanes, the defining equations and congruences are printed into the file with suffix ", TT "cst", ". For more suffixes see ", TO "output files written by Normaliz", ".  Use ", TO readNmzData , " to read the files into Macaulay 2 provided they have been written, except for the suffix ", TT "cst", " you should use ", TO readMultipleNmzData," as there are several matrices in this file. Which files are written depends on the input data and on the \"computation mode\" which is determined by the options set, here the option ", TT "allf", " means that all possible data is written. For more options see ", TO setNmzOption, ".",
EXAMPLE lines ///
extremeRays=readNmzData "ext"
constraints=readMultipleNmzData "cst"
///,
PARA{},"The filename is kept during the Macaulay 2 process until another filename is specified.",
EXAMPLE lines ///
nmzFilename="square";
nmzFilename=""; -- deletes the filename
///,
PARA{}, "The files are kept beyond the Macaulay 2 process. To delete them call the function ", TO rmNmzFiles,". Assure yourself that the right filename is specified before calling the function! This function also resets ", TO "nmzFilename", " to the empty string.",
EXAMPLE lines ///
nmzFilename="polytope";
rmNmzFiles();
nmzFilename
///,
PARA{},"If you want to change the directory where the files are saved (default is the current directory) you have two possibilities. If you want work in the same directory most of the time, you can define this in a file \"start.m2\" in the current directory and add a line in \"init.m2\" such that it is read when starting Macaulay 2. If you want to switch between directories more frequently, you can specify the directory in the global variable ", TO "nmzDataPath", ".",
}

document {
  Key => "nmzVersion",
  Headline => "global variable holding the Normaliz version",
PARA{},"The executable of the program ",  TT "Normaliz",  ". The package Normaliz uses the version stored in the global variable nmzVersion. It can be ", TT "normaliz", ", ",  TT "norm 64",  " or ",  TT "normbig", ". If no version is specified it will use ",  TT "normaliz", ". This can be used to use executables of Normaliz version 2.5 with this version of the Macaulay package.",
EXAMPLE lines ///
nmzVersion
nmzVersion="normbig";
nmzVersion
///,
}
document  {
   Key => "nmzDataPath",
   Headline => "global variable, the path where Normaliz stores its files",
PARA{}, "This global variable stores the file path where ", TT "Normaliz", " stores the files written. By default it is the empty string which means that the files are stored in the current directory. If no filename is specified, this variable is not used. There is no check whether the assigned directory exists. If it does not exist, ", TT  "Normaliz"," will issue an error message. Use e.g. ", TO makeDirectory," to create an directory within Macaulay 2.",
 EXAMPLE lines ///
 nmzDataPath
 nmzDataPath="d:/Normaliz2.5Windows/example";
 nmzDataPath
///,
Caveat=>"Note that the path should not contain $ since Macaulay 2 seems to have problems with such paths.",
SeeAlso=>"Keeping results of the computation by Normaliz"
}

document  {
   Key => "nmzFilename",
   Headline => "global variable holding the filename",
PARA{},"The user can specify a filename in the global variable nmzFilename to switch the file handling from \"delete\" to \"keep\". Note that the files written by ", TT "Normaliz", " are not removed automatically if nmzFilename is not the empty string. Use ", TO rmNmzFiles," to remove these files manually.",
EXAMPLE lines ///
nmzFilename="polytope";
setNmzOption("allf",true);
R=ZZ/37[x,y,z];
hb=intclToricRing {x^2,y^2,z^2};
extremalRays=readNmzData "ext"
rmNmzFiles();
nmzFilename
///,
SeeAlso =>"Keeping results of the computation by Normaliz"
}

document {
  Key => "nmzNumberThreads",
  Headline => "global variable holding the number of threads",
PARA{},"This global variable holds a positive integer limiting the number of threads that ", TT "Normaliz"," can access on your
system. If you want to run ", TT "Normaliz", " in a strictly serial mode, choose nmzNumberThreads=1. The content of this global variable is ignored unless the option \"threads\" is enabled.",
EXAMPLE lines ///
nmzNumberThreads=2;
showNmzOptions()
setNmzOption("threads",true);
showNmzOptions()
///,
}

document {
     Key => rmNmzFiles,
     Headline => "removes the files created by Normaliz",
     Usage => "rmNmzFiles()",
     PARA{},"This function removes the files created for and by ", TT "Normaliz", ", using the last filename created. These files are removed automatically unless a (non-empty) filename has been specified in the global variable ", TO "nmzFilename",". In this case the filename is reset to the empty string.",
     EXAMPLE lines ///
          nmzFilename="VeryInteresting";
          R=ZZ/37[x,y,z];
          normalToricRing {x^2*y, y^3};
          get ("VeryInteresting.cst")
          rmNmzFiles();
          nmzFilename
          ///,
     TEST ///
     nmzFilename="VeryInteresting";
          R=ZZ/37[x,y,z];

          normalToricRing {x^2*y, y^3};
          rmNmzFiles();
          assert( nmzFilename=="");
      assert ( fileExists("VeryInteresting.gen")==false)
     ///,
     }

document {
     Key => {writeNmzData, },
     Headline => "creates an input file for Normaliz",
     PARA{},"This function creates an input file for ", TT "Normaliz", " containing one or several matrices, whose rows are considered according to the type:",
     UL {
         "integral closure, normalization: generators of a rational cone",
         "polytope:   lattice points spanning a polytope",
         "rees_algebra:   exponent vectors of monomials generating an ideal",
         "inequalities, equations, congruences:   constraints defining the cone to be computed",
         "inhom_inequalities, inhom_equations, inhom_congruences:   inhomogeneous constraints defining the cone to be computed",
         "normal_toric_ideal:  generators of a lattice ideal",
         "grading:  a grading which gives positive degree to all generators"
     },
     "For a more detailed list see the Normaliz documentation.",
}

document {
     Key => {(writeNmzData, Matrix, String),},
     Headline => "creates an input file for Normaliz with one matrix",
     Usage => "writeNmzData(mat, nmzType)",
     Inputs =>{
                Matrix => "whose rows are interpreted according to the type",
                String => "the type"
      },
      Consequences => {"an input file filename.in is written, using the last filename created"},
     PARA{},"This function creates an input file for ", TT "Normaliz", " with a single input matrix and its type. ",
            "If no filename has been specified, an error occurs.",
     PARA{},
     EXAMPLE lines ///
          nmzFilename="example"; -- to keep the files
          mat=matrix({{1,2,3},{4,5,6},{7,8,10}})
          writeNmzData(mat,"normalization")
          get ("example.in")
          rmNmzFiles();
          ///,
     SeeAlso => readNmzData,
     TEST ///
      nmzFilename="example";
          sgr=matrix({{1,2,3},{4,5,6},{7,8,10}})
          writeNmzData(sgr,"normalization")
          assert (lines get (nmzDataPath|nmzFilename|".in")=={"3","3","1 2 3 ","4 5 6 ","7 8 10 ","normalization"})
     ///,
     }

document {
     Key => {(writeNmzData,List)},
     Headline => "creates an input file for Normaliz with several matrices",
     Usage => "writeNmzData L",
     Inputs =>{
               List => "containing pairs (mat,nmzType)",},
PARA{},"This function writes an input file for ", TT "Normaliz", " containing several matrices. The input is a list consisting of pairs (mat,nmzType), each is handled as in ", TO writeNmzData," but all written into the same input file. If no filename has been specified, an error occurs.",
     EXAMPLE lines ///
          nmzFilename="example"; -- to keep the files
          hy=(matrix {{1, -1, 0},{1, 1, -2}},"inequalities")
          eq=(matrix {{1, 2, 3},{2, 2, 3}},"equations")
          cg=(matrix {{9, 8, 7},{7, 6, 5}},"congruences")
          writeNmzData {hy, eq, cg};
          get ("example.in")
          rmNmzFiles();
          ///,
}


document {
     Key => {readNmzData, (readNmzData, String)},
     Headline => "reads an output file of Normaliz containing one matrix",
     Usage => "readNmzData s",
     Inputs => {
                String => "the suffix of the file to be read"
     },
     Outputs => {
                 Matrix => " the content of the file"
     },
     PARA{},"Reads an output file of ", TT "Normaliz", " containing an integer matrix and returns it as a ", TO "Matrix", ", whose rows contains the data computed (in contrast to the convention used in M2). To read the ", TT ".inv", " file, use ", TO getNumInvs, ". The filename is created from the current filename specified by the user and the suffix given to the function. The possible suffixes depend on the input and the computation mode. The computation mode is controlled via the options, see ", TO setNmzOption, ". For the possible output files see ", TO "output files written by Normaliz", ". For more details we refer to the documentation of ", TT "Normaliz", " available as pdf file at ", HREF "http://www.math.uos.de/normaliz/", ".",
     EXAMPLE lines ///
         nmzFilename="example" -- to keep the files
         setNmzOption("allf",true); -- to write all files
         mat=matrix({{1,2,3},{4,5,6},{7,8,10}});
         normaliz(mat,"integral_closure")
         readNmzData "typ"
         rmNmzFiles();
          ///,
     SeeAlso => {writeNmzData, normaliz, allComputations, "Keeping results of the computation by Normaliz","output files written by Normaliz" },
     TEST ///
          nmzFilename="example";
          sgr=matrix({{1,1,1,-1,-1,-1,0,0,0},
{1, 1, 1,  0,  0,  0, -1, -1, -1},
{0, 1, 1, -1,  0,  0, -1,  0,  0},
{1, 0, 1,  0, -1,  0,  0, -1,  0},
{1, 1, 0,  0,  0, -1,  0,  0, -1},
{0, 1, 1,  0, -1,  0,  0,  0, -1},
{1, 1, 0,  0, -1,  0, -1,  0,  0}}); --3x3magic
          normaliz(sgr,"equations");
          assert ( sort transpose readNmzData "gen"==sort transpose matrix({{1, 2, 0, 0, 1, 2, 2, 0, 1},
{0, 2, 1, 2, 1, 0, 1, 0, 2},
{1, 1, 1, 1, 1, 1, 1, 1, 1},
{2, 0, 1, 0, 1, 2, 1, 2, 0},
{1, 0, 2, 2, 1, 0, 0, 2, 1}}) )
    rmNmzFiles();
     ///,
     }

document {
     Key => {readMultipleNmzData, (readMultipleNmzData, String)},
     Headline => "reads an output file of Normaliz containing several matrices",
     Usage => "readNmzData s",
     Inputs => {
                String => "the suffix of the file to be read"
     },
     Outputs => {
                 List => " the content of the file"
     },
    PARA{},"This function can read several matrices from a ", TT "Normaliz", " output file. At the moment, the only output file that contains several matrices is the file with suffix ", TT "cst", ". It contains the supporting hyperplanes, the defining equations and the congruences defining the (same) cone. It is possible that one of the matrices is a matrix with zero rows.",
    PARA{},
    EXAMPLE lines ///
         nmzFilename="example" -- to keep the files
         mat=matrix({{1,2,3},{4,5,6},{7,8,10}});
         normaliz(allComputations => true,mat,"integral_closure")
         readMultipleNmzData "cst"
         rmNmzFiles();
///,
}

document {
     Key => {normaliz},
     Headline => "calls Normaliz",
     PARA{}, "This function applies ", TT "Normaliz", " to the input data, which can be a matrix specifying a cone and an integer indicating the type for ", TT "Normaliz", " or a list consisting of pairs of such a matrix and an integer. The function returns an object of type ", TO RationalCone, ". The type determines how the rows of the matrix are interpreted, see also ", TO writeNmzData, ":",
     UL{
        {"integral_closure: Computes the Hilbert basis of the rational cone generated by the rows with respect to the ambient lattice ", TEX "\\ZZ^n", ";"},
        {"normalization: The same as integral_closure, but with respect to the sublattice of ", TEX "\\ZZ^n", " generated by the rows;"},
        {"polytope: Computes the integral points in the polytope spanned by the rows and its Ehrhart semigroup (the semigroup determined by the polytope);"},
        {"rees_algebra: Computes the integral closure of the Rees algebra of the ideal generated by the monomials with exponent vectors the rows;"},
        {"inequalities: Computes the Hilbert basis of the rational cone in ", TEX "\\RR^m", " given by the system of homogeneous inequalities ", TT "mat ", TEX "x\\ \\geq\\ 0", ";"},
        {"equations: Computes the Hilbert basis of the rational cone given by the nonnegative solutions of the homogeneous system ", TT "mat ", TEX "x\\ =\\ 0", "."},
        {"congruences: Computes the Hilbert basis of the rational cone given by the nonnegative solutions of the system of congruences defined by the rows as follows: Each row (",TEX "x_1,\\dots,x_n,c",") represents a congruence ",TEX "x_1 z_1+\\dots+x_n z_n \\equiv \\ 0 \\mod \\ c","."},
        {"inhom_inequalities: Computes the Hilbert basis of the rational cone in ", TEX "\\RR^m", " given by the system of inhomogeneous inequalities. Each row (",TEX "x_1,\\dots,x_n,b",") represents an inequality ",TEX "x_1 z_1+\\dots+x_n z_n + b \\geq \\ 0","."},
        {"inhom_equations: Computes the Hilbert basis of the rational cone given by the nonnegative solutions of the inhomogeneous system ", TT "mat ", TEX "x\\ =\\ b", "."},
        {"inhom_congruences: Computes the Hilbert basis of the rational cone given by the nonnegative solutions of the system of congruences defined by the rows as follows: Each row (",TEX "x_1,\\dots,x_n,b,c",") represents a congruence ",TEX "x_1 z_1+\\dots+x_n z_n + b \\equiv \\ 0 \\mod \\ c","."},
        {"normal_toric_ideal: Computes the monoid as a quotient of ", TEX"\\ZZ_+^n"," modulo a system of congruences (in the semigroup sense) defined by the rows of the input matrix."},
     },
PARA{},"It is possible to combine certain input types, see ", TO (normaliz,List),". If you want to input only one matrix you can also use ", TO (normaliz,Matrix,String),".",
 PARA{},"By default, the cone returned contains only the content of the output file .gen, under the key \"gen\", i.e. the generators that have been computed, line by line, and the content of the output file .inv, under the key \"inv\".",
EXAMPLE lines ///
setNmzOption("allf",true);
eq=matrix {{1, 1, 1, -1, -1, -1,  0,  0,  0}, {1, 1, 1,  0,  0,  0, -1, -1, -1}, {0, 1, 1, -1,  0,  0, -1,  0,  0}, {1, 0, 1,  0, -1,  0,  0, -1,  0}, {1, 1, 0,  0,  0, -1,  0,  0, -1}, {0, 1, 1,  0, -1,  0,  0,  0, -1}, {1, 1, 0,  0, -1,  0, -1,  0,  0}};
rc=normaliz(eq,"equations")
///,
PARA{}, "To obtain all the information written by ", TT "Normaliz", " set the option ", TO allComputations, " to true (to decide which information shall be written by ", TT "Normaliz", " use the options for ", TT "Normaliz", ", see ", TO setNmzOption, "). Then the method returns an object of type RationalCone whose keys are the suffixes of all the output files written, with value the content of the corresponding output file, which is an matrix whose rows contain the data computed, except for the suffix ", TT "inv", ", for which the type is a ", TO HashTable," (see also ", TO getNumInvs,").",
EXAMPLE lines ///
arc=normaliz(allComputations=>true,eq,"equations");
arc#"gen"
arc#"ext"
arc#"inv"
///,
SeeAlso => {RationalCone, readNmzData,"Keeping results of the computation by Normaliz", "output files written by Normaliz"},
TEST ///
          nmzFilename="example";
          mat=matrix({{1,1,1,-1,-1,-1,0,0,0},
{1, 1, 1,  0,  0,  0, -1, -1, -1},
{0, 1, 1, -1,  0,  0, -1,  0,  0},
{1, 0, 1,  0, -1,  0,  0, -1,  0},
{1, 1, 0,  0,  0, -1,  0,  0, -1},
{0, 1, 1,  0, -1,  0,  0,  0, -1},
{1, 1, 0,  0, -1,  0, -1,  0,  0}}); --3x3magic
          normaliz(mat,"equations");
          assert ( sort transpose readNmzData "gen"==sort transpose matrix({{1, 2, 0, 0, 1, 2, 2, 0, 1},
{0, 2, 1, 2, 1, 0, 1, 0, 2},
{1, 1, 1, 1, 1, 1, 1, 1, 1},
{2, 0, 1, 0, 1, 2, 1, 2, 0},
{1, 0, 2, 2, 1, 0, 0, 2, 1}}) )
    rmNmzFiles();
     ///,
}


document {
     Key => {(normaliz, Matrix, String), [(normaliz, Matrix, String), allComputations], [(normaliz,Matrix,String),grading]},
     Headline => "calls Normaliz",
     Usage => "normaliz(mat,nmzType)",
     Inputs => {
                Matrix => {TT "mat", " the input matrix for ", TT "Normaliz"},
                String =>{TT "nmzType", " the type"}},
     Outputs => {RationalCone => {"generators of the integral closure and optional output from ", TT "Normaliz"}},
     PARA{}, "This function applies ", TT "Normaliz", " to the parameter ", TT "mat", " in the type set by ", TT "nmzType", ". The function returns an object of type ", TO RationalCone, " defined by the file with suffix ", TT "gen", " , if computed, and possibly some additional information. The type determines how the rows of the matrix are interpreted, see also ", TO (normaliz,List)," and ", TO writeNmzData, ".",
 PARA{},"By default, the cone returned contains only the content of the output file .gen, under the key \"gen\", i.e. the generators that have been computed, line by line, and the content of the output file .inv, under the key \"inv\".",
EXAMPLE lines ///
setNmzOption("allf",true);
eq=matrix {{1, 1, 1, -1, -1, -1,  0,  0,  0}, {1, 1, 1,  0,  0,  0, -1, -1, -1}, {0, 1, 1, -1,  0,  0, -1,  0,  0}, {1, 0, 1,  0, -1,  0,  0, -1,  0}, {1, 1, 0,  0,  0, -1,  0,  0, -1}, {0, 1, 1,  0, -1,  0,  0,  0, -1}, {1, 1, 0,  0, -1,  0, -1,  0,  0}};
rc=normaliz(eq,"equations");
rc#"gen"
///,
PARA{}, "To obtain all the information written by ", TT "Normaliz", " set the option ", TO allComputations, " to true (to decide which information shall be written by ", TT "Normaliz", " use the options for ", TT "Normaliz", ", see ", TO setNmzOption, "). Then the method returns an object of type RationalCone whose keys are the suffixes of all the output files written, with value the content of the corresponding output file, which is an matrix whose rows contain the data computed, except for the suffix ", TT "inv", ", for which the type is a ", TO HashTable," (see also ", TO getNumInvs,"). It can also be used with the option ", TO grading ,".",
EXAMPLE lines ///
arc=normaliz(allComputations=>true,eq,"equations");
arc#"gen"
arc#"ext"
arc#"inv"
///,
SeeAlso => {RationalCone, readNmzData,"Keeping results of the computation by Normaliz", "output files written by Normaliz"},
TEST ///
          nmzFilename="example";
          mat=matrix({{1,1,1,-1,-1,-1,0,0,0},
{1, 1, 1,  0,  0,  0, -1, -1, -1},
{0, 1, 1, -1,  0,  0, -1,  0,  0},
{1, 0, 1,  0, -1,  0,  0, -1,  0},
{1, 1, 0,  0,  0, -1,  0,  0, -1},
{0, 1, 1,  0, -1,  0,  0,  0, -1},
{1, 1, 0,  0, -1,  0, -1,  0,  0}}); --3x3magic
          normaliz(mat,"equations");
          assert ( sort transpose readNmzData "gen"==sort transpose matrix({{1, 2, 0, 0, 1, 2, 2, 0, 1},
{0, 2, 1, 2, 1, 0, 1, 0, 2},
{1, 1, 1, 1, 1, 1, 1, 1, 1},
{2, 0, 1, 0, 1, 2, 1, 2, 0},
{1, 0, 2, 2, 1, 0, 0, 2, 1}}) )
    rmNmzFiles();
     ///,
}

document{
    Key =>  {(normaliz, List), [(normaliz, List), allComputations],[(normaliz,List),grading]},
     Headline => "calls Normaliz with several input matrices",
  Usage => "normaliz L",
     Inputs => {
                List => {"a list of pairs (mat,nmzType)"}},
     Outputs => {RationalCone => {"generators of the integral closure and optional output from ", TT "Normaliz"}},

     PARA{}, "This function applies ", TT "Normaliz", " to all ", TT "mat", " each in the type set by the second parameter ", TT "nmzType", ". The function returns an object of type ", TO RationalCone, " defined by the file with suffix ", TT "gen", " , if computed, and possibly some additional information.",
"It is possibly to give several matrices of the same type. All matrices of one type are then appended to one matrix by ", TT "Normaliz",".",
 PARA{},"By default, the cone returned contains only the content of the output file .gen, under the key \"gen\", i.e. the generators that have been computed, line by line and the content of the output file .inv, under the key \"inv\".",
EXAMPLE lines ///
hy=matrix {{-1,0,-1,0,3,0,0,0,0},{-1,0,1,0,1,0,0,0,0},{1,0,1,0,-1,0,0,0,0},{1,0,-1,0,1,0,0,0,0}};
eq=matrix {{1,1,1,-1,-1,-1,0,0,0},{1,1,1,0,0,0,-1,-1,-1},{0,1,1,-1,0, 0,-1,0,0},{1,0,1,0,-1,0,0,-1,0},{1,1,0,0,0,-1,0,0,-1},{0,1,1,0,-1,0,0,0,-1},{1,1,0,0,-1,0,-1,0,0}};
cg=matrix {{1,0,0,0,0,0,0,0,0,2},{0,0,1,0,0,0,0,0,0,2},{0,0,0,0,0,0,1,0,0,2},{0,0,0,0,0,0,0,0,1,2}};
rc=normaliz({(hy,"inequalities"),(eq,"equations"),(cg,"congruences")});
rc#"gen"
///,
PARA{}, "To obtain all the information written by ", TT "Normaliz", " set the option ", TO allComputations, " to true (to decide which information shall be written by ", TT "Normaliz", " use the options for ", TT "Normaliz", ", see ", TO setNmzOption, "). Then the method returns an object of type RationalCone whose keys are the suffixes of all the output files written, with value the content of the corresponding output file, which is an matrix whose rows contain the data computed, except for the suffix ", TT "inv", ", for which the type is a ", TO HashTable," (see also ", TO getNumInvs,"). It can also be used with the option ", TO grading ,".",
EXAMPLE lines ///
setNmzOption("allf",true);
arc=normaliz(allComputations=>true,{(hy,"inequalities"),(eq,"equations"),(cg,"congruences")});
arc#"gen"
arc#"ext"
arc#"inv"
///,
SeeAlso => {RationalCone, readNmzData,"Keeping results of the computation by Normaliz", "output files written by Normaliz"},
}



document {
     Key => allComputations,
PARA{}, "allComputations is an option for ", TO normaliz, ", ", TO intclToricRing, ", ", TO normalToricRing, ", ", TO intclMonIdeal, ", ",TO torusInvariants, ", ",TO finiteDiagInvariants, ", ",TO diagInvariants, ", ",TO intersectionValRings, ", ",TO intersectionValRingIdeals, ". Its default value is false. If it is set to true, the cone in the cache table of the monomial subalgebra returned contains not only the generators, but all the data that have been computed by ", TT "Normaliz", " (what this includes depends on the type and the options set, see ", TO "output files written by Normaliz",").",
EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          T=intclToricRing(allComputations=>true,L)
          T.cache#"cone"
     ///,
}

document {
     Key => {grading},
PARA{}, "grading is an option for ", TO normaliz, ", ", TO intclToricRing, ", ", TO normalToricRing, ", ", TO intclMonIdeal, ", ",TO torusInvariants, ", ",TO finiteDiagInvariants, ", ",TO diagInvariants, ", ",TO intersectionValRings, ", ",TO intersectionValRingIdeals, ". Its default value is an empty list. If it is set to a list of integers it will be used as grading. This has no influence on the generators of the computed objects, but on additional data like the multiplicity or the Hilbert series.
The grading may have non-positive entries, but it must give positive values for all generators.",
EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          T=intclToricRing(allComputations=>true,L);
          T.cache#"cone"#"inv"#"hilbert series num"
          T.cache#"cone"#"inv"#"hilbert series denom"
          T=intclToricRing(allComputations=>true,grading=>{3,2,1},L);
          T.cache#"cone"#"inv"#"hilbert series num"
          T.cache#"cone"#"inv"#"hilbert series denom"
     ///,
SeeAlso => {allComputations},
}

document {
     Key => {getNumInvs},
     Headline => "returns the numerical invariants computed",
     Usage => "getNumInvs()",
     Outputs => {HashTable => "the numerical invariants"},
     PARA{},"This function returns a hashtable containing the invariants printed to the file with suffix ", TT "inv", ", if the files are kept, i.e., if a filename is specified (see ", TO "Keeping results of the computation by Normaliz", "). The key of an entry is a ", TO String, " describing the invariant, the value is the invariant, namely an ", TO ZZ, " for rank, index, multiplicity, a ", TO Sequence, " for the grading, the Hilbert series and the Hilbert quasi-polynomial and a ", TO Boolean, " for graded and primary (in the case of a Rees algebra).",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3,x^2*y,y^3);
          nmzFilename="example";
          setNmzOption("hilb",true);
          intclMonIdeal I;
          invs=getNumInvs()
          hvector=invs#"hilbert series num"
          invs#"hilbert series denom"
          rmNmzFiles();
          ///,
TEST ///
          nmzFilename="example";
          sgr=matrix({{2, 0, 0, 1},
{0, 3, 0, 1},
{0, 0, 5, 1},
{0, 0, 0, 1}});
          normaliz(sgr,"integral_closure");
          n=getNumInvs();
          assert ( n#"hilbert basis elements"==19 and n#"number extreme rays"==4 and n#"rank"==4 and n#"number support hyperplanes"==4 and n#"graded"==true and n#"degree 1 elements"==18 and n#"grading"==(0,0,0,1) and n#"multiplicity"==30 )
    rmNmzFiles();
     ///,
     }

document {
     Key => {setNmzOption,(setNmzOption,String,Boolean)},
     Headline => "sets a command line option for Normaliz",
     Usage => "setNmzOption(s,b)",
     Inputs => {
               String => "name of the option",
               Boolean => "true switches the option on, false off"
          },
     PARA{}, "The ", TT "Normaliz"," options are accessible via the following names: ", BR{},BR{},
     "Computation mode:",
     UL{
        {TT "-s", ":   supp, only the support hyperplanes are computed."},
        {TT "-tT", ":   triang, computes the support hyperplanes, the triangulation and the multiplicity."},
        {TT "-v", ":   volume, computes the support hyperplanes and the multiplicity."},
        {TT "-p", ":   hvect, computes the support hyperplanes, the multiplicity, the h-vector and the Hilbert polynomial."},
       {TT "-n",  ":   normal, computes the support hyperplanes, the triangulation, the multiplicity and the Hilbert basis."},
       {TT "-N",  ":   normal_l, computes the support hyperplanes and the Hilbert basis."},
       {TT "-h",  ":   hilb, computes the support hyperplanes, the multiplicity, the Hilbert basis, the h-vector and the Hilbert polynomial."},
       {TT "-1",  ":   heigth1, computes the Hilbert basis elements of height 1."},
       {TT "-d",  ":   dual, computes the Hilbert basis using Pottier's algorithm, cf. L. Pottier, ", EM "The Euclide algorithm in dimension n.", " Research report, ISSAC 96, ACM Press 1996."}
     },
	 "The options with _l indicate that they are in particular useful for big examples.",
     "Further options:",
     UL{
        {TT "-c", ":   control, gives you some access to 'control' data during the computation. When switched on, data will be printed on the screen."},
        {TT "-a", ":   allf, all files are written",},
        {TT "-e", ":   errorcheck, when switched on the arithmetic tests will be performed, in order to assure that no arithmetic errors do occur. This may slow down the computations. This option is ignored if bigint is activated, since in this case no arithmetic errors can occur."},
        {TT "-B", ":   bigint, use indefinite precision arithmetic",},
        {TT "-x=<N>", ": threads, there ",TT "<N>"," stands for a positive integer limiting the number of threads that ", TT "Normaliz"," can access on your system. The default value is ", TT "<N>=",TEX"\\infty",". If you want to run ", TT "Normaliz"," in a strictly serial mode, choose ", TT "<N>=1",". If this option is enabled,",TT "<N>"," is taken as the value stored in the global variable ", TO "nmzNumberThreads","."}
     },
     PARA{},"Note that it makes no sense to activate more than one of the computation mode options. The ", TT "-f", " option, which makes the ", TT ".gen, .inv, .cst", " files to be printed, is always set. The default value of all options is ", TT "false",".",
     PARA{}, "To check which options are set use ", TO showNmzOptions,".",
     EXAMPLE lines ///
          setNmzOption("triang",true);
          showNmzOptions()
               ///,
     SeeAlso => showNmzOptions,
     }

document {
     Key => {showNmzOptions},
     Headline => "prints the enabled options",
     Usage => "showNmzOptions()",
     PARA{},"Prints the enabled options to the standard output. The ", TT "-f", " option is always set. See ", TO setNmzOption, " for the possible options.",
     EXAMPLE lines ///
          setNmzOption("triang",true);
          showNmzOptions()
          ///,
     SeeAlso => setNmzOption
     }



document {
     Key => {normalToricRing},
     Headline => "normalization of a toric ring",
    "A toric ring S is a subalgebra of a polynomial ring generated by  monomials. The function computes the normalization T of S, which is the integral closure in its field of fractions.",
     }

document {
     Key => {(normalToricRing,List)},
     Headline => "normalization of a toric ring",
     Usage => "normalToricRing L",
     Inputs => {List => "the generators of the toric ring"},
     Outputs => {MonomialSubalgebra => "the normalization of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra of the basering generated by the monomials in the list L. The function computes the normalization T of S, which is the integral closure in its field of fractions.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          normalToricRing L
     ///,
     TEST ///
         R=ZZ/37[x_1..x_9];
         l={
{1, 0, 0, 0, 0, 0, 0, 0, 0},
{0, 1, 0, 0, 0, 0, 0, 0, 0},
{0, 0, 1, 0, 0, 0, 0, 0, 0},
{0, 0, 0, 1, 0, 0, 0, 0, 0},
{0, 0, 0, 0, 1, 0, 0, 0, 0},
{0, 0, 0, 0, 0, 1, 0, 0, 0},
{0, 0, 0, 0, 0, 0, 1, 0, 0},
{0, 0, 0, 0, 0, 0, 0, 1, 0},
{1, 1, 1, 1, 0, 0, 0, 0, 1},
{0, 0, 0, 0, 1, 1, 1, 1, 1},
{1, 1, 0, 1, 1, 0, 0, 0, 1},
{0, 0, 1, 1, 0, 1, 0, 1, 1},
{1, 0, 1, 0, 0, 0, 1, 0, 1},
{0, 1, 0, 0, 0, 0, 1, 1, 1}};
L=for i in l list R_i; --rafa1409
        assert(set(flatten \ exponents \ gens normalToricRing L)===set{{ 0, 0, 0, 1, 0, 0, 0, 0, 0},
{ 1, 0, 0, 0, 0, 0, 0, 0, 0},
{ 0, 0, 0, 0, 0, 0, 0, 1, 0},
{ 1, 0, 1, 0, 0, 0, 1, 0, 1},
{ 1, 1, 0, 1, 1, 0, 0, 0, 1},
{ 0, 0, 1, 1, 0, 1, 0, 1, 1},
{ 0, 0, 0, 0, 0, 1, 0, 0, 0},
{ 0, 1, 0, 0, 0, 0, 1, 1, 1},
{ 0, 0, 0, 0, 1, 0, 0, 0, 0},
{ 0, 0, 0, 0, 1, 1, 1, 1, 1},
{ 0, 0, 0, 0, 0, 0, 1, 0, 0},
{ 1, 1, 1, 1, 0, 0, 0, 0, 1},
{ 0, 0, 1, 0, 0, 0, 0, 0, 0},
{ 0, 1, 0, 0, 0, 0, 0, 0, 0}})
     ///
     }

document {
     Key => {[(normalToricRing,List),allComputations],[(normalToricRing,List),grading]},
     Headline => "normalization of a toric ring",
     Usage => "normalToricRing L",
     Inputs => {List => "the generators of the toric ring"},
     Outputs => {MonomialSubalgebra => "the normalization of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra of the basering generated by the monomials in the list L. The function computes the normalization T of S, which is the integral closure in its field of fractions. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          T=normalToricRing(allComputations=> true, L)
          T.cache#"cone"
     ///,
     }

document {
     Key => {(normalToricRing,MonomialSubalgebra)},
     Headline => "normalization of a toric ring",
     Usage => "normalToricRing S",
     Inputs => {MonomialSubalgebra => "the toric ring"},
     Outputs => {MonomialSubalgebra => "the normalization of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra given. The function computes the normalization T of S, which is the integral closure in its field of fractions.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          S=createMonomialSubalgebra {x^3, x^2*y, y^3, x*y^2};
          normalToricRing S
     ///,
}

document {
     Key => {[(normalToricRing,MonomialSubalgebra),allComputations],[(normalToricRing,MonomialSubalgebra),grading]},
     Headline => "normalization of a toric ring",
     Usage => "normalToricRing S",
     Inputs => {MonomialSubalgebra => "the toric ring"},
     Outputs => {MonomialSubalgebra => "the normalization of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra given. The function computes the normalization T of S, which is the integral closure in its field of fractions. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          S=createMonomialSubalgebra {x^3, x^2*y, y^3, x*y^2};
          T=normalToricRing(allComputations=> true, S)
          T.cache#"cone"
     ///,
}

document {
     Key => {(normalToricRing,Ideal,Thing),[(normalToricRing,Ideal,Thing),allComputations],[(normalToricRing,Ideal,Thing),grading]},
     Headline => "normalization of a toric ring given by a binomial ideal",
     Usage => "normalToricRing(I,t)",
     Inputs => {Ideal => "a binomial ideal defining the toric ring",
                Thing => "letter naming variables in new polynomial ring"},
     Outputs => {MonomialSubalgebra => "the normalization of the toric ring"},
     PARA{}, "The ideal I is generated by binomials of type ", TEX "X^a-X^b", " (multiindex notation) in the surrounding polynomial ring ", TEX "K[X]=K[X_1,...,X_n]",".  The binomials represent a congruence on the monoid ", TEX "\\ZZ^n"," with residue monoid M. Let N be the image of M in gp(M)/torsion. Then N is universal in the sense that every homomorphism from M to an affine monoid factors through N.  If I is a prime ideal, then ", TEX "K[N] \\cong K[X]/I",". In general, ", TEX "K[N]\\cong K[X]/P"," where P is the unique minimal prime ideal of I generated by binomials of type ", TEX "X^a-X^b",".",
     PARA{}, "The function computes the normalization of K[N] and returns it as a monomial subalgebra in a newly created polynomial ring of the same Krull dimension, whose variables are ", TEX "t_1,\\ldots,t_{n-r}",", where r is the rank of the matrix with rows a-b. (In general there is no canonical choice for such an embedding.) ",
     EXAMPLE lines ///
          R=ZZ/37[x,y,z,w];
          I=ideal(x*z-y^2, x*w-y*z);
          normalToricRing(I,t)
     ///,
}



document {
     Key => {intclToricRing},
     Headline => "integral closure of a toric ring",
    "A toric ring S is a subalgebra of a polynomial ring generated by  monomials. The function computes the integral closure T of S in the surrounding polynomial ring.",
     }

document {
     Key => {(intclToricRing, List)},
     Headline => "integral closure of a toric ring",
     Usage => "intclToricRing L",
     Inputs => {
                List => "generators of the toric ring"},
     Outputs => {
                 MonomialSubalgebra => "the integral closure of the toric ring"},
    PARA{},"The toric ring S is the subalgebra of the basering generated by the monomials in the list L. The function computes the integral closure T of S in the surrounding polynomial ring.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          intclToricRing L
     ///,
     TEST ///
      R=ZZ/37[x_1..x_7];
          l={{1, 0, 0, 0, 0, 0, 0},
{0, 1, 0, 0, 0, 0, 0},
{0, 0, 1, 0, 0, 0, 0},
{0, 0, 0, 1, 0, 0, 0},
{0, 0, 0, 0, 1, 0, 0},
{0, 0, 0, 0, 0, 1, 0},
{1, 1, 1, 0, 0, 0, 1},
{1, 1, 0, 1, 0, 0, 1},
{1, 0, 1, 0, 1, 0, 1},
{1, 0, 0, 1, 0, 1, 1},
{1, 0, 0, 0, 1, 1, 1},
{0, 1, 1, 0, 0, 1, 1},
{0, 1, 0, 1, 1, 0, 1},
{0, 1, 0, 0, 1, 1, 1},
{0, 0, 1, 1, 1, 0, 1},
{0, 0, 1, 1, 0, 1, 1}};
L=for i in l list R_i; -- rproj2
        assert(set(flatten \ exponents \ gens intclToricRing L)===set{{0, 0, 0, 0, 0, 1, 0},
{ 0, 0, 0, 0, 1, 0, 0},
{ 0, 0, 0, 1, 0, 0, 0},
{ 0, 0, 1, 0, 0, 0, 0},
{ 0, 0, 1, 1, 0, 1, 1},
{ 0, 0, 1, 1, 1, 0, 1},
{ 0, 1, 0, 0, 0, 0, 0},
{ 0, 1, 0, 0, 1, 1, 1},
{ 0, 1, 0, 1, 1, 0, 1},
{ 0, 1, 1, 0, 0, 1, 1},
{ 1, 0, 0, 0, 0, 0, 0},
{ 1, 0, 0, 0, 1, 1, 1},
{ 1, 0, 0, 1, 0, 1, 1},
{ 1, 0, 1, 0, 1, 0, 1},
{ 1, 1, 0, 1, 0, 0, 1},
{ 1, 1, 1, 0, 0, 0, 1},
{ 1, 1, 1, 1, 1, 1, 2}})
     ///
     }

document {
     Key => {[(intclToricRing,List),allComputations],[(intclToricRing,List),grading]},
     Headline => "integral closure of a toric ring",
     Usage => "intclToricRing L",
     Inputs => {
                List => "generators of the toric ring"},
     Outputs => {
                 MonomialSubalgebra => "the integral closure of the toric ring"},
    PARA{},"The toric ring S is the subalgebra of the basering generated by the monomials in the list L. The function computes the integral closure T of S in the surrounding polynomial ring. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          L={x^3, x^2*y, y^3, x*y^2};
          T=intclToricRing(allComputations=>true,L)
          T.cache#"cone"
     ///,
}

document {
     Key => {(intclToricRing, MonomialSubalgebra)},
     Headline => "integral closure of a toric ring",
     Usage => "intclToricRing S",
     Inputs => {
                MonomialSubalgebra => "the toric ring"},
     Outputs => {
                 MonomialSubalgebra => "the integral closure of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra given. The function computes the integral closure T of S in the surrounding polynomial ring.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          S=createMonomialSubalgebra {x^3, x^2*y, y^3, x*y^2};
          intclToricRing S
     ///,
}

document {
     Key => {[(intclToricRing,MonomialSubalgebra),allComputations], [(intclToricRing,MonomialSubalgebra),grading]},
     Headline => "integral closure of a toric ring",
     Usage => "intclToricRing S",
     Inputs => {
                MonomialSubalgebra => "the toric ring"},
     Outputs => {
                 MonomialSubalgebra => "the integral closure of the toric ring"},
    PARA{},"The toric ring S is the monomial subalgebra given. The function computes the integral closure T of S in the surrounding polynomial ring. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned.",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          S=createMonomialSubalgebra {x^3, x^2*y, y^3, x*y^2};
          T=intclToricRing(allComputations=>true,S)
          T.cache#"cone"
     ///,
}

document {
     Key => {ehrhartRing},
     Headline => "Ehrhart ring",
     "The input of this function is considered as vertices of a lattice polytope P. The Ehrhart ring of a (lattice) polytope P is the monoid algebra defined by the monoid of lattice points in the cone over the polytope P; see Bruns and Gubeladze, Polytopes, Rings, and K-theory, Springer 2009, pp. 228, 229. The function computes the lattice points of the polytope and its Ehrhart ring.",
}

document {
     Key => {(ehrhartRing, List)},
     Headline => "Ehrhart ring",
     Usage => "ehrhartRing L",
     Inputs => {List => "the monomials corresponding to the vertices of a lattice polytope"},
     Outputs => {
       MonomialSubalgebra => "generated by the lattice points of the polytope",
       MonomialSubalgebra => "the Ehrhart ring of the polytope"},
     PARA{},"The exponent vectors of the given monomials are considered as vertices of a lattice polytope P.
     The Ehrhart ring of a (lattice) polytope P is the monoid algebra defined by the monoid of lattice points in the cone over the polytope P; see Bruns and Gubeladze, Polytopes, Rings, and K-theory, Springer 2009, pp. 228, 229. The function returns two monomial subalgebras, the first has as generators the monomials representing the lattice points of the polytope, the second is the Ehrhart ring. Since these are defined in a polynomial ring with an additional variable, the function creates a new polynomial ring with an additional variable.",
     EXAMPLE lines ///
          R=ZZ/37[x,y];
          L={x^3, x^2*y, y^3, x*y^2};
          (A, B) = ehrhartRing L;
          A
          B
     ///,
   TEST ///
   R=ZZ/37[x,y,z,t];
   l={{0, 0, 0},{2, 0, 0},{0, 3, 0},{0, 0, 5}};
   L=for i in l list R_i; --polytope
   (p,e)=ehrhartRing(L,t);
    assert(set(flatten \ exponents \ gens p)===set{{0, 0, 0,0},
{ 0, 0, 1,0},
{ 0, 0, 2,0},
{ 0, 0, 3,0},
{ 0, 0, 4,0},
{ 0, 0, 5,0},
{ 0, 1, 0,0},
{ 0, 1, 1,0},
{ 0, 1, 2,0},
{ 0, 1, 3,0},
{ 0, 2, 0,0},
{ 0, 2, 1,0},
{ 0, 3, 0,0},
{ 1, 0, 0,0},
{ 1, 0, 1,0},
{ 1, 0, 2,0},
{ 1, 1, 0,0},
{ 2, 0, 0,0}})
assert(set(flatten \ exponents \ gens e)===set{{2, 0, 0, 1},
{ 0, 3, 0, 1},
{ 0, 0, 5, 1},
{ 0, 1, 3, 1},
{ 1, 0, 2, 1},
{ 0, 2, 1, 1},
{ 1, 1, 0, 1},
{ 1, 2, 4, 2},
{ 0, 0, 4, 1},
{ 0, 1, 2, 1},
{ 1, 0, 1, 1},
{ 0, 2, 0, 1},
{ 0, 0, 3, 1},
{ 0, 1, 1, 1},
{ 1, 0, 0, 1},
{ 0, 0, 2, 1},
{ 0, 1, 0, 1},
{ 0, 0, 1, 1},
{ 0, 0, 0, 1}})
   ///
}

document {
     Key => {[(ehrhartRing,List),allComputations]},
     Headline => "Ehrhart ring",
     Usage => "ehrhartRing L",
     Inputs => {List => "the monomials corresponding to the vertices of a lattice polytope"},
     Outputs => {
       MonomialSubalgebra => "generated by the lattice points of the polytope",
       MonomialSubalgebra => "the Ehrhart ring of the polytope"},
    PARA{}, "The exponent vectors of the given monomials are considered as vertices of a lattice polytope P.
     The Ehrhart ring of a (lattice) polytope P is the monoid algebra defined by the monoid of lattice points in the cone over the polytope P; see Bruns and Gubeladze, Polytopes, Rings, and K-theory, Springer 2009, pp. 228, 229. The function returns two monomial subalgebras, the first has as generators the monomials representing the lattice points of the polytope, the second is the Ehrhart ring. Since these are defined in a polynomial ring with an additional variable, the function creates a new polynomial ring with an additional variable. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebras returned (it is the same cone for both subalgebras).",
     EXAMPLE lines ///
          R=ZZ/37[x,y];
          L={x^3, x^2*y, y^3, x*y^2};
	  (A, B) = ehrhartRing(L, allComputations => true);
	  A.cache#"cone"
	  B.cache#"cone"
     ///,
}

document {
     Key => {(ehrhartRing,List,RingElement)},
     Headline => "Ehrhart ring",
     Usage => "ehrhartRing(L,t)",
     Inputs => {List => "the monomials corresponding to the vertices of a lattice polytope", RingElement => " the free variable that should be used for the computation"},
     Outputs => {
       MonomialSubalgebra => "generated by the lattice points of the polytope",
       MonomialSubalgebra => "the Ehrhart ring of the polytope"},
    PARA{}, "The exponent vectors of the given monomials are considered as vertices of a lattice polytope P.
     The Ehrhart ring of a (lattice) polytope P is the monoid algebra defined by the monoid of lattice points in the cone over the polytope P; see Bruns and Gubeladze, Polytopes, Rings, and K-theory, Springer 2009, pp. 228, 229. The function returns two monomial subalgebras, the first has as generators the monomials representing the lattice points of the polytope, the second is the Ehrhart ring. If there is already a free variable in the original ring (i.e. a variable that does not appear in any of the monomials in L), give the function that variable as second input. The function then uses it instead of creating a new one. ",
    EXAMPLE  lines ///
    R=ZZ/37[x,y,t];
    L={x^3, x^2*y, y^3, x*y^2};
    (A, B) = ehrhartRing(L, t);
    A
    B
   ///,
}

document {
     Key => {[(ehrhartRing,List,RingElement),allComputations]},
     Headline => "Ehrhart ring",
     Usage => "ehrhartRing(L,t)",
     Inputs => {List => "the monomials corresponding to the vertices of a lattice polytope", RingElement => " the free variable that should be used for the computation"},
     Outputs => {
       MonomialSubalgebra => "generated by the lattice points of the polytope",
       MonomialSubalgebra => "the Ehrhart ring of the polytope"},
     PARA{},"The exponent vectors of the given monomials are considered as vertices of a lattice polytope P.
     The Ehrhart ring of a (lattice) polytope P is the monoid algebra defined by the monoid of lattice points in the cone over the polytope P; see Bruns and Gubeladze, Polytopes, Rings, and K-theory, Springer 2009, pp. 228, 229. The function returns two monomial subalgebras, the first has as generators the monomials representing the lattice points of the polytope, the second is the Ehrhart ring. If there is already a free variable in the original ring (i.e. a variable that does not appear in any of the monomials in L), you can give the function that variable as second input. The function then uses it instead of creating a new one.  If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebras returned (it is the same cone for both subalgebras). ",
    EXAMPLE  lines ///
    R=ZZ/37[x,y,t];
    L={x^3, x^2*y, y^3, x*y^2};
    (A, B) = ehrhartRing(L, t, allComputations => true);
    A.cache#"cone"
    B.cache#"cone"
   ///,
}


document {
     Key => {intclMonIdeal},
     Headline => "normalization of Rees algebra",
     "This function computes the integral closure of a monomial ideal ", TEX "I=(f_1,\\ldots,f_m)\\subset R", " in the polynomial ring R[t] and the normalization of its Rees algebra. The normalization of the Rees algebra is the integral closure of ", TEX "R[f_1t,\\ldots,f_mt]", " in R[t]. For a definition of the Rees algebra (or Rees ring) see Bruns and Herzog, Cohen-Macaulay rings, Cambridge University Press 1998, p. 182. ",
     }

document {
     Key => {(intclMonIdeal, Ideal)},
     Headline => "normalization of Rees algebra",
     Usage => "intclMonIdeal I",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal are considered as generators of a monomial ideal"},
     Outputs => {Ideal => "the integral closure of the input ideal", MonomialSubalgebra => "the normalization of the Rees algebra of I" },
     PARA{},"The leading monomials of the elements of I are considered as generators of a monomial ideal. This function computes the integral closure of ", TEX "I\\subset R", " in the polynomial ring R[t] and the normalization of its Rees algebra. If ", TEX "f_1,\\ldots,f_m", " are the monomial generators of I, then the normalization of the Rees algebra is the integral closure of ", TEX "K[f_1t,\\ldots,f_nt]", " in R[t]. For a definition of the Rees algebra (or Rees ring) see Bruns and Herzog, Cohen-Macaulay rings, Cambridge University Press 1998, p. 182. The function returns the integral closure of the ideal I and the normalization of its Rees algebra. Since the Rees algebra is defined in a polynomial ring with an additional variable, the function creates a new polynomial ring with an additional variable.",
     EXAMPLE lines ///
          R=ZZ/37[x,y];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          (intCl,normRees)=intclMonIdeal I;
          intCl
      normRees
     ///,
   TEST ///
   R=ZZ/37[x_1..x_6,t];
   l={
{1, 1, 1, 0, 0, 0},
{1, 1, 0, 1, 0, 0},
{1, 0, 1, 0, 1, 0},
{1, 0, 0, 1, 0, 1},
{1, 0, 0, 0, 1, 1},
{0, 1, 1, 0, 0, 1},
{0, 1, 0, 1, 1, 0},
{0, 1, 0, 0, 1, 1},
{0, 0, 1, 1, 1, 0},
{0, 0, 1, 1, 0, 1}};
  I=ideal(for i in l list R_i); --rees
   (i,n)=intclMonIdeal(I,t);
    assert(set(flatten \ exponents \ first entries gens i)===set{
 {1, 1, 1, 0, 0, 0,0},
 {0, 1, 1, 0, 0, 1,0},
 {1, 0, 1, 0, 1, 0,0},
 {1, 0, 0, 0, 1, 1,0},
 {0, 1, 0, 0, 1, 1,0},
 {1, 1, 0, 1, 0, 0,0},
 {1, 0, 0, 1, 0, 1,0},
 {0, 0, 1, 1, 0, 1,0},
 {0, 1, 0, 1, 1, 0,0},
 {0, 0, 1, 1, 1, 0,0}})
 assert(set(flatten \ exponents \ gens n)===set{
 {1, 0, 0, 0, 0, 0, 0},
 {0, 1, 0, 0, 0, 0, 0},
 {0, 0, 1, 0, 0, 0, 0},
 {1, 1, 1, 0, 0, 0, 1},
 {0, 0, 0, 0, 0, 1, 0},
 {0, 1, 1, 0, 0, 1, 1},
 {0, 0, 0, 0, 1, 0, 0},
 {1, 0, 1, 0, 1, 0, 1},
 {1, 0, 0, 0, 1, 1, 1},
 {0, 1, 0, 0, 1, 1, 1},
 {0, 0, 0, 1, 0, 0, 0},
 {1, 1, 0, 1, 0, 0, 1},
 {1, 0, 0, 1, 0, 1, 1},
 {0, 0, 1, 1, 0, 1, 1},
 {0, 1, 0, 1, 1, 0, 1},
 {0, 0, 1, 1, 1, 0, 1},
 {1, 1, 1, 1, 1, 1, 2}})

   ///
     }

document {
     Key => {(intclMonIdeal,Ideal,RingElement),},
     Headline => "normalization of Rees algebra",
     Usage => "intclMonIdeal(I,t)",
     Inputs => {
         Ideal => "the leading monomials of the elements of the ideal are considered as generators of a monomial ideal",
         RingElement => " the free variable of the ring that should be used for the computation (optional)"},
     Outputs => {
	      Ideal => "the integral closure of the input ideal",
	      MonomialSubalgebra => "the normalization of the Rees algebra of I"},
     PARA{},"The leading monomials of the elements of I are considered as generators of a monomial ideal. This function computes the integral closure of ", TEX "I\\subset R", " in the polynomial ring R[t] and the normalization of its Rees algebra. If ", TEX "f_1,\\ldots,f_m", " are the monomial generators of I, then the normalization of the Rees algebra is the integral closure of ", TEX "K[f_1t,\\ldots,f_nt]", " in R[t]. For a definition of the Rees algebra (or Rees ring) see Bruns and Herzog, Cohen-Macaulay rings, Cambridge University Press 1998, p. 182. The function returns the integral closure of the ideal I and the normalization of its Rees algebra. If there is a free variable in the original ring (i.e. a variable that does not appear in any of the generators of I), you can give the function that variable as second input. The function then uses it instead of creating a new one. Note that in this case the input ideal is considered as ideal in the smaller polynomial ring.",
    EXAMPLE  lines ///
      R=ZZ/37[x,y,t];
      I=ideal(x^3, x^2*y, y^3, x*y^2);
      (intCl,normRees)=intclMonIdeal(I,t);
      intCl
      normRees
   ///,
}

document {
     Key => { [(intclMonIdeal,Ideal),allComputations],[(intclMonIdeal,Ideal),grading]},
     Headline => "normalization of Rees algebra",
     Usage => "intclMonIdeal I",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal are considered as generators of a monomial ideal",},
     Outputs => {
	      Ideal => "the integral closure of the input ideal",
	      MonomialSubalgebra => "the normalization of the Rees algebra of I"},
     PARA{},"The leading monomials of the elements of I are considered as generators of a monomial ideal. This function computes the integral closure of ", TEX "I\\subset R", " in the polynomial ring R[t] and the normalization of its Rees algebra. If ", TEX "f_1,\\ldots,f_m", " are the monomial generators of I, then the normalization of the Rees algebra is the integral closure of ", TEX "K[f_1t,\\ldots,f_nt]", " in R[t]. For a definition of the Rees algebra (or Rees ring) see Bruns and Herzog, Cohen-Macaulay rings, Cambridge University Press 1998, p. 182. The function returns the integral closure of the ideal I and the normalization of its Rees algebra. Since the Rees algebra is defined in a polynomial ring with an additional variable, the function creates a new polynomial ring with an additional variable.
If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned. This method can also be used with the option ",TO grading,".",
     EXAMPLE lines ///
          R=ZZ/37[x,y];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          (intCl,normRees)=intclMonIdeal(allComputations=>true,I)
           normRees.cache#"cone"
     ///,
}

document {
     Key => {[(intclMonIdeal,Ideal,RingElement),allComputations],
            [(intclMonIdeal,Ideal,RingElement),grading]},
     Headline => "normalization of Rees algebra",
     Usage => "intclMonIdeal(I,t)",
     Inputs => {Ideal => "the leading monomials of the elements of the ideal are considered as generators of a monomial ideal",
                RingElement => " the free variable of the ring that should be used for the computation (optional)"},
     Outputs => {
	      Ideal => "the integral closure of the input ideal",
	      MonomialSubalgebra => "the normalization of the Rees algebra of I"},
     PARA{},"The leading monomials of the elements of I are considered as generators of a monomial ideal. This function computes the integral closure of ", TEX "I\\subset R", " in the polynomial ring R[t] and the normalization of its Rees algebra. If ", TEX "f_1,\\ldots,f_m", " are the monomial generators of I, then the normalization of the Rees algebra is the integral closure of ", TEX "K[f_1t,\\ldots,f_nt]", " in R[t]. For a definition of the Rees algebra (or Rees ring) see Bruns and Herzog, Cohen-Macaulay rings, Cambridge University Press 1998, p. 182. The function returns the integral closure of the ideal I and the normalization of its Rees algebra. If there is a free variable in the original ring (i.e. a variable that does not appear in any of the generators of I), you can give the function that variable as second input. The function then uses it instead of creating a new one. Note that in this case the input ideal is considered as ideal in the smaller polynomial ring. If the option ", TO allComputations, " is set to true, all data that has been computed by ", TT "Normaliz", " is stored in a ", TO RationalCone, " in the CacheTable of the monomial subalgebra returned. This method can also be used with the option ",TO grading,".",
     EXAMPLE lines ///
          R=ZZ/37[x,y,t];
          I=ideal(x^3, x^2*y, y^3, x*y^2);
          (intCl,normRees)=intclMonIdeal(allComputations=>true,I)
           normRees.cache#"cone"
     ///,
}

document {
     Key => { torusInvariants, (torusInvariants, Matrix,Ring),
              [(torusInvariants,Matrix,Ring),allComputations],
              [(torusInvariants,Matrix,Ring),grading]
            },
     Headline => "ring of invariants of torus action",
     Usage => "torusInvariants(T,R)",
     Inputs => {
                Matrix=> {"matrix ", TEX "(a_{ij})", " of the action"},
                Ring => " the ring on which the action takes place"
     },
     Outputs => {
                 MonomialSubalgebra => {"the ring of invariants ",TEX "R^T"}
     },
     PARA{}," Let ", TEX "T=(K^*)^r"," be the ", TEX "r","-dimensional torus acting on the polynomial ring ",TEX "R=K[X_1,\\ldots,X_n]"," diagonally. Such an action can be described as follows: there are integers ",TEX "a_{ij}, i=1,\\ldots,r, j=1,\\ldots,n",", such that ",TEX "(\\lambda_1,\\ldots,\\lambda_r)\\in T"," acts by the substitution ",BR{},BR{}, TEX "X_j\\mapsto \\lambda_1^{a_{1j}}*\\ldots*\\lambda_r^{a_{rj}}X_j,", "    ", TEX "j=1,\\ldots,n.", BR{},BR{},"The function takes the matrix ", TEX "(a_{ij})", " as input and computes the ring of invariants ",TEX "R^T=\\{f\\in R: \\lambda f=f", " for all ", TEX "\\lambda \\in T\\}",".",
PARA{},"This method can be used with the options ", TO allComputations, " and ", TO grading , ".",
     EXAMPLE lines ///
          R=QQ[x,y,z,w];
          T=matrix({{-1,-1,2,0},{1,1,-2,-1}});
          torusInvariants(T,R)
     ///,
     SeeAlso => {finiteDiagInvariants, diagInvariants},
     }

document {
     Key => { diagInvariants, (diagInvariants,Matrix,Matrix,Ring),
              [(diagInvariants,Matrix,Matrix,Ring),allComputations],
              [(diagInvariants,Matrix,Matrix,Ring),grading]},
     Headline => "ring of invariants of a diagonalizable group action",
     Usage => "diagInvariants(T,U,R)",
     Inputs => {
                Matrix =>"whose rows are the values of the indeterminates under the torus action",
                Matrix=> "whose rows are the values of the indeterminates under action of the finite group",
                Ring => " the basering"
},
     Outputs => {
                 MonomialSubalgebra => {"the ring of invariants"}},
     PARA{}, "This function computes the ring of invariants of a diagonalizable group ", TEX "D = T\\times G", " where T is a torus and G is a finite abelian group, both acting diagonally on the polynomial ring ", TEX "K[X_1,\\ldots,X_n]",". The group actions are specified by the input matrices M and N. The first matrix specifies the torus action, the second the action of the finite group. See ", TO torusInvariants, " or ", TO finiteDiagInvariants, " for more detail. The output is the monomial subalgebra of invariants.",
PARA{},"This method can be used with the options ", TO allComputations, " and ", TO grading , ".",
     EXAMPLE lines ///
          R=QQ[x,y,z,w];
          T=matrix({{-1,-1,2,0},{1,1,-2,-1}});
          U=matrix{{1,1,1,1,5},{1,0,2,0,7}}
          diagInvariants(T,U,R)
    ///,
    SeeAlso => {torusInvariants, finiteDiagInvariants},
}

document {
     Key => { finiteDiagInvariants, (finiteDiagInvariants,Matrix,Ring),
              [(finiteDiagInvariants,Matrix,Ring),allComputations],
              [(finiteDiagInvariants,Matrix,Ring),grading],},
     Headline => "ring of invariants of a finite group action",
     Usage => "finiteDiagInvariants(U,R)",
     Inputs => {
                Matrix=> "whose rows are the values of the indeterminates under the action of a finite group",
                Ring => " the basering"
},
     Outputs => {
                 MonomialSubalgebra => {"the ring of invariants"}},
     PARA{}, "This function computes the ring of invariants of a finite abelian group G acting diagonally on the surrounding polynomial ring ", TEX "K[X_1,\\ldots,X_n]",". The group is the direct product of cyclic groups generated by finitely many elements ", TEX "g_1,\\ldots,g_w",". The element ", TEX "g_i", " acts on the indeterminate ", TEX "X_j", " by ", TEX "g_i(X_j)= \\lambda_i^{u_{ij}}X_j", "where ", TEX "\\lambda_i", " is a primitive root of unity of order equal to ord(",TEX "g_i","). The ring of invariants is generated by all monomials satisfying the system ",
TEX "u_{i1}a_1+...+u_{in} a_n \\equiv \\ 0 mod ord(g_i),  i=1,\\ldots,w", ".
The input to the function is the ", TEX "w\\times (n+1)", " matrix U with rows ", TEX "u_{i1} \\ldots u_{in} ord(g_i), i=1,\\ldots,w", ".
The output is the monomial subalgebra of invariants ", TEX "R^G=\\{f\\in R : g_i f= f ", " for all ", TEX "i=1,\\ldots,w\\}",".",
PARA{},"This method can be used with the options ", TO allComputations, " and ", TO grading , ".",
EXAMPLE lines ///
 R=QQ[x,y,z,w];

         U=matrix{{1,1,1,1,5},{1,0,2,0,7}}
          finiteDiagInvariants(U,R)
///,
SeeAlso => {diagInvariants, torusInvariants}
}

document {
     Key => { intersectionValRings, (intersectionValRings,Matrix,Ring),
              [(intersectionValRings,Matrix,Ring),grading],
              [(intersectionValRings,Matrix,Ring),allComputations]},
     Headline => "intersection of ring of valuations",
     Usage => "intersectionValRings(v,r)",
     Inputs => {
                Matrix=> "rows are the values of the indeterminates",
                Ring => " the basering"},
     Outputs => {
                 MonomialSubalgebra => {"the subalgebra consisting of the elements with valuation ",TEX "\\geq 0", " for all given valuations"}},
     PARA{},{"A discrete monomial valuation ", TEX "v"," on ", TEX "R=K[X_1,\\ldots,X_n]"," is determined by the values ", TEX "v(X_j)"," of the indeterminates. This function computes the subalgebra ", TEX "S=\\{f\\in R: v_i(f)\\geq 0, i=1,\\ldots,r\\}"," that is the intersection of the valuation rings of the given valuations ", TEX "v_1, \\ldots,v_r", ", i.e. it consists of all elements of R that have a nonnegative value for all r valuations. It takes as input the matrix ", TEX "V=(v_i(X_j))"," whose rows correspond to the values of the indeterminates.",
     PARA{},"This method can be used with the options ", TO allComputations, " and ", TO grading , ".",},
     EXAMPLE lines ///
         R=QQ[x,y,z,w];
         V0=matrix({{0,1,2,3},{-1,1,2,1}});
         intersectionValRings(V0,R)
     ///,
     SeeAlso => {intersectionValRingIdeals}
     }

document {
     Key => { intersectionValRingIdeals, (intersectionValRingIdeals,Matrix,Ring),
              [(intersectionValRingIdeals,Matrix,Ring),grading],
              [(intersectionValRingIdeals,Matrix,Ring),allComputations]},
     Headline => "intersection of valuation ideals",
     Usage => "intersectionValRingIdeals(v,r)",
     Inputs => {
                Matrix=> {"values of the indeterminates, the last column contains the lower bounds ", TT "w_i"},
                Ring => " the basering"
     },
     Outputs => {HashTable => "the subalgebra and the generators of the module over it"},
     PARA{},{"A discrete monomial valuation ", TEX "v"," on ", TEX "R=K[X_1,\\ldots,X_n]"," is determined by the values ", TEX "v(X_j)"," of the indeterminates. This function takes as input the matrix ", TEX "V=(v_i(X_j))", ", whose rows correspond to the values of the indeterminates for for r valuations ", TEX "v_1, \\ldots,v_r",", with an additional column holding lower bounds ", TEX "w_1,\\ldots,w_r \\in \\ZZ",". It returns the subalgebra ", TEX "S=\\{f\\in R: v_i(f)\\geq 0, i=1,\\ldots,n\\}",", the intersection of the valuation rings of the r valuations, and a system of generators of the S-submodule ", TEX "M=\\{f\\in R: v_i(f)\\geq w_i, i=1,\\ldots,n\\}"," over R, which consists of the elements whose i-th valuation is greater or equal to the i-th bound ", TEX "w_i",". If ", TEX "w_i>=0", " for all i, then M is an ideal in S.",
     PARA{},"This method can be used with the options ", TO allComputations, " and ", TO grading , ". The additional data can be accessed via the subalgebra in the ",TO HashTable,".",},
     EXAMPLE lines ///
           R=QQ[x,y,z,w];
           V=matrix({{0,1,2,3,4},{-1,1,2,1,3}});
           intersectionValRingIdeals(V,R)
     ///,
     SeeAlso=> {intersectionValRings}
     }

end--

restart
uninstallAllPackages()
check "Normaliz"
installPackage "Normaliz"

restart
errorDepth=2
needsPackage "Normaliz"
R=ZZ/37[x,y,z,w];
I=ideal(x*z-y^2, x*w-y*z);
normalToricRing(I,t)
