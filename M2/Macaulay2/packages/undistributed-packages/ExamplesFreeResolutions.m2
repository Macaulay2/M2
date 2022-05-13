newPackage(
    "ExamplesFreeResolutions",
    Version => "0.1",
    Date => "18 Mar 2022",
    Headline => "examples to benchmark and test free resolution and betti number code",
    Authors => {{ Name => "", Email => "", HomePage => ""}},
    PackageImports => {"InverseSystems"},
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

export {
    "outputIdeal",
    "AGR",
    "CNC",
    "PCNC",
    
    "prefixAGR",
    "makeAGR",
    "getAGR",
    "runAGR",
    
    "prefixCNC",
    "makeCNC",
    "getCNC",
    "runCNC",

    "checkCNC",
    "bettiCNC",

    "prefixPCNC",
    "makePCNC",
    "getPCNC",
    "runPCNC",

    "checkPCNC",
    "bettiPCNC",

    "RingName",
    "IdealName",
    "Directory",
    "Comment"
    }

outputIdeal = method(Options => {RingName => "R1", IdealName => "I1"})
outputIdeal Ideal := String => opts -> I -> (
    -- many caveats here: (1) the ring monomial order is the default one.
    --  (2) the ring must be a polynomial ring.
    -- any of these could be messed up if for example the coeff ring has a name too.
    R := ring I;
    if numgens R <= 26 then (
        R1 := (coefficientRing R)[vars(0..numgens R-1)];
        I1 := sub(I, vars R1);
        R = R1;
        I = I1;
        );
    ringstr := opts.RingName | " = ("|(toExternalString coefficientRing R)|")"|(toString new Array from gens R)|"\n";
    idealstr1 := opts.IdealName | " = ideal(\n    ";
    idealstr2 := "\n    )\n";
    ringstr | idealstr1 | concatenate between(",\n    ",for f in I_* list toString f) |idealstr2
    )


exampleFileName = method(Options => {Directory => null})
exampleFileName String := String => opts -> (prefix) -> (
    dir := if opts.Directory === null then currentDirectory()|"/ExamplesAndTimings/" else opts.Directory;
    completefilename := dir | prefix | ".m2"
    )

makeExampleFile = method(Options => {Directory => null})
makeExampleFile(String, String) := opts -> (prefix, computation) -> (
    filename := exampleFileName(prefix, opts);
    if fileExists filename then (
        << "example " << filename << " has already been constructed" << endl;
        )
    else (
        I1 := value computation;
        str := outputIdeal I1;
        << "writing " << filename << endl;
        filename << str << endl << close;
        );
    filename
    )

runExample = method(Options => {Directory => null, Comment => ""})
runExample String := BettiTally => opts -> (prefix) -> (
    -- e.g. prefix should be something like "cnc-5", "pcnc-5", "agr-3-5-7"
    dir := if opts.Directory === null then currentDirectory() else opts.Directory;
    filename := dir | prefix | ".m2";
    timefilename := dir | "time-" | prefix;
    << "filename is " << filename << " timefile is " << timefilename << endl;
    I1 := value get filename; -- last thing in the file is an ideal
    tim1 := elapsedTiming res(I1, FastNonminimal=>true);
    C := tim1#1;
    --<< "nonminimal betti: " << betti C << endl;
    tim2 := elapsedTiming minimalBetti I1;
    B := tim2#1;
    F := openOutAppend timefilename;
    comment := if opts.Comment == "" then "" else "["|opts.Comment|"] ";
    F << " times: (" << tim1#0 << " " << tim2#0 << ") "  << " " << comment << get "!date";
    close F;
    << " times: (" << tim1#0 << " " << tim2#0 << ") "  << " " << comment << " " << get "!date";    
    B
    )


prefixAGR = method()
prefixAGR(ZZ, ZZ, ZZ) := String => (deg, nvars, charac) ->
    "agr-"|deg|"-"|nvars|"-char"|charac
prefixAGR(ZZ, ZZ, ZZ, ZZ) := String => (deg, nvars, nlinear, charac) ->
    "agr-"|deg|"-"|nvars|"-"|nlinear|"-char"|charac

makeAGR = method(Options => options makeExampleFile)
makeAGR(ZZ,ZZ,ZZ,ZZ) :=
makeAGR(ZZ,ZZ,ZZ) := String => opts -> args -> (
    prefix := prefixAGR args;
    comp := "AGR "|(toString args);
    print comp;
    makeExampleFile(prefix, comp, opts)
    )

getAGR = method(Options => options makeExampleFile)
getAGR(ZZ,ZZ,ZZ,ZZ) :=
getAGR(ZZ,ZZ,ZZ) := Ideal => opts -> args -> (
    prefix := prefixAGR args;
    filename := exampleFileName(prefix, opts);
    if not fileExists filename then error "example has not been constructed yet";
    elapsedTime value get filename
    )

runAGR = method(Options => options runExample)
runAGR(ZZ,ZZ,ZZ,ZZ) :=
runAGR(ZZ,ZZ,ZZ) := BettiTally => opts -> args -> (
    prefix := prefixAGR args;
    runExample(prefix, opts)
    )

prefixCNC = method()
prefixCNC ZZ := String => g -> "cnc-"|g

makeCNC = method(Options => options makeExampleFile)
makeCNC ZZ := String => opts -> (g) -> (
    prefix := prefixCNC g;
    comp := "CNC "|g;
    makeExampleFile(prefix, comp, opts)
    )

getCNC = method(Options => options makeExampleFile)
getCNC ZZ := Ideal => opts -> (g) -> (
    prefix := prefixCNC g;
    filename := exampleFileName(prefix, opts);
    if not fileExists filename then error "example has not been constructed yet";
    elapsedTime value get filename
    )

runCNC = method(Options => options runExample)
runCNC ZZ := BettiTally => opts -> g -> runExample(prefixCNC g, opts)

prefixPCNC = method()
prefixPCNC(ZZ,ZZ) := String => (g,p) -> "pcnc-"|g|"-char"|p
prefixPCNC ZZ := String => g -> prefixPCNC(g, 101)

makePCNC = method(Options => options makeExampleFile)
makePCNC ZZ := String => opts -> (g) -> (
    prefix := prefixPCNC g;
    comp := "PCNC "|g;
    makeExampleFile(prefix, comp, opts)
    )
makePCNC(ZZ,ZZ) := String => opts -> (g,p) -> (
    prefix := prefixPCNC(g,p);
    comp := "PCNC("|g|","|p|")";
    makeExampleFile(prefix, comp, opts)
    )

getPCNC = method(Options => options makeExampleFile)
getPCNC(ZZ, ZZ) := Ideal => opts -> (g, p) -> (
    prefix := prefixPCNC(g, p);
    filename := exampleFileName(prefix, opts);
    if not fileExists filename then error "example has not been constructed yet";
    elapsedTime value get filename
    )
getPCNC ZZ := Ideal => opts -> g -> getPCNC(g, 101)

runPCNC = method(Options => options runExample)
runPCNC ZZ := BettiTally => opts -> g -> runExample(prefixPCNC g, opts)
runPCNC(ZZ,ZZ) := BettiTally => opts -> (g, p) -> runExample(prefixPCNC(g,p), opts)

------------------------------------------
-- Example: Arithmetic Gorenstein rings --
------------------------------------------
AGR = method()
AGR(ZZ,ZZ,ZZ,ZZ) := Ideal => (d,n,s,p) -> (
    kk := if p == 0 then QQ else ZZ/p;
    R := kk[vars(0..n)];
    F := sum(gens R, x -> x^d) + sum(s-(n+1), i -> (random(1,R))^d);
    inverseSystem F
    )
AGR(ZZ,ZZ,ZZ) := Ideal := (d,n,p) -> (
    kk := if p == 0 then QQ else ZZ/p;
    R := kk[vars(0..n)];
    F := random(d, R);
    inverseSystem F
    )

----------------------------------------------------------------
-- Examples: 
--   Nodal canonical curves (CNC)
--   Prym canonical nodal curves (PCNC)
-- This code was lifted from code that Frank Schreyer has written
-- See ??? *** get Frank to tell me the reference for this ***
----------------------------------------------------------------
CNC = method()
CNC ZZ := (n) -> (
    I := last randomCanonicalNodalCurve n;
    ideal gens gb I
    )

PCNC = method()
PCNC ZZ := g -> PCNC(g, 101)
PCNC(ZZ, ZZ) := (g, p) -> (
    -- this version: only works for half-canonical curves
    s := randLinearSeriesPrym(g,p);
    L := flatten entries s;
    s = matrix{L/monic};
    elapsedTime I := randomPrymCanonicalNodalCurve s;
    if p === 0 then (
        Rp := (ZZ/32003)(monoid ring I);
        Ip := sub(I,Rp);
        hf := poincare Ip;
        elapsedTime gbI := gens gb(I, Hilbert=>hf);
        )
    else (
        elapsedTime gbI = gens gb(I);
        );
    I
    )

-- The next routines were lifted and modified from "NodalCurves.m2" (A package under development by Frank Schreyer)
--getcf = (F) -> (terms F)/leadCoefficient/(x -> x + 0.0)
monic = (F) -> (1/(leadCoefficient F)) * F
--quots = (L) -> for i from 0 to #L-2 list L_i/L_(i+1)

randIntegerMatrix = (S, nrows, ncols, ht) -> (
    -- S is some ring
    -- (nows, ncols) is the size
    -- entries of the matrix will be uniformly chosen in the range -ht..ht
    map(ZZ^nrows, ZZ^ncols, (i,j) -> random(ZZ, Height=>2*ht+1) - ht) ** S
    )

distinctPointsOfP1=method()
distinctPointsOfP1(Matrix) := P-> (
    -- the 2xn matrix P with entries in the ground field K
    -- represents n distinct points of P^1(K) if and only if
    -- all 2x2 minors of P are nonzero
    P2:=exteriorPower(2,P);
    #select(toList(0..rank source P2-1),i-> P2_(0,i)==0)==0
    )

canonicalMultipliers=method()
canonicalMultipliers(Matrix,Matrix) := (P,Q) -> (
     -- P and  Q two 2xg matrices representing 2g distinct points of P^1
     S:=ring P;
     if dim S != 2 then error "not a Polynomialring in two variables";
     g:= numcols P; 
     quadrics := apply(g, i->(det(P_{i}|(transpose vars S)))*(det(Q_{i}|(transpose vars S))));
     -- the quadrics with zero locus P_{i} and Q_{i}
     sections := (apply(g, i -> product(g, j-> if i == j then 1_S else quadrics_j)));
     -- a basis of the canonical series as subseries of S_{2g-2} of the nodal curve
     A := transpose matrix(apply(g, i ->{sub(sections_i, transpose P_{i}), sub(sections_i, transpose Q_{i})}));
     A)

linearSeriesFromMultipliers=method()
linearSeriesFromMultipliers(Sequence,Matrix) := (PQ,A) ->(
     -- PQ a list of two 2xg matrices representing distinct points of P^1
     -- A a 2xg matrix of ratios of identifying O_{P^1}(2g-2) tensor k(P_i) with O_{P^1}(2g-2) tensor k(Q_i)
     -- the result is the matrix of sections of O(2g-2) which obey these sections
     P:=PQ_0; Q:=PQ_1; 
     S := ring P;
     g := numcols P; 
     Bs := basis(2*g-2, S);
     MP := matrix(apply(numcols P, i->flatten entries(A_(1,i)*(sub(Bs, transpose P_{i})))));
     MQ := matrix(apply(numcols P, i->flatten entries(A_(0,i)*(sub(Bs, transpose Q_{i})))));
     sy := syz(MP-MQ);
     ell:= rank source sy;
     map(S^1, S^{ell:-2*g+2}, Bs*sy)
     )

TEST ///
  debug needsPackage "ExamplesFreeResolutions"
  S=ZZ/10007[x_0,x_1]
  setRandomSeed("ok")
  g=8
  (P,Q)=(random(S^2,S^g),random(S^2,S^g))
  assert(distinctPointsOfP1(P|Q))
  A=canonicalMultipliers(P,Q)
  s=linearSeriesFromMultipliers((P,Q),A)
  assert(rank source s == g)
///

changeMultiplier=method()
changeMultiplier(Matrix,ZZ) := (A,r) -> (
     	  -- change all multpliers by a factor r
	  -- in applications r is often a root of unity
	  matrix apply(2,i->apply(rank source A,j-> if i==0 then A_(i,j) else r*A_(i,j))))
changeMultiplier(Matrix,ZZ,ZZ) := (A,r,k) -> (
          -- change k of the multpliers by a factor r
	  -- in applications r is often a root of unity
	  matrix apply(2,i->apply(rank source A,j-> 
		    if i==1 and j<k then r* A_(i,j) else A_(i,j)))
	  )     

randLinearSeriesPrym = method()
randLinearSeriesPrym(ZZ,ZZ) := (g,p)->(
    -- this version: only works for half-canonical curves
    ht := 10;
    kk := if p == 0 then QQ else ZZ/p;
    x:=getSymbol "x";
    S:=kk[x_0,x_1];
    while (
        PQ := if p == 0 then (randIntegerMatrix(S, 2, g, ht), randIntegerMatrix(S,2,g,ht))
              else (random(S^2,S^g),random(S^2,S^g));
        not distinctPointsOfP1(PQ_0|PQ_1)
        ) do ();
    --while not distinctPointsOfP1(PQ_0|PQ_1) do PQ=(random(S^2,S^g),random(S^2,S^g));
    A:=canonicalMultipliers(PQ);
    A=changeMultiplier(A,-1,g);
    s:=linearSeriesFromMultipliers((PQ),A);
    s)

randomCanonicalNodalCurve=method()
randomCanonicalNodalCurve(ZZ) := (g)->(
     isPrime(32003);
     kk:=ZZ/32003;
     x:=getSymbol "x";
     S:=kk[x_0,x_1];
     PQ:=(random(S^2,S^g),random(S^2,S^g)); -- 2g points on P^1
     while not distinctPointsOfP1(PQ_0|PQ_1) do PQ=(random(S^2,S^g),random(S^2,S^g));
     A:=canonicalMultipliers(PQ);
     s:=linearSeriesFromMultipliers(PQ,A);
     assert(rank source s==g); 
     t:= getSymbol "t";
     T:=kk[t_0..t_(rank source s - 1)];
     betti(I:=ideal mingens ker map(S,T,s));
     assert (degree I==2*g-2 and genus(T/I)==g);
     L:=(kk,S,PQ,A,s,T);
     (L,I))

randomPrymCanonicalNodalCurve = method()
randomPrymCanonicalNodalCurve Matrix := (s)->(
    -- this version: only works for half-canonical curves
    S := ring s;
    kk := coefficientRing S;
    t:=getSymbol "t"; 
    T:=kk[t_0..t_(rank source s - 1)];
    g := numgens T + 1;
    phi := map(S,T,s);
    B2 := basis(2, T);
    M := phi B2;
    (mons,cfs) := coefficients M;
    I2 := ideal(B2 * sub(syz cfs, T));
    if g >= 9 then return I2;
    B3 := matrix basis(3, comodule I2);
    M3 := phi B3;
    (mons,cfs) =  coefficients M3;
    I3 := ideal(B3 * sub(syz cfs, T));
    I2 + I3
    )
----------------------------------------------------------------
-- end of CNC and PCNC example construction --------------------
----------------------------------------------------------------

bettiCNC = new HashTable from {
    5 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 3, 
        (2,{4},4) => 3, 
        (3,{6},6) => 1
        },
    6 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 6, 
        (2,{3},3) => 5, 
        (2,{4},4) => 5, 
        (3,{5},5) => 6, 
        (4,{7},7) => 1
        },
    7 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 10, 
        (2,{3},3) => 16, 
        (3,{5},5) => 16, 
        (4,{6},6) => 10, 
        (5,{8},8) => 1
        },
    8 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 15, 
        (2,{3},3) => 35, 
        (3,{4},4) => 21, 
        (3,{5},5) => 21, 
        (4,{6},6) => 35, 
        (5,{7},7) => 15, 
        (6,{9},9) => 1
        },
    9 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 21, 
        (2,{3},3) => 64, 
        (3,{4},4) => 70, 
        (4,{6},6) => 70, 
        (5,{7},7) => 64, 
        (6,{8},8) => 21, 
        (7,{10},10) => 1
        },
    10 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 28, 
        (2,{3},3) => 105, 
        (3,{4},4) => 162, 
        (4,{5},5) => 84, 
        (4,{6},6) => 84, 
        (5,{7},7) => 162, 
        (6,{8},8) => 105, 
        (7,{9},9) => 28, 
        (8,{11},11) => 1
        },
    11 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 36, 
        (2,{3},3) => 160, 
        (3,{4},4) => 315, 
        (4,{5},5) => 288, 
        (5,{7},7) => 288, 
        (6,{8},8) => 315, 
        (7,{9},9) => 160, 
        (8,{10},10) => 36, 
        (9,{12},12) => 1
        },
    12 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 45, 
        (2,{3},3) => 231, 
        (3,{4},4) => 550, 
        (4,{5},5) => 693, 
        (5,{6},6) => 330, 
        (5,{7},7) => 330, 
        (6,{8},8) => 693, 
        (7,{9},9) => 550, 
        (8,{10},10) => 231, 
        (9,{11},11) => 45, 
        (10,{13},13) => 1
        },
    13 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 55, 
        (2,{3},3) => 320, 
        (3,{4},4) => 891, 
        (4,{5},5) => 1408, 
        (5,{6},6) => 1155, 
        (6,{8},8) => 1155, 
        (7,{9},9) => 1408, 
        (8,{10},10) => 891, 
        (9,{11},11) => 320, 
        (10,{12},12) => 55, 
        (11,{14},14) => 1
        },
    14 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 66, 
        (2,{3},3) => 429, 
        (3,{4},4) => 1365, 
        (4,{5},5) => 2574, 
        (5,{6},6) => 2860, 
        (6,{7},7) => 1287, 
        (6,{8},8) => 1287, 
        (7,{9},9) => 2860, 
        (8,{10},10) => 2574, 
        (9,{11},11) => 1365, 
        (10,{12},12) => 429, 
        (11,{13},13) => 66, 
        (12,{15},15) => 1
        },
    15 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 78, 
        (2,{3},3) => 560, 
        (3,{4},4) => 2002, 
        (4,{5},5) => 4368, 
        (5,{6},6) => 6006, 
        (6,{7},7) => 4576, 
        (7,{9},9) => 4576, 
        (8,{10},10) => 6006, 
        (9,{11},11) => 4368, 
        (10,{12},12) => 2002, 
        (11,{13},13) => 560, 
        (12,{14},14) => 78, 
        (13,{16},16) => 1
        }
    }

-- Untested.  Keep?
checkCNC = args -> (
    if not instance(args, Sequence) then args = 1:args;
    (lo,hi) := if #args == 0 then (5,13) 
      else if #args == 1 then (args#0,args#0) 
      else if #args == 2 then (args#0, args#1);
    for n from lo to hi do if not bettiCNC#?n then 
      error("only have answers for n in "|toString sort keys bettiCNC);
    for n from lo to hi do (
        B := runCNC n;
        if B =!= bettiCNC#n then (
            << "***ERROR***: betti table for CNC " << n << " is INCORRECT." << endl;
            << "  expected: " << bettiCNC#n << endl;
            << "  actual  : " << B << endl;
            );
    ))

----------------------------------------
-- PCNC example creation and running ---
----------------------------------------

bettiPCNC = new HashTable from {
    9 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 12, 
        (2,{3},3) => 16, 
        (2,{4},4) => 30, 
        (3,{5},5) => 96, 
        (4,{6},6) => 100, 
        (5,{7},7) => 48, 
        (6,{8},8) => 9
        },
    10 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 18, 
        (2,{3},3) => 42, 
        (3,{5},5) => 126, 
        (4,{6},6) => 210, 
        (5,{7},7) => 162, 
        (6,{8},8) => 63, 
        (7,{9},9) => 10
        },
    11 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 25, 
        (2,{3},3) => 80, 
        (3,{4},4) => 70, 
        (3,{5},5) => 112, 
        (4,{6},6) => 350, 
        (5,{7},7) => 400, 
        (6,{8},8) => 245, 
        (7,{9},9) => 80, 
        (8,{10},10) => 11
        },
    12 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 33, 
        (2,{3},3) => 132, 
        (3,{4},4) => 198, 
        (4,{6},6) => 462, 
        (5,{7},7) => 792, 
        (6,{8},8) => 693, 
        (7,{9},9) => 352, 
        (8,{10},10) => 99, 
        (9,{11},11) => 12
        },
    13 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 42, 
        (2,{3},3) => 200, 
        (3,{4},4) => 405, 
        (4,{5},5) => 288, 
        (4,{6},6) => 420, 
        (5,{7},7) => 1296, 
        (6,{8},8) => 1575, 
        (7,{9},9) => 1120, 
        (8,{10},10) => 486, 
        (9,{11},11) => 120, 
        (10,{12},12) => 13
        },
    14 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 52, 
        (2,{3},3) => 286, 
        (3,{4},4) => 715, 
        (4,{5},5) => 858, 
        (5,{7},7) => 1716, 
        (6,{8},8) => 3003, 
        (7,{9},9) => 2860, 
        (8,{10},10) => 1716, 
        (9,{11},11) => 650, 
        (10,{12},12) => 143, 
        (11,{13},13) => 14
        },
    15 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 63, 
        (2,{3},3) => 392, 
        (3,{4},4) => 1155, 
        (4,{5},5) => 1848, 
        (5,{6},6) => 1155, 
        (5,{7},7) => 1584, 
        (6,{8},8) => 4851, 
        (7,{9},9) => 6160, 
        (8,{10},10) => 4851, 
        (9,{11},11) => 2520, 
        (10,{12},12) => 847, 
        (11,{13},13) => 168, 
        (12,{14},14) => 15
        },
    16 => new BettiTally from {
        (0,{0},0) => 1, 
        (1,{2},2) => 75, 
        (2,{3},3) => 520, 
        (3,{4},4) => 1755, 
        (4,{5},5) => 3432, 
        (5,{6},6) => 3575, 
        (5,{7},7) => 1, 
        (6,{7},7) => 1, 
        (6,{8},8) => 6435, 
        (7,{9},9) => 11440, 
        (8,{10},10) => 11583, 
        (9,{11},11) => 7800, 
        (10,{12},12) => 3575, 
        (11,{13},13) => 1080, 
        (12,{14},14) => 195, 
        (13,{15},15) => 16
        }
    }

-- Untested.  Keep?
checkPCNC = args -> (
    if not instance(args, Sequence) then args = 1:args;
    (lo,hi) := if #args == 0 then (9,15) 
      else if #args == 1 then (args#0,args#0) 
      else if #args == 2 then (args#0, args#1);
    for n from lo to hi do if not bettiPCNC#?n then 
      error("only have answers for n in "|toString sort keys bettiPCNC);
    for n from lo to hi do (
        B := runPCNC n;
        if B =!= bettiPCNC#n then (
            << "***ERROR***: betti table for PCNC " << n << " is INCORRECT." << endl;
            << "  expected: " << bettiPCNC#n << endl;
            << "  actual  : " << B << endl;
            );
    ))


-* Documentation section *-
beginDocumentation()

///
  Key
    ExamplesFreeResolutions
  Headline
    examples to benchmark and test free resolution and betti number code
  Description
    Text
    Tree
    Example
  Acknowledgement
  Contributors
  References
  Caveat
  SeeAlso
  Subnodes
///

doc ///
  Key
    (outputIdeal, Ideal)
    outputIdeal
    [outputIdeal, RingName]
    [outputIdeal, IdealName]
  Headline
    output a machine readable version of an ideal with its ring
  Usage
    outputIdeal I
  Inputs
    I:Ideal
    RingName => String
    IdealName => String
  Outputs
    :String
      A string which can be evaluated to obtain an ideal in a later session
  Description
    Text
    Example
      A = ZZ/101[a..f]
      J = ideal(d^2*a + d*e*b + e^2*c, (d+e+f)^3, d^2 - 3 * (a+b)^2)
      str = outputIdeal J
      print str
      value str
      I1 == sub(J, ring I1)
  Caveat
    the variables of the new ring are letters, not the same as the variables you start with.
    This function currently only handles the simplest cases of rings.  This function should
    work in more general circumstances.
  SeeAlso
///

doc ///
  Key
    AGR
    (AGR, ZZ, ZZ, ZZ)
    (AGR, ZZ, ZZ, ZZ, ZZ)
  Headline
    construct a random arithemtically Gorenstein ring (AGR)
  Usage
    AGR(deg, n, deg, characteristic)
    AGR(deg, n, numpowers, characteristic)
  Inputs
    deg:ZZ
      the degree of the homogeneous form used to construct the AGR
    n:ZZ
      the number of variables is $n+1$
    numpowers:ZZ
      in the second for, this refers to the number of powers of linears forms that are summed.
      First, the powers of the variables are used, and then, after all of these are used,
      random linear forms are used.
    characteristic:ZZ
      The ideal will be in a polynomial ring over a prime field with this (prime or zero)  characteristic
  Outputs
    :Ideal
      the inverse system of a random degree {\tt deg} form in $n+1$ variables
      over a prime field of the given characteristic, in the first case, or of a
      sum of {\tt numpowers} degree {\tt deg} powers of either random linear forms or variables
  Description
    Text
      See @TO "InverseSystems::InverseSystems"@ for the definition of the inverse system of a polynomial.
      
      In the first usage, this creates an ideal in a newly created ring of the
      inverse system of a random homogeneous polynomial.  Such ideals (or rather, their quotients)
      are always Arithmetically Gorenstein Rings (AGR's).
      
      Here is an example of the inverse system of a random cubic in 5 variables, versus sums of 
      5, 6, 7, or 8 powers of (essentially ranfom) linear forms
    Example
      I = AGR(3, 4, 101)
      betti res I
    Example
      J = AGR(3, 4, 5, 101)
      minimalBetti J
    Example
      J = AGR(3, 4, 6, 101)
      minimalBetti J
    Example
      J = AGR(3, 4, 7, 101)
      minimalBetti J
    Example
      J = AGR(3, 4, 8, 101)
      minimalBetti J
  SeeAlso
///

-* Test section *-
TEST /// -* ouputIdeal *-
  R = ZZ/101[a..d]
  I = ideal(a^2*b^2-3*c-45, a*b*c*d - 1)
  result = outputIdeal I
  ans = "R1 = (ZZ/101)[a, b, c, d]
I1 = ideal(
    a^2*b^2-3*c-45,
    a*b*c*d-1
    )
"
  assert(result == ans)

///

TEST /// -* ouputIdeal *-
  setRandomSeed 13
  J = AGR(5, 3, 101)
ans = ".R1 = (ZZ/101)[a, b, c, d]
I1 = ideal(
    c^3-25*a^2*d+21*a*b*d-38*b^2*d-34*a*c*d-18*b*c*d+37*c^2*d+21*a*d^2+b*d^2-24*c*d^2+26*d^3,
    b*c^2+45*a^2*d-9*a*b*d+19*b^2*d-41*a*c*d-7*b*c*d+30*c^2*d-43*a*d^2-43*b*d^2-46*c*d^2+40*d^3,
    a*c^2+8*a^2*d-37*a*b*d+19*b^2*d-45*a*c*d-30*b*c*d+31*c^2*d-50*a*d^2-20*b*d^2+10*c*d^2-9*d^3,
    b^2*c+24*a^2*d-11*a*b*d-4*b^2*d-27*a*c*d-21*b*c*d+50*c^2*d-18*a*d^2-23*b*d^2+13*c*d^2+2*d^3,
    a*b*c-29*a^2*d+33*a*b*d-5*b^2*d-9*a*c*d-b*c*d-46*c^2*d+6*a*d^2-14*b*d^2+6*c*d^2-47*d^3,
    a^2*c-37*a^2*d+23*a*b*d-13*b^2*d-13*a*c*d-15*b*c*d-16*c^2*d-9*a*d^2+16*b*d^2-50*c*d^2+24*d^3,
    b^3-42*a^2*d+a*b*d-6*b^2*d+31*a*c*d-19*b*c*d+19*c^2*d+25*a*d^2+35*b*d^2-23*c*d^2+27*d^3,
    a*b^2-28*a^2*d+48*a*b*d-28*b^2*d-16*a*c*d+2*b*c*d+49*c^2*d-21*a*d^2-34*b*d^2+9*c*d^2+6*d^3,
    a^2*b+35*a^2*d+49*a*b*d+8*b^2*d-10*a*c*d-49*b*c*d+4*c^2*d+a*d^2+4*b*d^2-21*c*d^2+38*d^3,
    a^3-31*a^2*d+44*a*b*d-37*b^2*d-10*a*c*d-28*b*c*d+2*c^2*d-45*a*d^2+45*b*d^2+5*c*d^2-22*d^3
    )
."  
  str = "." | outputIdeal J | "."
  assert(ans == str)
  minimalBetti J
///

TEST ///
-*
  restart
*-
  debug needsPackage "ExamplesFreeResolutions"
  exampleFileName "cnc-6"
  assert(exampleFileName("cnc-6", Directory => "./") === "./cnc-6.m2")
  assert(exampleFileName("cnc-6", Directory => "Examples/") === "Examples/cnc-6.m2")
///



end--

-* Development section *-
restart
debug needsPackage "ExamplesFreeResolutions"
check "ExamplesFreeResolutions"

uninstallPackage "ExamplesFreeResolutions"
restart
installPackage "ExamplesFreeResolutions"
viewHelp "ExamplesFreeResolutions"

///
  -- Creating example files for CNC (canonical nodal curves) examples, over finite fields
  -- and PCNC (prym canonical nodal curves).
  restart
  debug needsPackage "ExamplesFreeResolutions"
  dir = "./ExamplesFreeResolutions/ExamplesAndTimings/"

  elapsedTime for i from 3 to 16 do (   -- 168 seconds, Mac M1 Max, 19 March 2022
      setRandomSeed "resolutions";
      makeCNC(i, Directory => dir)
      )

  elapsedTime for i from 17 to 18 do (  -- 495 seconds, Mac M1 Max, 19 March 2022
      setRandomSeed "resolutions";
      makeCNC(i, Directory => dir)
      )

  elapsedTime for i from 9 to 16 do (
      setRandomSeed "resolutions";
      elapsedTime makePCNC(i, 32003, Directory => dir)
      )

  elapsedTime for i from 17 to 24 do (
      setRandomSeed "resolutions";
      elapsedTime makePCNC(i, 32003, Directory => dir)
      )
  
  -- now we create the AGR examples.  This is slightly more complicated in that 
  -- we have quite a few choices for parameters.

  -- First, let's do a grid of degree, number of variables
  for d from 2 to 7 do
  for n from 6 to 10 do (
      setRandomSeed "resolutions";
      elapsedTime makeAGR(d,n,10007, Directory=>dir);
      )
///

TEST ///
  -----------------------------------------
  -- Running and checking all examples ----
  -----------------------------------------
  restart
  debug needsPackage "ExamplesFreeResolutions"
  dir = "./Foo/"
  dir = "./ExamplesFreeResolutions/ExamplesAndTimings/"
  comment = "MES M1 max M2 just pre 1.20 (no builds in background)"
  comment = "MES M1 max M2 1.19.1"
  -- CNC curves
  for i from 6 to 15 do (
      assert(elapsedTime runCNC(i, Directory => dir, Comment => comment) === bettiCNC#i);
      )

  -- PCNC curves
  for i from 9 to 16 do (
      assert(elapsedTime runPCNC(i, 32003, Directory => dir, Comment => comment) === bettiPCNC#i);
      )
  
  -- AGR examples
  deg = 2;
  for nv from 6 to 10 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)

  deg = 3;
  for nv from 6 to 10 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)

  deg = 4;
  for nv from 6 to 10 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)

  deg = 5;
  for nv from 6 to 10 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)
  -- note: (5,10): is not a generic resolution numerically (1 extra syzygy)?

  deg = 6;
  for nv from 6 to 9 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)

  deg = 7;
  for nv from 6 to 9 list elapsedTime runAGR(deg,nv,10007, Directory => dir, Comment => comment)

///


///
  -- This one takes a large amount of memory.
  restart
  debug needsPackage "ExamplesFreeResolutions"
  dir = "./Foo/"
  I = getAGR(6,10,10007, Directory => dir);
  hilbertSeries I  
  reduceHilbert oo
  
  gbTrace=2
  minimalBetti(I, DegreeLimit => 3)
///

///
-- Gr(2,6)
I = Grassmannian(2, 6, CoefficientRing => ZZ/101)
numgens I
numgens ring I
hilbertSeries I

gbTrace=2
minimalBetti(I, DegreeLimit => 3, LengthLimit => 5)

Is this the minimal betti diagram?

            0      1      2      3      4       5       6       7       8       9      10      11      12      13      14      15      16      17      18      19      20      21      22
o8 = total: 1    140   1246   7861  60214  283910  789481 1461922 2175152 4307471 7621818 9173216 7621818 4307471 2175152 1461922  789481  283910   60214    7861    1246     140       1
         0: 1      .      .      .      .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .
         1: .    140   1246   4529   5334       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .
         2: .      .      .   3332  54880  283910  788557 1374758 1380456  382396   70070   16380    1716       .       .       .       .       .       .       .       .       .       .
         3: .      .      .      .      .       .     924   87164  794696 3925075 7550032 9140456 7550032 3925075  794696   87164     924       .       .       .       .       .       .
         4: .      .      .      .      .       .       .       .       .       .    1716   16380   70070  382396 1380456 1374758  788557  283910   54880    3332       .       .       .
         5: .      .      .      .      .       .       .       .       .       .       .       .       .       .       .       .       .       .    5334    4529    1246     140       .
         6: .      .      .      .      .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       .       1

///


///

///
  -- Gr(1,7)
  I = Grassmannian(1, 7, CoefficientRing => ZZ/101)
  numgens I
  numgens ring I
  hilbertSeries I

  gbTrace=2
  minimalBetti(I, DegreeLimit => 3, LengthLimit => 5)

0  1   2    3    4     5     6     7     8     9    10   11   12  13 14 15
o6 = total: 1 70 420 2121 8274 20310 32550 38654 38654 32550 20310 8274 2121 420 70  1
         0: 1  .   .    .    .     .     .     .     .     .     .    .    .   .  .  .
         1: . 70 420  945  924   330     .     .     .     .     .    .    .   .  .  .
         2: .  .   . 1176 7350 19980 30030 26180 12474  2520     .    .    .   .  .  .
         3: .  .   .    .    .     .  2520 12474 26180 30030 19980 7350 1176   .  .  .
         4: .  .   .    .    .     .     .     .     .     .   330  924  945 420 70  .
         5: .  .   .    .    .     .     .     .     .     .     .    .    .   .  .  1
///
         
kk = ZZ/101
S = kk[a..d]
I = ideal(a^3-b, a*b^2-c^2-c)
gens gb I
gbTrace=2
F = res(I, FastNonminimal => true)
F.dd

kk = ZZ/101
S = kk[a..d]
I = ideal(a^3-b, a*b^2-c^2-c)
M = S^1/I
gbTrace=2
F = res(M, FastNonminimal => true)
F.dd
minimalBetti M -- should not be allowed!!  BUG

kk = QQ
S = kk[a..d]
I = ideal(a^3-b, a*b^2-c^2-c)
gens gb I
gbTrace=2
F = res(I, Strategy => 4.1)
F.dd

restart
kk = ZZ/101
S = kk[a..d]
I = ideal(a^3-b^3, a*b^2-c*d^2, b*c^4-d*a^4)
M = S^1/I
gbTrace=2
F = res(M, FastNonminimal => true)
F.dd
peek M.cache
debug Core
peek M.cache#(ResolutionContext{})
peek M.cache#(ResolutionContext{}).Result.Resolution.RawComputation
debug Core
peek M.cache.resolutionNonminimal.Resolution
minimalBetti M -- BUG: recomputes resolution!  -- why?
F = res(M, FastNonminimal => true, StopBeforeComputation => true) -- this seems to work
minimalBetti M -- this recompputes
res(M, FastNonminimal => true)
F = res(M, FastNonminimal => true) -- this seems to work
minimalBetti M -- this recompputes

  -- 

kk = ZZ/101
S = kk[a..d]
I = ideal(a^3-b^3, a*b^2-c*d^2, b*c^4-d*a^4)
M = S^1/I
gbTrace=2
F = res(M)
F.dd
peek M.cache
debug Core
peek M.cache#(ResolutionContext{}).Result.Resolution.RawComputation

restart
needsPackage "ExamplesFreeResolutions"
I = CNC 14;
R = ring I
F = res(I, FastNonminimal => true)
betti F
minimalBetti I
